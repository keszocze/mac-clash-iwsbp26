{-# LANGUAGE DuplicateRecordFields, RecordWildCards, AllowAmbiguousTypes #-}
module MAC.Mealy where

import Clash.Prelude
import Clash.Class.Counter

import Data.Maybe

import qualified Prelude

import MAC.Util
import qualified MAC.Util.FullAdder as FA


data Stage = Ready | Multiplying | Accumulating deriving (Show, Generic, NFDataX)

data MACConfig = MACConfig
  { useModuleFullAdder :: Bool,
    useState :: Bool,
    useVector :: Bool,
    useRotation :: Bool,
    useOneHot :: Bool
  }
  deriving (Show, Bounded)


defaultConfig = MACConfig {
  useModuleFullAdder = True,
  useState = False,
  useVector = False,
  useRotation = True,
  useOneHot = False
}

-- type MACState n = MACState' n n

data MACState (n :: Nat) (m :: Nat) = MACState {
  stage :: Stage,
  x :: Unsigned n,
  y :: Unsigned m,
  product :: BitVector (n+m), -- TODO vs. Vec
  accumulator :: BitVector (n+m), -- TODO vs Vec
  xCounter :: Index n, -- TODO vs. OneHot
  yCounter :: Index m, -- TODO vs. OneHot
  accumulateCounter :: Index (n+m), -- TODO vs. OneHotCounter
  carry :: Bit
} deriving (Show, Generic, NFDataX)


initialState :: forall n m. (KnownNat n, 1 <= n, KnownNat m, 1 <= m) => MACState n m
initialState = MACState {
  stage = Ready,
  x=0,
  y=0,
  product = 0,
  accumulator = 0,
  xCounter = countMin :: Index n,
  yCounter = countMin :: Index m,
  accumulateCounter = countMin :: Index (n+m),
  carry = 0
}

data MACInput (n :: Nat) (m :: Nat) = MACInput {
  values :: Maybe (Unsigned n, Unsigned m),
  newAcc :: Maybe (Unsigned (n+m))
} deriving (Show, Generic, NFDataX)

type MACInput' n = MACInput n n

data MACOutput n m = MACOutput {
  product :: Maybe (BitVector (n+m)),
  accumulated :: Maybe (BitVector (n+m))
} deriving (Generic, NFDataX)

instance (KnownNat n, KnownNat m) => Show (MACOutput n m) where
  show MACOutput{..} = "(product="<> maybe "" show product <> ", accumulated=" <> maybe "" show accumulated <> ")"


mac :: forall dom n m.
  (HiddenClockResetEnable dom,
  KnownNat n, 1 <= n,
  KnownNat m, 1 <= m) =>
  Signal dom (MACInput n m) -> Signal dom (MACOutput n m)
mac = mac' defaultConfig

mac' :: forall dom n m.
  (HiddenClockResetEnable dom,
  KnownNat n, 1 <= n,
  KnownNat m, 1 <= m) =>
  MACConfig ->
  Signal dom (MACInput n m) -> Signal dom (MACOutput n m)
mac' cfg = mealy @dom (macMealy @n @m cfg) (initialState @n @m)

-- TODO herausfinden, wieso diese Spezialisierung nicht funktioniert
-- type MACOutput n = MACOutput' n n
-- macMealy :: forall n. (KnownNat n) => forall n. MACState n  -> MACInput n -> (MACState n, MACOutput n)
-- macMealy = macMealy' @n @n

macMealy :: forall n m. (KnownNat n, KnownNat m, 1 <= n,  1<= m) => MACConfig -> MACState n m -> MACInput n m -> (MACState n m, MACOutput n m)
macMealy MACConfig{useModuleFullAdder} state@MACState{..} MACInput{values, newAcc}  = (state', output state')
  where
    fullAdder = if useModuleFullAdder then FA.fullAdderModule else FA.fullAdder
    state' = compute stateStart

    stateStart = case values of
      Just (x,y) -> stateNewAcc{stage=Multiplying, xCounter=countMin, yCounter=countMin, x=(fst. fromJust) values, y=(snd . fromJust) values}
      Nothing -> stateNewAcc


    stateNewAcc = state{accumulator= maybe accumulator bitCoerce newAcc}

    compute state@MACState{..} = case stage of
      Ready -> state
      Multiplying -> multiply state
      Accumulating -> accumulate state

    accumulate st@MACState{..} = let
      (carry', sum) = fullAdder (accumulator ! accumulateCounter) (product ! accumulateCounter) carry
      accumulator' = replaceBit accumulateCounter sum accumulator
      (stage', accumulateCounter') = case countSuccOverflow accumulateCounter of
        (True, a) -> (Ready, a)
        (False, a) -> (Accumulating, a)
      in st{
        stage=stage',
        carry=carry',
        accumulator=accumulator',
        accumulateCounter=accumulateCounter'
        }

    multiply st@MACState{..} =
      let currentRoundDone = xCounter == countMax
          inLastRound = yCounter == countMax
          multiplicationDone = currentRoundDone .&. inLastRound

          resetDistance = fromInteger ((natToInteger @n) - 1)

          rotateFwd = (`rotateR` 1)
          rotateBack = (`rotateL` resetDistance)

          a = (lsb x) .&. (lsb y)
          b = lsb product
          (carryOut, sum) = fullAdder a b carry

          -- just naming it to avoid magic numbers
          modifyIndex = 0 :: Bit

          productWithSum = replaceBit modifyIndex sum product
          productWithSumAndCarry = replaceBit modifyIndex carryOut (rotateFwd productWithSum)

          product' = case (currentRoundDone, inLastRound) of
            -- simply advance to the next bit within x and adjust the product accordingly
            (False, _) -> rotateFwd productWithSum
            -- we need to advance to the next bit of y and have to reset the product accordingly
            (True, False) -> rotateBack productWithSumAndCarry
            -- let d = resetDist' x
            --  in productWithSumAndCarry `rotateL` d
            -- the multiplication is done and we need one additional shift to put the LSB in the correct position
            (True, True) -> rotateFwd productWithSumAndCarry

          x' = x `rotateR` 1 -- continously shift through;
          -- only advance to the next y when one round is done
          y' = if currentRoundDone then y `shiftR` 1 else y
        in st
          {
            x=x',
            y=y',
            product = product',
            xCounter = countSucc xCounter,
            yCounter = countSucc yCounter,
            carry = if currentRoundDone then 0 else carryOut,
            stage = if multiplicationDone then Accumulating else Multiplying
          }

    output MACState{stage, product, accumulator} = case stage of
      Ready -> MACOutput (Just product) (Just accumulator)
      Multiplying -> MACOutput Nothing (Just accumulator)
      Accumulating -> MACOutput (Just product) Nothing

is :: [MACInput 2 3]
is = (MACInput {values = Just (2,3), newAcc = Nothing}):
  (Prelude.replicate 10 (MACInput {values = Nothing, newAcc = Nothing})) Prelude.++
  [(MACInput {values = Just (1,2), newAcc = Nothing})] Prelude.++
  Prelude.repeat (MACInput {values = Nothing, newAcc = Nothing})


topEntity = exposeClockResetEnable $ mac @System @2 @3

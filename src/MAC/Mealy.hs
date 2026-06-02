{-# LANGUAGE DuplicateRecordFields, RecordWildCards, DerivingVia, AllowAmbiguousTypes, UndecidableInstances, FlexibleInstances #-}
module MAC.Mealy where

import Clash.Prelude hiding (sum, product)
import Clash.Class.Counter

import Data.Maybe

import qualified Prelude

import MAC.Class.Storage
import MAC.Types
import MAC.Types.BVec
import MAC.Util
import MAC.Util.OneHotCounter
import qualified MAC.Util.FullAdder as FA

import Debug.Trace





--allConfigs = [ MACConfig a b c d e | a  <- [True, False], b  <- [True, False] , c  <- [True, False], d  <- [True, False] , e  <- [True, False]]
-- TODO nachher dann wirklich alles unterstützen
allConfigs = [ MACConfig useModuleAdder useState useVector useRotation useOneHot |
  useModuleAdder  <- [False, True],
  useState  <- [False] ,
  useVector  <- [False, True],
  useRotation  <- [False, True] ,
  useOneHot  <- [False, True]
  ]

multiplicationDelay :: forall n m. (KnownNat n, KnownNat m) => Int
multiplicationDelay = (nInt * mInt) - 1
  where
    nInt = natToNum @n @Int
    mInt = natToNum @m @Int

accumulationDelay :: forall n m. (KnownNat n, KnownNat m) => Int
accumulationDelay = nInt + mInt
  where
    nInt = natToNum @n @Int
    mInt = natToNum @m @Int

totalDelay :: forall n m. (KnownNat n, KnownNat m) => Int
totalDelay = multiplicationDelay @n @m + accumulationDelay @n @m


describe :: MACConfig -> String
describe MACConfig {..} =
  (if useModuleFullAdder then "module adder" else "inline adder") <> " / " <>
  (if useState then "state" else "mealy machine") <> " / " <>
  (if useRotation then "rotate" else "indexing") <> " / " <>
  (if useVector then "Vec" else "BitVector") <> " / " <>
  (if useOneHot then "OneHotCounter" else "IndexCounter")

defaultConfig = MACConfig {
  useModuleFullAdder = True,
  useState = False,
  useVector = False,
  useRotation = False,
  useOneHot = False
}






data MACInput (n :: Nat) (m :: Nat) = MACInput {
  values :: Maybe (Unsigned n, Unsigned m),
  newAcc :: Maybe (Unsigned (n+m))
} deriving (Show, Generic, NFDataX)

type MACInput' n = MACInput n n


data MACOutput (n :: Nat) (m :: Nat) = MACOutput {
  product :: Maybe (Unsigned (n+m)),
  accumulated :: Maybe (Unsigned (n+m))
} deriving (Eq, Generic, NFDataX)

instance (KnownNat n, KnownNat m) => Show (MACOutput n m) where
  show MACOutput{..} = "(product="<> maybe "" show product <> ", accumulated=" <> maybe "" show accumulated <> ")"

mac' :: forall dom n m.
  (
    HiddenClockResetEnable dom,
    KnownNat n, 1 <= n,  KnownNat m, 1 <= m
  )
  =>
    MACConfig ->
    Signal dom (MACInput n m) ->
    Signal dom (MACOutput n m)
mac' MACConfig{..} =
  if useRotation
    then
      if useVector
        then
          if useOneHot
            then
              let aFun = accumulateRotate @n @m @OneHotCounter @BVec fullAdder
                  mFun = mulRotate @n @m @OneHotCounter @BVec fullAdder
              in mealy @dom (macMealy @n @m @OneHotCounter aFun mFun) (initialState @n @m)
            else
              let aFun = accumulateRotate @n @m @Index @BVec fullAdder
                  mFun = mulRotate @n @m @Index fullAdder
              in mealy @dom (macMealy @n @m @Index @BVec aFun mFun) (initialState @n @m)
        else
          if useOneHot
            then
              let aFun = accumulateRotate @n @m @OneHotCounter @BitVector fullAdder
                  mFun = mulRotate @n @m @OneHotCounter @BitVector fullAdder
              in mealy @dom (macMealy @n @m @OneHotCounter aFun mFun) (initialState @n @m)
            else
              let aFun = accumulateRotate @n @m @Index @BitVector fullAdder
                  mFun = mulRotate @n @m @Index @BitVector fullAdder
              in mealy @dom (macMealy @n @m @Index @BitVector aFun mFun) (initialState @n @m)
    else
      if useVector
        then
          if useOneHot
            then
              let aFun = accumulateIndexing @n @m @OneHotCounter @BVec fullAdder
                  mFun = mulIndexing @n @m @OneHotCounter @BVec fullAdder
              in mealy @dom (macMealy @n @m @OneHotCounter aFun mFun) (initialState @n @m)
            else
              let aFun = accumulateIndexing @n @m @Index @BVec fullAdder
                  mFun = mulIndexing @n @m @Index fullAdder
              in mealy @dom (macMealy @n @m @Index @BVec aFun mFun) (initialState @n @m)
        else
          if useOneHot
            then
              let aFun = accumulateIndexing @n @m @OneHotCounter @BitVector fullAdder
                  mFun = mulIndexing @n @m @OneHotCounter @BitVector fullAdder
              in mealy @dom (macMealy @n @m @OneHotCounter aFun mFun) (initialState @n @m)
            else
              let aFun = accumulateIndexing @n @m @Index @BitVector fullAdder
                  mFun = mulIndexing @n @m @Index @BitVector fullAdder
              in mealy @dom (macMealy @n @m @Index @BitVector aFun mFun) (initialState @n @m)
  where
    fullAdder = if useModuleFullAdder then FA.fullAdderModule else FA.fullAdder



-- TODO herausfinden, wieso diese Spezialisierung nicht funktioniert
-- type MACOutput n = MACOutput' n n
-- macMealy :: forall n. (KnownNat n) => forall n. MACState n  -> MACInput n -> (MACState n, MACOutput n)
-- macMealy = macMealy' @n @n
-- TODO man kann constraints in types zusammenfassen
macMealy :: forall n m counterType storageType. (
    KnownNat n, KnownNat m, 1 <= n,  1<= m,
    Counter (counterType n), Counter (counterType m), Counter (counterType (n+m)),
    NFDataX (counterType n), NFDataX (counterType m), NFDataX (counterType (n+m)),
    BitSize (storageType (n + m)) ~ (n + m),
    BitPack (storageType (n+m)),
    Storage (storageType (n+m)),
    Show (storageType (n+m)),
    Show (counterType n), Show (counterType m), Show (counterType (n+m))
  ) =>
    AccumFun n m counterType storageType -> MulFun n m counterType storageType ->
    MACState n m counterType storageType->
    MACInput n m ->
    (MACState n m counterType storageType, MACOutput n m)
    -- TODO hier gucken, was ich aus dem initial MACState eigentlich alles wirklich brauch
macMealy accumulateFun multiplyFun state@MACState{..} MACInput{values, newAcc}  = (state', output state')
  where

    state' = compute stateStart

    stateStart = case values of
      Just (x,y) -> stateNewAcc{stage=Multiplying, xCounter=countMin, yCounter=countMin, accumulateCounter=countMin, x=x, y=y, product=zero}
      Nothing -> stateNewAcc


    stateNewAcc = state{accumulator= maybe accumulator bitCoerce newAcc}

    compute state@MACState{..} = case stage of
      Ready -> state
      Multiplying -> multiplyFun state
      Accumulating -> accumulateFun state


    output MACState{stage, product, accumulator, x, y} = case stage of
      Ready -> (MACOutput (Just $ bitCoerce product) (Just $ bitCoerce accumulator))
      Multiplying -> MACOutput Nothing (Just $ bitCoerce accumulator)
      Accumulating -> MACOutput Nothing Nothing

is :: [MACInput 2 2]
is = (MACInput {values = Just (1,1), newAcc = Nothing}):
  (Prelude.replicate 15 (MACInput {values = Nothing, newAcc = Nothing})) Prelude.++
  [(MACInput {values = Just (1,2), newAcc = Nothing})] Prelude.++
  Prelude.repeat (MACInput {values = Nothing, newAcc = Nothing})


accumulateIndexing :: forall n m counterType storageType.
  (
    KnownNat n, KnownNat m,
    BitPack (storageType (n + m)),
    Storage (storageType (n + m)),
    Counter (counterType (n + m)),
    Enum (counterType (n + m))
  ) =>
  (Bit -> Bit -> Bit -> (Bit, Bit)) ->
  MACState n m counterType storageType ->
  MACState n m counterType storageType
accumulateIndexing fullAdder st@MACState{..} =  let
    a = accumulator ! accumulateCounter
    b = product ! accumulateCounter
    (carry', sum) = fullAdder a b carry
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

-- * beide Addierer
-- * beide Counter (wird gar nicht explizit verwendet)
-- * beide storages
accumulateRotate :: forall n m counterType storageType.
  (
    KnownNat n, KnownNat m,
    BitPack (storageType (n + m)),
    Storage (storageType (n + m)),
    Counter (counterType (n + m))
  ) =>
  (Bit -> Bit -> Bit -> (Bit, Bit)) ->
  MACState n m counterType storageType ->
  MACState n m counterType storageType
accumulateRotate fullAdder st@MACState{..} =  let
    a = lsb accumulator
    b = lsb product
    (carry', sum) = fullAdder a b carry
    accumulator' = replaceBit (0 :: Bit) sum accumulator
    accumulator''= advance accumulator'
    product' = advance product

    (stage', accumulateCounter') = case countSuccOverflow accumulateCounter of
        (True, acc) -> (Ready, acc)
        (False, acc) -> (Accumulating, acc)

    in st{
      stage=stage',
      carry=carry',
      accumulator=accumulator'',
      accumulateCounter=accumulateCounter',
      product = product'
      }



counterToEnum :: forall n cnt. (KnownNat n, Counter cnt, Enum cnt)  => cnt -> Index n
counterToEnum = toEnum . fromEnum

mulIndexing :: forall n m counterType storageType.
  (
      KnownNat n, KnownNat m, 1 <= n, 1 <= m, 1 <= n + m,
      Counter (counterType n), Counter (counterType m),
      BitPack (storageType (n + m)),
      Storage (storageType (n + m)),
      Enum (counterType n), Enum (counterType m)
  ) =>
  (Bit -> Bit -> Bit -> (Bit, Bit)) ->
  MACState n m counterType storageType ->
  MACState n m counterType storageType
mulIndexing fullAdder st@MACState{..} =
  let (currentRoundDone, xCounter') = countSuccOverflow xCounter
      (inLastRound, yCounter')  = countSuccOverflow yCounter
      multiplicationDone = currentRoundDone .&. inLastRound

      xIndex = counterToEnum @n xCounter
      yIndex = counterToEnum @m yCounter
      productIndex = add xIndex yIndex
      modifyWithCarryIndex = add productIndex (1 :: Index 2)


      a = (x ! xIndex) .&. (y ! yIndex)
      b = product ! productIndex
      (carryOut, sum) = fullAdder a b carry


      productWithSum = replaceBit productIndex sum product
      productWithSumAndCarry = replaceBit modifyWithCarryIndex carryOut productWithSum

      product' = case (currentRoundDone, inLastRound) of
        -- simply advance to the next bit within x and adjust the product accordingly
        (False, _) -> productWithSum
        -- we need to advance to the next bit of y and have to reset the product accordingly
        (True, False) -> productWithSumAndCarry
        -- the multiplication is done and we need one additional shift to put the LSB in the correct position
        (True, True) -> productWithSumAndCarry

      -- only advance to the next y when one round is done
      (yCounter'', carry') = if currentRoundDone then
          (yCounter', 0)
        else
          (yCounter, carryOut)

      stage' = if multiplicationDone then Accumulating else Multiplying
    in st
      {
        product = product',
        xCounter = xCounter',
        yCounter = yCounter'',
        carry = carry',
        stage = stage'
      }


-- kann
-- * beide Addierer
-- * beide Counter
-- * beide storages
mulRotate :: forall n m counterType storageType.
  (
      KnownNat n, KnownNat m,
      Counter (counterType n), Counter (counterType m),
      BitPack (storageType (n + m)),
      Storage (storageType (n + m))
  ) =>
  (Bit -> Bit -> Bit -> (Bit, Bit)) ->
  MACState n m counterType storageType ->
  MACState n m counterType storageType
mulRotate fullAdder st@MACState{..} =
  let (currentRoundDone, xCounter') = countSuccOverflow xCounter
      (inLastRound, yCounter')  = countSuccOverflow yCounter
      multiplicationDone = currentRoundDone .&. inLastRound

      resetDistance = (natToNum @n @Int) - 1

      rotateFwd = advance
      rotateBack = reset resetDistance

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
        -- the multiplication is done and we need one additional shift to put the LSB in the correct position
        (True, True) -> rotateFwd productWithSumAndCarry

      x' = x `rotateR` 1 -- continously shift through;
      -- only advance to the next y when one round is done
      (y', yCounter'', carry') = if currentRoundDone then
          (y `shiftR` 1, yCounter', 0)
        else
          (y, yCounter, carryOut)

      stage' = if multiplicationDone then Accumulating else Multiplying
    in st
      {
        x=x',
        y=y',
        product = product',
        xCounter = xCounter',
        yCounter = yCounter'',
        carry = carry',
        stage = stage'
      }

{-# LANGUAGE DuplicateRecordFields, RecordWildCards, DerivingVia, AllowAmbiguousTypes, UndecidableInstances, FlexibleInstances #-}
module MAC.Mealy where

import Clash.Prelude hiding (sum, product)
import Clash.Class.Counter

import Data.Maybe

import qualified Prelude

import MAC.Util
import MAC.Util.OneHotCounter
import qualified MAC.Util.FullAdder as FA

import Debug.Trace


data Stage = Ready | Multiplying | Accumulating deriving (Show, Generic, NFDataX)

type AccumFun n m storageType = Bit -> storageType (n+m) -> storageType (n+m) -> (Bit, storageType (n+m), storageType (n+m))
type MulFun n m counterType storageType = MACState n m counterType storageType -> MACState n m counterType storageType

data MACConfig = MACConfig
  { useModuleFullAdder :: Bool,
    useState :: Bool,
    useVector :: Bool,
    useRotation :: Bool,
    useOneHot :: Bool
  }
  deriving (Show, Bounded)

class Storage a where
  zero :: a
  advance :: a -> a
  reset :: Enum e => e -> a -> a

newtype BVec (n :: Nat) = BVec (Vec n Bit)
  deriving (Generic, NFDataX) via (Vec n Bit)
  deriving (Show, BitPack) via (Vec n Bit)



instance (KnownNat n) => Storage (BVec n) where
  zero = BVec (replicate (SNat @n) (0 :: Bit))
  advance (BVec v) = BVec $ v `rotateRight` (1 :: Bit)
  reset e (BVec v) = BVec $ v `rotateLeft` e


instance (KnownNat n) => Storage (BitVector n) where
  zero = 0b0
  advance bv = bv `rotateR` 1
  reset e bv = bv `rotateL` (fromEnum e)

--allConfigs = [ MACConfig a b c d e | a  <- [True, False], b  <- [True, False] , c  <- [True, False], d  <- [True, False] , e  <- [True, False]]
-- TODO nachher dann wirklich alles unterstützen
allConfigs = [ MACConfig useModuleAdder useState useVector useRotation useOneHot |
  useModuleAdder  <- [False, True],
  useState  <- [False] ,
  useVector  <- [False, True],
  useRotation  <- [True] ,
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
  useRotation = True,
  useOneHot = False
}



data MACState (n :: Nat) (m :: Nat) counterType storageType = MACState {
  stage :: Stage,
  -- TODO x,y auch in den storageType wrappen
  x :: Unsigned n,
  y :: Unsigned m,
  product :: storageType (n+m),
  accumulator :: storageType (n+m),
  xCounter :: counterType n,
  yCounter :: counterType m,
  accumulateCounter :: counterType (n+m),
  carry :: Bit
} deriving (Generic)

-- Benötigt UndecidableInstances
deriving instance (
  KnownNat n,
  KnownNat m,
  NFDataX (counterType n),
  NFDataX (counterType m),
  NFDataX (counterType (n+m)),
  NFDataX (storageType (n+m))
  ) => NFDataX (MACState n m counterType storageType)

deriving instance (
  KnownNat n,
  KnownNat m,
  Show (counterType n),
  Show (counterType m),
  Show (counterType (n+m)),
  Show (storageType (n+m))
  ) => Show (MACState n m counterType storageType)


initialState :: forall n m counterType storageType.
  (
    KnownNat n, 1 <= n, KnownNat m, 1 <= m,
    Counter (counterType n), Counter (counterType m), Counter (counterType (n+m)),
    Storage (storageType (n+m))
  )
  => MACState n m counterType storageType
initialState = MACState {
  stage = Ready,
  x=0,
  y=0,
  product = zero,
  accumulator = zero,
  xCounter = countMin :: (counterType n),
  yCounter = countMin :: (counterType m),
  accumulateCounter = countMin :: (counterType (n+m)),
  carry = 0
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
          let aFun = accumulateRotate @n @m @BVec fullAdder in
          if useOneHot
            then
              let mFun = mulRotate @n @m @OneHotCounter @BVec fullAdder
              in mealy @dom (macMealy @n @m @OneHotCounter aFun mFun) (initialState @n @m)
            else
              let mFun = mulRotate @n @m @Index fullAdder
              in mealy @dom (macMealy @n @m @Index @BVec aFun mFun) (initialState @n @m)
        else
          let aFun = accumulateRotate @n @m @BitVector fullAdder in
          if useOneHot
            then
              let mFun = mulRotate @n @m @OneHotCounter @BitVector fullAdder
              in mealy @dom (macMealy @n @m @OneHotCounter aFun mFun) (initialState @n @m)
            else
              let mFun = mulRotate @n @m @Index @BitVector fullAdder
              in mealy @dom (macMealy @n @m @Index @BitVector aFun mFun) (initialState @n @m)
    else undefined
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
    Storage (storageType (n+m))
  ) =>
    AccumFun n m storageType -> MulFun n m counterType storageType ->
    MACState n m counterType storageType->
    MACInput n m ->
    (MACState n m counterType storageType, MACOutput n m)
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
      Accumulating -> accumulate state

    accumulate st@MACState{..} = let

      (carry', product', accumulator') = accumulateFun carry product accumulator

      (stage', accumulateCounter') = case countSuccOverflow accumulateCounter of
        (True, a) -> (Ready, a)
        (False, a) -> (Accumulating, a)

      in st{
        stage=stage',
        carry=carry',
        accumulator=accumulator',
        accumulateCounter=accumulateCounter',
        product = product'
        }

    output MACState{stage, product, accumulator} = case stage of
      Ready -> MACOutput (Just $ bitCoerce product) (Just $ bitCoerce accumulator)
      Multiplying -> MACOutput Nothing (Just $ bitCoerce accumulator)
      Accumulating -> MACOutput Nothing Nothing

is :: [MACInput 3 3]
is = (MACInput {values = Just (1,2), newAcc = Nothing}):
  (Prelude.replicate 15 (MACInput {values = Nothing, newAcc = Nothing})) Prelude.++
  [(MACInput {values = Just (1,2), newAcc = Nothing})] Prelude.++
  Prelude.repeat (MACInput {values = Nothing, newAcc = Nothing})


-- kann
-- * beide Addierer
-- * beide Counter (wird gar nicht explizit verwendet)
-- * beide storages
accumulateRotate :: forall n m storageType.
  (
    KnownNat n, KnownNat m,
    BitPack (storageType (n + m)),
    Storage (storageType (n+m))
  ) =>
  (Bit -> Bit -> Bit -> (Bit, Bit)) ->
    AccumFun n m storageType
accumulateRotate fullAdder carry prod acc =  let
    a = lsb acc
    b = lsb prod
    (carry', sum) = fullAdder a b carry
    accumulator' = replaceBit (0 :: Bit) sum acc
    accumulator''= advance accumulator'
    product' = advance prod
  in (carry', product', accumulator'')

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

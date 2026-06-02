{-# LANGUAGE DuplicateRecordFields, RecordWildCards, DerivingVia, AllowAmbiguousTypes, UndecidableInstances, FlexibleInstances #-}
module MAC.Mealy where

import Clash.Prelude hiding (sum, product)
import Clash.Class.Counter

import Data.Maybe

import qualified Prelude

import MAC.Access.Indexing.Mealy
import MAC.Access.Rotating.Mealy
import MAC.Class.Storage
import MAC.Types
import MAC.Types.BVec
import MAC.Types.Internal
import MAC.Types.Config
import MAC.Types.State
import MAC.Util
import MAC.Util.OneHotCounter
import qualified MAC.Util.FullAdder as FA

import Debug.Trace


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
    Config ->
    Signal dom (MACInput n m) ->
    Signal dom (MACOutput n m)
mac' Config{..} =
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
-- macMealy :: forall n. (KnownNat n) => forall n. State n  -> MACInput n -> (State n, MACOutput n)
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
    State n m counterType storageType->
    MACInput n m ->
    (State n m counterType storageType, MACOutput n m)
    -- TODO hier gucken, was ich aus dem initial State eigentlich alles wirklich brauch
macMealy accumulateFun multiplyFun state@State{..} MACInput{values, newAcc}  = (state', output state')
  where

    state' = compute stateStart

    stateStart = case values of
      Just (x,y) -> stateNewAcc{stage=Multiplying, xCounter=countMin, yCounter=countMin, accumulateCounter=countMin, x=x, y=y, product=zero}
      Nothing -> stateNewAcc


    stateNewAcc = state{accumulator= maybe accumulator bitCoerce newAcc}

    compute state@State{..} = case stage of
      Ready -> state
      Multiplying -> multiplyFun state
      Accumulating -> accumulateFun state


    output State{stage, product, accumulator, x, y} = case stage of
      Ready -> (MACOutput (Just $ bitCoerce product) (Just $ bitCoerce accumulator))
      Multiplying -> MACOutput Nothing (Just $ bitCoerce accumulator)
      Accumulating -> MACOutput Nothing Nothing

is :: [MACInput 2 2]
is = (MACInput {values = Just (1,1), newAcc = Nothing}):
  (Prelude.replicate 15 (MACInput {values = Nothing, newAcc = Nothing})) Prelude.++
  [(MACInput {values = Just (1,2), newAcc = Nothing})] Prelude.++
  Prelude.repeat (MACInput {values = Nothing, newAcc = Nothing})



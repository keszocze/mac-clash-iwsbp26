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
import qualified FullAdder as FA


mkMAC :: forall dom n m.
  (
    HiddenClockResetEnable dom,
    KnownNat n, 1 <= n,  KnownNat m, 1 <= m
  )
  =>
    Config ->
    Signal dom (Input n m) ->
    Signal dom (Output n m)
-- we ignore the state/Mealy flag as this has been already decided here
mkMAC Config{useModuleFullAdder, useRotation, useVector, useOneHot} =
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
-- type Output n = Output' n n
-- macMealy :: forall n. (KnownNat n) => forall n. State n  -> Input n -> (State n, Output n)
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
    (State n m counterType storageType -> State n m counterType storageType) ->
    (State n m counterType storageType -> State n m counterType storageType) ->
    State n m counterType storageType->
    Input n m ->
    (State n m counterType storageType, Output n m)
    -- TODO hier gucken, was ich aus dem initial State eigentlich alles wirklich brauch
macMealy accumulateFun multiplyFun state@State{..} Input{values, newAcc}  = (state', output state')
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
      Ready -> (Output (Just $ bitCoerce product) (Just $ bitCoerce accumulator))
      Multiplying -> Output Nothing (Just $ bitCoerce accumulator)
      Accumulating -> Output Nothing Nothing

{-# LANGUAGE DuplicateRecordFields, RecordWildCards, DerivingVia, AllowAmbiguousTypes, UndecidableInstances, FlexibleInstances #-}
module MAC.Monad where

import Clash.Prelude hiding (sum, product)
import Clash.Class.Counter

import qualified Control.Monad.State.Strict as ST
import Control.Monad.Extra

import MAC.Access.Indexing.Mealy
import MAC.Access.Rotating.Mealy
import MAC.Class.Storage
import MAC.Constraints
import MAC.Types

import qualified FullAdder as FA

type S (n :: Nat) (m :: Nat) counterType storageType = ST.State (State n m counterType storageType)


mkMAC :: forall dom n m.
  (
    HiddenClockResetEnable dom,
    NatConstraints n m
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
              in mealyS @dom (macMonad @n @m @OneHotCounter aFun mFun) (initialState @n @m)
            else
              let aFun = accumulateRotate @n @m @Index @BVec fullAdder
                  mFun = mulRotate @n @m @Index fullAdder
              in mealyS @dom (macMonad @n @m @Index @BVec aFun mFun) (initialState @n @m)
        else
          if useOneHot
            then
              let aFun = accumulateRotate @n @m @OneHotCounter @BitVector fullAdder
                  mFun = mulRotate @n @m @OneHotCounter @BitVector fullAdder
              in mealyS @dom (macMonad @n @m @OneHotCounter aFun mFun) (initialState @n @m)
            else
              let aFun = accumulateRotate @n @m @Index @BitVector fullAdder
                  mFun = mulRotate @n @m @Index @BitVector fullAdder
              in mealyS @dom (macMonad @n @m @Index @BitVector aFun mFun) (initialState @n @m)
    else
      if useVector
        then
          if useOneHot
            then
              let aFun = accumulateIndexing @n @m @OneHotCounter @BVec fullAdder
                  mFun = mulIndexing @n @m @OneHotCounter @BVec fullAdder
              in mealyS @dom (macMonad @n @m @OneHotCounter aFun mFun) (initialState @n @m)
            else
              let aFun = accumulateIndexing @n @m @Index @BVec fullAdder
                  mFun = mulIndexing @n @m @Index fullAdder
              in mealyS @dom (macMonad @n @m @Index @BVec aFun mFun) (initialState @n @m)
        else
          if useOneHot
            then
              let aFun = accumulateIndexing @n @m @OneHotCounter @BitVector fullAdder
                  mFun = mulIndexing @n @m @OneHotCounter @BitVector fullAdder
              in mealyS @dom (macMonad @n @m @OneHotCounter aFun mFun) (initialState @n @m)
            else
              let aFun = accumulateIndexing @n @m @Index @BitVector fullAdder
                  mFun = mulIndexing @n @m @Index @BitVector fullAdder
              in mealyS @dom (macMonad @n @m @Index @BitVector aFun mFun) (initialState @n @m)
  where
    fullAdder = if useModuleFullAdder then FA.fullAdderModule else FA.fullAdder

macMonad :: forall n m counterType storageType.
  (
    NatConstraints n m,
    ConstraintNM n m Counter counterType,
    StorageConstraints n m storageType
  ) =>
  (State n m counterType storageType -> State n m counterType storageType) ->
  (State n m counterType storageType -> State n m counterType storageType) ->
  Input n m -> S n m counterType storageType (Output n m)
macMonad accumulateFun multiplyFun Input{values, newAcc} = do
  setAccumulator newAcc
  startMultiplication values

  -- the order here is important! if we swap the following lines, the accumulation is done too quickly
  whenStage Accumulating accumulateFun
  whenStage Multiplying multiplyFun
  generateOutput


  where
    setAccumulator newAcc' = modifyWhenJust newAcc' (\s acc -> s{accumulator=bitCoerce acc})
    generateOutput = ST.gets extractOuptut
    startMultiplication vals = modifyWhenJust vals (\s (x,y) -> startMulState x y s)

    whenStage st f = do
      stage' <- ST.gets stage
      when (stage' == st) $ ST.modify' f

    modifyWhenJust mV f = whenJust mV (\v -> ST.modify' (\s -> f s v))


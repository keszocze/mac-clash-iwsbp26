{-# LANGUAGE DuplicateRecordFields, RecordWildCards, DerivingVia, AllowAmbiguousTypes, UndecidableInstances, FlexibleInstances #-}
module MAC.Monad where

import Clash.Prelude hiding (sum, product)
import Clash.Class.Counter

import qualified Control.Monad.State.Strict as ST
import Control.Monad.Extra

import qualified MAC.Access.Indexing as I
import qualified MAC.Access.Rotating as R
import MAC.Constraints
import MAC.Types

import qualified Util.FullAdder as FA

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
              let aFun = R.accumulate @n @m @OneHotCounter @BVec fullAdder
                  mFun = R.multiply @n @m @OneHotCounter @BVec fullAdder
              in mealyS @dom (macMonad @n @m @OneHotCounter aFun mFun) (initialState @n @m)
            else
              let aFun = R.accumulate @n @m @Index @BVec fullAdder
                  mFun = R.multiply @n @m @Index fullAdder
              in mealyS @dom (macMonad @n @m @Index @BVec aFun mFun) (initialState @n @m)
        else
          if useOneHot
            then
              let aFun = R.accumulate @n @m @OneHotCounter @BitVector fullAdder
                  mFun = R.multiply @n @m @OneHotCounter @BitVector fullAdder
              in mealyS @dom (macMonad @n @m @OneHotCounter aFun mFun) (initialState @n @m)
            else
              let aFun = R.accumulate @n @m @Index @BitVector fullAdder
                  mFun = R.multiply @n @m @Index @BitVector fullAdder
              in mealyS @dom (macMonad @n @m @Index @BitVector aFun mFun) (initialState @n @m)
    else
      if useVector
        then
          if useOneHot
            then
              let aFun = I.accumulate @n @m @OneHotCounter @BVec fullAdder
                  mFun = I.multiply @n @m @OneHotCounter @BVec fullAdder
              in mealyS @dom (macMonad @n @m @OneHotCounter aFun mFun) (initialState @n @m)
            else
              let aFun = I.accumulate @n @m @Index @BVec fullAdder
                  mFun = I.multiply @n @m @Index fullAdder
              in mealyS @dom (macMonad @n @m @Index @BVec aFun mFun) (initialState @n @m)
        else
          if useOneHot
            then
              let aFun = I.accumulate @n @m @OneHotCounter @BitVector fullAdder
                  mFun = I.multiply @n @m @OneHotCounter @BitVector fullAdder
              in mealyS @dom (macMonad @n @m @OneHotCounter aFun mFun) (initialState @n @m)
            else
              let aFun = I.accumulate @n @m @Index @BitVector fullAdder
                  mFun = I.multiply @n @m @Index @BitVector fullAdder
              in mealyS @dom (macMonad @n @m @Index @BitVector aFun mFun) (initialState @n @m)
  where
    fullAdder = if useModuleFullAdder then FA.fullAdderModule else FA.fullAdder

macMonad :: forall n m counterType storageType.
  (
    NatConstraints n m,
    ConstraintNM n m Counter counterType,
    StorageConstraintsNM n m storageType
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


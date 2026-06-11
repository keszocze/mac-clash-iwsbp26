{-# LANGUAGE DuplicateRecordFields, RecordWildCards, DerivingVia, AllowAmbiguousTypes, UndecidableInstances, FlexibleInstances #-}
module MAC.Simple.Monad where

import Clash.Prelude hiding (sum, product)
import Clash.Class.Counter

import qualified Control.Monad.State.Strict as ST
import Control.Monad.Extra

import qualified MAC.Simple.Access.Indexing as I
import qualified MAC.Simple.Access.Rotating as R
import MAC.Config
import MAC.IO
import MAC.BVec
import MAC.OneHotCounter
import MAC.Simple.Stage
import MAC.Simple.State
import MAC.Constraints

import qualified Util.FullAdder as FA

type S (n :: Nat) (m :: Nat) counterType storageType = ST.State (State n m counterType storageType)


mkMAC' :: forall dom n m.
  (
    HiddenClockResetEnable dom,
    NatConstraints n m
  )
  =>
    Config ->
    Signal dom (Input n m) ->
    Signal dom (Output n m)
-- we ignore the state/Mealy flag as this has been already decided here
mkMAC' Config{useModuleFullAdder, useRotation, useVector, useOneHot} =
  if useRotation
    then
      if useVector
        then
          if useOneHot
            then
              let aFun = R.accumulate @n @m @OneHotCounter @BVec fullAdder
                  mFun = R.multiply @n @m @OneHotCounter @BVec fullAdder
              in mealyS @dom (macMonad' @n @m @OneHotCounter aFun mFun) (initialState @n @m)
            else
              let aFun = R.accumulate @n @m @Index @BVec fullAdder
                  mFun = R.multiply @n @m @Index fullAdder
              in mealyS @dom (macMonad' @n @m @Index @BVec aFun mFun) (initialState @n @m)
        else
          if useOneHot
            then
              let aFun = R.accumulate @n @m @OneHotCounter @BitVector fullAdder
                  mFun = R.multiply @n @m @OneHotCounter @BitVector fullAdder
              in mealyS @dom (macMonad' @n @m @OneHotCounter aFun mFun) (initialState @n @m)
            else
              let aFun = R.accumulate @n @m @Index @BitVector fullAdder
                  mFun = R.multiply @n @m @Index @BitVector fullAdder
              in mealyS @dom (macMonad' @n @m @Index @BitVector aFun mFun) (initialState @n @m)
    else
      if useVector
        then
          if useOneHot
            then
              let aFun = I.accumulate @n @m @OneHotCounter @BVec fullAdder
                  mFun = I.multiply @n @m @OneHotCounter @BVec fullAdder
              in mealyS @dom (macMonad' @n @m @OneHotCounter aFun mFun) (initialState @n @m)
            else
              let aFun = I.accumulate @n @m @Index @BVec fullAdder
                  mFun = I.multiply @n @m @Index fullAdder
              in mealyS @dom (macMonad' @n @m @Index @BVec aFun mFun) (initialState @n @m)
        else
          if useOneHot
            then
              let aFun = I.accumulate @n @m @OneHotCounter @BitVector fullAdder
                  mFun = I.multiply @n @m @OneHotCounter @BitVector fullAdder
              in mealyS @dom (macMonad' @n @m @OneHotCounter aFun mFun) (initialState @n @m)
            else
              let aFun = I.accumulate @n @m @Index @BitVector fullAdder
                  mFun = I.multiply @n @m @Index @BitVector fullAdder
              in mealyS @dom (macMonad' @n @m @Index @BitVector aFun mFun) (initialState @n @m)
  where
    fullAdder = if useModuleFullAdder then FA.fullAdderModule else FA.fullAdder


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

macMonad' :: forall n m counterType storageType.
  (
    NatConstraints n m,
    ConstraintNM n m Counter counterType,
    StorageConstraintsNM n m storageType
  ) =>
  (State n m counterType storageType -> State n m counterType storageType) ->
  (State n m counterType storageType -> State n m counterType storageType) ->
  Input n m -> S n m counterType storageType (Output n m)
macMonad' accumulateFun multiplyFun Input{values, newAcc} = do
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


macMonad :: forall n m counterType storageType.
  (
    NatConstraints n m,
    ConstraintNM n m Counter counterType,
    ConstraintNM n m Show counterType,
    ConstraintNM n m Show storageType,
    StorageConstraintsNM n m storageType
  ) =>
  (State n m counterType storageType -> State n m counterType storageType) ->
  (State n m counterType storageType -> State n m counterType storageType) ->
  Input n m -> S n m counterType storageType (Output n m)
macMonad accumulateFun multiplyFun Input{values, newAcc} = do
  -- conditionally set the accumulator to a new value
  setAccumulator newAcc

  -- either start a new multiplication (discarding an ongoing one, breaking an ongoing accumulation)
  -- or continue with what is currently being done (nothing, multiplying, accumulating)
  case values of
    Just (x,y) -> ST.modify' (\s -> startMulState x y s)
    Nothing -> regularOperation

  generateOutput


  where
    setAccumulator newAcc' = modifyWhenJust newAcc' (\s acc -> s{accumulator=bitCoerce acc})
    startMultiplication vals = modifyWhenJust vals (\s (x,y) -> startMulState x y s)
    generateOutput = ST.gets extractOuptut
    modifyWhenJust mV f = whenJust mV (\v -> ST.modify' (\s -> f s v))
    regularOperation = do
      stage <- ST.gets stage
      case stage of
        Ready -> pure () -- do nothing
        Multiplying -> ST.modify' multiplyFun
        Accumulating -> ST.modify' accumulateFun

    -- whenStage st f = do
    --   stage' <- ST.gets stage
    --   when (stage' == st) $ ST.modify' f

    -- ifJust m f g = case m of
    --   (Just v) -> f v
    --   _ -> g

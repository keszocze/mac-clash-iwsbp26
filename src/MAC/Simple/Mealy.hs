{-# LANGUAGE DuplicateRecordFields, RecordWildCards, DerivingVia, AllowAmbiguousTypes, UndecidableInstances, FlexibleInstances #-}
module MAC.Simple.Mealy where

import Clash.Prelude hiding (sum, product)
import Clash.Class.Counter

import MAC.Constraints
import MAC.BVec
import MAC.Config
import MAC.IO
import MAC.OneHotCounter
import MAC.Simple.Stage
import MAC.Simple.State
import qualified MAC.Simple.Access.Indexing as I
import qualified MAC.Simple.Access.Rotating as R

import qualified Util.FullAdder as FA



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
              in mealy @dom (macMealy' @n @m @OneHotCounter aFun mFun) (initialState @n @m)
            else
              let aFun = R.accumulate @n @m @Index @BVec fullAdder
                  mFun = R.multiply @n @m @Index fullAdder
              in mealy @dom (macMealy' @n @m @Index @BVec aFun mFun) (initialState @n @m)
        else
          if useOneHot
            then
              let aFun = R.accumulate @n @m @OneHotCounter @BitVector fullAdder
                  mFun = R.multiply @n @m @OneHotCounter @BitVector fullAdder
              in mealy @dom (macMealy' @n @m @OneHotCounter aFun mFun) (initialState @n @m)
            else
              let aFun = R.accumulate @n @m @Index @BitVector fullAdder
                  mFun = R.multiply @n @m @Index @BitVector fullAdder
              in mealy @dom (macMealy' @n @m @Index @BitVector aFun mFun) (initialState @n @m)
    else
      if useVector
        then
          if useOneHot
            then
              let aFun = I.accumulate @n @m @OneHotCounter @BVec fullAdder
                  mFun = I.multiply @n @m @OneHotCounter @BVec fullAdder
              in mealy @dom (macMealy' @n @m @OneHotCounter aFun mFun) (initialState @n @m)
            else
              let aFun = I.accumulate @n @m @Index @BVec fullAdder
                  mFun = I.multiply @n @m @Index fullAdder
              in mealy @dom (macMealy' @n @m @Index @BVec aFun mFun) (initialState @n @m)
        else
          if useOneHot
            then
              let aFun = I.accumulate @n @m @OneHotCounter @BitVector fullAdder
                  mFun = I.multiply @n @m @OneHotCounter @BitVector fullAdder
              in mealy @dom (macMealy' @n @m @OneHotCounter aFun mFun) (initialState @n @m)
            else
              let aFun = I.accumulate @n @m @Index @BitVector fullAdder
                  mFun = I.multiply @n @m @Index @BitVector fullAdder
              in mealy @dom (macMealy' @n @m @Index @BitVector aFun mFun) (initialState @n @m)
  where
    fullAdder = if useModuleFullAdder then FA.fullAdderModule else FA.fullAdder




-- TODO herausfinden, wieso diese Spezialisierung nicht funktioniert
-- type Output n = Output' n n
-- macMealy' :: forall n. (KnownNat n) => forall n. State n  -> Input n -> (State n, Output n)
-- macMealy = macMealy' @n @n




macMealy' :: forall n m counterType storageType. (
    NatConstraints n m,
    ConstraintNM n m Counter counterType,
    ConstraintNM n m NFDataX counterType,
    StorageConstraintsNM n m storageType
  ) =>
    (State n m counterType storageType -> State n m counterType storageType) ->
    (State n m counterType storageType -> State n m counterType storageType) ->
    State n m counterType storageType->
    Input n m ->
    (State n m counterType storageType, Output n m)
macMealy' accumulateFun multiplyFun state@State{accumulator=initialAccumulator} Input{values, newAcc}  = (state', extractOuptut state')
  where
    stateNewAcc = state{accumulator= maybe initialAccumulator bitCoerce newAcc}

    stateStart = case values of
      Just (x,y) -> startMulState x y stateNewAcc
      Nothing -> stateNewAcc

    state' = compute stateStart



    compute compState@State{stage} = case stage of
      Ready -> compState
      Multiplying -> multiplyFun compState
      Accumulating -> accumulateFun compState


-- version starting the multiplication with a delay of one
macMealy :: forall n m counterType storageType. (
    NatConstraints n m,
    ConstraintNM n m Counter counterType,
    ConstraintNM n m NFDataX counterType,
    StorageConstraintsNM n m storageType
  ) =>
    (State n m counterType storageType -> State n m counterType storageType) ->
    (State n m counterType storageType -> State n m counterType storageType) ->
    State n m counterType storageType->
    Input n m ->
    (State n m counterType storageType, Output n m)
macMealy accumulateFun multiplyFun state@State{accumulator=initialAccumulator} Input{values, newAcc}  = (state', extractOuptut state')
  where
    stateNewAcc = state{accumulator= maybe initialAccumulator bitCoerce newAcc}

    state' = case values of
      Just (x,y) -> startMulState x y stateNewAcc
      Nothing -> compute stateNewAcc

    compute compState@State{stage} = case stage of
      Ready -> compState
      Multiplying -> multiplyFun compState
      Accumulating -> accumulateFun compState



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
              in mealy @dom (macMealy @n @m @OneHotCounter aFun mFun) (initialState @n @m)
            else
              let aFun = R.accumulate @n @m @Index @BVec fullAdder
                  mFun = R.multiply @n @m @Index fullAdder
              in mealy @dom (macMealy @n @m @Index @BVec aFun mFun) (initialState @n @m)
        else
          if useOneHot
            then
              let aFun = R.accumulate @n @m @OneHotCounter @BitVector fullAdder
                  mFun = R.multiply @n @m @OneHotCounter @BitVector fullAdder
              in mealy @dom (macMealy @n @m @OneHotCounter aFun mFun) (initialState @n @m)
            else
              let aFun = R.accumulate @n @m @Index @BitVector fullAdder
                  mFun = R.multiply @n @m @Index @BitVector fullAdder
              in mealy @dom (macMealy @n @m @Index @BitVector aFun mFun) (initialState @n @m)
    else
      if useVector
        then
          if useOneHot
            then
              let aFun = I.accumulate @n @m @OneHotCounter @BVec fullAdder
                  mFun = I.multiply @n @m @OneHotCounter @BVec fullAdder
              in mealy @dom (macMealy @n @m @OneHotCounter aFun mFun) (initialState @n @m)
            else
              let aFun = I.accumulate @n @m @Index @BVec fullAdder
                  mFun = I.multiply @n @m @Index fullAdder
              in mealy @dom (macMealy @n @m @Index @BVec aFun mFun) (initialState @n @m)
        else
          if useOneHot
            then
              let aFun = I.accumulate @n @m @OneHotCounter @BitVector fullAdder
                  mFun = I.multiply @n @m @OneHotCounter @BitVector fullAdder
              in mealy @dom (macMealy @n @m @OneHotCounter aFun mFun) (initialState @n @m)
            else
              let aFun = I.accumulate @n @m @Index @BitVector fullAdder
                  mFun = I.multiply @n @m @Index @BitVector fullAdder
              in mealy @dom (macMealy @n @m @Index @BitVector aFun mFun) (initialState @n @m)
  where
    fullAdder = if useModuleFullAdder then FA.fullAdderModule else FA.fullAdder

module MAC.Access.Rotating where

import Clash.Prelude hiding (product, sum)
import Clash.Class.Counter

import MAC.Class.Storage
import MAC.Constraints
import MAC.Stage
import MAC.State


accumulate :: forall n m counterType storageType.
  (
    NatConstraints n m,
    BitPack (storageType (n + m)),
    Storage (storageType (n + m)),
    Counter (counterType (n + m)),
    ConstraintNM n m Show storageType,
    ConstraintNM n m Show counterType
  ) =>
  (Bit -> Bit -> Bit -> (Bit, Bit)) ->
  State n m counterType storageType ->
  State n m counterType storageType
accumulate fullAdder st@State{..} =  let
    a = lsb accumulator
    b = lsb product
    (carry', sum) = fullAdder a b carry
    accumulator' = replaceBit (0 :: Bit) sum accumulator
    accumulator''= advance accumulator'
    product' = advance product

    (stage', accumulateCounter') = case countSuccOverflow accumulateCounter of
        (True, acc) -> (Ready, acc)
        (False, acc) -> (Accumulating, acc)

    st' = st{
      stage=stage',
      carry=carry',
      accumulator=accumulator'',
      accumulateCounter=accumulateCounter',
      product = product'
      }
    in st'


multiply :: forall n m counterType storageType.
  (
      NatConstraints n m,
      Counter (counterType n), Counter (counterType m),
      ConstraintNM n m Show storageType,
      ConstraintNM n m Show counterType,
      StorageConstraintsNM n m storageType
  ) =>
  (Bit -> Bit -> Bit -> (Bit, Bit)) ->
  State n m counterType storageType ->
  State n m counterType storageType
multiply fullAdder st@State{..} =
  let (currentRoundDone, xCounter') = countSuccOverflow xCounter

      a = (lsb x) .&. (lsb y)
      b = lsb product
      (carryOut, sum) = fullAdder a b carry

      -- just naming it to avoid magic numbers
      modifyIndex = 0 :: Bit

      productWithSum = replaceBit modifyIndex sum product


      product' = advance productWithSum


      x' = x `rotateR` 1 -- continously shift through;

      stage' = if currentRoundDone
        then EndRound
        else Multiplying
      st' = st
        {
          x=x',
          product = product',
          xCounter = xCounter',
          carry = carryOut,
          stage = stage'
        }
    in st'

endRound :: forall n m counterType storageType.
  (
      NatConstraints n m,
      Counter (counterType n), Counter (counterType m),
      ConstraintNM n m Show storageType,
      ConstraintNM n m Show counterType,
      StorageConstraintsNM n m storageType
  ) =>
  State n m counterType storageType ->
  State n m counterType storageType
endRound st@State{..} =
    let

      modifyIndex = 0 :: Bit
      productWithCarry = replaceBit modifyIndex carry product

      resetDist = (natToNum @n @Int) - 1
      resProd = reset resetDist  productWithCarry

      (inLastRound, yCounter'')  = countSuccOverflow yCounter
      (stage', product', yCounter') = if inLastRound
        then (Accumulating, advance productWithCarry, countMin)
        else (Multiplying, resProd, yCounter'')
      st' = st {
          carry = 0,
          stage = stage',
          product=product',
          yCounter=yCounter',
          y = y `shiftR` 1
        }

    in st'

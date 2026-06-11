module MAC.Extended.Access.Rotating where

import Clash.Prelude hiding (product, sum)
import Clash.Class.Counter

import MAC.Class.Storage
import MAC.Constraints
import MAC.Extended.Stage
import MAC.Extended.State


accumulate :: forall n m counterType storageType.
  (
    NatConstraints n m,
    BitPack (storageType (n + m)),
    Storage (storageType (n + m)),
    Counter (counterType (n + m))
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

    in st{
      stage=stage',
      carry=carry',
      accumulator=accumulator'',
      accumulateCounter=accumulateCounter',
      product = product'
      }


multiply :: forall n m counterType storageType.
  (
      NatConstraints n m,
      Counter (counterType n), Counter (counterType m),
      StorageConstraintsNM n m storageType
  ) =>
  (Bit -> Bit -> Bit -> (Bit, Bit)) ->
  State n m counterType storageType ->
  State n m counterType storageType
multiply fullAdder st@State{..} =
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

endRound :: forall n m counterType storageType.
  (
      NatConstraints n m,
      Counter (counterType n), Counter (counterType m),
      StorageConstraintsNM n m storageType
  ) =>
  State n m counterType storageType ->
  State n m counterType storageType
endRound = id

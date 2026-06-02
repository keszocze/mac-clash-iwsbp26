module MAC.Access.Indexing.Mealy where

import Clash.Prelude
import Clash.Class.Counter

import MAC.Class.Storage
import MAC.Types.Internal
import MAC.Types.State
import MAC.Util

accumulateIndexing :: forall n m counterType storageType.
  (
    KnownNat n, KnownNat m,
    BitPack (storageType (n + m)),
    Storage (storageType (n + m)),
    Counter (counterType (n + m)),
    Enum (counterType (n + m))
  ) =>
  (Bit -> Bit -> Bit -> (Bit, Bit)) ->
  State n m counterType storageType ->
  State n m counterType storageType
accumulateIndexing fullAdder st@State{..} =  let
    a = accumulator ! accumulateCounter
    b = product ! accumulateCounter
    (carry', sum) = fullAdder a b carry
    accumulator' = replaceBit accumulateCounter sum accumulator

    (stage', accumulateCounter') = case countSuccOverflow accumulateCounter of
        (True, acc) -> (Ready, acc)
        (False, acc) -> (Accumulating, acc)

    in st{
      stage=stage',
      carry=carry',
      accumulator=accumulator',
      accumulateCounter=accumulateCounter'
      }


mulIndexing :: forall n m counterType storageType.
  (
      KnownNat n, KnownNat m, 1 <= n, 1 <= m, 1 <= n + m,
      Counter (counterType n), Counter (counterType m),
      BitPack (storageType (n + m)),
      Storage (storageType (n + m)),
      Enum (counterType n), Enum (counterType m)
  ) =>
  (Bit -> Bit -> Bit -> (Bit, Bit)) ->
  State n m counterType storageType ->
  State n m counterType storageType
mulIndexing fullAdder st@State{..} =
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

module MAC.Extended.Access.Indexing where

import Clash.Prelude hiding (product, sum)
import Clash.Class.Counter

import MAC.Class.Storage
import MAC.Constraints
import MAC.Extended.Stage
import MAC.Extended.State

import Util

accumulate :: forall n m counterType storageType.
  (
    NatConstraints n m,
    BitPack (storageType (n + m)),
    Storage (storageType (n + m)),
    Counter (counterType (n + m)),
    Enum (counterType (n + m))
  ) =>
  (Bit -> Bit -> Bit -> (Bit, Bit)) ->
  State n m counterType storageType ->
  State n m counterType storageType
accumulate fullAdder st@State{..} =  let
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


multiply :: forall n m counterType storageType.
  (
      NatConstraints n m,
      Counter (counterType n), Counter (counterType m),
      StorageConstraintsNM n m storageType,
      Enum (counterType n), Enum (counterType m)
  ) =>
  (Bit -> Bit -> Bit -> (Bit, Bit)) ->
  State n m counterType storageType ->
  State n m counterType storageType
multiply fullAdder st@State{..} =
  let (currentRoundDone, xCounter') = countSuccOverflow xCounter
      (inLastRound, yCounter')  = countSuccOverflow yCounter
      multiplicationDone = currentRoundDone .&. inLastRound

      xIndex = enumCounterToIndex @n xCounter
      yIndex = enumCounterToIndex @m yCounter
      productIndex = add xIndex yIndex
      modifyWithCarryIndex = add productIndex (1 :: Index 2)


      a = (x ! xIndex) .&. (y ! yIndex)
      b = product ! productIndex
      (carryOut, sum) = fullAdder a b carry


      productWithSum = replaceBit productIndex sum product

      product' = if currentRoundDone
        -- propagate carry to next position in the product (which is known to contain a 0, i.e., simply putting the carry there is fine)
        then replaceBit modifyWithCarryIndex carryOut productWithSum
        else productWithSum

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

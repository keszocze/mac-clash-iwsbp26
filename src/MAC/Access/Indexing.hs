module MAC.Access.Indexing where

import Clash.Prelude hiding (product, sum)
import Clash.Class.Counter

import MAC.Class.Storage
import MAC.Constraints
import MAC.Stage
import MAC.State


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
      Enum (counterType n), Enum (counterType m),
      Show (counterType n), Show (counterType m)
  ) =>
  (Bit -> Bit -> Bit -> (Bit, Bit)) ->
  State n m counterType storageType ->
  State n m counterType storageType
multiply fullAdder st@State{..} =
  let (currentRoundDone, xCounterSucc) = countSuccOverflow xCounter


      xIndex = enumCounterToIndex @n xCounter
      yIndex = enumCounterToIndex @m yCounter
      productIndex = add xIndex yIndex



      a = (x ! xIndex) .&. (y ! yIndex)
      b = product ! productIndex
      (carryOut, sum) = fullAdder a b carry


      product' = replaceBit productIndex sum product

      (stage',xCounter') = if currentRoundDone
        then (EndRound,xCounter)
        else (Multiplying,xCounterSucc)
    in st
      {
        product = product',
        xCounter = xCounter',
        carry = carryOut,
        stage = stage'
      }

endRound :: forall n m counterType storageType.
  (
      NatConstraints n m,
      Counter (counterType n), Counter (counterType m),
      Enum (counterType n), Enum (counterType m),
      Show (storageType (n+m)),
      StorageConstraintsNM n m storageType
  ) =>
  State n m counterType storageType ->
  State n m counterType storageType
endRound st@State{..} =
  let
      xIndex = enumCounterToIndex @n xCounter
      yIndex = enumCounterToIndex @m yCounter
      carryIndex = add (add xIndex yIndex) (1 :: Index 2)
      -- c = trace ("in carry: " <> show carry) carry
      -- p = trace ("in product: " <> show product) product
      product' = replaceBit carryIndex carry product

      (inLastRound, yCounter'')  = countSuccOverflow yCounter
      (stage', yCounter') = if inLastRound
        then (Accumulating, countMin)
        else (Multiplying, yCounter'')

  in st{
      carry = 0,
      stage=stage',
      product={-trace ("out product" <> product')-} product',
      xCounter = countSucc xCounter,
      yCounter=yCounter'
    }

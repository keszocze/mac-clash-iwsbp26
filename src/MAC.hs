{-# LANGUAGE AllowAmbiguousTypes #-}

module MAC where

import Clash.Prelude hiding (product, replicate, (++))
import Prelude (replicate, (++))


import MAC.Constraints
import qualified MAC.Mealy as Mealy
import qualified MAC.Monad as Monad
import MAC.Types

multiplicationDelay :: forall n m. (KnownNat n, KnownNat m) => Int
multiplicationDelay = (nInt * mInt) - 1
  where
    nInt = natToNum @n @Int
    mInt = natToNum @m @Int

multiplicationDelay' :: forall n m. (KnownNat n, KnownNat m) => Int
multiplicationDelay' = (nInt * mInt)
  where
    nInt = natToNum @n @Int
    mInt = natToNum @m @Int

accumulationDelay :: forall n m. (KnownNat n, KnownNat m) => Int
accumulationDelay = nInt + mInt
  where
    nInt = natToNum @n @Int
    mInt = natToNum @m @Int

totalDelay :: forall n m. (KnownNat n, KnownNat m) => Int
totalDelay = multiplicationDelay @n @m + accumulationDelay @n @m

totalDelay' :: forall n m. (KnownNat n, KnownNat m) => Int
totalDelay' = multiplicationDelay @n @m + accumulationDelay @n @m + 1

-- unMaybe :: forall dom n m. (NatConstraints n m) =>
--   (Signal dom (Input n m) -> Signal dom (Output n m)) ->
--   (Signal dom (UnwrappedInput n m)-> Signal dom (UnwrappedOutput n m))
-- unMaybe f = \i ->
--     let
--       inStream = fmap wrapInput i
--       outStream = fmap f inStream
--     in fmap (unwrapOutput @n @m) outStream



mkMAC :: forall dom n m.
  (
    HiddenClockResetEnable dom,
    NatConstraints n m
  )
  =>
    Config ->
    Signal dom (Input n m) ->
    Signal dom (Output n m)
mkMAC cfg@Config{useState} = if useState then Monad.mkMAC cfg else Mealy.mkMAC cfg


-- version with a delay for the multiplication
mkMAC' :: forall dom n m.
  (
    HiddenClockResetEnable dom,
    NatConstraints n m
  )
  =>
    Config ->
    Signal dom (Input n m) ->
    Signal dom (Output n m)
mkMAC' cfg@Config{useState} = if useState then Monad.mkMAC' cfg else Mealy.mkMAC' cfg



{-# OPAQUE topEntity #-}
{-# ANN topEntity
  (Synthesize
      { t_name = "topEntityTesting"
      , t_inputs = [ PortName "clk"
                    , PortName "rst"
                    , PortName "ena"
                    , PortProduct "" [
                        PortProduct "mulParameters" [
                          PortName "doStartMultiplication",
                          PortProduct "values" [
                            PortName "x",
                            PortName "y"
                          ]
                        ],
                        PortProduct "accumulator" [
                          PortName "doSetAccumulator",
                          PortName "newAccumulatorValue"
                        ]
                      ]
                    ]
      , t_output =  PortProduct "" [
                      PortProduct "product" [PortName "is_valid", PortName "value"]
                    , PortProduct "accumulator" [PortName "is_valid", PortName "accumulator"]
        ]
  }) #-}
topEntity :: Clock System
            -> Reset System
            -> Enable System
            -> Signal System (Input 2 3)
            -> Signal System (Output 2 3)
topEntity = exposeClockResetEnable $ mkMAC @System @2 @3 defaultConfig


testInputs :: forall n m. (KnownNat n, KnownNat m) => (Unsigned n, Unsigned m) -> [Input n m]
testInputs (x, y) = (Input (Just (x,y)) Nothing) : replicate  (totalDelay @n @m + 1) (Input Nothing Nothing)


expectedMulOutput :: forall n m. (KnownNat n, KnownNat m) => (Unsigned n, Unsigned m) -> [Output n m]
expectedMulOutput (x,y) = multiplying ++ accumulating ++ displayingResult
  where
    multiplying = replicate (multiplicationDelay @n @m) (Output Nothing (Just 0))
    accumulating = replicate (accumulationDelay @n @m) (Output Nothing Nothing)
    displayingResult = replicate 1 (Output product product) -- extend for more cycles?
      where product = Just $ mul x y

testInputs' :: forall n m. (KnownNat n, KnownNat m) => (Unsigned n, Unsigned m) -> [Input n m]
testInputs' (x, y) = (Input (Just (x,y)) Nothing) : replicate  (totalDelay @n @m + 1) (Input Nothing Nothing)


expectedMulOutput' :: forall n m. (KnownNat n, KnownNat m) => (Unsigned n, Unsigned m) -> [Output n m]
expectedMulOutput' (x,y) = multiplying ++ accumulating ++ displayingResult
  where
    multiplying = replicate (multiplicationDelay' @n @m) (Output Nothing (Just 0))
    accumulating = replicate (accumulationDelay @n @m) (Output Nothing Nothing)
    displayingResult = replicate 1 (Output product product) -- extend for more cycles?
      where product = Just $ mul x y



is :: [Input 3 3]
is = (Input Nothing Nothing) : testInputs (2 :: Unsigned 3, 3 :: Unsigned 3)

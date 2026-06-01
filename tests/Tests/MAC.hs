module Tests.MAC where


import Debug.Trace

import Clash.Hedgehog.Sized.Unsigned
import Clash.Hedgehog.Sized.Index


import qualified Clash.Prelude as C
import Clash.Prelude (natToNum, simulateN, Unsigned, System,  KnownNat, type (<=), type (+))
import Prelude hiding (product)

import qualified Hedgehog as H
import Hedgehog ((===), withTests)
import qualified Hedgehog.Range as Range

import MAC.Mealy

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hedgehog

import Tests.Util

allInputVals :: forall n m. (KnownNat n, KnownNat m) =>  [(Unsigned n, Unsigned m)]
allInputVals = [(x, y) | x <- [minBound .. maxBound], y <- [minBound .. maxBound]]


-- TODO die Dauern, wie lange welcher Teil benötigt im HW Modul berechnen und als Funktionen bereitstellen

testInputs :: forall n m. (KnownNat n, KnownNat m) => (Unsigned n, Unsigned m) -> [MACInput n m]
testInputs (x, y) = (MACInput (Just (x,y)) Nothing) : replicate  ((nInt * mInt) + nInt + mInt) (MACInput Nothing Nothing)
  where
    nInt = natToNum @n @Int
    mInt = natToNum @m @Int

expectedMulOutput :: forall n m. (KnownNat n, KnownNat m) => (Unsigned n, Unsigned m) -> [MACOutput n m]
expectedMulOutput (x,y) = multiplying ++ accumulating ++ displayingResult
  where
    nInt = natToNum @n @Int
    mInt = natToNum @m @Int
    multiplying = replicate ((nInt*mInt)-1) (MACOutput Nothing (Just 0))
    accumulating = replicate (nInt + mInt) (MACOutput Nothing Nothing)
    displayingResult = replicate 1 (MACOutput product product) -- extend for more cycles?
      where product = Just $ C.mul x y


exhaustiveTests = testGroup "Exhaustive Tests" [
    exhaustiveTestsForSize @2 @2
  ]

exhaustiveTestsForSize ::
  forall n m.
  ( KnownNat n,
    1 <= n,
    KnownNat m,
    1 <= m
  ) =>
  TestTree
exhaustiveTestsForSize = testGroup name $ map (exhaustiveTest @n @m) allConfigs
  where name = "n=" <> prettySNat @n <> " m=" <> prettySNat @m

tests = testGroup "MAC Unit" [
    exhaustiveTests
  ]
exhaustiveTest ::
  forall n m.
  ( KnownNat n,
    1 <= n,
    KnownNat m,
    1 <= m
  ) =>
  MACConfig -> TestTree
exhaustiveTest cfg = testCase name prop
  where
    name = describe cfg
    nInt = natToNum @n @Int
    mInt = natToNum @m @Int
    delay = (nInt * mInt) + nInt + mInt
    inputStreams = map (testInputs @n @m) allInputVals
    expectedStreams = map (expectedMulOutput @n @m) allInputVals
    simulatedStreams = map (simulateN @System delay (mac' @System @n @m cfg)) inputStreams
    prop = do
      mapM_ (
          \((x,y), os, es) -> assertEqual ("Computing " <> show x <> " * " <> show y <> " failed") es os
        )
        $ zip3 (allInputVals @n @m) simulatedStreams expectedStreams

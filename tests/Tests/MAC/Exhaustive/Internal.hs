module Tests.MAC.Exhaustive.Internal where

import Clash.Prelude (simulateN, System, KnownNat, type (<=),)
import Prelude hiding (product)

import Test.Tasty
import Test.Tasty.HUnit

import Tests.Util

import MAC.Types.Config
import MAC
import Util

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


exhaustiveTest ::
  forall n m.
  ( KnownNat n,
    1 <= n,
    KnownNat m,
    1 <= m
  ) =>
  Config -> TestTree
exhaustiveTest cfg = testCase name prop
  where
    name = describe cfg
    delay = (totalDelay @n @m)
    inputStreams = map (testInputs @n @m) allInputVals
    expectedStreams = map (expectedMulOutput @n @m) allInputVals
    simulatedStreams = map (simulateN @System delay (mkMAC @System @n @m cfg)) inputStreams
    prop = do
      mapM_ (
          \((x,y), os, es) -> assertEqual ("Computing " <> show x <> " * " <> show y <> " failed") es os
        )
        $ zip3 (allInputVals @n @m) simulatedStreams expectedStreams

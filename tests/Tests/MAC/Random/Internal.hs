module Tests.MAC.Random.Internal where

import Clash.Prelude (natToNum, simulateN, Unsigned, System,  KnownNat, type (<=),)
import Clash.Hedgehog.Sized.Unsigned

import Prelude hiding (product)


import qualified Hedgehog as H
import Hedgehog (withTests, (===), checkParallel)
import qualified Hedgehog.Range as Range


import Test.Tasty
import Test.Tasty.Hedgehog

import Tests.Util

import MAC
import MAC.Types.Config

randomTestsForSize :: forall n m. (KnownNat n, KnownNat m, 1 <= n, 1 <= m) => TestTree
randomTestsForSize = testGroup name $ map (randomTest @n @m) allConfigs
  where name = "n=" <> prettySNat @n <> " m=" <> prettySNat @m


randomTest ::
  forall n m.
  ( KnownNat n,
    1 <= n,
    KnownNat m,
    1 <= m
  ) =>
  Config -> TestTree
randomTest cfg = testProperty name $ withTests 200 prop
  where
    name = describe cfg
    delay = (totalDelay @n @m) + 1
    prop = H.property $ do
      x <- H.forAll $ genUnsigned (Range.linear (minBound :: Unsigned n) maxBound)
      y <- H.forAll $ genUnsigned (Range.linear (minBound :: Unsigned m) maxBound)
      let
        inputStream = testInputs @n @m (x,y)
        expectedStream = expectedMulOutput @n @m (x,y)
        simulatedStream = simulateN @System delay (mkMAC @System @n @m cfg) inputStream
      H.annotate $ "Computing " <> show x <> " * " <> show y <> " failed"
      expectedStream === simulatedStream


module Tests.MAC where


import Clash.Hedgehog.Sized.Unsigned
import Clash.Hedgehog.Sized.Index

import Clash.Prelude
import qualified Prelude as P

import qualified Hedgehog as H
import qualified Hedgehog.Range as Range

import MAC.Mealy

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hedgehog

import Tests.Util

allInputVals :: forall n m. (KnownNat n, KnownNat m) =>  [(Unsigned n, Unsigned m)]
allInputVals = [(x, y) | x <- [minBound .. maxBound], y <- [minBound .. maxBound]]

mulSequence :: forall n m. (KnownNat n, KnownNat m) => (Unsigned n, Unsigned m) -> [MACInput n m]
mulSequence (x, y) = (MACInput (Just (x,y)) Nothing) : P.repeat (MACInput Nothing Nothing)

expectedMulOutput :: forall n m. (KnownNat n, KnownNat m) => (Unsigned n, Unsigned m) -> [MACOutput n m]
expectedMulOutput (x,y) = [] -- TODO fill, actually

tests = testGroup "MAC Unit" [
    --testProperty "dummy" $ H.property H.discard,
    exhaustiveTest @2 @4
  ]
exhaustiveTest ::
  forall n m.
  ( KnownNat n,
    1 <= n,
    KnownNat m,
    1 <= m
  ) =>
  TestTree
exhaustiveTest = testProperty name $ H.withTests 1 prop
  where
    name = ("n=" <> prettySNat @n <> " m=" <> prettySNat @m)
    nInt = natToNum @n @Int
    mInt = natToNum @m @Int
    delay = nInt * mInt
    inputStreams = P.map (mulSequence @n @m) allInputVals
    prop = H.property $ do
      H.discard

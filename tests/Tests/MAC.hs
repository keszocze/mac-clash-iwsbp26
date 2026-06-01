module Tests.MAC where


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
import qualified Tests.MAC.Exhaustive
import qualified Tests.MAC.Random


tests = testGroup "MAC Unit" [
-- TODO add options for different levels of exhaustiveness
    Tests.MAC.Exhaustive.tests,
    Tests.MAC.Random.tests
  ]




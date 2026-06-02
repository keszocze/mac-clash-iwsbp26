module Tests.MAC where


import Clash.Hedgehog.Sized.Unsigned
import Clash.Hedgehog.Sized.Index


import Prelude hiding (product)



import MAC.Mealy

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hedgehog

import Tests.Util
import qualified Tests.MAC.Exhaustive
import qualified Tests.MAC.Random


tests = testGroup "MAC Unit" [
-- TODO add options for different levels of exhaustiveness / iterations
    Tests.MAC.Exhaustive.tests
  --  Tests.MAC.Random.tests
  ]




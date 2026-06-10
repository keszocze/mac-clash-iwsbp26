module Tests.MAC where

import Test.Tasty

import qualified Tests.MAC.Exhaustive
import qualified Tests.MAC.Random

tests :: TestTree
tests = testGroup "MAC Unit" [
-- TODO add options for different levels of exhaustiveness / iterations
    Tests.MAC.Exhaustive.tests
   -- Tests.MAC.Random.tests
  ]




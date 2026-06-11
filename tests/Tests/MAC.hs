module Tests.MAC where

import Test.Tasty

import qualified Tests.MAC.Simple.Exhaustive
import qualified Tests.MAC.Simple.Random

tests :: TestTree
tests = testGroup "MAC Unit" [
-- TODO add options for different levels of exhaustiveness / iterations
    Tests.MAC.Simple.Exhaustive.tests,
    Tests.MAC.Simple.Random.tests
  ]




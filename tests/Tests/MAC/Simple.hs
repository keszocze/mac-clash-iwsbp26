module Tests.MAC.Simple where

import Test.Tasty

import qualified Tests.MAC.Simple.Exhaustive
import qualified Tests.MAC.Simple.Random


tests :: TestTree
tests = testGroup "Simple version" [
  Tests.MAC.Simple.Exhaustive.tests,
  Tests.MAC.Simple.Random.tests
  ]

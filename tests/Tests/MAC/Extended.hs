module Tests.MAC.Extended where

import Test.Tasty

import qualified Tests.MAC.Extended.Exhaustive
import qualified Tests.MAC.Extended.Random


tests :: TestTree
tests = testGroup "Extended version" [
  Tests.MAC.Extended.Exhaustive.tests
  --Tests.MAC.Extended.Random.tests
  ]

module Tests.MAC where

import Test.Tasty

import Tests.MAC.Extended
import Tests.MAC.Simple

tests :: TestTree
tests = testGroup "MAC Unit" [
-- TODO add options for different levels of exhaustiveness / iterations
    Tests.MAC.Extended.tests
    --Tests.MAC.Simple.tests
  ]




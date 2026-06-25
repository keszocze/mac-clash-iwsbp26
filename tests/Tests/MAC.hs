module Tests.MAC where

import Test.Tasty

import Tests.MAC.Extended

import Tests.MAC.Simple.Exhaustive as SE
import Tests.MAC.Simple.Random as SR


-- TODO unify test case generation (see the mkMAC functions)

tests :: TestTree
tests = testGroup "MAC Unit" [
-- TODO add options for different levels of exhaustiveness / iterations
    Tests.MAC.Extended.tests,
    testGroup "Simple version" [
      testGroup "Exhaustive Tests" SE.singleExhaustiveTest,
      testGroup "Random Tests" SR.tinyRandomTests
      ]
  ]




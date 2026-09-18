module Tests.MAC where

import Test.Tasty ( testGroup, TestTree )

import Tests.MAC.Exhaustive as E ( singleExhaustiveTest )
import Tests.MAC.Random as R ( tinyRandomTests )


tests :: TestTree
tests = testGroup "MAC Unit" [
-- TODO add options for different levels of exhaustiveness / iterations
    testGroup "Exhaustive Tests" E.singleExhaustiveTest,
    testGroup "Random Tests" R.tinyRandomTests
  ]


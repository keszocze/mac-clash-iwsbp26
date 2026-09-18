module Tests.MAC where

import Test.Tasty ( testGroup, TestTree )

import Tests.MAC.Extended.Exhaustive as EE ( singleExhaustiveTest )
import Tests.MAC.Extended.Random as ER ( tinyRandomTests )


tests :: TestTree
tests = testGroup "MAC Unit" [
-- TODO add options for different levels of exhaustiveness / iterations
    extendedTests
  ]


extendedTests :: TestTree
extendedTests = testGroup "Extended version" [
    testGroup "Exhaustive Tests" EE.singleExhaustiveTest,
    testGroup "Random Tests" ER.tinyRandomTests
  ]


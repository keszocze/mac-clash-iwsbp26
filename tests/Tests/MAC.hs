module Tests.MAC where

import Test.Tasty



import Tests.MAC.Simple.Exhaustive as SE
import Tests.MAC.Simple.Random as SR
import Tests.MAC.Extended.Exhaustive as EE
import Tests.MAC.Extended.Random as ER


-- TODO unify test case generation (see the mkMAC functions) -- halfway done :)

tests :: TestTree
tests = testGroup "MAC Unit" [
-- TODO add options for different levels of exhaustiveness / iterations
    extendedTests,
    simpleTests
  ]


extendedTests :: TestTree
extendedTests = testGroup "Extended version" [
    testGroup "Exhaustive Tests" EE.singleExhaustiveTest,
    testGroup "Random Tests" ER.tinyRandomTests
  ]

simpleTests :: TestTree
simpleTests =  testGroup "Simple version" [
    testGroup "Exhaustive Tests" SE.singleExhaustiveTest,
    testGroup "Random Tests" SR.tinyRandomTests
    ]

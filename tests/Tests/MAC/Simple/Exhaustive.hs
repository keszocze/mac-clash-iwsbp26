module Tests.MAC.Simple.Exhaustive where

import Test.Tasty

import Tests.MAC.Simple.Exhaustive.TestTrees

tests :: TestTree
tests = testGroup "Exhaustive Tests" singleExhaustiveTest

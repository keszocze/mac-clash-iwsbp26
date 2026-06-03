module Tests.MAC.Exhaustive where

import Test.Tasty

import Tests.MAC.Exhaustive.TestTrees

tests :: TestTree
tests = testGroup "Exhaustive Tests" singleExhaustiveTest

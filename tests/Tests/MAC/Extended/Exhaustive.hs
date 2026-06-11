module Tests.MAC.Extended.Exhaustive where

import Test.Tasty

import Tests.MAC.Extended.Exhaustive.TestTrees

tests :: TestTree
tests = testGroup "Exhaustive Tests" singleExhaustiveTest

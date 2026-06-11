module Tests.MAC.Simple.Random where

import Test.Tasty

import Tests.MAC.Simple.Random.TestTrees

-- TODO make the selection of the tests configurable
tests :: TestTree
tests = testGroup "Random Tests" largeRandomTests


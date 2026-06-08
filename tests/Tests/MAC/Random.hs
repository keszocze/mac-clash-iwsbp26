module Tests.MAC.Random where

import Test.Tasty

import Tests.MAC.Random.TestTrees

-- TODO make the selection of the tests configurable
tests :: TestTree
tests = testGroup "Random Tests" largeRandomTests


module Tests.MAC.Extended.Random where

import Test.Tasty

import Tests.MAC.Extended.Random.TestTrees

-- TODO make the selection of the tests configurable
tests :: TestTree
tests = testGroup "Random Tests" tinyRandomTests


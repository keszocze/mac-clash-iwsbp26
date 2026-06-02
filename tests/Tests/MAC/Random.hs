module Tests.MAC.Random where

import Test.Tasty
import Test.Tasty.Hedgehog

import Tests.Util
import Tests.MAC.Random.TestTrees

import MAC.Mealy


-- TODO make the selection of the tests configurable
tests :: TestTree
tests = testGroup "Random Tests" tinyRandomTests


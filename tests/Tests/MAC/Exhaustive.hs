module Tests.MAC.Exhaustive where

import Test.Tasty
import Test.Tasty.HUnit

import Tests.Util
import Tests.MAC.Exhaustive.TestTrees

tests :: TestTree
tests = testGroup "Exhaustive Tests" tinyExhaustiveTests

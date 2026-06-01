import Prelude

import Test.Tasty

import qualified Tests.Example.Project

import qualified Tests.MAC

import Control.Arrow.Transformer.Automaton
import qualified Clash.Prelude as C


import Tests.Util


main :: IO ()
main = do
  defaultMain $ testGroup "."
    [
   --   Tests.Example.Project.accumTests,
      Tests.MAC.tests
    ]



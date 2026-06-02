import Prelude

import Test.Tasty


import qualified Tests.MAC

import Control.Arrow.Transformer.Automaton
import qualified Clash.Prelude as C


import Tests.Util


main :: IO ()
main = do
  defaultMain $ testGroup "Clash MAC IWSBP 2026"
    [
      Tests.MAC.tests
    ]



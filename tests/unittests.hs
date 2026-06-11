import Prelude

import Test.Tasty

import qualified Tests.MAC
import qualified Tests.OneHot


main :: IO ()
main = do
  defaultMain $ testGroup "Clash MAC IWSBP 2026"
    [
      Tests.MAC.tests
    --  Tests.OneHot.tests
    ]



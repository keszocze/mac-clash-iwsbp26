module MAC.Types.Internal where

import Clash.Prelude

data Stage = Ready | Multiplying | Accumulating deriving (Show, Generic, NFDataX)

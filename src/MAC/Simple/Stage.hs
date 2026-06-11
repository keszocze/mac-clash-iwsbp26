module MAC.Simple.Stage where

import Clash.Prelude

data Stage = Ready | Multiplying | Accumulating deriving (Show, Eq, Generic, NFDataX)

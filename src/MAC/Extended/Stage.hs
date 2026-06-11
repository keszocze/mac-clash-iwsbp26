module MAC.Extended.Stage where

import Clash.Prelude


data Stage = Ready | Multiplying | Accumulating | EndRound deriving (Show, Eq, Generic, NFDataX)

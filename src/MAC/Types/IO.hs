module MAC.Types.IO where

import Clash.Prelude

data Input (n :: Nat) (m :: Nat) = Input {
  values :: Maybe (Unsigned n, Unsigned m),
  newAcc :: Maybe (Unsigned (n+m))
} deriving (Show, Generic, NFDataX)

type Input' n = Input n n

data Output (n :: Nat) (m :: Nat) = Output {
  product :: Maybe (Unsigned (n+m)),
  accumulated :: Maybe (Unsigned (n+m))
} deriving (Eq, Generic, NFDataX)

instance (KnownNat n, KnownNat m) => Show (Output n m) where
  show Output{..} = "(product="<> maybe "" show product <> ", accumulated=" <> maybe "" show accumulated <> ")"

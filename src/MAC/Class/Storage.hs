module MAC.Class.Storage where

import Clash.Prelude

class Storage a where
  zero :: a
  advance :: a -> a
  reset :: Enum e => e -> a -> a

instance (KnownNat n) => Storage (BitVector n) where
  zero = 0b0
  advance bv = bv `rotateR` 1
  reset e bv = bv `rotateL` (fromEnum e)

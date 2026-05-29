module MAC.Util.OneHotCounter where

import Clash.Prelude
import Clash.Class.Counter

newtype OneHotCounter n = OneHotCounter (BitVector n) deriving (Eq, Show, Generic, NFDataX)

instance (KnownNat n) => Counter (OneHotCounter n) where
  countMin = OneHotCounter (1 :: BitVector n)
  countMax = OneHotCounter ((1 :: BitVector n) `rotateR` 1)

  countSuccOverflow cnt@(OneHotCounter val) = (cnt == countMax, OneHotCounter (val `rotateL` 1))

  countPredOverflow cnt@(OneHotCounter val) = (cnt == countMin, OneHotCounter (val `rotateR` 1))

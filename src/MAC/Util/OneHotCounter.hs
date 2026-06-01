module MAC.Util.OneHotCounter where

import Clash.Prelude
import Clash.Class.Counter


newtype OneHotCounter (n :: Nat) = OneHotCounter (BitVector n) deriving (Eq, Show, Generic, NFDataX)



instance (KnownNat n) => Counter (OneHotCounter n) where
  countMin = OneHotCounter (1 :: BitVector n)
  countMax = OneHotCounter ((1 :: BitVector n) `rotateR` 1)

  countSuccOverflow cnt@(OneHotCounter val) = (cnt == countMax, OneHotCounter (val `rotateL` 1))
  countPredOverflow cnt@(OneHotCounter val) = (cnt == countMin, OneHotCounter (val `rotateR` 1))


instance (KnownNat n) => Bounded (OneHotCounter n) where
  minBound = OneHotCounter (1 :: BitVector n)
  maxBound = OneHotCounter ((1 :: BitVector n) `rotateR` 1)


-- TODO Test the tripping behaviour etc. for this counter

-- NOTE: This is not a faithful instance as it does not throw a runtime exception when the int is too large
instance (KnownNat n) => Enum (OneHotCounter n) where
  toEnum :: forall n. (KnownNat n) => Int -> OneHotCounter n
  toEnum i = OneHotCounter ((1:: BitVector n) `rotateL` i)

  fromEnum :: forall n. (KnownNat n) => OneHotCounter n -> Int
  fromEnum (OneHotCounter val) = l - (zeros + 1)
    where
      l = natToNum @n @Int
      zeros = countLeadingZeros val

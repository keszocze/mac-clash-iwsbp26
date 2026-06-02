{-# LANGUAGE DerivingVia, UndecidableInstances #-}

module MAC.Types.BVec where

import Clash.Prelude

import MAC.Class.Storage

newtype BVec (n :: Nat) = BVec (Vec n Bit)
  deriving (Generic, NFDataX) via (Vec n Bit)
  deriving (Show, BitPack) via (Vec n Bit)

instance (KnownNat n) => Storage (BVec n) where
  zero = BVec (replicate (SNat @n) (0 :: Bit))
  advance (BVec v) = BVec $ v `rotateRight` (1 :: Bit)
  reset e (BVec v) = BVec $ v `rotateLeft` e


module MAC.Constraints where

import Clash.Prelude

import MAC.Class.Storage

type ConstraintNM n m (c :: Type -> Constraint) t = (c (t n), c (t m), c (t (n+m)))
type NatConstraints n m = (KnownNat n, KnownNat m, 1 <= n, 1 <= m)
type StorageConstraints n m storageType =   (BitSize (storageType (n + m)) ~ (n + m),    BitPack (storageType (n+m)),   Storage (storageType (n+m)))

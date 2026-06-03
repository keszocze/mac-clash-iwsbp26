{-# LANGUAGE DerivingVia, UndecidableInstances #-}

module MAC.Types.State where

import Clash.Prelude hiding (product)
import Clash.Class.Counter

import MAC.Class.Storage
import MAC.Types.Stage
import MAC.Types.IO

data State (n :: Nat) (m :: Nat) counterType storageType = State {
  stage :: Stage,
  -- TODO x,y auch in den storageType wrappen
  x :: Unsigned n,
  y :: Unsigned m,
  product :: storageType (n+m),
  accumulator :: storageType (n+m),
  xCounter :: counterType n,
  yCounter :: counterType m,
  accumulateCounter :: counterType (n+m),
  carry :: Bit
} deriving (Generic)

-- Benötigt UndecidableInstances
deriving instance (
  KnownNat n,
  KnownNat m,
  NFDataX (counterType n),
  NFDataX (counterType m),
  NFDataX (counterType (n+m)),
  NFDataX (storageType (n+m))
  ) => NFDataX (State n m counterType storageType)

deriving instance (
  KnownNat n,
  KnownNat m,
  Show (counterType n),
  Show (counterType m),
  Show (counterType (n+m)),
  Show (storageType (n+m))
  ) => Show (State n m counterType storageType)


initialState :: forall n m counterType storageType.
  (
    KnownNat n, 1 <= n, KnownNat m, 1 <= m,
    Counter (counterType n), Counter (counterType m), Counter (counterType (n+m)),
    Storage (storageType (n+m))
  )
  => State n m counterType storageType
initialState = State {
  stage = Ready,
  x=0,
  y=0,
  product = zero,
  accumulator = zero,
  xCounter = countMin :: (counterType n),
  yCounter = countMin :: (counterType m),
  accumulateCounter = countMin :: (counterType (n+m)),
  carry = 0
}

startMulState :: forall (n :: Nat) (m :: Nat) counterType storageType.
  (
    Counter (counterType n), Counter (counterType m), Counter (counterType (n+m)),
    Storage (storageType (n+m))
  ) =>
  Unsigned n -> Unsigned m -> State n m counterType storageType -> State n m counterType storageType
startMulState x y s = s {
  stage = Multiplying,
  x=x,
  y=y,
  product=zero,
  xCounter = countMin,
  yCounter = countMin,
  accumulateCounter = countMin
}

extractOuptut :: forall (n :: Nat) (m :: Nat) counterType storageType.
  (
    Counter (counterType n), Counter (counterType m), Counter (counterType (n+m)),
    Storage (storageType (n+m)), BitPack (storageType (n+m)), BitSize (storageType (n + m)) ~ (n+m)
  ) =>
  State n m counterType storageType -> Output n m
extractOuptut State{stage, product, accumulator} = case stage of
      Ready -> (Output (Just $ bitCoerce product) (Just $ bitCoerce accumulator))
      Multiplying -> Output Nothing (Just $ bitCoerce accumulator)
      Accumulating -> Output Nothing Nothing

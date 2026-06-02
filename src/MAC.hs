module MAC where

import Clash.Prelude

import qualified MAC.Mealy as Mealy
import MAC.Types

multiplicationDelay :: forall n m. (KnownNat n, KnownNat m) => Int
multiplicationDelay = (nInt * mInt) - 1
  where
    nInt = natToNum @n @Int
    mInt = natToNum @m @Int

accumulationDelay :: forall n m. (KnownNat n, KnownNat m) => Int
accumulationDelay = nInt + mInt
  where
    nInt = natToNum @n @Int
    mInt = natToNum @m @Int

totalDelay :: forall n m. (KnownNat n, KnownNat m) => Int
totalDelay = multiplicationDelay @n @m + accumulationDelay @n @m



mkMAC :: forall dom n m.
  (
    HiddenClockResetEnable dom,
    KnownNat n, 1 <= n,  KnownNat m, 1 <= m
  )
  =>
    Config ->
    Signal dom (Input n m) ->
    Signal dom (Output n m)
mkMAC cfg@Config{useState} = if useState then undefined else Mealy.mkMAC cfg

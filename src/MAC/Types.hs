{-# LANGUAGE UndecidableInstances #-}

module MAC.Types where

import Clash.Prelude
import Clash.Class.Counter

import MAC.Class.Storage
import MAC.Types.Internal
import MAC.Types.State



-- TODO nur eine Typdefinition draus machen
type AccumFun n m counterType storageType = State n m counterType storageType -> State n m counterType storageType
type MulFun n m counterType storageType = State n m counterType storageType -> State n m counterType storageType



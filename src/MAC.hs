module MAC where

import Clash.Prelude

import qualified MAC.Simple as S
import qualified MAC.Extended as E

import MAC.Constraints
import MAC.Config
import MAC.IO

mkMAC :: forall dom n m.
  (
    HiddenClockResetEnable dom,
    NatConstraints n m
  )
  =>
    Config ->
    Signal dom (Input n m) ->
    Signal dom (Output n m)
mkMAC cfg@Config{useExtraRoundStage} = if useExtraRoundStage then E.mkMAC cfg else S.mkMAC cfg

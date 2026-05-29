module MAC.Util where

import Clash.Prelude

-- | Augments a combined transition/output function with debug information
--
-- Original output is replace by a tuple consisting of
--
-- * the state before the current input was processed @s@
-- * the current input @i@
-- * the next state computed from @s@ and @i@
-- * the next output computed from @s@ and @i@
addDebugInfo ::
  -- | The combined transition/output function
  (s -> i -> (s, o)) ->
  -- | The augmented transition/output function
  (s -> i -> (s, (s, i, s, o)))
addDebugInfo f s i = let (s', o) = f s i in (s', (s, i, s', o))

-- | Augmentation of the `mealy` function with debug information
--
-- Use  this function in conjunction with `prettySimulateN` to debug your Mealy machines.
debugMealy ::
  (HiddenClockResetEnable dom, NFDataX s) =>
  -- | The combined transition/output function
  (s -> i -> (s, o)) ->
  -- | The initial state
  s ->
  -- | The input stream
  Signal dom i ->
  -- | The output stream
  Signal dom (s, i, s, o)
debugMealy f = mealy (addDebugInfo f)



-- | Pretty prints a simulation run
--
-- > clashi> prettySimulateN @System 6 (fmap (\n -> 2*n)) [0..5]
-- > 0
-- > 2
-- > 4
-- > 6
-- > 8
-- > 10

prettySimulateN ::
  (KnownDomain dom, NFDataX a, NFDataX b, Show b) =>
  Int ->
  ((HiddenClockResetEnable dom) => Signal dom a -> Signal dom b) ->
  [a] ->
  IO ()
prettySimulateN n f vals = mapM_ print $ simulateN n f vals

-- | Pretty prints a sample run
--
-- > clashi> prettySampleN 10 (clock @2 @2)
-- > (0,0)
-- > (0,0)
-- > (0,1)
-- > (0,2)
-- > (0,3)
-- > (1,0)
-- > (1,1)
-- > (1,2)
-- > (1,3)
-- > (2,0)
prettySampleN ::
  (KnownDomain dom, NFDataX a, Show a) =>
  Int ->
  ((HiddenClockResetEnable dom) => Signal dom a) ->
  IO ()
prettySampleN n f = mapM_ print $ sampleN n f

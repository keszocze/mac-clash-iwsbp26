module Tests.Util where

import Control.Arrow.Transformer.Automaton
import Clash.Prelude (
  natToNum, signalAutomaton, register, exposeClockResetEnable, enableGen, clockGen, resetGen, mul, Signal,  Unsigned, System,  KnownNat,  type System)

import Prelude hiding (product, pred)

import MAC
import MAC.Types.IO


allInputVals :: forall n m. (KnownNat n, KnownNat m) =>  [(Unsigned n, Unsigned m)]
allInputVals = [(x, y) | x <- [minBound .. maxBound], y <- [minBound .. maxBound]]







































-- TODO Herausfinden, wie ich das hier in automatisierten Tests nutzen kann
runCycle :: (Automaton (->) a b) -> a -> (b, (Automaton (->) a b))
runCycle (Automaton f) x = f x

whileM :: Monad m => (a -> Bool) -> (a -> m a) -> a -> m ()
whileM pred step value = do
  if pred value
    then step value >>= whileM pred step
    else pure ()

dut :: Signal System Int -> Signal System Int
dut = exposeClockResetEnable (register 0) clockGen resetGen enableGen

test :: IO ()
test = do
  whileM
    ((< 10) . fst)
    (\(i, auto) -> do
      let (output, nextAuto) = runCycle auto (output + 2)
      print $ show i <> ": " <> show output
      return (i + 1, nextAuto)
    )
    (0 :: Int, signalAutomaton dut)

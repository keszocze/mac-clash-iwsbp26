module Tests.Util where

import Control.Arrow.Transformer.Automaton
import Clash.Prelude (
  natToNum, simulateN, signalAutomaton, register, exposeClockResetEnable, enableGen, clockGen, resetGen, mul, Signal,  Unsigned, System,  KnownNat, type (<=), type (+), type System)

import Prelude hiding (product, pred)

import MAC.Mealy

prettySNat :: forall n. (KnownNat n) => String
prettySNat = show $ natToNum @n @Int


allInputVals :: forall n m. (KnownNat n, KnownNat m) =>  [(Unsigned n, Unsigned m)]
allInputVals = [(x, y) | x <- [minBound .. maxBound], y <- [minBound .. maxBound]]


testInputs :: forall n m. (KnownNat n, KnownNat m) => (Unsigned n, Unsigned m) -> [MACInput n m]
testInputs (x, y) = (MACInput (Just (x,y)) Nothing) : replicate  (totalDelay @n @m + 1) (MACInput Nothing Nothing)


expectedMulOutput :: forall n m. (KnownNat n, KnownNat m) => (Unsigned n, Unsigned m) -> [MACOutput n m]
expectedMulOutput (x,y) = multiplying ++ accumulating ++ displayingResult
  where
    multiplying = replicate (multiplicationDelay @n @m) (MACOutput Nothing (Just 0))
    accumulating = replicate (accumulationDelay @n @m) (MACOutput Nothing Nothing)
    displayingResult = replicate 1 (MACOutput product product) -- extend for more cycles?
      where product = Just $ mul x y

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

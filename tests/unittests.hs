import Prelude

import Test.Tasty

import qualified Tests.Example.Project
import Control.Arrow.Transformer.Automaton
import qualified Clash.Prelude as C

dut :: C.Signal C.System Int -> C.Signal C.System Int
dut = C.exposeClockResetEnable (C.register 0) C.clockGen C.resetGen C.enableGen

runCycle :: (Automaton (->) Int Int) -> Int -> (Int, (Automaton (->) Int Int))
runCycle (Automaton f) x = f x

whileM :: Monad m => (a -> Bool) -> (a -> m a) -> a -> m ()
whileM pred step value = do
  if pred value
    then step value >>= whileM pred step
    else pure ()

test :: IO ()
test = do
  whileM
    ((< 10) . fst)
    (\(i, auto) -> do
      let (output, nextAuto) = runCycle auto (output + 2)
      print $ show i <> ": " <> show output
      return (i + 1, nextAuto)
    )
    (0 :: Int, C.signalAutomaton dut)

main :: IO ()
main = do
  -- defaultMain $ testGroup "."
  --   [ Tests.Example.Project.accumTests
  --   ]
  test

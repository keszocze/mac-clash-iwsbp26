module Tests.MAC.Util where

--import Control.Arrow.Transformer.Automaton # temporarily unused
import Clash.Prelude hiding (zip3, map, delay)
import Clash.Hedgehog.Sized.Unsigned ( genUnsigned )

import Prelude hiding (product, pred)

import Test.Tasty ( testGroup, TestTree )
import Test.Tasty.HUnit ( testCase, assertEqual )
import Test.Tasty.Hedgehog ( testProperty )

import qualified Hedgehog as H
import Hedgehog (withTests, (===))
import qualified Hedgehog.Range as Range

-- TODO remove this imput
import MAC.Extended
import MAC ( mkMAC )

import Util ( prettySNat )

import MAC.Config
    ( describe', Config(..) )

allInputVals :: forall n m. (KnownNat n, KnownNat m) =>  [(Unsigned n, Unsigned m)]
allInputVals = [(x, y) | x <- [minBound .. maxBound], y <- [minBound .. maxBound]]


configsIWSBP26 :: [Config]
configsIWSBP26 = [ Config useModuleAdder useState useVector useRotation useOneHot |
  useModuleAdder  <- [False], -- we decided not to use the explicit module adder
  useState  <- [False, True] ,
  useVector  <- [False, True],
  useRotation  <- [False, True] ,
  useOneHot  <- [False, True]
  ]


exhaustiveTestsForSize ::
  forall n m.
  ( KnownNat n,
    1 <= n,
    KnownNat m,
    1 <= m
  ) =>
  TestTree
exhaustiveTestsForSize = testGroup name $ map (exhaustiveTest @n @m) configsIWSBP26
  where name = "n=" <> prettySNat @n <> " m=" <> prettySNat @m


exhaustiveTest ::
  forall n m.
  ( KnownNat n,
    1 <= n,
    KnownNat m,
    1 <= m
  ) =>
  Config -> TestTree
exhaustiveTest cfg = testCase name prop
  where
    name = describe' cfg
    delay = totalDelay @n @m
    inputStreams = map (testInputs @n @m) allInputVals
    expectedStreams = map (expectedMulOutput @n @m) allInputVals
    simulatedStreams = map (simulateN @System delay (MAC.mkMAC @System @n @m cfg)) inputStreams
    prop = do
      mapM_ (
          \((x,y), os, es) -> assertEqual ("Computing " <> show x <> " * " <> show y <> " failed") es os
        )
        $ zip3 (allInputVals @n @m) simulatedStreams expectedStreams



randomTestsForSize :: forall n m. (KnownNat n, KnownNat m, 1 <= n, 1 <= m) => TestTree
randomTestsForSize = testGroup name $ map (randomTest @n @m) configsIWSBP26
  where name = "n=" <> prettySNat @n <> " m=" <> prettySNat @m


randomTest ::
  forall n m.
  ( KnownNat n,
    1 <= n,
    KnownNat m,
    1 <= m
  ) =>
  Config -> TestTree
randomTest cfg = testProperty name $ withTests 200 prop
  where
    name = describe' cfg
    delay = totalDelay @n @m
    prop = H.property $ do
      x <- H.forAll $ genUnsigned (Range.linear (minBound :: Unsigned n) maxBound)
      y <- H.forAll $ genUnsigned (Range.linear (minBound :: Unsigned m) maxBound)
      let
        inputStream = (testInputs @n @m)  (x,y)
        expectedStream = (expectedMulOutput @n @m) (x,y)
        simulatedStream = simulateN @System delay (MAC.mkMAC @System @n @m cfg) inputStream
      H.annotate $ "Computing " <> show x <> " * " <> show y <> " failed"
      expectedStream === simulatedStream































-- Testing ignore everything below
-- runCycle :: (Automaton (->) a b) -> a -> (b, (Automaton (->) a b))
-- runCycle (Automaton f) x = f x

-- whileM :: Monad m => (a -> Bool) -> (a -> m a) -> a -> m ()
-- whileM pred step value = do
--   if pred value
--     then step value >>= whileM pred step
--     else pure ()

-- dut :: Signal System Int -> Signal System Int
-- dut = exposeClockResetEnable (register 0) clockGen resetGen enableGen

-- test :: IO ()
-- test = do
--   whileM
--     ((< 10) . fst)
--     (\(i, auto) -> do
--       let (output, nextAuto) = runCycle auto (output + 2)
--       print $ show i <> ": " <> show output
--       return (i + 1, nextAuto)
--     )
--     (0 :: Int, signalAutomaton dut)

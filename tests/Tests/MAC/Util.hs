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

import qualified MAC.Extended as E
import MAC ( mkMAC )
import MAC.IO ( Input, Output )

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

-- TODO simplify/remove
getInputGenFun :: forall  n m. (KnownNat n, KnownNat m) => Config -> (Unsigned n, Unsigned m) -> [Input n m]
getInputGenFun cfg = E.testInputs @n @m

-- TODO simplify/remove
getOutputGenFun :: forall  n m. (KnownNat n, KnownNat m) =>  Config -> (Unsigned n, Unsigned m) -> [Output n m]
getOutputGenFun cfg = E.expectedMulOutput @n @m

-- TODO simplify/remove
getTotalDelay :: forall n m. (KnownNat n, KnownNat m) =>  Config -> Int
getTotalDelay cfg =  E.totalDelay @n @m

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
    delay = getTotalDelay @n @m cfg
    inputStreams = map (getInputGenFun @n @m cfg) allInputVals
    expectedStreams = map (getOutputGenFun @n @m cfg) allInputVals
    simulatedStreams = map (simulateN @System delay (mkMAC @System @n @m cfg)) inputStreams
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
    delay = (getTotalDelay @n @m cfg)
    prop = H.property $ do
      x <- H.forAll $ genUnsigned (Range.linear (minBound :: Unsigned n) maxBound)
      y <- H.forAll $ genUnsigned (Range.linear (minBound :: Unsigned m) maxBound)
      let
        inputStream = (getInputGenFun @n @m cfg)  (x,y)
        expectedStream = (getOutputGenFun @n @m cfg) (x,y)
        simulatedStream = simulateN @System delay (mkMAC @System @n @m cfg) inputStream
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

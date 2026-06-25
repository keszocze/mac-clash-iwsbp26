module MAC where

import Clash.Prelude

import qualified Prelude as P

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

helper :: HiddenClockResetEnable System => Config -> IO ()
helper cfg = mapM_ myShow $ P.zip3 [1..] results (P.tail input)
  where
    input = E.is
    mac = mkMAC @System cfg
    n = P.length input
    results = simulateN @System n mac input

myShow (m,r,i) = putStrLn $ show m <> ":\t" <> myShow' r <> "\t" <> myShow'' i
myShow' (Output (Just p) _) = "Ready: product=" <> show p
myShow' (Output Nothing (Just _)) = "Multiplying"
myShow' (Output Nothing Nothing) = "Accumulating"

myShow'' (Input (Just (x,y)) _) = "Start multiplying " <> show x <> " * " <> show y
myShow'' _ = ""


foo :: IO ()
foo = putStrLn "Dinge"

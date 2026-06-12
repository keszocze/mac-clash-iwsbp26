{-# LANGUAGE AllowAmbiguousTypes #-}

module Util.IWSBP where




import qualified Control.Monad.State.Strict as ST
import Control.Monad.Extra


import Clash.Prelude
import qualified Prelude as P

import Data.String.Interpolate ( i, __i'L )

import MAC.Config

import System.IO

import Data.List (intercalate)

benchmarkName :: Int ->  Config -> String
benchmarkName n Config{useExtraRoundStage, useState, useVector, useRotation, useOneHot} =
  intercalate "_" ["benchmark", "MAC", show n, extra, approach, storage, accessing, counting]
  where
    extra = if useExtraRoundStage then "withEndRound" else "noEndRound"
    approach = if useState then "Monadic" else "Mealy"
    storage = if useVector then "Vector" else "BitVector"
    accessing = if useRotation then "Rotating" else "Indexing"
    counting = if useOneHot then "OneHotCounter" else "IndexCounter"

paperBenchmarks :: IO ()
paperBenchmarks = benchmarkFile "Paper" [2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,20,24,28,32,36,40,44,48,52,56,60,64] configsIWSBP26

fullBenchmarks :: IO ()
fullBenchmarks = benchmarkFile "Full" [2..64] configsIWSBP26

testBenchmarkGeneration :: IO ()
testBenchmarkGeneration = benchmarkFile "Test" [2] configsIWSBP26

benchmarkFile :: String -> [Int] -> [Config] -> IO ()
benchmarkFile name bitWidths configs = do
  oF <- openFile (name <> ".hs") WriteMode
  hPutStrLn oF $ moduleName name
  hPutStrLn oF header
  mapM_ (hPutStrLn oF) $ P.concat $ P.map (\n -> P.map (\cfg -> macEntity n cfg) configs) bitWidths
  hClose oF

macEntity ::  Int -> Config -> String
macEntity n' cfg = [i|
    #{annotation}
    #{funDef}
  |]
  where
    annotation = [__i'L|
      {-\# OPAQUE  #{funName} \#-}
      {-\# ANN #{funName}
        (Synthesize
            { t_name = "#{funName}"
            , t_inputs = [ PortName "clk"
                          , PortName "rst"
                          , PortName "ena"
                          , PortProduct "" [
                              PortProduct "mulParameters" [
                                PortName "doStartMultiplication",
                                PortProduct "values" [
                                  PortName "x",
                                  PortName "y"
                                ]
                              ],
                              PortProduct "accumulator" [
                                PortName "doSetAccumulator",
                                PortName "newAccumulatorValue"
                              ]
                            ]
                          ]
            , t_output =  PortProduct "" [
                            PortProduct "product" [PortName "is_valid", PortName "value"]
                          , PortProduct "accumulator" [PortName "is_valid", PortName "accumulator"]
              ]
        }) \#-}
      |] :: String
    funName = benchmarkName n' cfg
    n = show n'
    funDef = [__i'L|
        #{funName} :: Clock System -> Reset System -> Enable System -> Signal System (Input #{n} #{n}) -> Signal System (Output #{n} #{n})
        #{funName} = exposeClockResetEnable $ mkMAC @System @#{n} @#{n} (#{show cfg})
      |] :: String

moduleName :: String -> String
moduleName name = "module Benchmarks." <> name <> " where"

header :: String
header = [__i'L|
  import Clash.Prelude

  import MAC
  import MAC.Config
  import MAC.IO
  |] :: String

import Prelude

import Data.String.Interpolate ( i, __i'L )

import MAC.Config
    ( Config(useOneHot, Config, useState, useVector,
             useRotation) )

import System.IO ( hClose, hPutStrLn, openFile, IOMode(WriteMode) )

import Data.List (intercalate)

main :: IO ()
main = generateBenchmarks "IWSBP26" iwsbp26BitWidths configsIWSBP26

iwsbp26BitWidths :: [Int]
iwsbp26BitWidths = [2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,20,24,28,32,36,40,44,48,52,56,60,64]

configsIWSBP26 :: [Config]
configsIWSBP26 = [ Config useModuleAdder useState useVector useRotation useOneHot |
  useModuleAdder  <- [False], -- we decided not to use the explicit module adder
  useState  <- [False, True] ,
  useVector  <- [False, True],
  useRotation  <- [False, True] ,
  useOneHot  <- [False, True]
  ]



generateBenchmarks :: String -> [Int] -> [Config] -> IO ()
generateBenchmarks name widths configs = do
  mapM_ helper widths

  where
    helper :: Int -> IO ()
    helper width = do
      let
        fileName = benchmarkFileName name width
        filePath = "benchmarks_iwsbp26/generated_clash/clash/" <> fileName
      oF <- openFile filePath WriteMode
      hPutStrLn oF $ moduleName name width
      hPutStrLn oF header
      hPutStrLn oF $ concat $ map (\cfg -> macEntity name width cfg) configs
      hClose oF



macEntity ::  String -> Int -> Config -> String
macEntity name n' cfg = [i|
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
    funName = benchmarkFunName name n' cfg
    n = show n'
    funDef = [__i'L|
        #{funName} :: Clock System -> Reset System -> Enable System -> Signal System (Input #{n} #{n}) -> Signal System (Output #{n} #{n})
        #{funName} = exposeClockResetEnable $ mkMAC @System @#{n} @#{n} (#{show cfg})
      |] :: String





benchmarkFileName :: Show a => String -> a -> String
benchmarkFileName name width = benchmarkName name width <> ".hs"

benchmarkName :: Show a => String -> a -> String
benchmarkName name width = name <> "_BitWidth_" <> show width

benchmarkFunName :: String -> Int ->  Config -> String
benchmarkFunName name n Config{useState, useVector, useRotation, useOneHot} =
  intercalate "_" ["benchmark", "MAC", name, show n, approach, storage, accessing, counting]
  where
    approach = if useState then "Monadic" else "Mealy"
    storage = if useVector then "Vector" else "BitVector"
    accessing = if useRotation then "Rotating" else "Indexing"
    counting = if useOneHot then "OneHotCounter" else "IndexCounter"

moduleName :: String -> Int -> String
moduleName name width = "module Benchmarks." <> benchmarkName name width <> " where"

header :: String
header = [__i'L|
  import Clash.Prelude

  import MAC
  import MAC.Config
  import MAC.IO
  |] :: String


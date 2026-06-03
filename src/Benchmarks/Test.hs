module Benchmarks.Test where

import Clash.Prelude

import MAC
import MAC.Types



{-# ANN benchmark_MAC_4_ModuleFA_Mealy_BitVector_Indexing_IndexCounter
  (Synthesize
      { t_name = "benchmark_MAC_4_ModuleFA_Mealy_BitVector_Indexing_IndexCounter"
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
  }) #-}


benchmark_MAC_4_ModuleFA_Mealy_BitVector_Indexing_IndexCounter :: Clock System -> Reset System -> Enable System -> Signal System (Input 4 4) -> Signal System (Output 4 4)
benchmark_MAC_4_ModuleFA_Mealy_BitVector_Indexing_IndexCounter = exposeClockResetEnable $ mkMAC @System @4 @4 (Config {useModuleFullAdder = True, useState = False, useVector = False, useRotation = False, useOneHot = False})



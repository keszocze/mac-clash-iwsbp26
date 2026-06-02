module FullAdder where

import Clash.Prelude


-- | Simple one-bit full adder
fullAdder :: Bit -> Bit -> Bit -> (Bit, Bit)
fullAdder a b cIn = (cOut, s)
  where
    cOut = cIn .&. (a `xor` b) .|. (a .&. b)
    s = (a `xor` b) `xor` cIn


{-# OPAQUE fullAdderModule #-}
{-# ANN
  fullAdderModule
  ( Synthesize
      { t_name = "full_adder",
        t_inputs =
          [ PortName "a",
            PortName "b",
            PortName "c_in"
          ],
        t_output =
          PortProduct
            ""
            [ PortName "c_out",
              PortName "s"
            ]
      }
  )
  #-}
-- | Simple one-bit full adder with module annotation.
-- |
-- This is a version of fullAdder that should enforce the instantiation of a module.
fullAdderModule :: Bit -> Bit -> Bit -> (Bit, Bit)
fullAdderModule = fullAdder

{-# LANGUAGE OverloadedStrings #-}
module Weft.Setup (
  PrinterConfig (..),
  defaultConfig,
  setup
  ) where
import Weft.Monad
import Weft.Commands

setup conf = initialize conf >> home >> wait
data PrinterConfig = Config { extSpeed::Double,
                              extTemp::Int,
                              platTemp::Int
                            } deriving(Show,Eq)                                  
                                      
defaultConfig = Config {extSpeed = 1.98, extTemp = 220, platTemp = 125}

initialize conf = do
  emit "(initialization - set modes, temperature etc)"
  setMilimeters
  absolutePosition
  extruderOff
  toolSpeed 0 $ extSpeed conf
  extruderTemp 0 $ extTemp conf
  platformTemp 0 $ platTemp conf
home = do
  emit "(homing)"
  homeMax "Z" 500
  emit "G92 Z10"
  emit "G1 Z0.0"
  homeMax "Z" 100
  homeMin "X" 2500
  homeMin "Y" 2500
  emit "M132 X Y Z A B"
wait = do
  emit "(waiting)"
  move3 (52,-57,10) (Just 3300)
  waitForTemp 0
  extruderForward
  pause 5000
  extruderOff
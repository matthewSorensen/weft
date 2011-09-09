module Robotics.Thingomatic.Config where

data PrinterConfig = Config { extSpeed::Double,
                              extTemp::Int,
                              platTemp::Int,
                              layer::Double
                            } deriving(Show,Read,Eq)                                  
                                      
defaultConfig = Config {extSpeed = 1.98, extTemp = 220, platTemp = 125, layer = 0.35}
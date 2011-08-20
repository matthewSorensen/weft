{-# LANGUAGE OverloadedStrings #-}
module Weft.Commands where
import Weft.Monad
import qualified Data.ByteString.Char8 as B

ap = B.append
con = B.concat
bshow::Show a=>a->ByteString
bshow = B.pack . show

setMilimeters    = emit "G21"
setInches        = emit "G20"
steppersOff      = emit "M18"
extruderForward  = emit "M101"
extruderReverse  = emit "M102"
extruderOff      = emit "M103"
absolutePosition = emit "G90"

pause::Int->Print ()
pause milis      = emit $ ap "G4 P" (bshow milis)

waitForTemp::Int->Int->Print ()
waitForTemp tool temp = do
  emit $ con ["M104 S",bshow temp," T",bshow tool] -- Set the temp
  emit $ ap "M6 T" (bshow tool) -- Wait for the tool to reach temp
  
-- Should we set the location to (0,0,0) after this?
setCurrentHome::(Int,Int,Int)->Print ()
setCurrentHome (x,y,z) = emit $ con ["G92 X",bshow x," Y",bshow y," Z",bshow z, " "]

setToolSpeed::Int->Int->Print ()
setToolSpeed tool speed = emit $ con ["M",bshow (1+tool),"08 S",bshow speed]

platformTemp::Int->Int->Print ()
platformTemp tool temp = emit $ con ["M109 S",bshow temp," T",bshow tool]

move3::Point3->Double->Print ()
move3 p@(x,y,z) speed = do
  setLocation p
  emit $ con ["G1 X",bshow x," Y",bshow y," Z",bshow z," F",bshow speed]  
-- Make a 2d movement on the same z-plane
move2::Point2->Double->Print ()
move2 (x,y) speed = do
  (_,_,z) <- getLocation
  move3 (x,y,z) speed
-- move relative to the current location
moveRelative::Point3->Double->Print ()
moveRelative (x,y,z) speed = do
  (x',y',z') <- getLocation
  move3 (x+x',y+y',z+z') speed
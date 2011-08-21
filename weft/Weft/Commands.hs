{-# LANGUAGE OverloadedStrings #-}
module Weft.Commands where
import Weft.Monad
import Weft.Points
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
conveyorOn       = emit "M106"
conveyorOff      = emit "M107"

pause::Int->Print ()
pause milis      = emit $ ap "G4 P" (bshow milis)

waitForTemp::Int->Print ()
waitForTemp tool = emit $ ap "M6 T" (bshow tool) 

setCurrentHome::(Int,Int,Int)->Print ()
setCurrentHome (x,y,z) = emit $ con ["G92 X",bshow x," Y",bshow y," Z",bshow z, " "]

toolSpeed::Int->Double->Print ()
toolSpeed tool speed = emit $ con ["M",bshow (1+tool),"08 R",bshow speed]

platformTemp::Int->Int->Print ()
platformTemp tool temp = emit $ con ["M109 S",bshow temp," T",bshow tool]

extruderTemp::Int->Int->Print ()
extruderTemp tool temp =   emit $ con ["M104 S",bshow temp," T",bshow tool] 

setFeedrate::Double->Print ()
setFeedrate r = getPrinterState >>= setPrinterState . (\s-> s {feedrate = Just r})


move::Point a=>a->Maybe Double->Print ()
move p speed = do
  feed <- normalizeFeedrate speed
  l@(x,y,z) <- fmap (lift3 p) getLocation
  setLocation l
  emit $ con ["G1 X",bshow x," Y",bshow y," Z",bshow z," F",bshow feed]  
moveRel::Point a=>a->Maybe Double->Print ()
moveRel p speed = do
  feed <- normalizeFeedrate speed
  l@(x,y,z) <- fmap (lift3Rel p) getLocation
  setLocation l
  emit $ con ["G1 X",bshow x," Y",bshow y," Z",bshow z," F",bshow feed]  

homeMax::B.ByteString->Double->Print ()
homeMax axis feed = emit $ con ["G162 ",axis," F",bshow feed]
  
homeMin::B.ByteString->Double->Print ()
homeMin axis feed = emit $ con ["G161 ",axis," F",bshow feed]

comment::String->Print ()
comment s = emit $ con ["( ",B.pack s," )"]
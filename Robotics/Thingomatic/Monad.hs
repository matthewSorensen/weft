{-# LANGUAGE OverloadedStrings, PackageImports, TupleSections #-} 
module Robotics.Thingomatic.Monad where

import Robotics.Thingomatic.Points
import "monads-fd" Control.Monad.State
import "monads-fd" Control.Monad.Trans
import qualified Data.ByteString as B

type ByteString = B.ByteString

data PrinterState = PState {output::ByteString->IO (),
                            feedrate::Double}
               
type Print = StateT (Point3,PrinterState) IO

getLocation::Print Point3
getLocation = fmap fst get

getPrinterState::Print PrinterState
getPrinterState = fmap snd get

setLocation::Point3->Print ()
setLocation l = get >>= put . (l,) . snd

setPrinterState::PrinterState->Print ()
setPrinterState s = get >>= put . (,s) . fst

emit::ByteString->Print ()
emit b = getPrinterState >>= (\s-> liftIO $ (output s) b)

runWithIOAction::(ByteString->IO ())->Print a->IO a
runWithIOAction dest instr = let init = ((0,0,0), PState {output = dest, feedrate = 1000})
                             in evalStateT instr init

getFeedrate::Print Double
getFeedrate = fmap feedrate getPrinterState
  
setFeedrate::Double->Print ()
setFeedrate r = getPrinterState >>= setPrinterState . (\s-> s {feedrate = r})

withRate::Double->Print a->Print a
withRate r action = do
  r' <- getFeedrate
  setFeedrate r
  a <- action
  setFeedrate r'
  return a
  
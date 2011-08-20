{-# LANGUAGE OverloadedStrings, PackageImports, TupleSections #-} 
module Weft.Monad where

import "monads-fd" Control.Monad.State
import "monads-fd" Control.Monad.Trans
import qualified Data.ByteString as B

type ByteString = B.ByteString

data PrinterState = PState {output::ByteString->IO ()}
               
type Point3 = (Double,Double,Double)
type Point2 = (Double,Double)

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
runWithIOAction dest instr = let init = ((0,0,0), PState {output = dest})
                             in evalStateT instr init
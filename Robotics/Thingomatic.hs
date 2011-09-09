module Robotics.Thingomatic 
       (driver,testRaft)
       where
import Robotics.Thingomatic.Commands
import Robotics.Thingomatic.Setup
import Robotics.Thingomatic.Monad
import Data.ByteString hiding (filter,readFile)

import System.IO  hiding (hPutStrLn,putStrLn)
import System.FilePath.Posix (takeExtension)
import System.Environment (getArgs)
import Prelude    hiding (putStrLn)
import Data.Maybe (listToMaybe)

-- Attempt to find an output file and a conf file
driver::(PrinterConfig->Print ())->IO ()
driver act = do
  args <- getArgs
  print <-  fmap act  $ maybe (return defaultConfig) (fmap read . readFile) $ fileOfType "txt" args
  let toFile path = withFile path WriteMode $ ($ print) . runWithIOAction . hPutStrLn
    in  maybe (runWithIOAction putStrLn print) toFile  $ fileOfType ".gcode" args
 
fileOfType::String->[String]->Maybe String
fileOfType ext = listToMaybe . filter ((ext==).takeExtension)

testRaft c = printModel c $ raft c (10,10) (30,30)
  
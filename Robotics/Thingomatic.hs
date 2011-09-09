module Robotics.Thingomatic 
       (driver,testRaft)
       where
import Robotics.Thingomatic.Commands
import Robotics.Thingomatic.Setup
import Robotics.Thingomatic.Monad
import Robotics.Thingomatic.Config
import Data.ByteString hiding (filter,readFile)

import System.IO  hiding (hPutStrLn,putStrLn)
import System.FilePath.Posix (takeExtension)
import System.Environment (getArgs)
import Prelude    hiding (putStrLn)
import Data.Maybe (listToMaybe)

-- Attempt to find an output file and a conf file
driver::Print ()->IO ()
driver act = do
  args <- getArgs
  conf <- maybe (return defaultConfig) (fmap read . readFile) $ fileOfType ".conf" args 
  let toFile path = withFile path WriteMode $ ($ act) . runWithIOAction conf . hPutStrLn
    in  maybe (runWithIOAction conf putStrLn act) toFile  $ fileOfType ".gcode" args
 
fileOfType::String->[String]->Maybe String
fileOfType ext = listToMaybe . filter ((ext==).takeExtension)

testRaft = printModel $ raft  (10,10) (30,30)
  
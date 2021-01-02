import qualified Data.Map as Map
import System.Environment
import System.IO
import System.Process
import Control.Monad
import Data.List
import Data.Maybe (fromJust)

import StuckParse
import StuckCompile
import Parsing

{- IO stuff -}

getFileName :: [String] -> IO String
getFileName [] = error "No file provided"
getFileName (s : str)
  | (take 6 . reverse) s == "kcuts." = return s
  | otherwise = getFileName str


main = do
  args      <- getArgs
  filename  <- getFileName args
  handle    <- openFile filename ReadMode
  contents  <- hGetContents handle

  let outFilename = reverse $ drop 6 $ reverse filename
      fileLines   = filter (\x -> lineContents x /= []) . linesToStuckLines 1 . lines $ contents
      fnLines     = tail $ collectFunctionBlocks fileLines []
      functions   = collectFunctions fnLines Map.empty
      argMap      = Map.fromList [ (fName f, fArgs f) | f <- functions ]
      lFuncName   = fName $ last functions
      argCount    = length (fromJust (Map.lookup lFuncName argMap))
      compiled    = boilerplate1 ++ compileFunctions functions argMap ++ boilerplate2 lFuncName argCount
      gccCommand  = "gcc " ++ outFilename ++ ".c -o " ++ outFilename
      cFileRemove = "rm " ++ outFilename ++ ".c"

  writeFile (outFilename ++ ".c") compiled

  callCommand gccCommand
  callCommand cFileRemove
  hClose handle

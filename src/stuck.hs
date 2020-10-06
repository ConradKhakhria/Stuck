import System.Environment
import System.IO
import Control.Monad
import Data.List
import Data.Map (empty)

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
      functions   = collectFunctions fnLines empty
      
  writeFile outFilename $ show functions
  hClose handle

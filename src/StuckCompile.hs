{- Converts parse-trees to assembly -}

module StuckCompile where
import Data.List (elemIndex)

import StuckParse

data CompileState =
  State { cName     ::  String
        , cArgCount ::  Int
        , cJmpStack :: [Int]
        , cJmpCount ::  Int }

compileMaths :: ParseTree -> Integer -> [String] -> String
compileMaths (NUM n) args lineNumber
  | "." `elem` n  = error $ pLine ++ ": floating point numbers not supported"
  | not32bits     = error $ pLine ++ ": numbers must be 32 bits"
  | otherwise     = "push" ++ n
  where pLine     = "Line " ++ show lineNumber
        not32bits = abs (read n :: Integer) > 2147483647

compileMaths (SYM a) args lineNumber
  | 
  where argIndex = elemIndexs a args


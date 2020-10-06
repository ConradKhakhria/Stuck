{- Parse functions for the Stuck language
 -
 - As the language is so minimal, the process of tokenisation
 - can be reduced to a pair of functions that transform each
 - line into a StuckLine, and then the Stucklines are directly
 - turned into parse trees
 -}

module StuckParse where
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import Data.List (elemIndex)

import Parsing

data StuckLine =
  StuckLine { lineIndent   :: Int
            , lineContents :: [String]
            , lineNumber   :: Integer } deriving (Show, Eq)

data Instruction 
  = END
  | INPUT
  | OUTPUT
  | PUSH Int
  | POP  Int
  | COND Int
  | CALL { callFuncName  :: String
         , callArguments :: [ParseTree] } deriving (Show, Eq)

data Function = 
  Function { fName :: String
           , fArgs :: [String]
           , fBody :: [Instruction] } deriving (Show, Eq)

type FMAP = Map.Map String [String]

{- Misc -}

-- init with first arg  = []
getStrings :: String -> String -> [String]
getStrings [] curr      = if null curr then [] else [reverse curr]
getStrings (s : str) curr
  | s `elem` ['a'..'z'] = getStrings str (s : curr)
  | null curr           = getStrings str curr
  | otherwise           = reverse curr : getStrings str ""

stringsInArgs :: [String] -> [String] -> Bool
stringsInArgs _ [] = True
stringsInArgs args (s : str)
  | s `elem` args = stringsInArgs args str
  | otherwise     = False

{- Here is where each line is converted to a StuckLine -}

indentTuple :: String -> Int -> (Int, String)
indentTuple [] spaces          = (spaces, "")
indentTuple (' ' : str) spaces = indentTuple str $ succ spaces
indentTuple ( s  : str) spaces = (spaces, s : str)

removeComment :: String -> String
removeComment []        = []
removeComment ('#' : _) = []
removeComment (s : str) = s : removeComment str

processLine :: String -> Integer -> StuckLine
processLine line n =
  StuckLine { lineIndent = indent, lineContents = words pLine, lineNumber = n }
  where (indent, rLine)  = indentTuple line 0
        pLine = removeComment rLine

linesToStuckLines :: Integer -> [String] -> [StuckLine]
linesToStuckLines _ [] = []
linesToStuckLines n (l : ls) = processLine l n : linesToStuckLines (succ n) ls

{- Here is where each Stuckline is converted to a Function record -}

-- Init with curr = []
collectFunctionBlocks :: [StuckLine] -> [StuckLine] -> [[StuckLine]]
collectFunctionBlocks [] curr = if null curr then [] else [reverse curr]
collectFunctionBlocks (l : lines) curr
  | lineIndent l == 0 = reverse curr : collectFunctionBlocks lines [l]
  | otherwise         = collectFunctionBlocks lines (l : curr)

{- Generates Instruction record of a known type -}

genPush :: StuckLine -> [String] -> Instruction
genPush line args
  | lineLen < 2 = error $ pLine ++ ": no arguments supplied to push operation"
  | lineLen > 2 = error $ pLine ++ ": too many arguments supplied to push operation"
  | not $ pushArg `elem` args = error $ pLine ++ ": unknown argument" ++ pushArg
  | otherwise   = PUSH  $ Maybe.fromJust $ elemIndex pushArg args
  where lineLen = length . lineContents $ line
        pLine   = "Line " ++ (show . lineNumber) line
        pushArg = (lineContents line) !! 1

genPop :: StuckLine -> [String] -> Instruction
genPop line args
  | lineLen < 2 = error $ pLine ++ ": no arguments supplied to pop operation"
  | lineLen > 2 = error $ pLine ++ ": too many arguments supplied to pop operation"
  | not $ popArg `elem` args = error $ pLine ++ ": unknown argument" ++ popArg
  | otherwise   = POP   $ Maybe.fromJust $ elemIndex popArg args
  where lineLen = length . lineContents $ line
        pLine   = "Line " ++ (show . lineNumber) line
        popArg  = (lineContents line) !! 1

genCond :: StuckLine -> [String] -> Instruction
genCond line args = COND $ Maybe.fromJust $ elemIndex (head (lineContents line)) args

genCall :: StuckLine -> FMAP -> String -> Instruction
genCall line m func
  | not $ Map.member func m       = error $ pLine ++ ": unknown function " ++ func
  | length argExprs < length args = error $ pLine ++ ": insufficient arguments " ++ suppl
  | length argExprs > length args = error $ pLine ++ ": too many arguments "     ++ suppl
  | not argsValid = error $ pLine ++ ": function arguments contain unknown parameters"
  | otherwise     = CALL { callFuncName = func, callArguments = map parse argExprs }
  where pLine     = "Line " ++ (show . lineNumber) line
        args      = Maybe.fromJust $ Map.lookup func m
        argExprs  = tail  $ lineContents line
        argsValid = not   $ False `elem` [stringsInArgs args $ getStrings s "" | s <- argExprs]
        suppl     = "supplied to function" -- only way to make the line length reasonable

generateInstructions :: [StuckLine] -> FMAP -> Int -> String -> [Instruction]
generateInstructions [] _ _ _ = []
generateInstructions (l : lines) map prevIndent currName
  | lineIndent l < prevIndent = END : generateInstructions (l : lines) map (lineIndent l) currName
  | firstWord == ">"          = genPush l args : next
  | firstWord == "<"          = genPop  l args : next
  | firstWord `elem` args     = genCond l args : next
  | Map.member firstWord map  = call           : next
  | lineContents l == ["?"]   = INPUT          : next
  | lineContents l == ["!"]   = OUTPUT         : next
  | otherwise     = error $ "Line " ++ show (lineNumber l) ++ ": unrecognised syntax"
  where firstWord = (head . lineContents) l
        args      = Maybe.fromJust $ Map.lookup currName map
        call      = genCall l map firstWord
        next      = generateInstructions lines map (lineIndent l) currName

-- Init with funcs = []
collectFunctions :: [[StuckLine]] -> FMAP -> [Function]
collectFunctions [] _ = []
collectFunctions (f : fns) map =
  Function { fName = name, fArgs = args, fBody = body } : next
  where firstLine  = head f
        name       = head . lineContents $ firstLine
        args       = tail . lineContents $ firstLine
        newMap     = Map.insert name args map
        body       = generateInstructions (tail f) newMap 0 name
        next       = collectFunctions fns newMap

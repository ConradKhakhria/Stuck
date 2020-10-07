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
            , lineNumber   :: Int } deriving (Show, Eq)

data Instruction 
  = END      Int
  | INPUT    Int
  | OUTPUT   Int
  | PUSH Int Int
  | POP  Int Int
  | COND Int Int
  | CALL { cFuncName :: String
         , cArgs     :: [ParseTree]
         , cLineNo   :: Int } deriving (Show, Eq)

data Function = 
  Function { fName :: String
           , fArgs :: [String]
           , fBody :: [Instruction] } deriving (Show, Eq)

type FMAP = Map.Map String [String]

{- Confirm what the Instruction is -}

isEnd  (END _)      = True
isEnd _             = False

isInput (INPUT _)   = True
isInput _           = False

isOutput (OUTPUT _) = True
isOutput _          = False

isPush (PUSH _ _)   = True
isPush _            = False

isPop (POP _ _)     = True
isPop _             = False

isCond (COND _ _ )  = True
isCond _            = False

isCall (CALL _ _ _) = True
isCall _            = False


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

processLine :: String -> Int -> StuckLine
processLine line n =
  StuckLine { lineIndent = indent, lineContents = words pLine, lineNumber = n }
  where (indent, rLine)  = indentTuple line 0
        pLine = removeComment rLine

linesToStuckLines :: Int -> [String] -> [StuckLine]
linesToStuckLines _ [] = []
linesToStuckLines n (l : ls) = processLine l n : linesToStuckLines (succ n) ls

{- Here is where each StuckLine is converted to a function record -}

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
  | otherwise   = PUSH (Maybe.fromJust (elemIndex pushArg args)) lineNo
  where lineLen = length . lineContents $ line
        lineNo  = lineNumber line
        pLine   = "Line " ++ show lineNo
        pushArg = (lineContents line) !! 1

genPop :: StuckLine -> [String] -> Instruction
genPop line args
  | lineLen < 2 = error $ pLine ++ ": no arguments supplied to pop operation"
  | lineLen > 2 = error $ pLine ++ ": too many arguments supplied to pop operation"
  | not $ popArg `elem` args = error $ pLine ++ ": unknown argument" ++ popArg
  | otherwise   = POP (Maybe.fromJust (elemIndex popArg args)) lineNo
  where lineLen = length . lineContents $ line
        lineNo  = lineNumber line
        pLine   = "Line " ++ show lineNo
        popArg  = (lineContents line) !! 1

genCond :: StuckLine -> [String] -> Instruction
genCond line args
  | lineLen /= 1     = error $ pLine ++ ": multiple symbols found in conditional operation"
  | otherwise        = COND condArgIndex lineNo
  where condArgument = head . lineContents $ line
        lineLen      = length . lineContents $ line
        lineNo       = lineNumber line
        pLine        = "Line " ++ show lineNo
        condArgIndex = Maybe.fromJust $ elemIndex condArgument args

genCall :: StuckLine -> [String] -> Instruction
genCall line args
  | length argExprs  < length args = error $ pLine ++ ": not enough arguments" ++ suppl
  | length argExprs  > length args = error $ pLine ++ ": too many arguments"   ++ suppl
  | not argsValid    = error $ pLine ++ ": function arguments contain unknown parameters"
  | otherwise        = CALL { cFuncName = func, cArgs = map parse argExprs, cLineNo = lineNo }
  where lineNo       = lineNumber line
        pLine        = "Line " ++ show lineNo
        func         = head $ lineContents line
        argExprs     = tail $ lineContents line
        argsValid    = not  $ False `elem` [stringsInArgs args $ getStrings s "" | s <- argExprs]
        suppl        = "supplied to function" -- only way to make the line length reasonable

-- currName is the name of the function these instructions are in
generateInstructions :: [StuckLine] -> FMAP -> Int -> String -> [Instruction]
generateInstructions [] _ _ _ = []
generateInstructions (l : lines) map pIndent currName
  | lineIndent l < pIndent    = END lineNo         : endNext
  | firstWord == ">"          = genPush l currArgs : next
  | firstWord == "<"          = genPop  l currArgs : next
  | firstWord `elem` currArgs = genCond l currArgs : next
  | Map.member firstWord map  = genCall l callArgs : next
  | lineContents l == ["?"]   = INPUT  lineNo      : next
  | lineContents l == ["!"]   = OUTPUT lineNo      : next
  | otherwise     = error $ pLine ++ ": unrecognised syntax"
  where firstWord = head . lineContents $ l
        currArgs  = Maybe.fromJust $ Map.lookup currName map
        callArgs  = Maybe.fromJust $ Map.lookup firstWord map
        lineNo    = lineNumber l
        pLine     = "Line " ++ show lineNo
        endNext   = generateInstructions (l : lines) map (lineIndent l) currName
        next      = generateInstructions lines map (lineIndent l) currName

collectFunctions :: [[StuckLine]] -> FMAP -> [Function]
collectFunctions [] _          = []
collectFunctions (f : fns) map =
  Function { fName = name, fArgs = args, fBody = body } : collectFunctions fns newMap
  where firstLine  = lineContents $ head f
        name       = head firstLine
        args       = tail firstLine
        newMap     = Map.insert name args map
        bodyBlock  = tail f
        (i1, i2)   = (lineIndent (head bodyBlock), lineIndent (last bodyBlock))
        ends       = [ END (lineNumber (last bodyBlock)) | _ <- [1..(i2 `div` i1)] ]
        body       = generateInstructions bodyBlock newMap 0 name ++ ends


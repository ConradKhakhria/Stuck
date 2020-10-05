{- A library for parsing and evaluating algebraic expressions
 -
 - Currently, it converts every number to Float because there's no
 - good way to pass types to functions. However, as the actual
 - conversion from string to number doesn't take place until
 - eval is called, this creates no real problems
 -
 - Author: Conrad Khakhria
 -}

module Parsing
( pLexer
, parseTokens
, parse
, ParseNode (..)
, ParseTree (..)
) where
import qualified Data.Maybe as M
import Data.Char (ord)

{- Parse tree stuff -}

-- Both numbers and functions here are stored as strings so that
-- evaluation of them can be delayed until eval is called.
-- Also, SYM uses a string to allow for multi-character variable names
data ParseNode = NUM String
               | SYM String
               | NODE { pOperation :: String
                      , pElems     :: [ParseNode] } deriving (Show, Eq)

type ParseTree = ParseNode

-- This is unfortunately the best solution
pIsNUM :: ParseNode -> Bool
pIsNUM (NUM _)  = True
pIsNUM _        = False

pIsSYM :: ParseNode -> Bool
pIsSYM (SYM _)  = True
pIsSYM _        = False

pGetNUM :: ParseNode -> M.Maybe String
pGetNUM (NUM a) = M.Just a
pGetNUM _       = M.Nothing

{- Lexer stuff -}

data Token = TNum         String
           | TOperator    String
           | TWord        String
           | TOpenBracket
           | TCloseBracket deriving (Show, Eq)

getTNum :: Token -> M.Maybe String
getTNum (TNum a) = M.Just a
getTNum _        = M.Nothing

getTOperator :: Token -> M.Maybe String
getTOperator (TOperator a) = M.Just a
getTOperator _             = M.Nothing

getTWord :: Token -> M.Maybe String
getTWord (TWord a) = M.Just a
getTWord _         = M.Nothing

isTOpenBracket :: Token -> Bool
isTOpenBracket (TOpenBracket) = True
isTOpenBracket _              = False

isTCloseBracket :: Token -> Bool
isTCloseBracket (TCloseBracket) = True
isTCloseBracket _               = False



pCollectList :: String -> String -> [Char] -> (String, String)
pCollectList ps [] _ = (ps, [])
pCollectList ps (s : str) xs
  | s `elem` xs = pCollectList (s : ps) str xs
  | otherwise   = (ps, s : str)

pLexer :: String -> [Token]
pLexer [] = []
pLexer (s : str)
  | s `elem` "+-*/^%"      = TOperator [s] : pLexer str
  | s `elem` "1234567890.-" = TNum      (reverse collectNum) : pLexer nRem
  | s `elem` ['a'..'z']    = TWord     (reverse collectWrd) : pLexer wRem
  | s == '('  = TOpenBracket  : pLexer str
  | s == ')'  = TCloseBracket : pLexer str
  | s == ' '  = pLexer str
  | otherwise = error "Unrecognised syntax"
  where (collectNum, nRem) = pCollectList [] (s : str) "1234567890."
        (collectWrd, wRem) = pCollectList [] (s : str) ['a'..'z']

{- Parser stuff -}

-- init with expr [] [] op 0
splitAtOp :: [Token] -> [Token] -> [[Token]] -> Char -> Integer -> [[Token]]
splitAtOp [] l elems _ _ = reverse $ map reverse $ l : elems
splitAtOp (t : ts) l elems x n
  | isTOpenBracket  t   = splitAtOp ts (t : l) elems x (n + 1)
  | isTCloseBracket t   = splitAtOp ts (t : l) elems x (n - 1)
  | isTOperator t       = confirmOperator
  | otherwise           = splitAtOp ts (t : l) elems x n
  where isTOperator     = M.isJust . getTOperator
        confirmOperator = if (M.fromJust . getTOperator) t == [x] && n == 0
                            then splitAtOp ts [] (l : elems) x n
                            else splitAtOp ts (t : l) elems x n

findSplit :: String -> [Token] -> (Char, [[Token]])
findSplit [] _ = ('f', []) -- Because the parse function can handle this error itself
findSplit (p : ops) expr
  | length elems > 1 = (p, elems)
  | otherwise        = findSplit ops expr
  where elems = splitAtOp expr [] [] p 0

isLn :: Token -> Bool
isLn (TWord w) = w `elem` ["log", "ln"]
isLn _         = False

justNum :: [Token] -> Bool
justNum [(TNum _)] = True
justNum _          = False

parseTokens :: [Token] -> ParseTree
parseTokens expr
  | justNum expr        = NUM  $ M.fromJust . getTNum  . head $ expr
  | length expr == 1    = SYM  $ M.fromJust . getTWord . head $ expr
  | not . null $ elems  = NODE { pOperation = [op], pElems = parsedElems }
  | isLn . head $ expr  = NODE { pOperation = "ln", pElems = bracketContents }
  | otherwise           = parseTokens . init . tail $ expr
  where (op, elems)     = findSplit "+-*/^" expr
        parsedElems     = map parseTokens elems
        bracketContents = [(parseTokens . drop 2 . init) expr]

parse :: String -> ParseTree
parse = parseTokens . pLexer

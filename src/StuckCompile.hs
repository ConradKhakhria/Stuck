{- Converts parse-trees to assembly -}

module StuckCompile where
import qualified Data.Maybe as M
import Data.List (elemIndex, intersperse)

import StuckParse
import Parsing

data CompileState =
  State { cName     ::  String
        , cArgCount ::  Int
        , cJmpStack :: [Int]
        , cJmpCount ::  Int }

{- Compiles parse trees -}

compileMaths :: ParseTree -> [String] -> Integer -> String
compileMaths (NUM n) args lineNumber
  | '.' `elem` n  = error $ pLine ++ ": floating point numbers not supported"
  | not32bits     = error $ pLine ++ ": numbers must be 32 bits"
  | otherwise     = "push "  ++ n ++ "\n"
  where pLine     = "Line " ++ show lineNumber ++ "\n"
        not32bits = abs (read n :: Integer) > 2147483647

compileMaths (SYM a) args lineNumber = "push [ebp + " ++ show argOffset ++ "]\n"
  where argIndex  = M.fromJust $ elemIndex a args
        argOffset = 4 * argIndex + 4

compileMaths (NODE {pOperation = op, pElems = elems}) args lineNumber =
  pushElems ++ "mov eax, [esp]\n" ++ combine ++ cleanup
  where pushElems = concat $ reverse [compileMaths e args lineNumber | e <- elems]
        movEBX    = ["mov ebx, [esp+" ++ show (i*4) ++ "]\n" | i <- [1..(length args)]]
        combine   = concat [mov ++ opInst | mov <- movEBX]
        cleanup   = "add esp, " ++ show (4 * length args) ++ "\nmov [esp], eax\n"
        opInst    = case op of
                      "+" -> "add eax, ebx\n"
                      "-" -> "sub eax, ebx\n"
                      "*" -> "mul ebx\n"
                      "/" -> "xor edx, edx\ndiv ebx"
                      "^" -> error "Exponents not yet implemented"

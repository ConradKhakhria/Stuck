module StuckCompile where

import Parsing
import StuckParse

data CompileState =
  CompileState { cFuncName :: String
               , cFuncArgs :: [String]
               , cJmpStack :: [Int]
               , cJmpCount :: Int } deriving (Show, Eq)

{- Misc -}

argOffset :: Int -> String
argOffset n = show $ 4 * (n + 1)

{- Compiles ParseTrees -}

compileMaths :: ParseTree -> [String] -> Int -> String
compileMaths (NUM n) args lineNum
  | '.' `elem` n  = error $ pLine ++ ": floating point numbers not supported"
  | not32bits     = error $ pLine ++ ": numbers must be 32 bits"
  | otherwise     = "push "  ++ n ++ "\n"
  where pLine     = "Line " ++ show lineNum ++ "\n"
        not32bits = abs (read n :: Integer) > 2147483647

compileMaths (SYM a) args lineNum = "push [ebp + " ++ show argOffset ++ "]\n"
  where argIndex  = M.fromJust $ elemIndex a args
        argOffset = 4 * argIndex + 4

compileMaths (NODE {pOperation = op, pElems = elems}) args lineNum =
  pushElems ++ "mov eax, [esp]\n" ++ combine ++ cleanup
  where pushElems = concat $ reverse [compileMaths e args lineNum | e <- elems]
        movEBX    = [ "mov ebx, [esp+" ++ show (i*4) ++ "]\n" | i <- [1..(length args)] ]
        combine   = concat [mov ++ opInst | mov <- movEBX]
        cleanup   = "add esp, " ++ show (4 * length args) ++ "\nmov [esp], eax\n"
        opInst    = case op of
                      "+" -> "add eax, ebx\n"
                      "-" -> "sub eax, ebx\n"
                      "*" -> "mul ebx\n"
                      "/" -> "xor edx, edx\ndiv ebx"
                      "^" -> error "Exponents not yet implemented"

{- Compile Instructions -}

compileInstruction :: Instruction -> CompileState -> String
compileInstruction (PUSH i l) _ = pushL1 ++ pushLs
  where pushL1 = "mov edx, [ebp+" ++ argOffset i ++ "]\n"
        pushLs = "mov [STACK+ecx], edx\nadd ecx, 4\n"

compileInstruction (POP i l)  _ = popL1 ++ popL2
  where popL1  = "mov [ebp+" ++ argumentOffset i ++ "], [STACK + ecx]\n"
        popL2  = "sub ecx, 4"



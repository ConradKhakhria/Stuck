{- Converts parse-trees to assembly -}

module StuckCompile where
import qualified Data.Maybe as M
import Data.List (elemIndex, intersperse)

import StuckParse
import Parsing

data CompileState =
  State { cName     ::  String
        , cArgStrs  :: [String]
        , cArgCount ::  Int
        , cJmpStack :: [Int]
        , cJmpCount ::  Int
        , cLineNo   ::  Int }

{- Compiles parse trees -}

-- Gets the offset in the stack of an argument
argumentOffset :: Int -> String
argumentOffset n = show (4 * (n + 1))

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

compileInstruction :: Instruction -> Integer -> [String] -> String
compileInstruction (PUSH i) _ _ = pushL1 ++ pushL2 ++ pushL3
  where pushL1 = "mov edx, [ebp+" ++ argumentOffset i ++ "]\n"
        pushL2 = "mov   [STACK + ecx], edx"
        pushL3 = "add ecx, 4"

compileInstruction (POP i)  _ _ = popL1 ++ popL2
  where popL1  = "mov [ebp+" ++ argumentOffset i ++ "], [STACK + ecx]\n"
        popL2  = "sub ecx, 4"
        
compileInstruction (END)    l _ = ".JMP_" ++ show l ++ ":\n"

compileInstruction (INPUT)  _ _ = "call user_input"

compileInstruction (OUTPUT) _ _ = "call user_output"

compileInstruction (COND i) l _ = condL1 ++ condL2
  where condL1 = "cmp [ebp+" ++ argumentOffset i ++ "], 0\n"
        condL2 = "jng .JMP_" ++ show l ++ "\n"

compileInstruction (CALL { callFuncName = fname, callArguments = args }) l as =
  concat [compileMaths a as l | a <- args] ++ "call func_" ++ fname ++ "\n" 

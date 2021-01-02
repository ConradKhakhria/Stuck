module StuckCompile where
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Data.List  (elemIndex, intercalate)

import Parsing
import StuckParse

data CompileState =
  CompileState { sFuncName :: String
               , sFuncArgs :: [String]
               , sJmpStack :: [Int]
               , sJmpCount :: Int } deriving (Show, Eq)

{- Misc -}

argOffset :: Int -> String
argOffset n = show $ 4 * (n + 1)

{- Compiles ParseTrees -}

--compileMaths :: ParseTree -> [String] -> Int -> String
--compileMaths (NUM n) args lineNum
--  | '.' `elem` n  = error $ pLine ++ ": floating point numbers not supported"
--  | not64bits     = error $ pLine ++ ": numbers must be 32 bits"
--  | otherwise     = "push "  ++ n ++ "\n"
--  where pLine     = "Line " ++ show lineNum ++ "\n"
--        not32bits = abs (read n :: Integer) > 0x8000000000000000
--
--compileMaths (SYM a) args lineNum = "push dword [ebp + " ++ show argOffset ++ "]\n"
--  where argIndex  = fromJust $ elemIndex a args
--        argOffset = 4 * argIndex + 4
--
--compileMaths (NODE {pOperation = op, pElems = elems}) args lineNum =
--  pushElems ++ "mov eax, [esp]\n" ++ combine ++ cleanup
--  where pushElems = concat $ reverse [compileMaths e args lineNum | e <- elems]
--        movEBX    = [ "mov ebx, [esp+" ++ show (i*4) ++ "]\n" | i <- [1..(length args)] ]
--        combine   = concat [mov ++ opInst | mov <- movEBX]
--        cleanup   = "add esp, " ++ show (4 * (length args - 1)) ++ "\nmov [esp], eax\n"
--        opInst    = case op of
--                      "+" -> "add eax, ebx\n"
--                      "-" -> "sub eax, ebx\n"
--                      "*" -> "mul ebx\n"
--                      "/" -> "xor edx, edx\ndiv ebx"
--                      "^" -> error "Exponents not yet implemented"

compileMaths :: ParseTree -> [String] -> Int -> String
compileMaths (NUM n) args lineNum
  | '.' `elem` n = error $ pLine ++ ": floating point numbers not supported"
  | not64bits    = error $ pLine ++ ": numbers must be 64 bits"
  | otherwise    = n
  where pLine     = "Line " ++ show lineNum ++ "\n"
        not64bits = abs (read n :: Integer) > 0x8000000000000000

compileMaths (SYM s) args lineNum = s

compileMaths NODE { pOperation = op, pElems = elems } args lineNum =
  if op == "^" then
    error "Exponents not yet implemented"
  else
    intercalate op [compileMaths e args lineNum | e <- elems]

{- Compile Instructions -}

compileEnd :: Int -> String
compileEnd 0 = "" -- Doesn't print label zero (which only occured at the end of the function)
compileEnd n = ".JMP_" ++ show n ++ ":\n"

compilePush :: Instruction -> String
compilePush (PUSH a _) = "STACK[STACK_INDEX++] = " ++ a ++ ";\n"

compilePop :: Instruction -> String
compilePop (POP a _) = a ++ " = STACK[--STACK_INDEX];\n"

compileCond :: Instruction -> CompileState -> String
compileCond (COND a _) state = "if (" ++ a ++ " > 0) {\n"

-- compileCall :: Instruction -> [String] -> String
-- compileCall (CALL name as lineNo) args =
--   callL1 ++ callL2 ++ callL3
--   where callL1 = concat [ compileMaths a args lineNo | a <- as ]
--         callL2 = "call func_" ++ name ++ "\n"
--         callL3 = "add esi, "  ++ show (4 * length args) ++ "\n"

compileCall :: Instruction -> [String] -> String
compileCall (CALL name as lineNo) args =
  "func_" ++ name ++ "(" ++ intercalate ", " [ compileMaths a args lineNo | a <- as ] ++ ");\n"

compileInstructions :: [Instruction] -> FMAP -> CompileState -> String
compileInstructions [] _ _ = ""
compileInstructions (i : is) m state
  | isEnd    i = "}\n"                ++ next eState
  | isInput  i = "uinput();\n"        ++ next state
  | isOutput i = "uoutput();\n"       ++ next state
  | isPush   i = compilePush i        ++ next state
  | isPop    i = compilePop  i        ++ next state
  | isCond   i = compileCond i state  ++ next cState
  | isCall   i = compileCall i clArgs ++ next state
  where n      = if null is then 0 else head $ sJmpStack state
        clArgs = fromJust $ Map.lookup (cFuncName i) m
        next   = compileInstructions is m
        eState = state { sJmpStack = tail (sJmpStack state) }
        cState = state { sJmpCount = succ (sJmpCount state)
                       , sJmpStack = sJmpCount state : sJmpStack state }

compileFunctions :: [Function] -> FMAP -> String
compileFunctions [] _       = ""
compileFunctions (f : fs) m =
  n ++ as ++ ") {\n" ++ compileInstructions (fBody f) m state ++ compileFunctions fs m
  where n     = "void func_" ++ fName f ++ "("
        as    = "int64_t " ++ intercalate ", int64_t " (fArgs f)
        state = (CompileState { sFuncName = fName f
                              , sFuncArgs = fArgs f
                              , sJmpStack = []
                              , sJmpCount = 1 })

boilerplate1 :: String
boilerplate1 = "\
\#include <stdio.h>                             \n\
\#include <stdint.h>                            \n\
\                                               \n\
\int64_t  STACK[0x100000];                      \n\
\uint64_t STACK_INDEX;                          \n\
\                                               \n\
\int64_t ipow(int64_t n, int64_t r)             \n\
\{                                              \n\
\    int64_t retval = 1;                        \n\
\                                               \n\
\    for (int i = 0; i < sizeof(r); i++) {      \n\
\         if (r & 1 << i) {                     \n\
\            int64_t comp = n;                  \n\
\                                               \n\
\            for (int c = 0; c < i; c++) {      \n\
\                comp *= comp;                  \n\
\            }                                  \n\
\                                               \n\
\            retval *= comp;                    \n\
\        }                                      \n\
\    }                                          \n\
\                                               \n\
\    return retval;                             \n\
\}                                              \n\
\                                               \n\
\void uinput()                                  \n\
\{                                              \n\
\    int64_t input_val;                         \n\
\                                               \n\
\    while (scanf(\"%ld\", &input_val) == EOF) {\n\
\        fprintf(stderr, \"Invalid input\\n\"); \n\
\    }                                          \n\
\                                               \n\
\    STACK[STACK_INDEX++] = input_val;          \n\
\}                                              \n\
\                                               \n\
\void uoutput()                                 \n\
\{                                              \n\
\    printf(\"%ld\\n\", STACK[STACK_INDEX - 1]);\n\
\}\n"


boilerplate2 :: String -> Int -> String
boilerplate2 lastFuncName argCount =
  let
    funcall = lastFuncName ++ "(" ++ init (concat (replicate argCount "0,")) ++ ");\n"
  in
    "int main() {\n STACK_INDEX = 0; \nfunc_" ++ funcall ++ "}"


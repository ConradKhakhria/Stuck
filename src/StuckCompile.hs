module StuckCompile where
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Data.List  (elemIndex)

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

compileMaths :: ParseTree -> [String] -> Int -> String
compileMaths (NUM n) args lineNum
  | '.' `elem` n  = error $ pLine ++ ": floating point numbers not supported"
  | not32bits     = error $ pLine ++ ": numbers must be 32 bits"
  | otherwise     = "push "  ++ n ++ "\n"
  where pLine     = "Line " ++ show lineNum ++ "\n"
        not32bits = abs (read n :: Integer) > 2147483647

compileMaths (SYM a) args lineNum = "push dword [ebp + " ++ show argOffset ++ "]\n"
  where argIndex  = fromJust $ elemIndex a args
        argOffset = 4 * argIndex + 4

compileMaths (NODE {pOperation = op, pElems = elems}) args lineNum =
  pushElems ++ "mov eax, [esp]\n" ++ combine ++ cleanup
  where pushElems = concat $ reverse [compileMaths e args lineNum | e <- elems]
        movEBX    = [ "mov ebx, [esp+" ++ show (i*4) ++ "]\n" | i <- [1..(length args)] ]
        combine   = concat [mov ++ opInst | mov <- movEBX]
        cleanup   = "add esp, " ++ show (4 * (length args - 1)) ++ "\nmov [esp], eax\n"
        opInst    = case op of
                      "+" -> "add eax, ebx\n"
                      "-" -> "sub eax, ebx\n"
                      "*" -> "mul ebx\n"
                      "/" -> "xor edx, edx\ndiv ebx"
                      "^" -> error "Exponents not yet implemented"

{- Compile Instructions -}

compileEnd :: Int -> String
compileEnd 0 = "" -- Stupid workaround
compileEnd n = ".JMP_" ++ show n ++ ":\n"

compilePush :: Instruction -> String
compilePush (PUSH i _) = pushL1 ++ pushL2 ++ pushL3
  where pushL1 = "mov edx, [ebp+" ++ argOffset i ++ "]\n"
        pushL2 = "mov [STACK+esi], edx\n"
        pushL3 = "add esi, 4\n"

compilePop :: Instruction -> String
compilePop (POP i _) = popL1 ++ popL2 ++ popL3
  where popL1  = "mov edx, [STACK+esi-4]\n"
        popL2  = "mov dword [ebp+" ++ argOffset i ++ "], edx\n"
        popL3  = "sub esi, 4\n"

compileCond :: Instruction -> CompileState -> String
compileCond (COND i _) state = condL1 ++ condL2
  where condL1 = "cmp dword [ebp+" ++ argOffset i ++ "], 0\n"
        condL2 = "jng .JMP_" ++ show n ++ "\n"
        n      = sJmpCount state

compileCall :: Instruction -> [String] -> String
compileCall (CALL name as lineNo) args =
  callL1 ++ callL2 ++ callL3
  where callL1 = concat [ compileMaths a args lineNo | a <- reverse as ]
        callL2 = "call func_" ++ name ++ "\n"
        callL3 = "add esi, "  ++ show (4 * length args) ++ "\n"

compileInstructions :: [Instruction] -> FMAP -> CompileState -> String
compileInstructions [] _ _ = ""
compileInstructions (i : is) m state
  | isEnd    i = compileEnd n               ++ next eState
  | isInput  i = "call user_input\n"        ++ next state
  | isOutput i = "call user_output\n"       ++ next state
  | isPush   i = compilePush i              ++ next state
  | isPop    i = compilePop  i              ++ next state
  | isCond   i = compileCond i state        ++ next cState
  | isCall   i = compileCall i clArgs       ++ next state
  where n      = if null is then 0 else head $ sJmpStack state
        clArgs = fromJust $ Map.lookup (cFuncName i) m
        next   = compileInstructions is m
        eState = state { sJmpStack = tail (sJmpStack state) }
        cState = state { sJmpCount = succ (sJmpCount state)
                       , sJmpStack = sJmpCount state : sJmpStack state }

compileFunctions :: [Function] -> FMAP -> String
compileFunctions [] _       = ""
compileFunctions (f : fs) m =
  s ++ compileInstructions (fBody f) m state ++ e ++ compileFunctions fs m
  where s     = "func_" ++ fName f ++ ":\npush ebp\nmov ebp, esp\n"
        e     = "mov esp, ebp\npop ebp\nret\n"
        state = (CompileState { sFuncName = fName f
                              , sFuncArgs = fArgs f
                              , sJmpStack = []
                              , sJmpCount = 1 })

boilerplate1 :: String
boilerplate1 = "\
\global main                  \n\
\extern printf                \n\
\extern read                  \n\
\extern atoi                  \n\
\segment .data                \n\
\fmt: db \"%d\", 10, 0        \n\
\segment .bss                 \n\
\STACK: resd 0x10000          \n\
\INBUF: resd 0x100            \n\
\segment .text                \n\
\user_output:                 \n\
\push  ebp                    \n\
\mov   ebp, esp               \n\
\push  dword [STACK+esi-4]    \n\
\push  fmt                    \n\
\call  printf                 \n\
\add   esp, 8                 \n\
\mov   esp, ebp               \n\
\pop   ebp                    \n\
\ret                          \n\
\user_input:                  \n\
\push  ebp                    \n\
\mov   ebp, esp               \n\
\push  0x100                  \n\
\push  INBUF                  \n\
\push  0                      \n\
\call  read                   \n\
\add   esi, 12                \n\
\push  INBUF                  \n\
\call  atoi                   \n\
\mov   [STACK+esi], eax       \n\
\add   esi, 4                 \n\
\mov   esp, ebp               \n\
\pop   ebp                    \n\
\ret                          \n"

boilerplate2 :: String -> String -> String
boilerplate2 lFuncName argcOffset = "\
\main:                          \n\
\push  ebp                      \n\
\mov   ebp, esp                 \n\
\xor   eax, eax                 \n\
\xor   ebx, ebx                 \n\
\xor   esi, esi                 \n\
\mov   dword [STACK+esi], 5     \n\
\add   esi, 4                   \n\
\sub   esp, " ++ argcOffset ++ "\n\
\call  func_" ++ lFuncName  ++ "\n\
\add   esp, " ++ argcOffset ++ "\n\
\mov   eax, 1                   \n\
\mov   ebx, 0                   \n\
\int   0x80                     \n"


-- \mov   esp, ebp                 \n\
-- \pop   ebp                      \n\
-- \ret                            \n"

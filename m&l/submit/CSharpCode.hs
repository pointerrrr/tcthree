module CSharpCode where

import Prelude hiding (LT, GT, EQ)

import Data.Char
import Data.Map (Map, fromList, elems, size, member, (!))
import qualified Data.Map as M

import CSharpAlgebra
import CSharpGram
import CSharpLex
import SSM

data ValueOrAddress = Value | Address
    deriving Show

type Env = Map String Int

codeAlgebra :: CSharpAlgebra Code Code (Env -> (Code, Env)) (Env -> ValueOrAddress -> Code)
codeAlgebra =
    ( fClas
    , (fMembDecl, fMembMeth)
    , (fStatDecl, fStatExpr, fStatIf, fStatWhile, fStatReturn, fStatBlock, fStatFor)
    , (fExprCon, fExprVar, fExprOp, fExprMethCall)
    )

fClas :: Token -> [Code] -> Code
fClas c ms = [Bsr "main", HALT] ++ concat ms

fMembDecl :: Decl -> Code
fMembDecl d = []

------------------
-- assingment 6 --
------------------
  -- environments were added to all functions that needed to pass an environment
  -- the algebra was adjusted accordingly.

fMembMeth :: Type -> Token -> [Decl] -> (Env -> (Code, Env)) -> Code
fMembMeth t (LowerId x) ds s = [LABEL x, LINK k] ++ c1 ++ [UNLINK, STS (-n), AJS(-n + 1), RET]
  where 
    n = length ds
    k = size env1 - n
    (c1, env1) = s env
    env = fromList $ zip (map (\(Decl _ (LowerId k)) -> k) ds) [-1 - length ds .. -1] -- mapping parameters ID to negative numbers

-------------------
-- assignment 10 --
-------------------
  -- Function to put local variables at a correct index when declared
  
fStatDecl :: Decl -> Env -> (Code, Env)
fStatDecl (Decl _ (LowerId label)) env = ([], M.insert label index env)
  where
    index
      | M.size env == 0 = 1                -- Empty environment, thus we start with putting our first local variable at index 1
      | maxIndex > 0    = maxIndex + 1     -- If not empty, find highest index, if greater than 0, we put our new local variable one index up.
      | otherwise       = 1                -- If not empty, but no positive index, we only have parameters present, thus we put our local variable at index 1.
    maxIndex = M.foldr (\maxI k -> if maxI > k then maxI else k) 0 env

fStatExpr :: (Env -> ValueOrAddress -> Code) -> Env -> (Code, Env)
fStatExpr e env = (e env Value ++ [pop],env)

fStatIf :: (Env -> ValueOrAddress -> Code) -> (Env -> (Code, Env)) -> (Env -> (Code, Env)) -> Env -> (Code,Env)
fStatIf e s1 s2 env = (c ++ [BRF (n1 + 2)] ++ c1 ++ [BRA n2] ++ c2, env2)
  where
    c          = e env Value
    (c1, env1) = s1 env
    (c2, env2) = s2 env1
    (n1, n2)   = (codeSize c1, codeSize c2)

fStatWhile :: (Env -> ValueOrAddress -> Code) -> (Env -> (Code, Env)) -> Env -> (Code, Env)
fStatWhile e s1 env = ([BRA n] ++ c1 ++ c ++ [BRT (-(n + k + 2))], env1)
  where
    c = e env Value
    (c1, env1) = s1 env
    (n, k) = (codeSize c1, codeSize c)
      
-----------------
-- assigment 5 --
-----------------
  -- for loop created as a while loop with:
  -- Declaration in front of loop body and test
  -- Expression changing loop variable put at end of loop body
  -- Changed branching numbers accordingly.
  -- The for loop, unlike in C# itself, takes 4 expressions, because we cannot have a declaration and an assingment in the same expression.
  -- like `for (int x; x = 0; x < 5 ; x = x + 1) {}`
  
fStatFor :: (Env -> (Code, Env)) 
         -> (Env -> ValueOrAddress -> Code) 
         -> (Env -> ValueOrAddress -> Code) 
         -> (Env -> ValueOrAddress -> Code) 
         -> (Env -> (Code, Env)) 
         -> Env -> (Code, Env)
fStatFor d e1 e2 e3 s env = (d1 ++ c1 ++ [BRA (n + k)] ++ s1 ++ c3 ++ c2 ++ [BRT (-(n + k + l + 2))],env2)
  where
    (d1, env1)   = d env
    (s1, env2)   = s env1
    (c1, c2, c3) = (e1 env1 Value, e2 env2 Value, e3 env2 Value)
    (n, k, l)    = (codeSize s1, codeSize c3, codeSize c2)

  -- methods now return a value to the register (assingment 9)
fStatReturn :: (Env -> ValueOrAddress -> Code) -> Env -> (Code, Env)
fStatReturn e env = (e env Value ++ [STR RR, UNLINK, STS (-n), AJS(-n + 1), RET], env)
  where
    n = foldr (\x i -> if x < 0 then i + 1 else i) 0 (elems env) -- Count parameters (mapped to negative numbers in environment)

fStatBlock :: [Env -> (Code,Env)] -> Env -> (Code,Env)
fStatBlock cs env = foldl (\(c1, env1) s -> passEnv c1 (s env1) ) ([], env) cs
  where
    passEnv c (c1, env2) = (c ++ c1, env2)

fExprCon :: Token -> Env -> ValueOrAddress -> Code
fExprCon (ConstInt n)  env va = [LDC n]
fExprCon (ConstBool b) env va = [LDC (boolToInt b)] -- assingment 1
fExprCon (ConstChar c) env va = [LDC (ord c)]       -- assingment 1

  -- Method call implemented (assingment 6)
  -- added special case for printing (assingment 8)
fExprMethCall :: Token -> [Env -> ValueOrAddress -> Code] -> Env -> ValueOrAddress -> Code
fExprMethCall (LowerId label) es env va = concatMap (\f -> f env va) es
                                       ++ (if label == "print" then replicate (length es) (TRAP 0) else [Bsr label])
                                       ++ [LDR RR]

boolToInt :: Bool -> Int
boolToInt True = 1
boolToInt _ = 0

fExprVar :: Token -> Env ->  ValueOrAddress -> Code
fExprVar (LowerId x) env va = 
    case va of
      Value    ->  [LDL  loc]
      Address  ->  [LDLA loc]
  where
    loc = if member x env then env ! x else error ("unknown variable \'" ++ x ++ "\'.")

------------------
-- assingment 7 --
------------------
  -- Using BRT and BRF to jump directly if the right operand doesn't have to be evaluated for OR and AND
  -- Since BRT and BRF pop a value, the left operand has to be pushed twice.

fExprOp :: Token -> (Env -> ValueOrAddress -> Code) -> (Env -> ValueOrAddress -> Code) -> Env -> ValueOrAddress -> Code
fExprOp (Operator "=")  e1 e2 env va = e2 env Value ++ [LDS 0] ++ e1 env Address ++ [STA 0]
fExprOp (Operator "||") e1 e2 env va = e1 env Value ++ e1 env Value ++ [BRT (codeSize (e2 env Value) + 1)] ++ e2 env Value ++ [OR]
fExprOp (Operator "&&") e1 e2 env va = e1 env Value ++ e1 env Value ++ [BRF (codeSize (e2 env Value) + 1)] ++ e2 env Value ++ [AND]
fExprOp (Operator op)   e1 e2 env va = e1 env Value ++ e2 env Value ++ [opCodes ! op]

opCodes :: Map String Instr
opCodes = fromList [ ("+", ADD), ("-", SUB),  ("*", MUL), ("/", DIV), ("%", MOD)
                   , ("<=", LE), (">=", GE),  ("<", LT),  (">", GT),  ("==", EQ)
                   , ("!=", NE), ("&&", AND), ("||", OR), ("^", XOR)
                   ]

module CSharpCode where

import Prelude hiding (LT, GT, EQ)
import Data.Map as M
import Data.Char
import CSharpLex
import CSharpGram
import CSharpAlgebra
import SSM


data ValueOrAddress = Value | Address
    deriving Show
    
type Env = Map String Int

codeAlgebra :: CSharpAlgebra Code Code Code (ValueOrAddress -> Code)
codeAlgebra =
    ( fClas
    , (fMembDecl, fMembMeth)
    , (fStatDecl, fStatExpr, fStatIf, fStatWhile, fStatReturn, fStatBlock, fStatFor)
    , (fExprCon, fExprVar, fExprOp, fMethCall)
    )

fClas :: Token -> [Code] -> Code
fClas c ms = [Bsr "main", HALT] ++ concat ms

fMembDecl :: Decl -> Code
fMembDecl d = []

fMembMeth :: Type -> Token -> [Decl] -> Code -> Code
fMembMeth t (LowerId x) ps s = [LABEL x] ++ (loadParams 0 (length ps) ps) ++ s ++ [AJS (-(length ps))] ++ [RET]

loadParams :: Int -> Int -> [Decl] -> Code
loadParams current max []     = []
loadParams current max (x:xs) = [LDS (-max)] ++ loadParams (current + 1) max xs

fStatDecl :: Decl -> Code
fStatDecl d = []

fStatExpr :: (ValueOrAddress -> Code) -> Code
fStatExpr e = e Value ++ [pop]

fStatIf :: (ValueOrAddress -> Code) -> Code -> Code -> Code
fStatIf e s1 s2 = c ++ [BRF (n1 + 2)] ++ s1 ++ [BRA n2] ++ s2
    where
        c        = e Value
        (n1, n2) = (codeSize s1, codeSize s2)

fStatWhile :: (ValueOrAddress -> Code) -> Code -> Code
fStatWhile e s1 = [BRA n] ++ s1 ++ c ++ [BRT (-(n + k + 2))]
    where
        c = e Value
        (n, k) = (codeSize s1, codeSize c)
        
fStatFor :: Code -> (ValueOrAddress -> Code) -> (ValueOrAddress -> Code) -> (ValueOrAddress -> Code) -> Code -> Code
fStatFor d1 e1 e2 e3 s = (d1 ++ (e1 Value) ++ [BRA (blockSize + incrementSize)] ++ s ++ increment ++ test ++ [BRT (-(blockSize + testSize + incrementSize + 2))])
    where
        test = e2 Value
        increment = e3 Value
        blockSize = codeSize s
        testSize  = codeSize (e2 Value)
        incrementSize = codeSize (e3 Value)

fStatReturn :: (ValueOrAddress -> Code) -> Code
fStatReturn e = e Value ++ [STR R3] ++ [RET]

fStatBlock :: [Code] -> Code
fStatBlock = concat

fExprCon :: Token -> ValueOrAddress -> Code
fExprCon (ConstInt n) va = [LDC n]
fExprCon (ConstBool b) va = [LDC (boolToInt b)]
fExprCon (ConstChar c) va = [LDC (ord c)]

fMethCall :: Token -> [ValueOrAddress -> Code] -> ValueOrAddress -> Code
fMethCall (LowerId label) es va = concatMap (\f -> f va) es ++
                                    (if label == "print"
                                    then replicate (length es) (TRAP 0)
                                    else [Bsr label]) ++ [LDR R3]

boolToInt :: Bool -> Int
boolToInt False = 0
boolToInt True  = -1

fExprVar :: Token -> ValueOrAddress -> Code
fExprVar (LowerId x) va = let loc = 37 in case va of
                                              Value    ->  [LDL  loc]
                                              Address  ->  [LDLA loc]

fExprOp :: Token -> (ValueOrAddress -> Code) -> (ValueOrAddress -> Code) -> ValueOrAddress -> Code
fExprOp (Operator "=")  e1 e2 va = e2 Value ++ [LDS 0] ++ e1 Address ++ [STA 0]
fExprOp (Operator "||") e1 e2 va = e1 Value ++ e1 Value ++ [BRT (codeSize (e2 Value) + 1)] ++ e2 Value ++ [OR]
fExprOp (Operator "&&") e1 e2 va = e1 Value ++ e1 Value ++ [BRF (codeSize (e2 Value) + 1)] ++ e2 Value ++ [AND]
fExprOp (Operator op)   e1 e2 va = e1 Value ++ e2 Value ++ [opCodes ! op]


opCodes :: Map String Instr
opCodes = fromList [ ("+", ADD), ("-", SUB),  ("*", MUL), ("/", DIV), ("%", MOD)
                   , ("<=", LE), (">=", GE),  ("<", LT),  (">", GT),  ("==", EQ)
                   , ("!=", NE), ("&&", AND), ("||", OR), ("^", XOR)
                   ]


module CSharpLex where

import Data.Char
import Control.Monad
import ParseLib.Abstract
import Prelude hiding ((<*), (*>), (<$>), (<$))

data Token = POpen    | PClose      -- parentheses     ()
           | SOpen    | SClose      -- square brackets []
           | COpen    | CClose      -- curly braces    {}
           | Comma    | Semicolon
           | KeyIf    | KeyElse
           | KeyWhile | KeyFor
           | KeyReturn| KeyTry
           | KeyCatch | KeyClass
           | KeyVoid
           | StdType   String       -- the 8 standard types
           | Operator  String       -- the 15 operators
           | UpperId   String       -- uppercase identifiers
           | LowerId   String       -- lowercase identifiers
           | ConstInt  Int
           | ConstChar Char
           | ConstBool Bool
           deriving (Eq, Show)

keyword :: String -> Parser Char String
keyword [] = succeed ""
keyword xs@(x:_) | isLetter x = do ys <- greedy (satisfy isAlphaNum)
                                   guard (xs == ys)
                                   return ys
                 | otherwise  = token xs


greedyChoice :: [Parser s a] -> Parser s a
greedyChoice = foldr (<<|>) empty


terminals :: [(Token, String)]
terminals =
    [ ( POpen     , "("      )
    , ( PClose    , ")"      )
    , ( SOpen     , "["      )
    , ( SClose    , "]"      )
    , ( COpen     , "{"      )
    , ( CClose    , "}"      )
    , ( Comma     , ","      )
    , ( Semicolon , ";"      )
    , ( KeyIf     , "if"     )
    , ( KeyElse   , "else"   )
    , ( KeyWhile  , "while"  )
    , ( KeyReturn , "return" )
    , ( KeyTry    , "try"    )
    , ( KeyCatch  , "catch"  )
    , ( KeyClass  , "class"  )
    , ( KeyVoid   , "void"   )
    , ( KeyFor    , "for"    )
    ]

lexCleanup :: Parser Char String
lexCleanup = lexWhiteSpace <* greedy (lexComment <* lexWhiteSpace)
    
lexWhiteSpace :: Parser Char String
lexWhiteSpace = greedy (satisfy isSpace)

lexComment :: Parser Char String
lexComment = token "//" <* greedy (satisfy (/= '\n'))

lexLowerId :: Parser Char Token
lexLowerId = (\x xs -> LowerId (x:xs)) <$> satisfy isLower <*> greedy (satisfy isAlphaNum)

lexUpperId :: Parser Char Token
lexUpperId = (\x xs -> UpperId (x:xs)) <$> satisfy isUpper <*> greedy (satisfy isAlphaNum)

lexConstInt :: Parser Char Token
lexConstInt = (ConstInt . read) <$> greedy1 (satisfy isDigit)

lexConstChar :: Parser Char Token
lexConstChar = ConstChar <$ symbol '\'' <*> anySymbol <* symbol '\''

lexConstBool :: Parser Char Token
lexConstBool = ConstBool <$> (False <$ token "false" <|> True <$ token "true")

lexEnum :: (String -> Token) -> [String] -> Parser Char Token
lexEnum f xs = f <$> choice (map keyword xs)

lexTerminal :: Parser Char Token
lexTerminal = choice [t <$ keyword s | (t,s) <- terminals]

stdTypes :: [String]
stdTypes = ["int", "long", "double", "float", "byte", "short", "bool", "char"]

operators :: [String]
operators = ["+", "-", "*", "/", "%", "&&", "||", "^", "<=", "<", ">=", ">", "==", "!=", "="]


lexToken :: Parser Char Token
lexToken = greedyChoice
             [ lexTerminal
             , lexEnum StdType stdTypes
             , lexEnum Operator operators
             , lexConstInt
             , lexConstBool
             , lexConstChar
             , lexLowerId
             , lexUpperId
             ]

-- assignment 3
lexicalScanner :: Parser Char [Token]
lexicalScanner = lexWhiteSpace *> greedy (lexToken <* lexCleanup) <* eof


sStdType :: Parser Token Token
sStdType = satisfy isStdType
    where isStdType (StdType _) = True
          isStdType _           = False

sUpperId :: Parser Token Token
sUpperId = satisfy isUpperId
    where isUpperId (UpperId _) = True
          isUpperId _           = False

sLowerId :: Parser Token Token
sLowerId = satisfy isLowerId
    where isLowerId (LowerId _) = True
          isLowerId _           = False

-- assignment 1          
sConst :: Parser Token Token
sConst  = satisfy isConst
    where isConst (ConstInt  _) = True
          isConst (ConstBool _) = True
          isConst (ConstChar _) = True
          isConst _             = False

-- assignment 2
-- type of operators source: https://msdn.microsoft.com/en-us/library/2bxt6kc4.aspx

multiplicative :: Parser Token Token
multiplicative = satisfy isMultiplicative
    where
        isMultiplicative (Operator "*") = True
        isMultiplicative (Operator "/") = True
        isMultiplicative (Operator "%") = True
        isMultiplicative _              = False
          
additive :: Parser Token Token
additive = satisfy isAdditive
    where 
        isAdditive (Operator "+") = True
        isAdditive (Operator "-") = True
        isAdditive _              = False
          
relational :: Parser Token Token
relational = satisfy isRelational
    where
        isRelational (Operator "<=") = True
        isRelational (Operator "<")  = True
        isRelational (Operator ">=") = True
        isRelational (Operator ">")  = True
        isRelational _               = False

equality :: Parser Token Token
equality = satisfy isEquality
    where
        isEquality (Operator "==") = True
        isEquality (Operator "!=") = True
        isEquality _               = False
        
bitwiseXOR :: Parser Token Token
bitwiseXOR = satisfy isBitwiseXOR
    where
        isBitwiseXOR (Operator "^") = True
        isBitwiseXOR _              = False
        
logicalAND :: Parser Token Token
logicalAND = satisfy isLogicalAnd
    where
        isLogicalAnd (Operator "&&") = True
        isLogicalAnd _               = False
        
logicalOR :: Parser Token Token
logicalOR = satisfy isLogicalOr
    where
        isLogicalOr (Operator "||") = True
        isLogicalOr _               = False
        
assignment :: Parser Token Token
assignment = satisfy isAssignment
    where
        isAssignment (Operator "=") = True
        isAssignment _              = False

sSemi :: Parser Token Token
sSemi =  symbol Semicolon

module CSharpLex where

import Prelude hiding ((<*), (*>), (<$>), (<$))

import Control.Monad
import Data.Char

import ParseLib.Abstract

data Token = POpen    | PClose      -- parentheses     ()
           | SOpen    | SClose      -- square brackets []
           | COpen    | CClose      -- curly braces    {}
           | Comma    | Semicolon
           | KeyIf    | KeyElse
           | KeyWhile | KeyReturn
           | KeyTry   | KeyCatch
           | KeyClass | KeyVoid
           | KeyFor                 -- adding for loop support (assignment 5)
           | StdType   String       -- the 8 standard types
           | Operator  String       -- the 15 operators
           | UpperId   String       -- uppercase identifiers
           | LowerId   String       -- lowercase identifiers
           | ConstInt  Int
           | ConstBool Bool
           | ConstChar Char         -- added char support
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

lexWhiteSpace :: Parser Char String
lexWhiteSpace = greedy (satisfy isSpace)

lexLowerId :: Parser Char Token
lexLowerId = (\x xs -> LowerId (x:xs)) <$> satisfy isLower <*> greedy (satisfy isAlphaNum)

lexUpperId :: Parser Char Token
lexUpperId = (\x xs -> UpperId (x:xs)) <$> satisfy isUpper <*> greedy (satisfy isAlphaNum)

lexConstInt :: Parser Char Token
lexConstInt = (ConstInt . read) <$> greedy1 (satisfy isDigit)

lexEnum :: (String -> Token) -> [String] -> Parser Char Token
lexEnum f xs = f <$> choice (map keyword xs)

lexTerminal :: Parser Char Token
lexTerminal = choice [t <$ keyword s | (t,s) <- terminals]

------------------
-- assignment 1 --
------------------

  -- lexing boolean keywords false and true
lexConstBool :: Parser Char Token
lexConstBool = ConstBool <$> (False <$ token "false" <|> True <$ token "true")

  -- lexing characters
lexConstChar :: Parser Char Token
lexConstChar = ConstChar <$ symbol '\'' <*> anySymbol <* symbol '\''

------------------
-- assignment 3 --
------------------

 -- used to trhow away comments and whitespaces
lexWhiteSpaceAndComments :: Parser Char String
lexWhiteSpaceAndComments = lexWhiteSpace <* greedy (lexComment <* lexWhiteSpace)

 -- lexing single line comments
lexComment :: Parser Char String
lexComment = token "//" <* greedy (satisfy (/= '\n'))

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

lexicalScanner :: Parser Char [Token]
lexicalScanner = lexWhiteSpaceAndComments *> greedy (lexToken <* lexWhiteSpaceAndComments) <* eof -- added whitespace and comment lexer for assignment 3

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

sConst :: Parser Token Token
sConst  = satisfy isConst
    where isConst (ConstInt  _) = True
          isConst (ConstBool _) = True
          isConst _             = False

sSemi :: Parser Token Token
sSemi =  symbol Semicolon

------------------
-- assignment 2 --
------------------

-- Replaced sOperator with sAssignment, sLogicalOr etc. to check for correct
-- operator in a same manner as sLowerId etc.

sAssignment :: Parser Token Token
sAssignment = satisfy isAssignment
    where isAssignment (Operator "=") = True
          isAssignment _              = False

sLogicalOr :: Parser Token Token
sLogicalOr = satisfy isLogicalOr
    where isLogicalOr (Operator "||") = True
          isLogicalOr _              = False

sLogicalAnd :: Parser Token Token
sLogicalAnd = satisfy isLogicalAnd
    where isLogicalAnd (Operator "&&") = True
          isLogicalAnd _               = False

sBitwiseXOR :: Parser Token Token
sBitwiseXOR = satisfy isBitwiseXOR
    where isBitwiseXOR (Operator "^") = True
          isBitwiseXOR _              = False

sEquality :: Parser Token Token
sEquality = satisfy isEquality
    where isEquality (Operator "==") = True
          isEquality (Operator "!=") = True
          isEquality _               = False

sRelational :: Parser Token Token
sRelational = satisfy isRelational
    where isRelational (Operator "<")  = True
          isRelational (Operator "<=") = True
          isRelational (Operator ">")  = True
          isRelational (Operator ">=") = True
          isRelational _               = False

sAdditive :: Parser Token Token
sAdditive = satisfy isAdditive
    where isAdditive (Operator "+") = True
          isAdditive (Operator "-") = True
          isAdditive _              = False

sMultiplicative :: Parser Token Token
sMultiplicative = satisfy isMultiplicative
    where isMultiplicative (Operator "*") = True
          isMultiplicative (Operator "/") = True
          isMultiplicative (Operator "%") = True
          isMultiplicative _              = False

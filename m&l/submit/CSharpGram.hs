module CSharpGram where

import Prelude hiding ((<*), (<$>), (<$))

import ParseLib.Abstract hiding (braced, bracketed, parenthesised)

import CSharpLex

data Class = Class Token [Member]
    deriving Show

data Member = MemberD Decl
            | MemberM Type Token [Decl] Stat
            deriving Show

data Stat = StatDecl   Decl
          | StatExpr   Expr
          | StatIf     Expr Stat Stat
          | StatWhile  Expr Stat
          | StatReturn Expr
          | StatBlock  [Stat]
          | StatFor    Stat Expr Expr Expr Stat -- for loop support (assignment 5)
          deriving Show

data Expr = ExprConst  Token
          | ExprVar    Token
          | ExprOper   Token Expr Expr
          | ExprCall   Token [Expr] -- method call support (Assignment 6)
          deriving Show

data Decl = Decl Type Token
    deriving Show

data Type = TypeVoid
          | TypePrim  Token
          | TypeObj   Token
          | TypeArray Type
          deriving (Eq,Show)

parenthesised p = pack (symbol POpen) p (symbol PClose)
bracketed     p = pack (symbol SOpen) p (symbol SClose)
braced        p = pack (symbol COpen) p (symbol CClose)

pExprSimple :: Parser Token Expr
pExprSimple =  ExprConst <$> sConst
           <|> ExprVar   <$> sLowerId
           <|> parenthesised pExpr
           <|> ExprCall  <$> sLowerId <*> parenthesised (option (listOf pExpr (symbol Comma)) []) -- method call support (Assignment 6)
           
----------------------
-- assignment 2 & 4 --
----------------------

-- Introducing new parsers used by pExpr to allow correct
-- operator associativity and prioririty.
-- Parsers in order of prioririty from low to high prioririty (pMultiplicative highest)

pExpr :: Parser Token Expr
pExpr = chainr pLogicalOr $ ExprOper <$> sAssignment

pLogicalOr :: Parser Token Expr
pLogicalOr = chainl pLogicalAnd $ ExprOper <$> sLogicalOr

pLogicalAnd :: Parser Token Expr
pLogicalAnd = chainl pBitwiseXOR $ ExprOper <$> sLogicalAnd

pBitwiseXOR :: Parser Token Expr
pBitwiseXOR = chainl pEquality $ ExprOper <$> sBitwiseXOR

pEquality :: Parser Token Expr
pEquality = chainl pRelational $ ExprOper <$> sEquality

pRelational :: Parser Token Expr
pRelational = chainl pAdditive $ ExprOper <$> sRelational

pAdditive :: Parser Token Expr
pAdditive = chainl pMultiplicative $ ExprOper <$> sAdditive

pMultiplicative :: Parser Token Expr
pMultiplicative = chainl pExprSimple $ ExprOper <$> sMultiplicative


pMember :: Parser Token Member
pMember =  MemberD <$> pDeclSemi
       <|> pMeth

pStatDecl :: Parser Token Stat
pStatDecl =  pStat
         <|> StatDecl <$> pDeclSemi

pStat :: Parser Token Stat
pStat =  StatExpr <$> pExpr <*  sSemi
     <|> StatIf     <$ symbol KeyIf     <*> parenthesised pExpr <*> pStat <*> optionalElse
     <|> StatWhile  <$ symbol KeyWhile  <*> parenthesised pExpr <*> pStat
     <|> StatFor    <$ symbol KeyFor    <* symbol POpen 
                                        <*> pStatDecl <*> pExpr <* sSemi <*> pExpr <* sSemi <*> pExpr <* sSemi 
                                        <* symbol PClose <*> pStat -- adding for loop support (assignment 5)
     <|> StatReturn <$ symbol KeyReturn <*> pExpr <*  sSemi
     <|> pBlock
     where optionalElse = option ((\_ x -> x) <$> symbol KeyElse <*> pStat) (StatBlock [])

pBlock :: Parser Token Stat
pBlock = StatBlock <$> braced (many pStatDecl)

pMeth :: Parser Token Member
pMeth = MemberM <$> methRetType <*> sLowerId <*> methArgList <*> pBlock
  where
    methRetType = pType <|> (const TypeVoid <$> symbol KeyVoid)
    methArgList = parenthesised (option (listOf pDecl (symbol Comma)) [])

pType0 :: Parser Token Type
pType0 =  TypePrim <$> sStdType
      <|> TypeObj  <$> sUpperId

pType :: Parser Token Type
pType = foldr (const TypeArray) <$> pType0 <*> many (bracketed (succeed ()))

pDecl :: Parser Token Decl
pDecl = Decl <$> pType <*> sLowerId

pDeclSemi :: Parser Token Decl
pDeclSemi = const <$> pDecl <*> sSemi

pClass :: Parser Token Class
pClass = Class <$ symbol KeyClass <*> sUpperId <*> braced (many pMember)

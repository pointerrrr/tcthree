module CSharpGram where

import ParseLib.Abstract hiding (braced, bracketed, parenthesised)
import CSharpLex
import Prelude hiding ((<*), (*>), (<$>), (<$))

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
          deriving Show

data Expr = ExprConst  Token
          | ExprVar    Token
          | ExprOper   Token Expr Expr
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

-- start assignment 2 (from lecture Parser Combinators (II) )
-- source: https://msdn.microsoft.com/en-us/library/2bxt6kc4.aspx

pExpr :: Parser Token Expr
pExpr = chainr pLogicalOR (ExprOper <$> assignment)

pLogicalOR :: Parser Token Expr
pLogicalOR = chainl pLogicalAND (ExprOper <$> logicalOR)

pLogicalAND :: Parser Token Expr
pLogicalAND = chainl pBitwiseXOR (ExprOper <$> logicalAND)

pBitwiseXOR :: Parser Token Expr
pBitwiseXOR = chainl pEquality (ExprOper <$> bitwiseXOR)

pEquality :: Parser Token Expr
pEquality = chainl pRelational (ExprOper <$> equality)

pRelational :: Parser Token Expr
pRelational = chainl pAdditive (ExprOper <$> relational)

pAdditive :: Parser Token Expr
pAdditive = chainl pMultiplicative (ExprOper <$> additive)

pMultiplicative :: Parser Token Expr
pMultiplicative = chainl pExprSimple (ExprOper <$> multiplicative)

-- end assignment 2

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
     <|> StatReturn <$ symbol KeyReturn <*> pExpr               <*  sSemi
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


{-# LANGUAGE OverloadedStrings #-}
module RazorsLambda.Parser where

import Control.Applicative hiding (Const)
import Control.Lens hiding (op, Const)
import Control.Monad.IO.Class
import qualified Data.HashSet as HS
import Data.Text (Text)
import qualified Data.Text.Encoding as Text

import Prelude hiding (const)

import Text.Parser.Combinators
import Text.Parser.Expression
import Text.Parser.Token
import Text.Parser.Token.Style
import Text.Trifecta.Delta
import Text.Trifecta.Parser
import Text.Trifecta.Result

import RazorsLambda.AST

parseModule :: Text -> Result Module
parseModule = parseByteString modul (Lines 0 0 0 0) . Text.encodeUtf8

parseModuleFromFile :: MonadIO m => FilePath -> m (Result Module)
parseModuleFromFile fp = parseFromFileEx modul fp

parseExpr :: Text -> Result Expr
parseExpr = parseByteString expr (Lines 0 0 0 0) . Text.encodeUtf8

modul :: Parser Module
modul = Module <$> (reserved "module" *> identifier <* reserved "where")
               <*> (many impt)
               <*> (many decl)
               <*  eof

impt :: Parser Import
impt = Import <$> (reserved "import" *> identifier) <?> "import statement"

decl :: Parser Decl
decl = Decl <$> identifier
            <*> args
            <*> typ
            <*> (reserved "=" *> braces expr)
    <?> "declaration"
  where
  args = manyTill (parens ((,) <$> identifier <*> (colon *> typ))) colon

expr :: Parser Expr
expr = buildExpressionParser table expr'
    <?> "expression"
  where
  table = [
      [ prefix "~" (unop BNot) ]
    , [ binary "*" (binop ITimes) AssocLeft
      , binary "/" (binop IDiv)   AssocLeft ]
    , [ binary "+" (binop IPlus)  AssocLeft
      , binary "-" (binop IMinus) AssocLeft ]
    , [ binary "==" (binop IEq)   AssocNone
      , binary "^" (binop BXor)   AssocRight ]
    , [ binary "&" (binop BAnd)   AssocRight ]
    , [ binary "|" (binop BOr)    AssocRight ]
    ]
  unop op  = pure (EUnop op)
  binop op = pure (EBinop op)

expr' :: Parser Expr
expr' =  foldl1 EApp <$> some expr''
     <|> expr''

expr'' :: Parser Expr
expr'' =  parens expr
      <|> EConst <$> const
      <|> EVar <$> identifier
      <|> lam
      <|> EIfThenElse <$> (reserved "if"   *> expr)
                      <*> (reserved "then" *> expr)
                      <*> (reserved "else" *> expr)
      <?> "simple expression"

lam :: Parser Expr
lam =  reserved "\\"
    *> (uncurry ELam
         <$> parens ((,) <$> identifier <*> (colon *> typ))
         <*> (reserved "->" *> expr))
   <?> "lambda expression"

typ :: Parser Type
typ = buildExpressionParser table typ' <?> "type"
  where
  table = [[ binary "->" (pure TFun) AssocRight ]]

typ' :: Parser Type
typ' =  parens typ
    <|> reserved "unit"    *> pure TUnit
    <|> reserved "bool"    *> pure TBool
    <|> reserved "integer" *> pure TInteger

const :: Parser Const
const =  CInteger <$> natural
     <|> textSymbol "true"  *> pure (CBool True)
     <|> textSymbol "false" *> pure (CBool False)
     <|> textSymbol "()"    *> pure CUnit

idStyle :: IdentifierStyle Parser
idStyle = emptyIdents & styleReserved .~ HS.fromList [
    ":", "=", "->", "+", "-", "*", "/", "==", "()", "\\"
  , "true", "false", "~", "&", "|", "^", "if", "then", "else"
  , "unit", "bool", "integer"
  , "module", "where"
  ]

identifier :: Parser Id
identifier = ident idStyle

reserved :: String -> Parser ()
reserved = reserve idStyle

binary :: String -> Parser (a -> a -> a) -> Assoc -> Operator Parser a
binary name fun assoc = Infix (fun <* reserved name) assoc

prefix :: String -> Parser (a -> a) -> Operator Parser a
prefix name fun       = Prefix (fun <* reserved name)

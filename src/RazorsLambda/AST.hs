{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module RazorsLambda.AST where

import Data.String

import Text.PrettyPrint.Annotated.Leijen

import RazorsLambda.PP

newtype Id = Id String
  deriving (Show, Eq, Ord)

instance IsString Id where
  fromString = Id

data Module = Module Id [Import] [Decl]
  deriving Show

data Import = Import Id
  deriving Show

data Decl = Decl Id [(Id, Type)] Type Expr
  deriving Show

data Const = CInteger Integer | CBool Bool | CUnit
  deriving Show

data Expr
  = EVar Id
  | ELam Id Type Expr
  | EApp Expr Expr
  | EConst Const
  | EUnop Unop Expr
  | EBinop Binop Expr Expr
  | EIfThenElse Expr Expr Expr
  deriving Show

isAtomicExpr :: Expr -> Bool
isAtomicExpr = \case
  EVar _   -> True
  EConst _ -> True
  _        -> False

data Unop = BNot
  deriving Show

data Binop = BAnd | BOr | BXor | IPlus | IMinus | ITimes | IDiv | IEq
  deriving Show

isInfixBinop :: Binop -> Bool
isInfixBinop = \case
  _    -> True

data Type = TUnit | TInteger | TBool | TFun Type Type
  deriving (Eq, Show)

isBaseType :: Type -> Bool
isBaseType = \case
  TFun _ _ -> False
  _        -> True

infixr 0 ~>
(~>) :: Type -> Type -> Type
(~>) = TFun

-- PP

instance PP Id where
  pp (Id x) = text x

instance PP Module where
  pp (Module x is decls) =
    "module" <+> pp x <+> "where" <> line <>
    vsep (map pp is) <> line <>
    vsep (map pp decls)

instance PP Import where
  pp (Import x) = "import" <+> pp x

instance PP Decl where
  pp (Decl x args ret body) =
    pp x <+> hsep (map ppArg args) <+> colon <+> pp ret <+> equals <+> pp body
    where ppArg (y, t) = parens (pp y <+> colon <+> pp t)

instance PP Const where
  pp = annotate AnnConst . \case
    CInteger i -> integer i
    CBool True -> "true"
    CBool False -> "false"
    CUnit -> "()"

instance PP Expr where
  pp expr = (if isAtomicExpr expr then id else parens) $ case expr of
    EVar x -> pp x
    ELam x t e -> "\\" <> parens (pp x <+> colon <+> pp t) <+> "->" <+> pp e
    EApp e1 e2 -> pp e1 <+> pp e2
    EConst c -> pp c
    EUnop u e -> pp u <+> pp e
    EBinop b e1 e2 | isInfixBinop b -> pp e1 <+> pp b <+> pp e2
                   | otherwise      -> pp b <+> pp e1 <+> pp e2
    EIfThenElse e1 e2 e3
      -> (annotate AnnKeyword "if")   <+> pp e1 <+>
         (annotate AnnKeyword "then") <+> pp e2 <+>
         (annotate AnnKeyword "else") <+> pp e3

instance PP Unop where
  pp = annotate AnnKeyword . \case
    BNot -> "not"

instance PP Binop where
  pp = annotate AnnKeyword . \case
    BAnd   -> "&&"
    BOr    -> "||"
    BXor   -> "^"
    IPlus  -> "+"
    IMinus -> "-"
    ITimes -> "*"
    IDiv   -> "/"
    IEq    -> "=="

instance PP Type where
  pp = annotate AnnType . \case
    TUnit -> "unit"
    TInteger -> "integer"
    TBool -> "bool"
    t@(TFun _ _) -> encloseSep empty empty " -> " (assoced t)
    where assoced = \case
            TFun t1 t2 | isBaseType t1 -> pp t1 : assoced t2
                       | otherwise     -> parens (pp t1) : assoced t2
            t                          -> [pp t]

-- Example

factMod :: Module
factMod = Module "Factorial" [] [factDecl]
  where factDecl = Decl "fact" [("x", TInteger)] TInteger factBody
        factBody = EIfThenElse (EBinop IEq (EVar "x") (EConst (CInteger 1)))
                               (EConst (CInteger 1))
                               (EBinop ITimes (EVar "x")
                                              (EApp (EVar "fact")
                                                    (EBinop IMinus (EVar "x") (EConst (CInteger 1)))))

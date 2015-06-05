{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TypeSynonymInstances #-}
module RazorsLambda.TypeCheck where

import Data.Map (Map)
import qualified Data.Map as Map

import MonadLib hiding (Id)
import MonadLib.Derive
import MonadLib.Monads hiding (Id)

import Text.PrettyPrint.Annotated.Leijen

import RazorsLambda.AST
import RazorsLambda.PP

data TCError
  = TCMismatch Expr Type Type -- ^ decl, expr, expected, actual
  | TCNonFun Expr Type -- ^ expr expected function, actual
  | TCUnbound Id -- ^ unbound id
  deriving (Show, Eq)

instance PP TCError where
  pp = \case
    TCMismatch e tExp tAct ->
      "Type mismatch for expression:" <+> pp e <+>
      "expected:" <+> pp tExp <+> "but found:" <+> pp tAct
    TCNonFun e t ->
      "Not a function, or applied to too many arguments:" <+> pp e <+>
      "found type:" <+> pp t
    TCUnbound x ->
      "Unbound variable:" <+> pp x

type TCEnv = Map Id Type

newtype TypeCheck a = TC { unTC :: ExceptionT TCError (Reader TCEnv) a }

tcIso :: Iso (ExceptionT TCError (Reader TCEnv)) TypeCheck
tcIso = Iso TC unTC

instance Functor TypeCheck where
  fmap = derive_fmap tcIso

instance Applicative TypeCheck where
  pure  = derive_pure tcIso
  (<*>) = derive_apply tcIso

instance Monad TypeCheck where
  (>>=) = derive_bind tcIso

instance ExceptionM TypeCheck TCError where
  raise = derive_raise tcIso

instance RunExceptionM TypeCheck TCError where
  try = derive_try tcIso

instance ReaderM TypeCheck TCEnv where
  ask = derive_ask tcIso

instance RunReaderM TypeCheck TCEnv where
  local = derive_local tcIso

runTC :: TypeCheck a -> Either TCError a
runTC m = runReader Map.empty (runExceptionT (unTC m))

typeCheckConst :: Const -> TypeCheck Type
typeCheckConst = \case
  CInteger _ -> return TInteger
  CBool _    -> return TBool
  CUnit      -> return TUnit

typeCheckUnop :: Unop -> TypeCheck Type
typeCheckUnop = \case
  BNot -> return (TBool ~> TBool)

typeCheckBinop :: Binop -> TypeCheck Type
typeCheckBinop = \case
  BAnd   -> return b2
  BOr    -> return b2
  BXor   -> return b2
  IPlus  -> return i2
  IMinus -> return i2
  ITimes -> return i2
  IDiv   -> return i2
  IEq    -> return (TInteger ~> TInteger ~> TBool)
  where b2 = (TBool ~> TBool ~> TBool)
        i2 = (TInteger ~> TInteger ~> TInteger)

typeCheckExpr :: Expr -> TypeCheck Type
typeCheckExpr = \case
  EVar x -> do
    env <- ask
    case Map.lookup x env of
      Just varTy -> return varTy
      Nothing -> raise (TCUnbound x)

  ELam x tArg body -> do
    tRet <- mapReader (Map.insert x tArg) (typeCheckExpr body)
    return (TFun tArg tRet)

  EApp e1 e2 -> do
    t1 <- typeCheckExpr e1
    t2 <- typeCheckExpr e2
    case t1 of
      TFun tArg tRet | tArg == t2 -> return tRet
                     | otherwise  -> raise (TCMismatch e2 tArg t2)
      _ -> raise (TCNonFun e1 t1)

  EConst c -> typeCheckConst c

  EUnop u e -> do
    tOp <- typeCheckUnop u
    t <- typeCheckExpr e
    case tOp of
      TFun tArg tRet | tArg == t -> return tRet
                     | otherwise -> raise (TCMismatch e tArg t)
      _ -> error "impossible: unop not a function type"

  EBinop b e1 e2 -> do
    tOp <- typeCheckBinop b
    t1 <- typeCheckExpr e1
    t2 <- typeCheckExpr e2
    case tOp of
      TFun tArg1 (TFun tArg2 tRet)
        | tArg1 == t1 && tArg2 == t2 -> return tRet
        | tArg1 == t1                -> raise (TCMismatch e2 tArg2 t2)
        |                tArg2 == t2 -> raise (TCMismatch e1 tArg1 t1)
      _ -> error "impossible: binop not a function type"

  EIfThenElse e1 e2 e3 -> do
    t1 <- typeCheckExpr e1
    unless (t1 == TBool) $ raise (TCMismatch e1 TBool t1)
    t2 <- typeCheckExpr e2
    t3 <- typeCheckExpr e3
    unless (t2 == t3) $ raise (TCMismatch e3 t2 t3)
    return t2

typeCheckDecl :: Decl -> TypeCheck Type
typeCheckDecl = \case
  Decl x args ret e -> do
    let t          = foldr TFun ret (map snd args)
        extend env = Map.insert x t (Map.union env (Map.fromList args))
    ret' <- mapReader extend (typeCheckExpr e)
    unless (ret == ret') $ raise (TCMismatch e ret ret')
    return t

typeCheckModule :: Module -> TypeCheck ()
typeCheckModule = \case
  Module _ _is decls -> do
    let tcDecl d@(Decl x _ _ _) = mapReader (Map.delete x) $ typeCheckDecl d
        tDecl (Decl x args ret _) = (x, foldr TFun ret (map snd args))
        tDecls = map tDecl decls
        -- shadow existing bindings
        extend env = Map.union (Map.fromList tDecls) env
    _ <- mapReader extend (mapM tcDecl decls)
    return ()

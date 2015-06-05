{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TypeSynonymInstances #-}
module RazorsLambda.Eval where

import Control.Monad.Fix

import Data.Map (Map)
import qualified Data.Map as Map

import MonadLib hiding (Id)
import MonadLib.Derive
import MonadLib.Monads hiding (Id)

import Text.PrettyPrint.Annotated.Leijen hiding ((<$>))

import RazorsLambda.AST
import RazorsLambda.PP

data Value = VInteger Integer | VBool Bool | VUnit | VClos Id Expr EvalEnv

instance Show Value where
  show = \case
    VInteger i     -> "VInteger " ++ show i
    VBool b        -> "VBool " ++ show b
    VUnit          -> "VUnit"
    VClos x e _env -> "VClos " ++ show x ++ " " ++ show e ++ " <env>"

instance PP Value where
  pp = \case
    VInteger i  -> integer i
    VBool True  -> text "true"
    VBool False -> text "false"
    VUnit       -> text "()"
    VClos _ _ _ -> text "<closure>"

type EvalEnv = Map Id Value

data EvalError = EDivByZero
  deriving Show

newtype Eval a = Eval { unEval :: ExceptionT EvalError (State EvalEnv) a }

evalIso :: Iso (ExceptionT EvalError (State EvalEnv)) Eval
evalIso = Iso Eval unEval

instance Functor Eval where
  fmap = derive_fmap evalIso

instance Applicative Eval where
  pure  = derive_pure evalIso
  (<*>) = derive_apply evalIso

instance Monad Eval where
  (>>=) = derive_bind evalIso

instance ExceptionM Eval EvalError where
  raise = derive_raise evalIso

instance RunExceptionM Eval EvalError where
  try = derive_try evalIso

instance StateM Eval EvalEnv where
  get = derive_get evalIso
  set = derive_set evalIso

instance MonadFix Eval where
  mfix = derive_mfix evalIso

runEval :: Eval a -> (Either EvalError a, EvalEnv)
runEval m = runState Map.empty (runExceptionT (unEval m))

localEnv :: (EvalEnv -> EvalEnv) -> Eval a -> Eval a
localEnv f m = do
  env <- get
  set (f env)
  x <- m
  set env
  return x

addModule :: Module -> Eval ()
addModule (Module _ _ decls) = do
  env <- get
  let evalDecl' d@(Decl x _ _ _) = do
        vDecl <- evalDecl d
        return (x, vDecl)
  rec set (Map.union (Map.fromList vDecls) env)
      vDecls <- mapM evalDecl' decls
  return ()

evalDecl :: Decl -> Eval Value
evalDecl (Decl x args _ e) = do
  let e' = foldr (\(y, t) e'' -> ELam y t e'') e args
  rec v <- localEnv (Map.insert x v) (evalExpr e')
  return v

evalExpr :: Expr -> Eval Value
evalExpr = \case
  EVar x -> do
    mv <- Map.lookup x <$> get
    case mv of
      Just v -> return v
      Nothing -> error "unbound variable during eval"
  ELam x _ e -> do
    env <- get
    return (VClos x e env)
  EApp e1 e2 -> do
    v2 <- evalExpr e2
    v1 <- evalExpr e1
    case v1 of
      VClos x e env -> localEnv (const (Map.insert x v2 env)) (evalExpr e)
      _ -> error "non-function in function position during eval"
  EConst c -> evalConst c
  EUnop u e -> do
    v <- evalExpr e
    f <- evalUnop u
    f v
  EBinop b e1 e2 -> do
    v1 <- evalExpr e1
    v2 <- evalExpr e2
    f <- evalBinop b
    f v1 v2
  EIfThenElse e1 e2 e3 -> do
    v1 <- evalExpr e1
    case v1 of
      VBool True -> evalExpr e2
      VBool False -> evalExpr e3
      _ -> error "non-boolean in test position during eval"

evalConst :: Const -> Eval Value
evalConst = \case
  CInteger i -> return (VInteger i)
  CBool b    -> return (VBool b)
  CUnit      -> return VUnit

evalUnop :: Unop -> Eval (Value -> Eval Value)
evalUnop = \case
  BNot -> return $ \(VBool b) -> return (VBool (not b))

evalBinop :: Binop -> Eval (Value -> Value -> Eval Value)
evalBinop = \case
  BAnd   -> boolOp (&&)
  BOr    -> boolOp (||)
  BXor   -> boolOp (/=)
  IPlus  -> iOp (+)
  IMinus -> iOp (-)
  ITimes -> iOp (*)
  IDiv   -> return $ \(VInteger i1) (VInteger i2) -> do
    when (i2 == 0) $ raise EDivByZero
    return (VInteger (i1 `div` i2))
  IEq    -> return $ \(VInteger i1) (VInteger i2) -> return (VBool (i1 == i2))
  where
  boolOp op =
    return $ \(VBool b1) (VBool b2) -> return (VBool (op b1 b2))
  iOp op =
    return $ \(VInteger b1) (VInteger b2) -> return (VInteger (op b1 b2))

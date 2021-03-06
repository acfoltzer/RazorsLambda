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
import RazorsLambda.TypeCheck

-- | Standard except for the 'Type' in @VClos@ which we use as
-- meta-information for printing and serialization
data Value = VInteger Integer | VBool Bool | VUnit | VClos Id Expr EvalEnv Type

instance Show Value where
  show = \case
    VInteger i        -> "VInteger " ++ show i
    VBool b           -> "VBool " ++ show b
    VUnit             -> "VUnit"
    VClos x e _env ty ->
      "VClos " ++ show x ++ " " ++ show e ++ " <env> " ++ show ty

instance PP Value where
  pp = \case
    VInteger i     -> integer i
    VBool True     -> text "true"
    VBool False    -> text "false"
    VUnit          -> text "()"
    VClos _ _ _ ty -> text "<closure>" <+> colon <+> pp ty

type EvalEnv = Map Id Value

data EvalError = EDivByZero
  deriving Show

instance PP EvalError where
  pp = \case
    EDivByZero -> text "Division by zero"

newtype Eval a = Eval { unEval :: StateT EvalEnv (Exception EvalError) a }

evalIso :: Iso (StateT EvalEnv (Exception EvalError)) Eval
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

runEval :: Eval a -> (Either EvalError (a, EvalEnv))
runEval m = runException (runStateT Map.empty (unEval m))

runEvalIn :: EvalEnv -> Eval a -> (Either EvalError (a, EvalEnv))
runEvalIn env m = runEval (localEnv (const env) m)

localEnv :: (EvalEnv -> EvalEnv) -> Eval a -> Eval a
localEnv f m = do
  env <- get
  set (f env)
  x <- m
  set env
  return x

evalModule :: Module -> Eval EvalEnv
evalModule (Module _ _ decls) = do
  env <- get
  let evalDecl' d@(Decl x _ _ _) = do
        vDecl <- evalDecl d
        return (x, vDecl)
  rec set (Map.union (Map.fromList vDecls) env)
      vDecls <- mapM evalDecl' decls
  get

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
  expr@(ELam x _ e) -> do
    -- Hack... should really have type-annotated AST
    let Right ty = runTC (typeCheckExpr expr)
    env <- get
    return (VClos x e env ty)
  EApp e1 e2 -> do
    v2 <- evalExpr e2
    v1 <- evalExpr e1
    case v1 of
      VClos x e env _ty -> localEnv (const (Map.insert x v2 env)) (evalExpr e)
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

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module RazorsLambda.Interactive where

import Control.Monad
import Control.Monad.Free
import Control.Monad.Free.TH
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Text.PrettyPrint.ANSI.Leijen as ANSI
import Text.Trifecta.Result hiding (Result)

import RazorsLambda.AST
import RazorsLambda.Eval
import RazorsLambda.Parser
import RazorsLambda.TypeCheck

type IEnv = (TCEnv, EvalEnv)

iEmptyEnv :: IEnv
iEmptyEnv = (Map.empty, Map.empty)

data Interactive r
  = IGetInput (Text -> r)
  | ILoadFile FilePath (Text -> r)
  | IParseCommand Text (Command -> r)
  | IReportResult Result r
  | IGetEnv (IEnv -> r)
  | ISetEnv IEnv r
  | IExit
  deriving (Functor)

data UnknownCommand = UnknownCommand Text
  deriving (Show)

data Command
  = CEvalExpr Text
  | CTypeOf Text
  | CLoadFile FilePath
  | CExit
  | CUnknown Text
  | CEmpty

data Result
  = RValue Value
  | RType Type
  | RUnknownCommand Text
  | RParseError ANSI.Doc
  | REvalError EvalError
  | RTCError TCError
  | REmpty

$(makeFree ''Interactive)

repl :: Free Interactive ()
repl = forever $ do
  l <- iGetInput
  c <- iParseCommand l
  iReportResult =<< runCmd c

runCmd :: Command -> Free Interactive Result
runCmd c = do
  (tcEnv, eEnv) <- iGetEnv
  case c of
    CEvalExpr txt ->
      case parseExpr txt of
        Failure err -> return (RParseError err)
        Success e ->
          case runTCIn tcEnv (typeCheckExpr e) of
            Left err -> return (RTCError err)
            Right _ ->
              case runEvalIn eEnv (evalExpr e) of
                Left err -> return (REvalError err)
                Right (v, _) -> return (RValue v)
    CTypeOf txt ->
      case parseExpr txt of
        Failure err -> return (RParseError err)
        Success e ->
          case runTCIn tcEnv (typeCheckExpr e) of
            Left err -> return (RTCError err)
            Right t -> return (RType t)
    CLoadFile fp -> do
      txt <- iLoadFile fp
      case parseModule txt of
        Failure err -> return (RParseError err)
        Success m ->
          case runTCIn tcEnv (typeCheckModule m) of
            Left err -> return (RTCError err)
            Right tcEnv' -> do
              case runEvalIn eEnv (evalModule m) of
                Left err -> return (REvalError err)
                Right (eEnv', _) -> do
                  iSetEnv (tcEnv', eEnv')
                  return REmpty
    CExit -> iExit
    CEmpty -> return REmpty
    CUnknown cmd -> return (RUnknownCommand cmd)

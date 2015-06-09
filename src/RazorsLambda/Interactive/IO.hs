{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module RazorsLambda.Interactive.IO where

import Control.Monad.Free
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import MonadLib
import System.Exit
import System.IO
import qualified Text.PrettyPrint.ANSI.Leijen as ANSI
import Text.PrettyPrint.Annotated.Leijen

import RazorsLambda.Eval
import RazorsLambda.Interactive
import RazorsLambda.PP
import RazorsLambda.TypeCheck

runRepl :: IO ()
runRepl = void (runStateT iEmptyEnv (runIO repl))

runIO :: Free Interactive r -> StateT (TCEnv, EvalEnv) IO r
runIO = \case
  Free (IGetInput k) -> do
    inBase $ Text.putStr "λ> " *> hFlush stdout
    l <- inBase $ Text.getLine
    runIO (k l)
  Free (ILoadFile fp k) -> do
    txt <- inBase $ Text.readFile fp
    runIO (k txt)
  Free (IParseCommand l k) ->
    case Text.words l of
      [] -> runIO (k CEmpty)
      c : r -> runIO (k $ case c of
                 ":t" -> CTypeOf (Text.unwords r)
                 ":l" -> CLoadFile (Text.unpack (Text.unwords r))
                 ":q" -> CExit
                 other | Text.isPrefixOf ":" other -> CUnknown other
                 _expr -> CEvalExpr l)
  Free (IReportResult r k) ->
    let prt x = inBase (putDoc (pp x) *> putStrLn "") *> runIO k
    in case r of
      RValue v -> prt v
      RType t -> prt t
      RUnknownCommand cmd -> do
        inBase (putDoc ("Unknown command:" <+> text (Text.unpack cmd)))
        runIO k
      RParseError e -> do
        inBase (ANSI.putDoc e *> putStrLn "")
        runIO k
      REvalError e -> prt e
      RTCError e   -> prt e
      REmpty -> runIO k
  Free (IGetEnv k) -> runIO . k =<< get
  Free (ISetEnv env k) -> set env *> runIO k
  Free IExit -> inBase exitSuccess
  Pure r -> return r

module RazorsLambda where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

import Text.Trifecta.Delta
import Text.Trifecta.Parser
import Text.Trifecta.Result

import RazorsLambda.AST
import RazorsLambda.Eval
import qualified RazorsLambda.Parser as P
import RazorsLambda.TypeCheck

parseModule :: Text -> Result Module
parseModule = parseByteString P.modul (Lines 0 0 0 0) . Text.encodeUtf8

parseExpr = undefined

evalExprIn = undefined

consoleOutput = undefined

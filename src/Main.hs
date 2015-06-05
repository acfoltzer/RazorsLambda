{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad
import Text.PrettyPrint.Annotated.Leijen
import Text.Trifecta.Result

import RazorsLambda.Eval
import RazorsLambda.Parser
import RazorsLambda.PP
import RazorsLambda.TypeCheck

main :: IO ()
main = do
  Success m <- parseModuleFromFile "example/Factorial.rzl"
  guard (Right () == runTC (typeCheckModule m))
  let Success e = parseExpr "fact 5"
      (Right v, _env) = runEval (do addModule m; evalExpr e)
  putDoc (pp v)
  putStrLn ""

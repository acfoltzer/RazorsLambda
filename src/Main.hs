module Main where

import RazorsLambda

main :: IO ()
main = do
  Right m <- parseModule <$> readFile "examples/Factorial.rzl"
  let Right e = parseExpr "fact5"
      Right v = evalExprIn m e
  consoleOutput v

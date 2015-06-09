{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Concurrent
import Control.Monad
import qualified Data.ByteString.Char8 as BS
import qualified Data.Map as Map
import MonadLib
import System.ZMQ4.Monadic
import Text.PrettyPrint.Annotated.Leijen
import Text.Trifecta.Result

import RazorsLambda.Eval
import qualified RazorsLambda.Interactive.IO as IO
import RazorsLambda.Parser
import RazorsLambda.PP
import RazorsLambda.TypeCheck

main = IO.runRepl

main' :: IO ()
main' = do
  forkOS server
  forkOS client
  forever (threadDelay (2^20))

server :: IO ()
server = runZMQ $ do
  liftIO $ putStrLn "[server] coming online"
  rep <- socket Rep
  bind rep "tcp://127.0.0.1:5555"
  forever $ do
    msg <- receive rep
    liftIO $ putStr "[server] " >> BS.putStrLn msg
    send rep [] "Pong"

client :: IO ()
client = runZMQ $ do
  liftIO $ putStrLn "[client] coming online"
  req <- socket Req
  connect req "tcp://127.0.0.1:5555"
  forever $ do
    send req [] "Ping"
    msg <- receive req
    liftIO $ putStr "[client] " >> BS.putStrLn msg
    liftIO $ threadDelay (2^20)

{-
main :: IO ()
main = do
  Success m <- parseModuleFromFile "example/Factorial.rzl"
  guard (Right () == runTC (typeCheckModule m))
  let Success e = parseExpr "fact 5"
      (Right v, _env) = runEval (do addModule m; evalExpr e)
  putDoc (pp v)
  putStrLn ""
-}

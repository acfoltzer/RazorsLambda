{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Concurrent
import Control.Monad
import qualified Data.ByteString.Char8 as BS
import qualified Data.Map as Map
import MonadLib
import System.Environment
import System.ZMQ4.Monadic
import Text.Read
import Text.PrettyPrint.Annotated.Leijen
import Text.Trifecta.Result

import RazorsLambda.Eval
import qualified RazorsLambda.Interactive.IO as IO
import qualified RazorsLambda.Interactive.JSON as JSON
import RazorsLambda.Parser
import RazorsLambda.PP
import RazorsLambda.TypeCheck

main' = IO.runRepl

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> JSON.runRepl 5555
    [portStr] ->
       case readMaybe portStr of
         Just port -> JSON.runRepl port
         Nothing -> JSON.runRepl 5555
    _ -> error "port is the only allowed argument"

mainLoopback :: IO ()
mainLoopback = do
  void $ forkOS client
  JSON.runRepl 5555

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
  void . replicateM 5 $ do
    send req [] "{\"expr\":\"5 + 5\",\"cmdType\":\"evalExpr\"}"
    msg <- receive req
    liftIO $ putStr "[client] " >> BS.putStrLn msg
    liftIO $ threadDelay (2^20)
  send req [] "{\"cmdType\":\"exit\"}"
  close req

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

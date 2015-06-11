module Main where

import Data.Aeson
import Data.Aeson.Encode.Pretty
import Control.Monad
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Maybe
import System.Environment
import System.IO
import System.ZMQ4.Monadic
import Text.Read

main = do
  args <- getArgs
  let port = case args of
        [] -> 5555
        [portStr] -> fromMaybe 5555 (readMaybe portStr)
        _ -> error "port is the only allowed argument"
  runZMQ $ do
    liftIO $ putStrLn "[client] coming online"
    req <- socket Req
    connect req ("tcp://127.0.0.1:" ++ show port)
    forever $ do
      l <- liftIO $ do
        putStr "json> "
        hFlush stdout
        BS.getLine
      when (not (BS.null l)) $ do
        send req [] l
        msg <- receive req
        case decodeStrict msg of
          Nothing -> liftIO $ BS.putStrLn msg
          Just js -> liftIO $ BSL.putStrLn (encodePretty (js :: Value))

{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | There are a lot of newtypes in here prefixed with @RPC@. This is
-- because we're defining instances for types that are somewhat
-- incomplete, but work well enough for remote interaction.
module RazorsLambda.Interactive.JSON where

import Control.Monad.Free
import Data.Aeson hiding (Result, Value)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.IO as Text
import Data.Word
import MonadLib
--import qualified Text.PrettyPrint.ANSI.Leijen as ANSI
import System.ZMQ4.Monadic

import RazorsLambda.AST
import RazorsLambda.Eval
import RazorsLambda.Interactive

default (Text)

newtype RPCCommand = RPCCommand { unRPCCommand :: Command }

instance ToJSON RPCCommand where
  toJSON rc = case unRPCCommand rc of
    CEvalExpr txt -> object
      [ "cmdType" .= "evalExpr", "expr" .= txt ]
    CTypeOf txt -> object
      [ "cmdType" .= "typeOf", "expr" .= txt ]
    CLoadFile fp -> object
      [ "cmdType" .= "loadFile", "filepath" .= fp ]
    CExit -> object
      [ "cmdType" .= "exit" ]
    CUnknown txt -> object
      [ "cmdType" .= "unknown", "unknown" .= txt ]
    CEmpty -> object
      [ "cmdType" .= "empty" ]

instance FromJSON RPCCommand where
  parseJSON = withObject "command" $ \o -> do
    tag <- o .: "cmdType"
    case tag :: Text of
      "evalExpr" -> RPCCommand . CEvalExpr <$> o .: "expr"
      "typeOf"   -> RPCCommand . CTypeOf   <$> o .: "expr"
      "loadFile" -> RPCCommand . CLoadFile <$> o .: "filepath"
      "exit"     -> return (RPCCommand CExit)
      "unknown"  -> RPCCommand . CUnknown  <$> o .: "unknown"
      "empty"    -> return (RPCCommand CEmpty)
      unknown    -> return (RPCCommand (CUnknown unknown))

newtype RPCResult = RPCResult { unRPCResult :: Result }

instance ToJSON RPCResult where
  toJSON rr = case unRPCResult rr of
    RValue v -> object
      [ "resultType" .= "value", "value" .= RPCValue v ]
    RType t -> object
      [ "resultType" .= "type", "type" .= RPCType t ]
    RUnknownCommand cmd -> object
      [ "resultType" .= "unknownCommand", "command" .= cmd ]
    RParseError err -> object
      [ "resultType" .= "parseError", "error" .= show err ]
    REvalError err -> object
      [ "resultType" .= "evalError", "error" .= show err ]
    RTCError err -> object
      [ "resultType" .= "typeCheckError", "error" .= show err ]
    REmpty -> object
      [ "resultType" .= "empty" ]

newtype RPCValue = RPCValue { unRPCValue :: Value }

instance ToJSON RPCValue where
  toJSON rv = case unRPCValue rv of
    VInteger i -> object
      [ "valueType" .= RPCType TInteger, "value" .= i ]
    VBool b -> object
      [ "valueType" .= RPCType TBool, "value" .= b ]
    VUnit -> object
      [ "valueType" .= RPCType TUnit ]
    -- Just send the type across
    VClos _x _e _env t -> object
      [ "valueType" .= RPCType t ]

newtype RPCType = RPCType { unRPCType :: Type }

instance ToJSON RPCType where
  toJSON rt = case unRPCType rt of
    TUnit -> object
      [ "typeType" .= "base", "type" .= "unit" ]
    TInteger -> object
      [ "typeType" .= "base", "type" .= "integer" ]
    TBool -> object
      [ "typeType" .= "base", "type" .= "bool" ]
    TFun t1 t2 -> object
      [ "typeType" .= "function", "arg" .= (RPCType t1), "ret" .= (RPCType t2) ]

runRepl :: Word16 -> IO ()
runRepl port = runZMQ $ setup >>= start
  where

  setup = do
    let addr = "tcp://127.0.0.1:" ++ show port
    liftIO $ putStrLn ("[server] coming online on interface " ++ addr)
    rep <- socket Rep
    bind rep addr
    return rep

  start :: forall z . Socket z Rep -> ZMQ z ()
  start rep = do
    let io :: IO a -> StateT IEnv (ZMQ z) a
        io = lift . liftIO
        loop :: Free Interactive () -> StateT IEnv (ZMQ z) ()
        loop = \case
          Free (IGetInput k) -> do
            msg <- lift $ receive (rep :: Socket z Rep)
            io $ putStr "[server] " >> BS.putStrLn msg
            loop (k (Text.decodeUtf8 msg))
          Free (ILoadFile fp k) -> do
            txt <- io $ Text.readFile fp
            loop (k txt)
          Free (IParseCommand msg k) ->
            case eitherDecodeStrict (Text.encodeUtf8 msg) of
              Left err -> loop (k (CUnknown (Text.pack err)))
              Right rc -> loop (k (unRPCCommand rc))
          Free (IReportResult r k) -> do
            let msg = encode (RPCResult r)
            lift $ send rep [] (BS.concat (BSL.toChunks msg))
            loop k
          Free (IGetEnv k) -> loop . k =<< get
          Free (ISetEnv env k) -> set env *> loop k
          Free IExit -> do
            io $ putStrLn "[server] shutting down"
            lift $ close rep
          Pure x -> return x
    void (runStateT iEmptyEnv (loop repl))

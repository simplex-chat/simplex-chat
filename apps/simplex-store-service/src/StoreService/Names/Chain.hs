{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module StoreService.Names.Chain
  ( ChainClient (..),
    ChainError (..),
    TxHash (..),
    sendCommit,
    sendRegister,
  )
where

import StoreService.Names.Abi (hex)
import StoreService.Names.Registration (RegisterIntent, commitCalldata, registerWithCreditCalldata)
import Control.Concurrent.STM (TVar)
import Control.Exception (try)
import Control.Monad.Except (ExceptT, liftEither, runExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import Crypto.Random (ChaChaDRG)
import Data.Aeson ((.:), (.:?), (.=))
import qualified Data.Aeson as J
import qualified Data.Aeson.Types as JT
import Data.ByteArray.Encoding (Base (Base16), convertFromBase)
import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Word (Word64)
import Network.HTTP.Client (Manager)
import qualified Network.HTTP.Client as HTTP
import Network.HTTP.Types (hContentType, methodPost)
import Numeric (readHex)
import Simplex.Messaging.Crypto.Secp256k1 (Secp256k1PrivateKey)
import Simplex.Messaging.Eth.Address (Address, addressFromPrivateKey)
import Simplex.Messaging.Eth.Transaction (Eip1559Tx (..), signEip1559Tx)
import Simplex.Messaging.Util (liftError', tshow)

data ChainClient = ChainClient
  { ccManager :: Manager,
    ccRpcUrl :: String,
    ccChainId :: Word64,
    ccController :: Address,
    ccSponsor :: Address,
    ccRegistrarKey :: Secp256k1PrivateKey
  }

newtype ChainError = ChainError Text
  deriving (Eq, Show)

newtype TxHash = TxHash ByteString
  deriving (Eq, Show)

sendCommit :: TVar ChaChaDRG -> ChainClient -> ByteString -> IO (Either ChainError TxHash)
sendCommit g c@ChainClient {ccController} = sendTransaction g c ccController . commitCalldata

sendRegister :: TVar ChaChaDRG -> ChainClient -> RegisterIntent -> IO (Either ChainError TxHash)
sendRegister g c@ChainClient {ccSponsor} = sendTransaction g c ccSponsor . registerWithCreditCalldata

sendTransaction :: TVar ChaChaDRG -> ChainClient -> Address -> ByteString -> IO (Either ChainError TxHash)
sendTransaction g c@ChainClient {ccChainId, ccRegistrarKey} to calldata = runExceptT $ do
  from <- liftIO $ addressFromPrivateKey g ccRegistrarKey
  gas <- rpc c "eth_estimateGas" [J.object ["from" .= from, "to" .= to, "data" .= hex calldata], "pending"] quantity
  nonce <- rpc c "eth_getTransactionCount" [J.toJSON from, "pending"] quantity
  tip <- rpc c "eth_maxPriorityFeePerGas" [] quantity
  baseFee <- rpc c "eth_getBlockByNumber" ["latest", J.Bool False] $ J.withObject "block" $ \b -> quantity =<< b .: "baseFeePerGas"
  let tx =
        Eip1559Tx
          { txChainId = ccChainId,
            txNonce = nonce,
            txMaxPriorityFeePerGas = tip,
            txMaxFeePerGas = 2 * baseFee + tip,
            txGasLimit = gas,
            txTo = to,
            txValue = 0,
            txData = calldata
          }
  raw <- liftError' (ChainError . T.pack) $ signEip1559Tx g ccRegistrarKey tx
  TxHash <$> rpc c "eth_sendRawTransaction" [J.String $ hex raw] bytes

rpc :: ChainClient -> Text -> [J.Value] -> (J.Value -> JT.Parser a) -> ExceptT ChainError IO a
rpc ChainClient {ccManager, ccRpcUrl} method params parseResult = do
  resp <- liftError' (\(e :: HTTP.HttpException) -> ChainError $ tshow e) $ try $ do
    req <- HTTP.parseRequest ccRpcUrl
    HTTP.httpLbs
      req
        { HTTP.method = methodPost,
          HTTP.redirectCount = 0,
          HTTP.requestHeaders = (hContentType, "application/json") : HTTP.requestHeaders req,
          HTTP.requestBody = HTTP.RequestBodyLBS body
        }
      ccManager
  either (throwError . ChainError . T.pack) liftEither $ JT.parseEither response =<< J.eitherDecode (HTTP.responseBody resp)
  where
    body = J.encode $ J.object ["jsonrpc" .= ("2.0" :: Text), "id" .= (1 :: Int), "method" .= method, "params" .= params]
    response = J.withObject "JSON-RPC response" $ \o -> o .:? "error" >>= maybe (Right <$> (parseResult =<< o .: "result")) rpcError
    rpcError = J.withObject "JSON-RPC error" $ \e -> Left . ChainError <$> e .: "message"

quantity :: (Eq a, Num a) => J.Value -> JT.Parser a
quantity = J.withText "quantity" $ \t -> case T.stripPrefix "0x" t of
  Just h | [(n, "")] <- readHex (T.unpack h) -> pure n
  _ -> fail "not a quantity"

bytes :: J.Value -> JT.Parser ByteString
bytes = J.withText "data" $ \t -> case T.stripPrefix "0x" t of
  Just h | Right bs <- convertFromBase Base16 (encodeUtf8 h) -> pure bs
  _ -> fail "not hex data"

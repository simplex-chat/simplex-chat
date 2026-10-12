{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module BadgeService.Names.Chain
  ( ChainClient (..),
    ChainError (..),
    TxHash (..),
    sendCommit,
    sendRegister,
  )
where

import BadgeService.Names.Registration (RegisterIntent, commitCalldata, registerWithCreditCalldata)
import Control.Concurrent.STM (TVar)
import Control.Exception (try)
import Control.Monad.Except (ExceptT (..), liftEither, runExceptT, withExceptT)
import Control.Monad.IO.Class (liftIO)
import Crypto.Random (ChaChaDRG)
import Data.Aeson ((.:), (.:?), (.=))
import qualified Data.Aeson as J
import qualified Data.Aeson.Types as JT
import Data.ByteArray.Encoding (Base (Base16), convertFromBase, convertToBase)
import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeLatin1, encodeUtf8)
import Data.Word (Word64)
import Network.HTTP.Client (Manager)
import qualified Network.HTTP.Client as HTTP
import Network.HTTP.Types (hContentType)
import Numeric (readHex)
import Simplex.Messaging.Crypto.Secp256k1 (Secp256k1PrivateKey)
import Simplex.Messaging.Eth.Address (Address, addressFromPrivateKey)
import Simplex.Messaging.Eth.Transaction (Eip1559Tx (..), signEip1559Tx)
import Simplex.Messaging.Util (eitherToMaybe, tshow)

data ChainClient = ChainClient
  { ccManager :: Manager,
    ccRpcUrl :: String,
    ccChainId :: Word64,
    ccController :: Address,
    ccSponsor :: Address,
    ccRegistrarKey :: Secp256k1PrivateKey
  }

data ChainError
  = CERpc Text
  | CEReverted ByteString
  | CEInternal Text
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
            txNonce = fromInteger nonce,
            txMaxPriorityFeePerGas = fromInteger tip,
            txMaxFeePerGas = fromInteger $ 2 * baseFee + tip,
            txGasLimit = fromInteger gas,
            txTo = to,
            txValue = 0,
            txData = calldata
          }
  raw <- withExceptT (CEInternal . T.pack) $ ExceptT $ signEip1559Tx g ccRegistrarKey tx
  TxHash <$> rpc c "eth_sendRawTransaction" [J.String $ hex raw] bytes

rpc :: ChainClient -> Text -> [J.Value] -> (J.Value -> JT.Parser a) -> ExceptT ChainError IO a
rpc ChainClient {ccManager, ccRpcUrl} method params parseResult = do
  resp <- withExceptT (CERpc . httpError) $ ExceptT $ try $ do
    req <- HTTP.parseRequest ccRpcUrl
    HTTP.httpLbs req {HTTP.method = "POST", HTTP.requestHeaders = [(hContentType, "application/json")], HTTP.requestBody = HTTP.RequestBodyLBS body} ccManager
  liftEither $ either (Left . CERpc . T.pack) id $ JT.parseEither response =<< J.eitherDecode (HTTP.responseBody resp)
  where
    body = J.encode $ J.object ["jsonrpc" .= ("2.0" :: Text), "id" .= (1 :: Int), "method" .= method, "params" .= params]
    response = J.withObject "JSON-RPC response" $ \o -> o .:? "error" >>= maybe (Right <$> (parseResult =<< o .: "result")) rpcError
    rpcError = J.withObject "JSON-RPC error" $ \e -> do
      revert <- e .:? "data"
      message <- e .: "message"
      pure $ Left $ case revert of
        Just (J.String d) | Just bs <- fromHex d -> CEReverted bs
        _ -> CERpc message

httpError :: HTTP.HttpException -> Text
httpError = \case
  HTTP.HttpExceptionRequest _ content -> tshow content
  e -> tshow e

quantity :: J.Value -> JT.Parser Integer
quantity = J.withText "quantity" $ \t -> case T.stripPrefix "0x" t of
  Just h | [(n, "")] <- readHex (T.unpack h) -> pure n
  _ -> fail $ "not a quantity: " <> T.unpack t

bytes :: J.Value -> JT.Parser ByteString
bytes = J.withText "data" $ maybe (fail "not hex data") pure . fromHex

fromHex :: Text -> Maybe ByteString
fromHex t = T.stripPrefix "0x" t >>= eitherToMaybe . convertFromBase Base16 . encodeUtf8

hex :: ByteString -> Text
hex bs = "0x" <> decodeLatin1 (convertToBase Base16 bs)

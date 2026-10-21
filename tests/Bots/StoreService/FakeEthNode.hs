{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Bots.StoreService.FakeEthNode
  ( FakeEthNode (..),
    withFakeEthNode,
    revertCalls,
    sentTransactions,
    fakeGas,
    fakeTip,
    fakeBaseFee,
  )
where

import Control.Concurrent.STM
import Data.Aeson ((.:), (.=))
import qualified Data.Aeson as J
import qualified Data.Aeson.Types as JT
import Data.ByteArray.Encoding (Base (Base16), convertFromBase, convertToBase)
import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeLatin1, encodeUtf8)
import Network.HTTP.Client (Manager)
import qualified Network.HTTP.Client as HTTP
import Network.HTTP.Types (badRequest400, hContentType, ok200)
import Network.Wai (Application, responseLBS, strictRequestBody)
import qualified Network.Wai.Handler.Warp as Warp
import Numeric (showHex)
import Simplex.Messaging.Crypto (keccak256)
import Simplex.Messaging.Util (eitherToMaybe)

data FakeEthNode = FakeEthNode
  { fnUrl :: String,
    fnManager :: Manager,
    fnState :: TVar FakeState
  }

data FakeState = FakeState
  { fsRevert :: Bool,
    fsSent :: [ByteString]
  }

fakeGas, fakeTip, fakeBaseFee :: Num a => a
fakeGas = 200000
fakeTip = 1000000000
fakeBaseFee = 2000000000

withFakeEthNode :: (FakeEthNode -> IO a) -> IO a
withFakeEthNode action = do
  fnState <- newTVarIO FakeState {fsRevert = False, fsSent = []}
  fnManager <- HTTP.newManager HTTP.defaultManagerSettings
  Warp.testWithApplication (pure $ fakeApp fnState) $ \port ->
    action FakeEthNode {fnUrl = "http://127.0.0.1:" <> show port, fnManager, fnState}

revertCalls :: FakeEthNode -> IO ()
revertCalls FakeEthNode {fnState} = atomically $ modifyTVar' fnState $ \s -> s {fsRevert = True}

sentTransactions :: FakeEthNode -> IO [ByteString]
sentTransactions FakeEthNode {fnState} = fsSent <$> readTVarIO fnState

fakeApp :: TVar FakeState -> Application
fakeApp st req respond = do
  body <- strictRequestBody req
  case JT.parseMaybe request =<< J.decode body of
    Just (method, params) -> do
      reply <- atomically $ answer st method params
      respond $ responseLBS ok200 [(hContentType, "application/json")] $ J.encode $ J.object $ ["jsonrpc" .= ("2.0" :: Text), "id" .= (1 :: Int)] <> reply
    Nothing -> respond $ responseLBS badRequest400 [] "not a JSON-RPC request"
  where
    request = J.withObject "request" $ \o -> (,) <$> o .: "method" <*> o .: "params"

answer :: TVar FakeState -> Text -> [J.Value] -> STM [JT.Pair]
answer st method params = do
  s@FakeState {fsRevert, fsSent} <- readTVar st
  case (method, params) of
    ("eth_estimateGas", _) -> pure $ if fsRevert then reverted else result $ quantity fakeGas
    ("eth_getTransactionCount", _) -> pure $ result $ quantity $ toInteger $ length fsSent
    ("eth_maxPriorityFeePerGas", _) -> pure $ result $ quantity fakeTip
    ("eth_getBlockByNumber", _) -> pure $ result $ J.object ["baseFeePerGas" .= quantity fakeBaseFee]
    ("eth_sendRawTransaction", [J.String raw]) | Just tx <- fromHex raw -> do
      writeTVar st s {fsSent = tx : fsSent}
      pure $ result $ hex $ keccak256 tx
    _ -> pure ["error" .= J.object ["code" .= (-32601 :: Int), "message" .= ("method not found" :: Text)]]
  where
    result v = ["result" .= v]
    reverted = ["error" .= J.object ["code" .= (3 :: Int), "message" .= ("execution reverted" :: Text)]]

quantity :: Integer -> Text
quantity n = "0x" <> T.pack (showHex n "")

hex :: ByteString -> Text
hex bs = "0x" <> decodeLatin1 (convertToBase Base16 bs)

fromHex :: Text -> Maybe ByteString
fromHex t = T.stripPrefix "0x" t >>= eitherToMaybe . convertFromBase Base16 . encodeUtf8

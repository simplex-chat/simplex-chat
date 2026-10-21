{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Bots.StoreService.FakeEthNode
  ( FakeEthNode (..),
    CallAnswer (..),
    Fault (..),
    withFakeEthNode,
    fakeChainConfig,
    answerCall,
    setFault,
    clearFault,
    setBlock,
    mine,
    sentTransactions,
    revertFault,
    fakeChainId,
    fakeController,
    fakeSponsor,
    fakeRegistrarKey,
    fakeGas,
    fakeTip,
    fakeBaseFee,
  )
where

import Control.Concurrent.STM
import Control.Exception (finally)
import Data.Aeson ((.:), (.=))
import qualified Data.Aeson as J
import qualified Data.Aeson.Types as JT
import qualified Data.ByteArray as BA
import Data.ByteArray.Encoding (Base (Base16), convertFromBase)
import Data.ByteString (ByteString)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Word (Word64)
import Network.HTTP.Types (badGateway502, badRequest400, hContentType, ok200)
import Network.Wai (Application, responseLBS, strictRequestBody)
import qualified Network.Wai.Handler.Warp as Warp
import Numeric (showHex)
import Simplex.Messaging.Crypto (keccak256)
import qualified Simplex.Messaging.Crypto.Secp256k1 as S
import Simplex.Messaging.Encoding.String (strDecode)
import Simplex.Messaging.Eth.Address (Address, unAddress)
import Simplex.Messaging.Util (eitherToMaybe)
import StoreService.Config (ChainConfig (..))
import StoreService.Names.Abi (hex)

data FakeEthNode = FakeEthNode
  { fnUrl :: String,
    fnState :: TVar FakeState
  }

data CallAnswer = Returns ByteString | Reverts ByteString

data Fault
  = RpcError Int Text (Maybe ByteString)
  | -- | answers only once the node is closed
    Hang
  | HttpFailure

data FakeState = FakeState
  { fsCalls :: Map (ByteString, ByteString) CallAnswer,
    fsFaults :: Map Text Fault,
    fsSent :: [ByteString],
    fsReceipts :: Map ByteString (Bool, Word64),
    fsBlock :: (Word64, Word64),
    fsClosed :: Bool
  }

fakeGas, fakeTip, fakeBaseFee :: Num a => a
fakeGas = 200000
fakeTip = 1000000000
fakeBaseFee = 2000000000

fakeChainId :: Word64
fakeChainId = 31337

fakeController, fakeSponsor, fakeBaseRegistrar, fakeResolver :: Address
fakeController = address "0x0165878a594ca255338adfa4d48449f69242eb8f"
fakeSponsor = address "0x9a676e781a523b5d0c0e43731313a708cb607508"
fakeBaseRegistrar = address "0xe7f1725e7734ce288f8367e1bb143e90bb3f0512"
fakeResolver = address "0x2279b7a0a67db372996a5fab50d91eaa73d2ebe6"

fakeRegistrarKey :: S.Secp256k1PrivateKey
fakeRegistrarKey = either error id $ S.mkPrivateKey $ BA.replicate 32 0x46

-- | 2026-01-01, block 1; each mined block is this many seconds later.
genesisTime, blockSeconds :: Word64
genesisTime = 1767225600
blockSeconds = 12

withFakeEthNode :: (FakeEthNode -> IO a) -> IO a
withFakeEthNode action = do
  fnState <- newTVarIO FakeState {fsCalls = M.empty, fsFaults = M.empty, fsSent = [], fsReceipts = M.empty, fsBlock = (1, genesisTime), fsClosed = False}
  Warp.testWithApplication (pure $ fakeApp fnState) $ \port ->
    action FakeEthNode {fnUrl = "http://127.0.0.1:" <> show port, fnState}
      `finally` atomically (modifyTVar' fnState $ \s -> s {fsClosed = True})

fakeChainConfig :: FakeEthNode -> ChainConfig
fakeChainConfig FakeEthNode {fnUrl} =
  ChainConfig
    { cRpcUrl = fnUrl,
      cChainId = fakeChainId,
      cController = fakeController,
      cSponsor = fakeSponsor,
      cBaseRegistrar = fakeBaseRegistrar,
      cResolver = fakeResolver,
      cServiceKey = fakeRegistrarKey,
      cConfirmations = 3,
      cGasLimitCap = 8000000
    }

answerCall :: FakeEthNode -> Address -> ByteString -> CallAnswer -> IO ()
answerCall FakeEthNode {fnState} to calldata a = atomically $ modifyTVar' fnState $ \s -> s {fsCalls = M.insert (unAddress to, calldata) a $ fsCalls s}

-- | Every request for the method fails this way until 'clearFault'.
setFault :: FakeEthNode -> Text -> Fault -> IO ()
setFault FakeEthNode {fnState} method f = atomically $ modifyTVar' fnState $ \s -> s {fsFaults = M.insert method f $ fsFaults s}

clearFault :: FakeEthNode -> Text -> IO ()
clearFault FakeEthNode {fnState} method = atomically $ modifyTVar' fnState $ \s -> s {fsFaults = M.delete method $ fsFaults s}

revertFault :: ByteString -> Fault
revertFault = RpcError 3 "execution reverted" . Just

-- | Sets the latest block's number and time.
setBlock :: FakeEthNode -> Word64 -> Word64 -> IO ()
setBlock FakeEthNode {fnState} number time = atomically $ modifyTVar' fnState $ \s -> s {fsBlock = (number, time)}

-- | Mines a new block with these transaction hashes and receipt statuses, and returns its number.
mine :: FakeEthNode -> [(ByteString, Bool)] -> IO Word64
mine FakeEthNode {fnState} txs = atomically $ do
  s@FakeState {fsBlock = (number, time), fsReceipts} <- readTVar fnState
  let number' = number + 1
      mined = M.fromList [(h, (ok, number')) | (h, ok) <- txs]
  writeTVar fnState s {fsBlock = (number', time + blockSeconds), fsReceipts = M.union mined fsReceipts}
  pure number'

-- | The raw transactions in the order they were sent.
sentTransactions :: FakeEthNode -> IO [ByteString]
sentTransactions FakeEthNode {fnState} = reverse . fsSent <$> readTVarIO fnState

fakeApp :: TVar FakeState -> Application
fakeApp st req respond = do
  body <- strictRequestBody req
  case JT.parseMaybe request =<< J.decode body of
    Just (method, params) ->
      fault method >>= \case
        Just HttpFailure -> respond $ responseLBS badGateway502 [] "bad gateway"
        Just Hang -> do
          atomically $ readTVar st >>= check . fsClosed
          reply $ rpcError (-32000) "node closed" Nothing
        Just (RpcError code message revertData) -> reply $ rpcError code message revertData
        Nothing -> reply =<< atomically (answer st method params)
    Nothing -> respond $ responseLBS badRequest400 [] "not a JSON-RPC request"
  where
    request = J.withObject "request" $ \o -> (,) <$> o .: "method" <*> o .: "params"
    fault method = M.lookup method . fsFaults <$> readTVarIO st
    reply r = respond $ responseLBS ok200 [(hContentType, "application/json")] $ J.encode $ J.object $ ["jsonrpc" .= ("2.0" :: Text), "id" .= (1 :: Int)] <> r

answer :: TVar FakeState -> Text -> [J.Value] -> STM [JT.Pair]
answer st method params = do
  s@FakeState {fsCalls, fsSent, fsReceipts, fsBlock = (number, time)} <- readTVar st
  case (method, params) of
    ("eth_call", [call, _])
      | Just (to, calldata) <- JT.parseMaybe callTarget call ->
          pure $ case M.lookup (unAddress to, calldata) fsCalls of
            Just (Returns ret) -> result $ hex ret
            Just (Reverts revertData) -> rpcError 3 "execution reverted" (Just revertData)
            Nothing -> rpcError (-32000) "no fake answer for this call" Nothing
    ("eth_estimateGas", _) -> pure $ result $ quantity fakeGas
    ("eth_getTransactionCount", [_, J.String "latest"]) -> pure $ result $ quantity $ toInteger $ M.size fsReceipts
    ("eth_getTransactionCount", [_, J.String "pending"]) -> pure $ result $ quantity $ toInteger $ length fsSent
    ("eth_maxPriorityFeePerGas", _) -> pure $ result $ quantity fakeTip
    ("eth_getBlockByNumber", [J.String "latest", _]) ->
      pure $ result $ J.object ["number" .= quantity (toInteger number), "timestamp" .= quantity (toInteger time), "baseFeePerGas" .= quantity fakeBaseFee]
    ("eth_getTransactionReceipt", [J.String h]) | Just hash <- fromHex h -> pure $ result $ case M.lookup hash fsReceipts of
      Just (ok, block) -> J.object ["transactionHash" .= h, "status" .= quantity (if ok then 1 else 0), "blockNumber" .= quantity (toInteger block)]
      Nothing -> J.Null
    ("eth_getTransactionByHash", [J.String h]) | Just hash <- fromHex h -> pure $ result $ case M.lookup hash fsReceipts of
      Just (_, block) -> J.object ["hash" .= h, "blockNumber" .= quantity (toInteger block)]
      Nothing
        | any ((== hash) . keccak256) fsSent -> J.object ["hash" .= h, "blockNumber" .= J.Null]
        | otherwise -> J.Null
    ("eth_sendRawTransaction", [J.String raw]) | Just tx <- fromHex raw -> do
      writeTVar st s {fsSent = tx : fsSent}
      pure $ result $ hex $ keccak256 tx
    _ -> pure $ rpcError (-32601) "method not found" Nothing
  where
    result v = ["result" .= v]
    callTarget = J.withObject "call" $ \o -> (,) <$> o .: "to" <*> (maybe (fail "not hex data") pure . fromHex =<< o .: "data")

rpcError :: Int -> Text -> Maybe ByteString -> [JT.Pair]
rpcError code message revertData = ["error" .= J.object (["code" .= code, "message" .= message] <> maybe [] (\d -> ["data" .= hex d]) revertData)]

quantity :: Integer -> Text
quantity n = "0x" <> T.pack (showHex n "")

fromHex :: Text -> Maybe ByteString
fromHex t = T.stripPrefix "0x" t >>= eitherToMaybe . convertFromBase Base16 . encodeUtf8

address :: ByteString -> Address
address = either error id . strDecode

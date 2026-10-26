{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Bots.StoreService.FakeEthNode
  ( FakeEthNode (..),
    CallAnswer (..),
    Fault (..),
    Gate,
    withFakeEthNode,
    fakeChainConfig,
    answerCall,
    answerCallOnce,
    answerCallFrom,
    forgetCall,
    setFault,
    clearFault,
    releaseGate,
    pausedAt,
    setBlock,
    setFees,
    revertDryRun,
    mine,
    dropTransaction,
    pruneReceipt,
    useNextNonce,
    sentTransactions,
    awaitSent,
    revertFault,
    fakeChainId,
    fakeController,
    fakeNamePolicy,
    listCommunityName,
    setCommunityTerms,
    fakeCommunityTerms,
    fakeCommunityTermsText,
    fakeNewCommunityTerms,
    fakeTermsUrl,
    fakeSponsor,
    fakeBaseRegistrar,
    fakeResolver,
    fakeRegistrarKey,
    fakeGas,
    fakeTip,
    fakeBaseFee,
    fakeMaxFeePerGas,
  )
where

import Control.Applicative ((<|>))
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (Async, async, cancel)
import Control.Concurrent.STM
import Control.Exception (finally)
import Data.Aeson ((.:), (.=))
import qualified Data.Aeson as J
import qualified Data.Aeson.Types as JT
import qualified Data.ByteArray as BA
import Data.ByteArray.Encoding (Base (Base16), convertFromBase)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Word (Word64)
import Network.HTTP.Types (badGateway502, badRequest400, hContentType, ok200)
import Network.Wai (Application, pathInfo, responseLBS, strictRequestBody)
import qualified Network.Wai.Handler.Warp as Warp
import Numeric (readHex, showHex)
import Simplex.Messaging.Crypto (keccak256)
import qualified Simplex.Messaging.Crypto.Secp256k1 as S
import Simplex.Messaging.Encoding.String (strDecode)
import Simplex.Messaging.Eth.Address (Address, unAddress)
import Simplex.Messaging.SimplexName (LabelHash (..))
import Simplex.Messaging.Util (eitherToMaybe)
import StoreService.Config (ChainConfig (..))
import StoreService.Names.Abi (addressWord, hex, selector)
import StoreService.Names.Chain (TxRequest (..), decodeSignedTx)
import System.Timeout (timeout)

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
  | -- | a 200 page that is not JSON, as a proxy may answer
    NotJson
  | Answers J.Value
  | -- | answers as usual after this many microseconds
    Slow Int
  | -- | the next request waits at the gate until it is released, then answers as usual; later ones pass
    Gated Gate

data Gate = Gate {gArrived :: TMVar (), gRelease :: TMVar ()}

data FakeState = FakeState
  { fsCalls :: Map (ByteString, ByteString) CallAnswer,
    -- | answered once, before 'fsCalls'
    fsCallsOnce :: Map (ByteString, ByteString) CallAnswer,
    -- | answered from the block on, before 'fsCalls'
    fsCallsFrom :: Map (ByteString, ByteString) (Word64, CallAnswer),
    fsFaults :: Map Text Fault,
    -- | every raw transaction received, newest first
    fsSent :: [ByteString],
    -- | the transactions the node knows and has not mined, by nonce
    fsPool :: Map Word64 ByteString,
    fsReceipts :: Map ByteString (Bool, Word64),
    -- | the revert data of the dry runs of these calldata
    fsDryRuns :: Map ByteString ByteString,
    -- | the key's transaction count at the latest block
    fsUsed :: Word64,
    fsBlock :: (Word64, Word64),
    -- | the times of the blocks mined or set, by number
    fsBlockTimes :: Map Word64 Word64,
    -- | the suggested tip and the base fee
    fsFees :: (Word64, Word64),
    fsClosed :: Bool
  }

fakeGas, fakeTip, fakeBaseFee, fakeMaxFeePerGas :: Num a => a
fakeGas = 200000
fakeTip = 1000000000
fakeBaseFee = 2000000000
fakeMaxFeePerGas = 100000000000

fakeChainId :: Word64
fakeChainId = 31337

fakeController, fakeSponsor, fakeBaseRegistrar, fakeResolver :: Address
fakeController = address "0x0165878a594ca255338adfa4d48449f69242eb8f"
fakeSponsor = address "0x9a676e781a523b5d0c0e43731313a708cb607508"
fakeBaseRegistrar = address "0xe7f1725e7734ce288f8367e1bb143e90bb3f0512"
fakeResolver = address "0x2279b7a0a67db372996a5fab50d91eaa73d2ebe6"

-- | The controller's name policy; it lists no community name unless a test answers otherwise, and its terms are the text
-- the node serves at /terms.
fakeNamePolicy :: Address
fakeNamePolicy = address "0x5fc8d32690cc91d4c39d9d3abcbd16989f875707"

fakeCommunityTermsText :: ByteString
fakeCommunityTermsText = "bona fide community terms v1"

fakeCommunityTerms :: ByteString
fakeCommunityTerms = keccak256 fakeCommunityTermsText

-- | Community terms the node's page does not serve, as new terms set on chain before the page is updated.
fakeNewCommunityTerms :: ByteString
fakeNewCommunityTerms = keccak256 "bona fide community terms v2"

fakeTermsUrl :: FakeEthNode -> Text
fakeTermsUrl FakeEthNode {fnUrl} = T.pack fnUrl <> "/terms"

-- | The policy lists the name as a community name.
listCommunityName :: FakeEthNode -> LabelHash -> IO ()
listCommunityName node (LabelHash lh) = answerCall node fakeNamePolicy (selector "bonaFideNames(bytes32)" <> lh) $ Returns $ B.replicate 31 0 <> "\x01"

-- | The policy's community terms are the text with this keccak256.
setCommunityTerms :: FakeEthNode -> ByteString -> IO ()
setCommunityTerms node termsHash = answerCall node fakeNamePolicy (selector "termsHash()") $ Returns termsHash

defaultCall :: (ByteString, ByteString) -> Maybe CallAnswer
defaultCall (to, calldata)
  | to == unAddress fakeController && calldata == selector "namePolicy()" = Just $ Returns $ addressWord fakeNamePolicy
  | to == unAddress fakeNamePolicy && selector "bonaFideNames(bytes32)" `B.isPrefixOf` calldata = Just $ Returns $ B.replicate 32 0
  | to == unAddress fakeNamePolicy && calldata == selector "termsHash()" = Just $ Returns fakeCommunityTerms
  | otherwise = Nothing

fakeRegistrarKey :: S.Secp256k1PrivateKey
fakeRegistrarKey = either error id $ S.mkPrivateKey $ BA.replicate 32 0x46

-- | 2026-01-01, block 1 after the genesis block 0; each mined block is this many seconds later.
genesisTime, blockSeconds :: Word64
genesisTime = 1767225600
blockSeconds = 12

withFakeEthNode :: (FakeEthNode -> IO a) -> IO a
withFakeEthNode action = do
  fnState <-
    newTVarIO
      FakeState
        { fsCalls = M.empty,
          fsCallsOnce = M.empty,
          fsCallsFrom = M.empty,
          fsFaults = M.empty,
          fsSent = [],
          fsPool = M.empty,
          fsReceipts = M.empty,
          fsDryRuns = M.empty,
          fsUsed = 0,
          fsBlock = (1, genesisTime),
          fsBlockTimes = M.fromList [(0, genesisTime - blockSeconds), (1, genesisTime)],
          fsFees = (fakeTip, fakeBaseFee),
          fsClosed = False
        }
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
      cGasLimitCap = 8000000,
      cMaxFeePerGas = fakeMaxFeePerGas
    }

answerCall :: FakeEthNode -> Address -> ByteString -> CallAnswer -> IO ()
answerCall FakeEthNode {fnState} to calldata a = atomically $ modifyTVar' fnState $ \s -> s {fsCalls = M.insert (unAddress to, calldata) a $ fsCalls s}

-- | The next call only is answered this way, then the call answers as before.
answerCallOnce :: FakeEthNode -> Address -> ByteString -> CallAnswer -> IO ()
answerCallOnce FakeEthNode {fnState} to calldata a = atomically $ modifyTVar' fnState $ \s -> s {fsCallsOnce = M.insert (unAddress to, calldata) a $ fsCallsOnce s}

-- | At this block and later ones the call answers this way, at earlier blocks as before.
answerCallFrom :: FakeEthNode -> Word64 -> Address -> ByteString -> CallAnswer -> IO ()
answerCallFrom FakeEthNode {fnState} block to calldata a = atomically $ modifyTVar' fnState $ \s -> s {fsCallsFrom = M.insert (unAddress to, calldata) (block, a) $ fsCallsFrom s}

-- | The call has no answer any more, as if the node failed it.
forgetCall :: FakeEthNode -> Address -> ByteString -> IO ()
forgetCall FakeEthNode {fnState} to calldata = atomically $ modifyTVar' fnState $ \s -> s {fsCalls = M.delete (unAddress to, calldata) $ fsCalls s}

-- | Every request for the method fails this way until 'clearFault'.
setFault :: FakeEthNode -> Text -> Fault -> IO ()
setFault FakeEthNode {fnState} method f = atomically $ modifyTVar' fnState $ \s -> s {fsFaults = M.insert method f $ fsFaults s}

clearFault :: FakeEthNode -> Text -> IO ()
clearFault FakeEthNode {fnState} method = atomically $ modifyTVar' fnState $ \s -> s {fsFaults = M.delete method $ fsFaults s}

-- | The next request for the method stops at the gate it returns.
gateNext :: FakeEthNode -> Text -> IO Gate
gateNext node method = do
  gate <- Gate <$> newEmptyTMVarIO <*> newEmptyTMVarIO
  gate <$ setFault node method (Gated gate)

-- | Returns once a request is held at the gate.
awaitGate :: Gate -> IO ()
awaitGate Gate {gArrived} = atomically $ readTMVar gArrived

releaseGate :: Gate -> IO ()
releaseGate Gate {gRelease} = atomically $ putTMVar gRelease ()

-- | Far longer than a request takes to reach its first chain read.
gateArrival :: Int
gateArrival = 5000000

-- | The action, run until its first request for the method is held at the gate.
pausedAt :: FakeEthNode -> Text -> IO a -> IO (Async a, Gate)
pausedAt node method action = do
  gate <- gateNext node method
  inFlight <- async action
  timeout gateArrival (awaitGate gate) >>= maybe (cancel inFlight >> fail ("no " <> T.unpack method <> " reached the gate")) (const $ pure (inFlight, gate))

revertFault :: ByteString -> Fault
revertFault = RpcError 3 "execution reverted" . Just

-- | Sets the latest block's number and time.
setBlock :: FakeEthNode -> Word64 -> Word64 -> IO ()
setBlock FakeEthNode {fnState} number time = atomically $ modifyTVar' fnState $ \s -> s {fsBlock = (number, time), fsBlockTimes = M.insert number time $ fsBlockTimes s}

-- | The dry run of this calldata reverts with this data, or passes again with Nothing.
revertDryRun :: FakeEthNode -> ByteString -> Maybe ByteString -> IO ()
revertDryRun FakeEthNode {fnState} calldata revertData = atomically $ modifyTVar' fnState $ \s -> s {fsDryRuns = M.alter (const revertData) calldata $ fsDryRuns s}

setFees :: FakeEthNode -> Word64 -> Word64 -> IO ()
setFees FakeEthNode {fnState} tip baseFee = atomically $ modifyTVar' fnState $ \s -> s {fsFees = (tip, baseFee)}

-- | Mines a new block with these transaction hashes and receipt statuses, and returns its number.
-- A mined transaction of the pool uses its nonce, and the pool loses every transaction below it.
mine :: FakeEthNode -> [(ByteString, Bool)] -> IO Word64
mine FakeEthNode {fnState} txs = atomically $ do
  s@FakeState {fsBlock = (number, time), fsReceipts, fsPool, fsUsed} <- readTVar fnState
  let number' = number + 1
      mined = M.fromList [(h, (ok, number')) | (h, ok) <- txs]
      used = maximum $ fsUsed : [nonce + 1 | (nonce, raw) <- M.toList fsPool, keccak256 raw `M.member` mined]
  writeTVar fnState s {fsBlock = (number', time + blockSeconds), fsBlockTimes = M.insert number' (time + blockSeconds) $ fsBlockTimes s, fsReceipts = M.union mined fsReceipts, fsPool = M.dropWhileAntitone (< used) fsPool, fsUsed = used}
  pure number'

-- | The node forgets a transaction it has not mined, as after its own restart.
dropTransaction :: FakeEthNode -> ByteString -> IO ()
dropTransaction FakeEthNode {fnState} hash = atomically $ modifyTVar' fnState $ \s -> s {fsPool = M.filter ((/= hash) . keccak256) $ fsPool s}

-- | The node forgets a receipt, as a pruned node does; the nonce stays used.
pruneReceipt :: FakeEthNode -> ByteString -> IO ()
pruneReceipt FakeEthNode {fnState} hash = atomically $ modifyTVar' fnState $ \s -> s {fsReceipts = M.delete hash $ fsReceipts s}

-- | The key's next nonce goes to a transaction the node shows nothing of, and the pool loses its own at that nonce.
useNextNonce :: FakeEthNode -> IO ()
useNextNonce FakeEthNode {fnState} = atomically $ modifyTVar' fnState $ \s@FakeState {fsUsed, fsPool} -> s {fsUsed = fsUsed + 1, fsPool = M.delete fsUsed fsPool}

-- | The raw transactions in the order they were sent.
sentTransactions :: FakeEthNode -> IO [ByteString]
sentTransactions FakeEthNode {fnState} = reverse . fsSent <$> readTVarIO fnState

-- | Waits until the node has received at least this many raw transactions, and returns them in the order they were sent.
awaitSent :: FakeEthNode -> Int -> IO [ByteString]
awaitSent FakeEthNode {fnState} n = atomically $ do
  sent <- fsSent <$> readTVar fnState
  check $ length sent >= n
  pure $ reverse sent

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
        Just NotJson -> respond $ responseLBS ok200 [] "<html>Sign in to provider-api-key</html>"
        Just (Answers v) -> reply ["result" .= v]
        Just (Slow delay) -> threadDelay delay >> answered method params
        Just (Gated Gate {gArrived, gRelease}) -> do
          atomically $ putTMVar gArrived ()
          atomically $ takeTMVar gRelease `orElse` (readTVar st >>= check . fsClosed)
          answered method params
        Nothing -> answered method params
    Nothing
      | pathInfo req == ["terms"] -> respond $ responseLBS ok200 [] $ LB.fromStrict fakeCommunityTermsText
      | otherwise -> respond $ responseLBS badRequest400 [] "not a JSON-RPC request"
  where
    request = J.withObject "request" $ \o -> (,) <$> o .: "method" <*> o .: "params"
    -- a gate holds one request only
    fault method = atomically $ do
      s <- readTVar st
      case M.lookup method $ fsFaults s of
        f@(Just Gated {}) -> f <$ writeTVar st s {fsFaults = M.delete method $ fsFaults s}
        f -> pure f
    answered method params = reply =<< atomically (answer st method params)
    reply r = respond $ responseLBS ok200 [(hContentType, "application/json")] $ J.encode $ J.object $ ["jsonrpc" .= ("2.0" :: Text), "id" .= (1 :: Int)] <> r

answer :: TVar FakeState -> Text -> [J.Value] -> STM [JT.Pair]
answer st method params = do
  s@FakeState {fsCalls, fsCallsOnce, fsCallsFrom, fsSent, fsPool, fsReceipts, fsDryRuns, fsUsed, fsBlock = (number, time), fsBlockTimes, fsFees = (tip, baseFee)} <- readTVar st
  case (method, params) of
    ("eth_call", [call, J.String tag])
      | Just (to, calldata) <- JT.parseMaybe callTarget call,
        Just at <- if tag == "latest" then Just number else fromQuantity tag ->
          let key = (unAddress to, calldata)
              callAnswer = \case
                Just (Returns ret) -> result $ hex ret
                Just (Reverts revertData) -> rpcError 3 "execution reverted" (Just revertData)
                Nothing -> rpcError (-32000) "no fake answer for this call" Nothing
           in case M.lookup key fsCallsOnce of
                Just a -> callAnswer (Just a) <$ writeTVar st s {fsCallsOnce = M.delete key fsCallsOnce}
                Nothing -> pure $ callAnswer $ case M.lookup key fsCallsFrom of
                  Just (from, a) | at >= from -> Just a
                  _ -> M.lookup key fsCalls <|> defaultCall key
    ("eth_estimateGas", call : _)
      | Just (_, calldata) <- JT.parseMaybe callTarget call,
        Just revertData <- M.lookup calldata fsDryRuns ->
          pure $ rpcError 3 "execution reverted" (Just revertData)
    ("eth_estimateGas", _) -> pure $ result $ quantity fakeGas
    ("eth_getTransactionCount", [_, J.String "latest"]) -> pure $ result $ quantity $ toInteger fsUsed
    ("eth_getTransactionCount", [_, J.String "pending"]) ->
      pure $ result $ quantity $ toInteger $ fsUsed + fromIntegral (length $ takeWhile (`M.member` fsPool) [fsUsed ..])
    ("eth_maxPriorityFeePerGas", _) -> pure $ result $ quantity $ toInteger tip
    ("eth_chainId", _) -> pure $ result $ quantity $ toInteger fakeChainId
    ("eth_getBlockByNumber", [J.String "latest", _]) -> pure $ result $ blockJson baseFee number time
    -- a block not mined or set here has the time of the last one before it
    ("eth_getBlockByNumber", [J.String n, _])
      | Just at <- fromQuantity n -> pure $ result $ case M.lookupLE at fsBlockTimes of
          Just (_, t) | at <= number -> blockJson baseFee at t
          _ -> J.Null
    ("eth_getTransactionReceipt", [J.String h]) | Just hash <- fromHex h -> pure $ result $ case M.lookup hash fsReceipts of
      Just (ok, block) -> J.object ["transactionHash" .= h, "status" .= quantity (if ok then 1 else 0), "blockNumber" .= quantity (toInteger block)]
      Nothing -> J.Null
    ("eth_getTransactionByHash", [J.String h]) | Just hash <- fromHex h -> pure $ result $ case M.lookup hash fsReceipts of
      Just (_, block) -> J.object ["hash" .= h, "blockNumber" .= quantity (toInteger block)]
      Nothing
        | any ((== hash) . keccak256) fsPool -> J.object ["hash" .= h, "blockNumber" .= J.Null]
        | otherwise -> J.Null
    ("eth_sendRawTransaction", [J.String raw])
      | Just tx <- fromHex raw,
        Right TxRequest {trNonce, trMaxFeePerGas, trMaxPriorityFeePerGas} <- decodeSignedTx tx -> do
          let accept = (result $ hex $ keccak256 tx) <$ writeTVar st s {fsSent = tx : fsSent, fsPool = M.insert trNonce tx fsPool}
          case M.lookup trNonce fsPool of
            _ | trNonce < fsUsed -> pure $ rpcError (-32000) "nonce too low" Nothing
            Just known
              | known == tx -> pure $ rpcError (-32000) "already known" Nothing
              -- geth and reth replace a pending transaction only when both its fees are at least 10% higher
              | Right TxRequest {trMaxFeePerGas = knownMax, trMaxPriorityFeePerGas = knownTip} <- decodeSignedTx known,
                trMaxFeePerGas * 10 < knownMax * 11 || trMaxPriorityFeePerGas * 10 < knownTip * 11 ->
                  pure $ rpcError (-32000) "replacement transaction underpriced" Nothing
            _ -> accept
    _ -> pure $ rpcError (-32601) "method not found" Nothing
  where
    result v = ["result" .= v]
    callTarget = J.withObject "call" $ \o -> (,) <$> o .: "to" <*> (maybe (fail "not hex data") pure . fromHex =<< o .: "data")

blockJson :: Word64 -> Word64 -> Word64 -> J.Value
blockJson baseFee number time = J.object ["number" .= quantity (toInteger number), "timestamp" .= quantity (toInteger time), "baseFeePerGas" .= quantity (toInteger baseFee)]

rpcError :: Int -> Text -> Maybe ByteString -> [JT.Pair]
rpcError code message revertData = ["error" .= J.object (["code" .= code, "message" .= message] <> maybe [] (\d -> ["data" .= hex d]) revertData)]

quantity :: Integer -> Text
quantity n = "0x" <> T.pack (showHex n "")

fromQuantity :: Text -> Maybe Word64
fromQuantity t = case T.stripPrefix "0x" t of
  Just h | [(n, "")] <- readHex (T.unpack h) -> Just n
  _ -> Nothing

fromHex :: Text -> Maybe ByteString
fromHex t = T.stripPrefix "0x" t >>= eitherToMaybe . convertFromBase Base16 . encodeUtf8

address :: ByteString -> Address
address = either error id . strDecode

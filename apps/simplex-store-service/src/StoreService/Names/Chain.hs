{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module StoreService.Names.Chain
  ( ChainClient (..),
    ChainError (..),
    TxHash (..),
    SignedTx (..),
    TxRequest (..),
    Block (..),
    Receipt (..),
    TxState (..),
    BlockTag (..),
    newChainClient,
    ethCall,
    latestBlock,
    blockAt,
    transactionReceipt,
    transactionByHash,
    transactionCount,
    estimateGas,
    maxPriorityFee,
    signTransaction,
    decodeSignedTx,
    sendRawTransaction,
    httpFailureDetail,
  )
where

import Control.Concurrent.STM (TVar)
import Control.Exception (evaluate, try)
import Control.Monad ((<=<), (>=>))
import Crypto.Number.Serialize (os2ip)
import Crypto.Random (ChaChaDRG)
import Data.Aeson ((.:), (.:?), (.=))
import qualified Data.Aeson as J
import qualified Data.Aeson.Types as JT
import Data.ByteArray.Encoding (Base (Base16), convertFromBase)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import Data.List (foldl')
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeLatin1, encodeUtf8)
import Data.Word (Word64, Word8)
import Network.HTTP.Client (Manager)
import qualified Network.HTTP.Client as HTTP
import Network.HTTP.Client.TLS (newTlsManager)
import Network.HTTP.Types (hContentType, methodPost, statusCode, statusIsSuccessful)
import Numeric (readHex, showHex)
import Simplex.Messaging.Crypto (keccak256)
import Simplex.Messaging.Eth.Address (Address, addressFromPrivateKey)
import Simplex.Messaging.Eth.RLP (RLPItem (..))
import Simplex.Messaging.Eth.Transaction (Eip1559Tx (..), signEip1559Tx)
import Simplex.Messaging.Util (tshow)
import StoreService.Config (ChainConfig (..))
import StoreService.Log (logWarn)
import StoreService.Names.Abi (addressFromBytes, hex)
import StoreService.Names.Revert (RevertName (..), decodeRevert)
import System.Timeout (timeout)

data ChainClient = ChainClient
  { ccManager :: Manager,
    ccConfig :: ChainConfig,
    ccRegistrar :: Address,
    -- | microseconds for a whole call, from the request to the end of its body
    ccTimeout :: Int
  }

-- | 'ChainUnavailable' never holds the RPC URL, which can carry a provider's key.
data ChainError
  = ChainReverted (Maybe RevertName)
  | ChainUnavailable Text
  | ChainGasAboveCap Word64
  deriving (Eq, Show)

newtype TxHash = TxHash ByteString
  deriving (Eq, Show)

data SignedTx = SignedTx {stRaw :: ByteString, stHash :: TxHash}
  deriving (Eq, Show)

data TxRequest = TxRequest
  { trTo :: Address,
    trData :: ByteString,
    trNonce :: Word64,
    trGasLimit :: Word64,
    trMaxFeePerGas :: Word64,
    trMaxPriorityFeePerGas :: Word64
  }
  deriving (Eq, Show)

data Block = Block {blockNumber :: Word64, blockTimestamp :: Word64, blockBaseFee :: Word64}
  deriving (Eq, Show)

data Receipt = Receipt {receiptSuccess :: Bool, receiptBlock :: Word64}
  deriving (Eq, Show)

data TxState = TxPending | TxMined Word64
  deriving (Eq, Show)

data BlockTag = Latest | Pending

rpcTimeout :: Int
rpcTimeout = 10000000

-- | Far above the largest answer the service reads: a block with its transaction hashes, or two records at the link bound.
maxRpcResponseBytes :: Int
maxRpcResponseBytes = 1024 * 1024

revertCode :: Int
revertCode = 3

revertMessage :: Text
revertMessage = "execution reverted"

-- | A node's error message is cut to this many characters.
maxNodeMessageLength :: Int
maxNodeMessageLength = 200

-- | The hex digits of a 256-bit quantity.
maxQuantityDigits :: Int
maxQuantityDigits = 64

newChainClient :: TVar ChaChaDRG -> ChainConfig -> IO ChainClient
newChainClient g ccConfig@ChainConfig {cServiceKey} = do
  ccManager <- newTlsManager
  ccRegistrar <- addressFromPrivateKey g cServiceKey
  pure ChainClient {ccManager, ccConfig, ccRegistrar, ccTimeout = rpcTimeout}

ethCall :: ChainClient -> Address -> ByteString -> IO (Either ChainError ByteString)
ethCall c to calldata = rpc c "eth_call" [J.object ["to" .= to, "data" .= hex calldata], "latest"] bytes

latestBlock :: ChainClient -> IO (Either ChainError Block)
latestBlock c = blockByNumber c "latest"

-- | A block the node does not have is a failed read.
blockAt :: ChainClient -> Word64 -> IO (Either ChainError Block)
blockAt c number = blockByNumber c $ J.String $ "0x" <> T.pack (showHex number "")

blockByNumber :: ChainClient -> J.Value -> IO (Either ChainError Block)
blockByNumber c tag =
  rpc c "eth_getBlockByNumber" [tag, J.Bool False] $ J.withObject "block" $ \b ->
    Block <$> (quantity =<< b .: "number") <*> (quantity =<< b .: "timestamp") <*> (quantity =<< b .: "baseFeePerGas")

-- | Nothing while the transaction is not mined, or after the node pruned its receipt.
transactionReceipt :: ChainClient -> TxHash -> IO (Either ChainError (Maybe Receipt))
transactionReceipt c (TxHash h) =
  rpc c "eth_getTransactionReceipt" [J.String $ hex h] $ nullable $ J.withObject "receipt" $ \r ->
    Receipt <$> (status =<< r .: "status") <*> (quantity =<< r .: "blockNumber")
  where
    status =
      quantity >=> \case
        (1 :: Word64) -> pure True
        0 -> pure False
        _ -> fail "not a receipt status"

-- | Nothing when the node does not know the transaction.
transactionByHash :: ChainClient -> TxHash -> IO (Either ChainError (Maybe TxState))
transactionByHash c (TxHash h) =
  rpc c "eth_getTransactionByHash" [J.String $ hex h] $ nullable $ J.withObject "transaction" $ \t ->
    maybe TxPending TxMined <$> (traverse quantity =<< t .:? "blockNumber")

transactionCount :: ChainClient -> Address -> BlockTag -> IO (Either ChainError Word64)
transactionCount c a tag = rpc c "eth_getTransactionCount" [J.toJSON a, blockTag] quantity
  where
    blockTag = case tag of
      Latest -> "latest"
      Pending -> "pending"

-- | The dry run of a registrar transaction; an estimate above the configured cap is refused.
estimateGas :: ChainClient -> Address -> ByteString -> IO (Either ChainError Word64)
estimateGas c@ChainClient {ccRegistrar, ccConfig = ChainConfig {cGasLimitCap}} to calldata =
  (>>= capped) <$> rpc c "eth_estimateGas" [J.object ["from" .= ccRegistrar, "to" .= to, "data" .= hex calldata], "pending"] quantity
  where
    capped gas
      | gas > cGasLimitCap = Left $ ChainGasAboveCap gas
      | otherwise = Right gas

maxPriorityFee :: ChainClient -> IO (Either ChainError Word64)
maxPriorityFee c = rpc c "eth_maxPriorityFeePerGas" [] quantity

signTransaction :: TVar ChaChaDRG -> ChainClient -> TxRequest -> IO (Either String SignedTx)
signTransaction g ChainClient {ccConfig = ChainConfig {cChainId, cServiceKey}} TxRequest {trTo, trData, trNonce, trGasLimit, trMaxFeePerGas, trMaxPriorityFeePerGas} =
  fmap signed <$> signEip1559Tx g cServiceKey tx
  where
    tx =
      Eip1559Tx
        { txChainId = cChainId,
          txNonce = trNonce,
          txMaxPriorityFeePerGas = fromIntegral trMaxPriorityFeePerGas,
          txMaxFeePerGas = fromIntegral trMaxFeePerGas,
          txGasLimit = trGasLimit,
          txTo = trTo,
          txValue = 0,
          txData = trData
        }
    signed raw = SignedTx {stRaw = raw, stHash = TxHash $ keccak256 raw}

eip1559TxType :: Word8
eip1559TxType = 2

-- | The request a raw transaction from 'signTransaction' was signed from.
decodeSignedTx :: ByteString -> Either String TxRequest
decodeSignedTx raw = case B.uncons raw of
  Just (txType, encoded) | txType == eip1559TxType -> case rlpItem encoded of
    Right (RLPList [_chainId, nonce, tip, maxFee, gas, RLPBytes to, _value, RLPBytes trData, _accessList, _yParity, _r, _s], "") -> do
      trTo <- maybe (Left "transaction: not an address") Right $ addressFromBytes to
      TxRequest trTo trData <$> scalar nonce <*> scalar gas <*> scalar maxFee <*> scalar tip
    Right _ -> Left "transaction: not a signed EIP-1559 transaction"
    Left e -> Left e
  _ -> Left "transaction: not an EIP-1559 transaction"
  where
    scalar = \case
      RLPBytes b | B.length b <= 8 -> Right $ fromInteger $ os2ip b
      _ -> Left "transaction: not a 64-bit scalar"

rlpItem :: ByteString -> Either String (RLPItem, ByteString)
rlpItem s = case B.uncons s of
  Nothing -> Left "RLP: no item"
  Just (prefix, rest)
    | prefix < 0x80 -> Right (RLPBytes $ B.singleton prefix, rest)
    | prefix <= 0xb7 -> bytesItem (fromIntegral $ prefix - 0x80) rest
    | prefix < 0xc0 -> longLength (fromIntegral $ prefix - 0xb7) rest >>= uncurry bytesItem
    | prefix <= 0xf7 -> listItem (fromIntegral $ prefix - 0xc0) rest
    | otherwise -> longLength (fromIntegral $ prefix - 0xf7) rest >>= uncurry listItem
  where
    bytesItem n r = (\(payload, r') -> (RLPBytes payload, r')) <$> split n r
    listItem n r = split n r >>= \(payload, r') -> (\items -> (RLPList items, r')) <$> rlpItems payload
    rlpItems payload
      | B.null payload = Right []
      | otherwise = rlpItem payload >>= \(item, r) -> (item :) <$> rlpItems r
    longLength n r = split n r >>= \(len, r') -> (,r') <$> fits (os2ip len) r'
    fits len r
      | len <= toInteger (B.length r) = Right $ fromInteger len
      | otherwise = Left "RLP: truncated"
    split n r
      | n <= B.length r = Right $ B.splitAt n r
      | otherwise = Left "RLP: truncated"

sendRawTransaction :: ChainClient -> ByteString -> IO (Either ChainError TxHash)
sendRawTransaction c raw = rpc c "eth_sendRawTransaction" [J.String $ hex raw] (fmap TxHash . bytes)

-- | The answer is parsed within the timeout, so a slow parse is bounded too.
rpc :: ChainClient -> Text -> [J.Value] -> (J.Value -> JT.Parser a) -> IO (Either ChainError a)
rpc ChainClient {ccManager, ccConfig = ChainConfig {cRpcUrl}, ccTimeout} method params parseResult =
  timeout ccTimeout (try $ post >>= evaluate . answer) >>= \case
    Nothing -> pure $ Left $ unavailable "timed out"
    Just (Left (e :: HTTP.HttpException)) -> do
      logWarn $ "chain RPC " <> method <> " failed: " <> httpFailureDetail cRpcUrl e
      pure $ Left $ unavailable $ httpFailure e
    Just (Right r) -> pure r
  where
    answer (status, body)
      | LB.length body > fromIntegral maxRpcResponseBytes = Left $ unavailable $ "response over " <> tshow maxRpcResponseBytes <> " bytes"
      | otherwise = case J.eitherDecode body of
          Left _ -> refused Nothing
          Right v -> either (refused . Just) id $ JT.parseEither response v
      where
        refused parseError
          | not (statusIsSuccessful status) = Left $ unavailable $ "HTTP " <> tshow (statusCode status)
          -- aeson's decode error quotes the body, which can be any page a proxy answers with
          | otherwise = Left $ unavailable $ "not a JSON-RPC response" <> maybe "" ((": " <>) . T.pack) parseError
    post = do
      req <- HTTP.parseRequest cRpcUrl
      HTTP.withResponse
        req
          { HTTP.method = methodPost,
            HTTP.redirectCount = 0,
            HTTP.requestHeaders = (hContentType, "application/json") : HTTP.requestHeaders req,
            HTTP.requestBody = HTTP.RequestBodyLBS requestBody
          }
        ccManager
        $ \resp -> (HTTP.responseStatus resp,) <$> HTTP.brReadSome (HTTP.responseBody resp) (maxRpcResponseBytes + 1)
    requestBody = J.encode $ J.object ["jsonrpc" .= ("2.0" :: Text), "id" .= (1 :: Int), "method" .= method, "params" .= params]
    response = J.withObject "JSON-RPC response" $ \o -> o .:? "error" >>= maybe (Right <$> (parseResult =<< o .: "result")) (fmap Left . rpcError)
    rpcError = J.withObject "JSON-RPC error" $ \e -> do
      code <- e .: "code"
      message <- e .: "message"
      revertData <- e .:? "data"
      pure $
        if code == revertCode || revertMessage `T.isPrefixOf` message
          then ChainReverted $ maybe (dataless message) (decodeRevert <=< JT.parseMaybe bytes) revertData
          else unavailable $ "error " <> tshow code <> ": " <> T.take maxNodeMessageLength message
    unavailable reason = ChainUnavailable $ method <> ": " <> reason
    -- a node or proxy that drops the data can keep a reason in the message, as in "execution reverted: ERC721: invalid token ID"
    dataless message
      | message == revertMessage = Just RNWithoutData
      | otherwise = Nothing

-- | Only the kind of failure, for callers: an exception's text can hold the URL or its host.
httpFailure :: HTTP.HttpException -> Text
httpFailure = \case
  HTTP.InvalidUrlException {} -> "invalid RPC URL"
  HTTP.HttpExceptionRequest _ content -> case content of
    HTTP.ResponseTimeout -> "timed out"
    HTTP.ConnectionTimeout -> "connection timed out"
    HTTP.ConnectionFailure _ -> "connection failed"
    _ -> "request failed"

-- | The failure's detail for the service log, with the RPC URL and its host, path and query replaced, since they can carry a provider's key.
httpFailureDetail :: String -> HTTP.HttpException -> Text
httpFailureDetail rpcUrl e = foldl' (\t secret -> T.replace secret "<redacted>" t) detail secrets
  where
    detail = case e of
      HTTP.InvalidUrlException _ reason -> "invalid URL: " <> T.pack reason
      HTTP.HttpExceptionRequest _ content -> tshow content
    secrets = filter ((> 1) . T.length) $ T.pack rpcUrl : maybe [] urlParts (HTTP.parseRequest rpcUrl)
    urlParts req = map decodeLatin1 [HTTP.host req, HTTP.path req, HTTP.queryString req]

nullable :: (J.Value -> JT.Parser a) -> J.Value -> JT.Parser (Maybe a)
nullable p = \case
  J.Null -> pure Nothing
  v -> Just <$> p v

quantity :: forall a. (Integral a, Bounded a) => J.Value -> JT.Parser a
quantity = J.withText "quantity" $ \t -> case T.stripPrefix "0x" t of
  Just h | T.length h <= maxQuantityDigits, [(n, "")] <- readHex (T.unpack h), n <= toInteger (maxBound :: a) -> pure $ fromInteger n
  _ -> fail "not a quantity"

bytes :: J.Value -> JT.Parser ByteString
bytes = J.withText "data" $ \t -> case T.stripPrefix "0x" t of
  Just h | Right bs <- convertFromBase Base16 (encodeUtf8 h) -> pure bs
  _ -> fail "not hex data"

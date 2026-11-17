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
    Block (..),
    ChainSeconds,
    chainTime,
    chainSeconds,
    Receipt (..),
    BlockTag (..),
    newChainClient,
    ethCallAt,
    latestBlock,
    nodeChainId,
    blockAt,
    confirmedNumber,
    transactionReceipt,
    transactionKnown,
    transactionCount,
    registrarBalance,
    estimateGas,
    recentTip,
    signTransaction,
    decodeSignedTx,
    transactionSigner,
    sendRawTransaction,
    httpFailure,
    httpFailureDetail,
  )
where

import Control.Concurrent.STM (TVar)
import Control.Exception (evaluate, try)
import Control.Logger.Simple (logWarn)
import Control.Monad ((>=>))
import Crypto.Random (ChaChaDRG)
import Data.Aeson ((.:), (.:?), (.=))
import qualified Data.Aeson as J
import qualified Data.Aeson.Types as JT
import qualified Data.Attoparsec.ByteString.Char8 as A
import Data.ByteArray.Encoding (Base (Base16), convertFromBase)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as LB
import Data.Char (digitToInt, isHexDigit)
import Data.List (foldl', sort)
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeLatin1)
import Data.Time.Clock (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Word (Word64)
import Network.HTTP.Client (Manager)
import qualified Network.HTTP.Client as HTTP
import Network.HTTP.Client.TLS (newTlsManager)
import Network.HTTP.Types (hContentType, methodPost, statusCode, statusIsSuccessful)
import Numeric (showHex)
import Simplex.Messaging.Agent.Store.DB (Binary (..), FromField (..), ToField (..), blobFieldDecoder)
import Simplex.Messaging.Crypto (keccak256)
import Simplex.Messaging.Eth.Address (Address, addressFromPrivateKey, recoverAddress)
import Simplex.Messaging.Eth.Transaction (Eip1559Tx (..), decodeEip1559Tx, eip1559SigningHash, signEip1559Tx)
import Simplex.Messaging.Util (tshow)
import StoreService.Config (ChainConfig (..))
import StoreService.Names.Abi (ChainSeconds, hex, parseText)
import StoreService.Names.Revert (RevertReason (..), decodeRevert, maxNodeMessageLength)
import StoreService.Store.Invoices (unixSeconds)
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
  = ChainReverted RevertReason
  | ChainUnavailable Text
  | ChainUnderfunded Text
  | ChainGasAboveCap Word64
  deriving (Eq, Show)

newtype TxHash = TxHash ByteString
  deriving (Eq, Show)

instance FromField TxHash where fromField = blobFieldDecoder (Right . TxHash)

instance ToField TxHash where toField (TxHash h) = toField (Binary h)

data SignedTx = SignedTx {stRaw :: ByteString, stHash :: TxHash}
  deriving (Eq, Show)

data Block = Block {blockNumber :: Word64, blockTimestamp :: ChainSeconds, blockBaseFee :: Word64}
  deriving (Eq, Show)

data Receipt = Receipt {receiptSuccess :: Bool, receiptBlock :: Word64}
  deriving (Eq, Show)

data BlockTag = Latest | Pending | AtBlock Word64

rpcTimeout :: Int
rpcTimeout = 10000000

-- | Far above the largest answer the service reads: a block with its transaction hashes, or two records at the link bound.
maxRpcResponseBytes :: Int
maxRpcResponseBytes = 1024 * 1024

revertCode :: Int
revertCode = 3

revertMessage :: Text
revertMessage = "execution reverted"

-- | How geth, reth and erigon refuse a transaction whose key cannot pay gas × fee + value, in lower case; anvil capitalizes it.
underfundedMessage :: Text
underfundedMessage = "insufficient funds"

-- | The hex digits of a 256-bit quantity.
maxQuantityDigits :: Int
maxQuantityDigits = 64

chainTime :: ChainSeconds -> UTCTime
chainTime = posixSecondsToUTCTime . fromIntegral

chainSeconds :: UTCTime -> ChainSeconds
chainSeconds = fromInteger . unixSeconds

newChainClient :: TVar ChaChaDRG -> ChainConfig -> IO ChainClient
newChainClient g ccConfig@ChainConfig {cServiceKey} = do
  ccManager <- newTlsManager
  ccRegistrar <- addressFromPrivateKey g cServiceKey
  pure ChainClient {ccManager, ccConfig, ccRegistrar, ccTimeout = rpcTimeout}

ethCallAt :: ChainClient -> BlockTag -> Address -> ByteString -> IO (Either ChainError ByteString)
ethCallAt c tag to calldata = rpc c "eth_call" [J.object ["to" .= to, "data" .= hex calldata], blockTagParam tag] bytes

nodeChainId :: ChainClient -> IO (Either ChainError Word64)
nodeChainId c = rpc c "eth_chainId" [] quantity

latestBlock :: ChainClient -> IO (Either ChainError Block)
latestBlock c = blockByNumber c Latest

-- | A block the node does not have is a failed read.
blockAt :: ChainClient -> Word64 -> IO (Either ChainError Block)
blockAt c = blockByNumber c . AtBlock

-- | The newest block at the configured confirmation depth below this latest one, as the sender counts it.
confirmedNumber :: ChainClient -> Word64 -> Word64
confirmedNumber ChainClient {ccConfig = ChainConfig {cConfirmations}} latest = latest - min latest (fromIntegral (max 1 cConfirmations) - 1)

blockByNumber :: ChainClient -> BlockTag -> IO (Either ChainError Block)
blockByNumber c tag =
  rpc c "eth_getBlockByNumber" [blockTagParam tag, J.Bool False] $ J.withObject "block" $ \b ->
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

-- | Whether the node knows the transaction, pending or mined.
transactionKnown :: ChainClient -> TxHash -> IO (Either ChainError Bool)
transactionKnown c (TxHash h) =
  rpc c "eth_getTransactionByHash" [J.String $ hex h] $ fmap isJust . nullable (J.withObject "transaction" $ \_ -> pure ())

transactionCount :: ChainClient -> Address -> BlockTag -> IO (Either ChainError Word64)
transactionCount c a tag = rpc c "eth_getTransactionCount" [J.toJSON a, blockTagParam tag] quantity

registrarBalance :: ChainClient -> IO (Either ChainError Integer)
registrarBalance c@ChainClient {ccRegistrar} = rpc c "eth_getBalance" [J.toJSON ccRegistrar, blockTagParam Latest] natural

blockTagParam :: BlockTag -> J.Value
blockTagParam = \case
  Latest -> "latest"
  Pending -> "pending"
  AtBlock number -> J.String $ "0x" <> T.pack (showHex number "")

-- | The dry run of a registrar transaction; an estimate above the configured cap is refused.
estimateGas :: ChainClient -> Address -> ByteString -> IO (Either ChainError Word64)
estimateGas c@ChainClient {ccRegistrar, ccConfig = ChainConfig {cGasLimitCap}} to calldata =
  (>>= capped) <$> rpc c "eth_estimateGas" [J.object ["from" .= ccRegistrar, "to" .= to, "data" .= hex calldata], blockTagParam Pending] quantity
  where
    capped gas
      | gas > cGasLimitCap = Left $ ChainGasAboveCap gas
      | otherwise = Right gas

-- | The median of the last blocks' median tips: eth_maxPriorityFeePerGas samples each block's cheapest transactions,
-- which reth answers near zero.
recentTip :: ChainClient -> IO (Either ChainError Word64)
recentTip c = rpc c "eth_feeHistory" [J.String tipBlocks, blockTagParam Latest, J.toJSON [50 :: Int]] medianTip
  where
    tipBlocks = "0xa"
    medianTip = J.withObject "fee history" $ \o -> o .: "reward" >>= mapM blockTip >>= median . sort
    blockTip = \case
      [tip] -> quantity tip
      _ -> fail "not one reward a block"
    median = \case
      [] -> fail "no rewards"
      tips -> pure $ tips !! (length tips `div` 2)

signTransaction :: TVar ChaChaDRG -> ChainClient -> Eip1559Tx -> IO (Either String SignedTx)
signTransaction g ChainClient {ccConfig = ChainConfig {cServiceKey}} tx = fmap signed <$> signEip1559Tx g cServiceKey tx
  where
    signed raw = SignedTx {stRaw = raw, stHash = TxHash $ keccak256 raw}

decodeSignedTx :: ByteString -> Either String Eip1559Tx
decodeSignedTx = fmap fst . decodeEip1559Tx

transactionSigner :: ByteString -> IO (Either String (Eip1559Tx, Address))
transactionSigner raw = case decodeEip1559Tx raw of
  Right (tx, signature) -> fmap (tx,) <$> recoverAddress signature (eip1559SigningHash tx)
  Left e -> pure $ Left e

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
          then ChainReverted $ maybe (dataless message) (maybe (unnamed message) decodeRevert . JT.parseMaybe bytes) revertData
          else nodeError message $ method <> ": error " <> tshow code <> ": " <> T.take maxNodeMessageLength message
    nodeError message
      | underfundedMessage `T.isInfixOf` T.toLower message = ChainUnderfunded
      | otherwise = ChainUnavailable
    unavailable reason = ChainUnavailable $ method <> ": " <> reason
    -- a node or proxy that drops the data can keep a reason in the message, as in "execution reverted: ERC721: invalid token ID"
    dataless message
      | message == revertMessage = RRWithoutData
      | otherwise = unnamed message
    unnamed message = RRUnnamed $ T.take maxNodeMessageLength $ fromMaybe message $ T.stripPrefix (revertMessage <> ": ") message

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
quantity =
  natural >=> \n ->
    if n <= toInteger (maxBound :: a) then pure $ fromInteger n else fail "not a quantity"

natural :: J.Value -> JT.Parser Integer
natural = nodeHex "quantity" "not a quantity" $ A.takeWhile1 isHexDigit >>= \h ->
  if B.length h <= maxQuantityDigits then pure $ BC.foldl' (\n d -> 16 * n + toInteger (digitToInt d)) 0 h else fail "over 256 bits"

bytes :: J.Value -> JT.Parser ByteString
bytes = nodeHex "data" "not hex data" $ A.takeWhile isHexDigit >>= either fail pure . convertFromBase Base16

nodeHex :: String -> String -> A.Parser a -> J.Value -> JT.Parser a
nodeHex name failure p = J.withText name $ either (const $ fail failure) pure . parseText (A.string "0x" *> p)

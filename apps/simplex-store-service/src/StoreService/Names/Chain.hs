{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module StoreService.Names.Chain
  ( ChainConfig (..),
    ChainClient (..),
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
    AbiValue (..),
    AbiError (..),
    abiWordSize,
    abiTuple,
    abiBytes,
    abiArray,
    uint256,
    addressWord,
    selector,
    hex,
    hexP,
    lowerHexP,
    parseText,
    bytes32FromHex,
    decodeUint256,
    decodeWord64,
    decodeAddress,
    decodeBool,
    decodeBytes32,
    decodeBytes,
    decodeString,
    decodeLabels,
    RevertName (..),
    RevertReason (..),
    maxNodeMessageLength,
    decodeRevert,
    revertText,
    revertReasonText,
  )
where

import Control.Concurrent.STM (TVar)
import Control.Exception (evaluate, try)
import Control.Logger.Simple (logWarn)
import Control.Monad (unless, (>=>))
import Crypto.Number.Serialize (i2ospOf_, os2ip)
import Crypto.Random (ChaChaDRG)
import Data.Aeson ((.:), (.:?), (.=))
import qualified Data.Aeson as J
import qualified Data.Aeson.Types as JT
import qualified Data.Attoparsec.ByteString.Char8 as A
import Data.Bifunctor (first)
import Data.ByteArray.Encoding (Base (Base16), convertFromBase, convertToBase)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as LB
import Data.Char (digitToInt, isDigit, isHexDigit)
import Data.List (foldl', mapAccumL, sort)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeLatin1, decodeUtf8', encodeUtf8)
import Data.Time.Clock (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import Data.Word (Word64)
import Network.HTTP.Client (Manager)
import qualified Network.HTTP.Client as HTTP
import Network.HTTP.Client.TLS (newTlsManager)
import Network.HTTP.Types (hContentType, methodPost, statusCode, statusIsSuccessful)
import Numeric (showHex)
import Simplex.Messaging.Agent.Store.DB (Binary (..), FromField (..), ToField (..), blobFieldDecoder)
import Simplex.Messaging.Crypto (keccak256)
import Simplex.Messaging.Crypto.Secp256k1 (Secp256k1PrivateKey)
import Simplex.Messaging.Eth.Address (Address, addressFromBytes, addressFromPrivateKey, recoverAddress, unAddress)
import Simplex.Messaging.Eth.Transaction (Eip1559Tx (..), decodeEip1559Tx, eip1559SigningHash, signEip1559Tx)
import Simplex.Messaging.Parsers (parseAll)
import Simplex.Messaging.Util (eitherToMaybe, tshow)
import System.Timeout (timeout)

data ChainConfig = ChainConfig
  { cRpcUrl :: String,
    cChainId :: Word64,
    cController :: Address,
    cSponsor :: Address,
    cBaseRegistrar :: Address,
    cResolver :: Address,
    cServiceKey :: Secp256k1PrivateKey,
    cConfirmations :: Int,
    cGasLimitCap :: Word64,
    -- | wei; no transaction is signed with a higher max fee per gas
    cMaxFeePerGas :: Word64
  }
  deriving (Eq)

-- The RPC URL can carry a provider's API key, so neither it nor the service key is shown.
instance Show ChainConfig where
  show ChainConfig {cChainId} = "chain " <> show cChainId

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

type ChainSeconds = Word64

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
chainSeconds = floor . utcTimeToPOSIXSeconds

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

data AbiValue = Static ByteString | Dynamic ByteString

data AbiError
  = AEShort
  | AEOffset
  | AENotAddress
  | AENotBool
  | AENotUtf8
  | AERange
  deriving (Eq, Show)

abiWordSize :: Int
abiWordSize = 32

addressPadding :: Int
addressPadding = 12

abiTuple :: [AbiValue] -> ByteString
abiTuple values = B.concat heads <> B.concat [tl | Dynamic tl <- values]
  where
    heads = snd $ mapAccumL encodeHead (abiWordSize * length values) values
    encodeHead offset = \case
      Static w -> (offset, w)
      Dynamic tl -> (offset + B.length tl, uint256 offset)

abiBytes :: ByteString -> ByteString
abiBytes bs = uint256 (B.length bs) <> bs <> B.replicate (negate (B.length bs) `mod` abiWordSize) 0

abiArray :: [ByteString] -> ByteString
abiArray items = uint256 (length items) <> abiTuple (map Dynamic items)

uint256 :: Integral a => a -> ByteString
uint256 = i2ospOf_ abiWordSize . toInteger

addressWord :: Address -> ByteString
addressWord a = B.replicate addressPadding 0 <> unAddress a

selector :: ByteString -> ByteString
selector = B.take 4 . keccak256

hex :: ByteString -> Text
hex bs = "0x" <> decodeLatin1 (convertToBase Base16 bs)

hexP :: Int -> A.Parser ByteString
hexP n = A.string "0x" *> lowerHexP n

lowerHexP :: Int -> A.Parser ByteString
lowerHexP n = A.count (2 * n) (A.satisfy lowerHexDigit) >>= either fail pure . convertFromBase Base16 . BC.pack
  where
    lowerHexDigit c = isDigit c || (c >= 'a' && c <= 'f')

parseText :: A.Parser a -> Text -> Either String a
parseText p = parseAll p . encodeUtf8

bytes32FromHex :: Text -> Maybe ByteString
bytes32FromHex = eitherToMaybe . parseText (hexP abiWordSize)

decodeUint256 :: ByteString -> Either AbiError Integer
decodeUint256 = uintAt 0

decodeWord64 :: ByteString -> Either AbiError Word64
decodeWord64 ret = do
  n <- decodeUint256 ret
  if n <= toInteger (maxBound :: Word64) then Right (fromInteger n) else Left AERange

decodeAddress :: ByteString -> Either AbiError Address
decodeAddress ret = do
  (padding, a) <- B.splitAt addressPadding <$> wordAt 0 ret
  unless (B.all (== 0) padding) $ Left AENotAddress
  first (const AENotAddress) $ addressFromBytes a

decodeBool :: ByteString -> Either AbiError Bool
decodeBool ret =
  uintAt 0 ret >>= \case
    0 -> Right False
    1 -> Right True
    _ -> Left AENotBool

decodeBytes32 :: ByteString -> Either AbiError ByteString
decodeBytes32 = wordAt 0

decodeBytes :: ByteString -> Either AbiError ByteString
decodeBytes ret = offsetAt 0 ret >>= bytesAt ret

decodeString :: ByteString -> Either AbiError Text
decodeString ret = decodeBytes ret >>= first (const AENotUtf8) . decodeUtf8'

-- | A return of a bytes32[] and a string[] of the same length, paired.
decodeLabels :: ByteString -> Either AbiError [(ByteString, Text)]
decodeLabels ret = do
  (hashCount, hashesStart) <- offsetAt 0 ret >>= arrayAt
  (labelCount, labelsStart) <- offsetAt abiWordSize ret >>= arrayAt
  unless (hashCount == labelCount) $ Left AEOffset
  hashes <- mapM (\i -> wordAt (hashesStart + i * abiWordSize) ret) [0 .. hashCount - 1]
  labels <- mapM (\i -> offsetAt (labelsStart + i * abiWordSize) ret >>= bytesAt ret . (labelsStart +) >>= first (const AENotUtf8) . decodeUtf8') [0 .. labelCount - 1]
  pure $ zip hashes labels
  where
    arrayAt o = (,o + abiWordSize) <$> offsetAt o ret

wordAt :: Int -> ByteString -> Either AbiError ByteString
wordAt o ret
  | o + abiWordSize <= B.length ret = Right $ B.take abiWordSize $ B.drop o ret
  | otherwise = Left AEShort

uintAt :: Int -> ByteString -> Either AbiError Integer
uintAt o ret = os2ip <$> wordAt o ret

-- an offset or a length, never more than the data's size, so it fits an Int
offsetAt :: Int -> ByteString -> Either AbiError Int
offsetAt o ret = do
  n <- uintAt o ret
  if n <= toInteger (B.length ret) then Right (fromInteger n) else Left AEOffset

bytesAt :: ByteString -> Int -> Either AbiError ByteString
bytesAt ret o = do
  n <- offsetAt o ret
  let start = o + abiWordSize
  if start + n <= B.length ret then Right $ B.take n $ B.drop start ret else Left AEShort

-- | The custom errors of SimplexController, SimplexSponsor, BaseRegistrarImplementation and SimplexResolver, and the reservation and terms errors of SimplexNamePolicy, that the service tells apart.
-- The sponsor passes the controller's and the base registrar's reverts through, so any call can revert with any of them.
data RevertName
  = RNAlreadyFrozen
  | RNCommitmentNotFound
  | RNCommitmentTooNew
  | RNCommitmentTooOld
  | RNDurationTooShort
  | RNFrozen
  | RNInsufficientAllowance
  | RNInsufficientValue
  | RNInvalidEVMAddress
  | RNInvalidNonce
  | RNInvalidSignature
  | RNLabelTooLong
  | RNMaxCommitmentAgeTooHigh
  | RNMaxCommitmentAgeTooLow
  | RNMinCharLengthCanOnlyDecrease
  | RNMinCharLengthZero
  | RNMinCommitmentAgeZero
  | RNNameNotAvailable
  | RNNameNotReserved
  | RNNameReserved
  | RNNameTooShort
  | RNNftGateCanOnlyBeDisabled
  | RNNftRequired
  | RNNodeNotOwnedByRegistrar
  | RNNotBeneficiary
  | RNNotNameOwner
  | RNNotOffered
  | RNNotOwnerOrBeneficiary
  | RNNotSponsor
  | RNNotSubnameRegistrar
  | RNOffsetOutOfBoundsError
  | RNOwnerMustBeCaller
  | RNPublicSalesClosed
  | RNReasonRequired
  | RNRecipientMustBeCaller
  | RNReentrantCall
  | RNResolverRequiredWhenDataSupplied
  | RNReverseRecordNotSupported
  | RNSignatureExpired
  | RNTermsMismatch
  | RNTermsNotAccepted
  | RNTransferFailed
  | RNTransferToSelf
  | RNUnexpiredCommitmentExists
  | RNZeroAddress
  deriving (Eq, Show, Enum, Bounded)

data RevertReason = RRWithoutData | RRNamed RevertName | RRUnnamed Text
  deriving (Eq, Show)

revertSignature :: RevertName -> ByteString
revertSignature = \case
  RNAlreadyFrozen -> "AlreadyFrozen()"
  RNCommitmentNotFound -> "CommitmentNotFound(bytes32)"
  RNCommitmentTooNew -> "CommitmentTooNew(bytes32,uint256,uint256)"
  RNCommitmentTooOld -> "CommitmentTooOld(bytes32,uint256,uint256)"
  RNDurationTooShort -> "DurationTooShort(uint256)"
  RNFrozen -> "Frozen()"
  RNInsufficientAllowance -> "InsufficientAllowance(uint256,uint256)"
  RNInsufficientValue -> "InsufficientValue()"
  RNInvalidEVMAddress -> "InvalidEVMAddress(bytes)"
  RNInvalidNonce -> "InvalidNonce()"
  RNInvalidSignature -> "InvalidSignature()"
  RNLabelTooLong -> "LabelTooLong(uint256,uint256)"
  RNMaxCommitmentAgeTooHigh -> "MaxCommitmentAgeTooHigh()"
  RNMaxCommitmentAgeTooLow -> "MaxCommitmentAgeTooLow()"
  RNMinCharLengthCanOnlyDecrease -> "MinCharLengthCanOnlyDecrease()"
  RNMinCharLengthZero -> "MinCharLengthZero()"
  RNMinCommitmentAgeZero -> "MinCommitmentAgeZero()"
  RNNameNotAvailable -> "NameNotAvailable(string)"
  RNNameNotReserved -> "NameNotReserved(string)"
  RNNameReserved -> "NameReserved(string)"
  RNNameTooShort -> "NameTooShort(string,uint8)"
  RNNftGateCanOnlyBeDisabled -> "NftGateCanOnlyBeDisabled()"
  RNNftRequired -> "NftRequired()"
  RNNodeNotOwnedByRegistrar -> "NodeNotOwnedByRegistrar()"
  RNNotBeneficiary -> "NotBeneficiary()"
  RNNotNameOwner -> "NotNameOwner()"
  RNNotOffered -> "NotOffered()"
  RNNotOwnerOrBeneficiary -> "NotOwnerOrBeneficiary()"
  RNNotSponsor -> "NotSponsor()"
  RNNotSubnameRegistrar -> "NotSubnameRegistrar()"
  RNOffsetOutOfBoundsError -> "OffsetOutOfBoundsError(uint256,uint256)"
  RNOwnerMustBeCaller -> "OwnerMustBeCaller()"
  RNPublicSalesClosed -> "PublicSalesClosed()"
  RNReasonRequired -> "ReasonRequired()"
  RNRecipientMustBeCaller -> "RecipientMustBeCaller()"
  RNReentrantCall -> "ReentrantCall()"
  RNResolverRequiredWhenDataSupplied -> "ResolverRequiredWhenDataSupplied()"
  RNReverseRecordNotSupported -> "ReverseRecordNotSupported()"
  RNSignatureExpired -> "SignatureExpired()"
  RNTermsMismatch -> "TermsMismatch(bytes32,bytes32)"
  RNTermsNotAccepted -> "TermsNotAccepted(string)"
  RNTransferFailed -> "TransferFailed()"
  RNTransferToSelf -> "TransferToSelf()"
  RNUnexpiredCommitmentExists -> "UnexpiredCommitmentExists(bytes32)"
  RNZeroAddress -> "ZeroAddress()"

-- | The error's name as the contracts declare it.
revertText :: RevertName -> Text
revertText = decodeLatin1 . BC.takeWhile (/= '(') . revertSignature

revertReasonText :: RevertReason -> Maybe Text
revertReasonText = \case
  RRWithoutData -> Nothing
  RRNamed n -> Just $ revertText n
  RRUnnamed reason -> Just reason

revertSelectors :: Map ByteString RevertName
revertSelectors = M.fromList [(selector $ revertSignature n, n) | n <- [minBound .. maxBound]]

-- | A node's error message is cut to this many characters.
maxNodeMessageLength :: Int
maxNodeMessageLength = 200

decodeRevert :: ByteString -> RevertReason
decodeRevert revertData
  | B.null revertData = RRWithoutData
  | Just n <- M.lookup revertSelector revertSelectors = RRNamed n
  | revertSelector == selector "Error(string)", Right message <- decodeString arguments = RRUnnamed $ T.take maxNodeMessageLength message
  | otherwise = RRUnnamed $ hex revertSelector
  where
    (revertSelector, arguments) = B.splitAt selectorSize revertData
    selectorSize = 4

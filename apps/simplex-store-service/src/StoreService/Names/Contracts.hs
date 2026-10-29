{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module StoreService.Names.Contracts
  ( Contracts (ctDeadline, ctTermsBackoff),
    NameStatus (..),
    RegisteredName (..),
    NameRecords (..),
    CommitmentAges (..),
    Terms (..),
    TermsRefusal (..),
    newContracts,
    secondsPerYear,
    nameStatus,
    nameRecords,
    textRecord,
    chainPriceCents,
    sponsorNonce,
    resolverNonce,
    namePolicy,
    bonaFideName,
    termsLinks,
    termsOf,
    termsText,
    policyNonce,
    acceptedTerms,
    confirmedTag,
    ownerOfToken,
    nameExpires,
    commitmentTime,
    controllerAvailable,
    commitmentAges,
    together,
    concurrent,
  )
where

import Control.Applicative ((<|>))
import Control.Concurrent.Async (Concurrently (..))
import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import Control.Monad (guard)
import Control.Monad.Except (ExceptT (..), runExceptT)
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import Data.Functor ((<&>))
import Data.Functor.Compose (Compose (..))
import Data.IORef (IORef, atomicWriteIORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeLatin1, decodeUtf8', encodeUtf8)
import Data.Time.Clock (UTCTime, addUTCTime, diffUTCTime, getCurrentTime)
import Data.Word (Word64)
import Simplex.Messaging.Crypto (keccak256, sha256Hash)
import Simplex.Messaging.Eth.Address (Address, unAddress)
import Simplex.Messaging.SimplexName (LabelHash (..), SimplexTLD)
import Simplex.Messaging.Util (tshow)
import StoreService.Config (ChainConfig (..), localChainId)
import StoreService.Log (logWarn)
import StoreService.Names.Abi
import StoreService.Names.Chain (Block (..), BlockTag (..), ChainClient (..), ChainError (..), confirmedNumber, ethCallAt, fetchText, latestBlock, nodeChainId)
import StoreService.Names.Registration (channelRecordKey, contactRecordKey, labelNode, nodeBytes)
import StoreService.Names.Revert (RevertName (..))
import System.Timeout (timeout)
import Text.Read (readMaybe)

data Contracts = Contracts
  { ctChain :: ChainClient,
    ctTLD :: SimplexTLD,
    ctRegistrar :: IORef (Maybe RegistrarConstants),
    ctCommitmentAges :: IORef (Maybe CommitmentAges),
    ctNamePolicy :: IORef (Maybe Address),
    ctTermsTexts :: IORef (Map ByteString Text),
    ctFetchAfter :: IORef UTCTime,
    ctFetching :: MVar (),
    -- | microseconds for all the reads of one name status or of one name's records
    ctDeadline :: Int,
    ctTermsBackoff :: Int
  }

data RegistrarConstants = RegistrarConstants {rcRegistry :: Address, rcGracePeriod :: Integer}

data NameStatus = NSAvailable | NSReserved | NSRegistered RegisteredName
  deriving (Eq, Show)

data RegisteredName = RegisteredName
  { rnOwner :: Address,
    rnResolver :: Address,
    rnExpiresAt :: Word64,
    rnGracePeriod :: Integer
  }
  deriving (Eq, Show)

-- | The default resolver's records ("" when unset) and the owner's nonce for the next edit.
data NameRecords = NameRecords {rdContact :: Text, rdChannel :: Text, rdNonce :: Word64}
  deriving (Eq, Show)

data CommitmentAges = CommitmentAges {caMin :: Integer, caMax :: Integer}
  deriving (Eq, Show)

data Terms = Terms {tmLink :: Text, tmHash :: ByteString}
  deriving (Eq, Show)

data TermsRefusal = TRLink | TRWait Int
  deriving (Eq, Show)

secondsPerYear :: Integer
secondsPerYear = 31536000

attoUsdPerCent :: Integer
attoUsdPerCent = 10 ^ (16 :: Int)

-- | One call's timeout, so a node that hangs fails a status or a records read once, not once per read.
nameReadsDeadline :: Int
nameReadsDeadline = 10000000

termsBackoffSeconds :: Int
termsBackoffSeconds = 30

maxTermsBytes :: Int
maxTermsBytes = 256 * 1024

-- | Reads nothing: the registrar's constants, the commitment ages and the name policy are read on first use and kept once read.
newContracts :: SimplexTLD -> ChainClient -> IO Contracts
newContracts ctTLD ctChain = do
  ctRegistrar <- newIORef Nothing
  ctCommitmentAges <- newIORef Nothing
  ctNamePolicy <- newIORef Nothing
  ctTermsTexts <- newIORef M.empty
  ctFetchAfter <- newIORef =<< getCurrentTime
  ctFetching <- newMVar ()
  pure Contracts {ctChain, ctTLD, ctRegistrar, ctCommitmentAges, ctNamePolicy, ctTermsTexts, ctFetchAfter, ctFetching, ctDeadline = nameReadsDeadline, ctTermsBackoff = termsBackoffSeconds}

-- | Registered while @nameExpires + GRACE_PERIOD@ is not before the latest block's time.
nameStatus :: Contracts -> LabelHash -> IO (Either ChainError NameStatus)
nameStatus ct@Contracts {ctChain} lh = withinDeadline ct "name status" $ runExceptT $ do
  (RegistrarConstants {rcRegistry, rcGracePeriod}, expires, Block {blockTimestamp}) <-
    ExceptT $ together $ (,,) <$> concurrent (registrarConstants ct) <*> concurrent (nameExpires ct Latest lh) <*> concurrent (latestBlock ctChain)
  if toInteger expires + rcGracePeriod >= toInteger blockTimestamp
    then do
      let node = nodeArg ct lh
          registryRead signature = concurrent $ call ct rcRegistry (signature, node) decodeAddress
      (rnOwner, rnResolver) <- ExceptT $ together $ (,) <$> registryRead "owner(bytes32)" <*> registryRead "resolver(bytes32)"
      pure $ NSRegistered RegisteredName {rnOwner, rnResolver, rnExpiresAt = expires, rnGracePeriod = rcGracePeriod}
    else do
      reason <- ExceptT $ policyCall ct ("reservedNames(bytes32)", labelHashArg lh) decodeUint256
      pure $ if reason /= 0 then NSReserved else NSAvailable

-- | Nothing for a name on another resolver contract, or none: the service reads no records from a contract it does not know.
nameRecords :: Contracts -> LabelHash -> RegisteredName -> IO (Either ChainError (Maybe NameRecords))
nameRecords ct lh RegisteredName {rnOwner, rnResolver}
  | rnResolver /= resolver ct = pure $ Right Nothing
  | otherwise =
      withinDeadline ct "name records" $ together $
        (\rdContact rdChannel rdNonce -> Just NameRecords {rdContact, rdChannel, rdNonce})
          <$> concurrent (textRecord ct lh contactRecordKey)
          <*> concurrent (textRecord ct lh channelRecordKey)
          <*> concurrent (resolverNonce ct rnOwner)

-- | The default resolver's record, "" when unset.
textRecord :: Contracts -> LabelHash -> ByteString -> IO (Either ChainError Text)
textRecord ct lh key = call ct (resolver ct) ("text(bytes32,string)", abiTuple [Static $ nodeArg ct lh, Dynamic $ abiBytes key]) decodeString

-- | Cents for a label of this length and these years. The oracle prices by length only,
-- so a placeholder label of that length keeps the label from the RPC node.
chainPriceCents :: Contracts -> Int -> Int -> IO (Either ChainError Int)
chainPriceCents ct len years = call ct (controller ct) ("rentPriceUSD(string,uint256)", args) $ \ret -> do
  base <- decodeUint256 ret
  let cents = base `div` attoUsdPerCent
  if cents <= toInteger (maxBound :: Int) then Right (fromInteger cents) else Left AERange
  where
    args = abiTuple [Dynamic $ abiBytes $ B.replicate len 'a', Static $ uint256 $ toInteger years * secondsPerYear]

sponsorNonce :: Contracts -> Address -> IO (Either ChainError Word64)
sponsorNonce ct owner = call ct (cSponsor $ chainConfig ct) ("nonces(address)", addressWord owner) decodeWord64

resolverNonce :: Contracts -> Address -> IO (Either ChainError Word64)
resolverNonce ct owner = call ct (resolver ct) ("nonces(address)", addressWord owner) decodeWord64

-- | The policy holding the reserved and the community names, read once: as the service starts, or on first use after that
-- read failed or found none set.
namePolicy :: Contracts -> IO (Either ChainError Address)
namePolicy ct@Contracts {ctNamePolicy} = readOnce ctNamePolicy $ (>>= nonZero) <$> call ct (controller ct) ("namePolicy()", "") decodeAddress
  where
    nonZero policy = if B.all (== '\NUL') (unAddress policy) then Left (ChainUnavailable "the controller has no name policy yet") else Right policy

bonaFideName :: Contracts -> LabelHash -> IO (Either ChainError Bool)
bonaFideName ct lh = policyCall ct ("bonaFideNames(bytes32)", labelHashArg lh) decodeBool

termsLinks :: Contracts -> IO (Either ChainError (Text, Text))
termsLinks ct@Contracts {ctChain} = runExceptT $ do
  policy <- ExceptT $ namePolicy ct
  Block {blockNumber} <- ExceptT $ latestBlock ctChain
  let link signature = concurrent $ callAt ct (AtBlock blockNumber) policy (signature, "") decodeString
  ExceptT $ together $ (,) <$> link "nameTermsLink()" <*> link "communityNameTermsLink()"

termsOf :: Text -> Maybe Terms
termsOf link
  | T.null link = Nothing
  | otherwise = Just Terms {tmLink = link, tmHash = keccak256 $ encodeUtf8 link}

termsText :: Contracts -> Terms -> IO (Either TermsRefusal Text)
termsText ct@Contracts {ctTermsTexts, ctFetching} t@Terms {tmHash} =
  kept >>= \case
    Just text -> pure $ Right text
    Nothing -> withMVar ctFetching $ \_ -> kept >>= maybe (fetchTerms ct t) (pure . Right)
  where
    kept = M.lookup tmHash <$> readIORef ctTermsTexts

fetchTerms :: Contracts -> Terms -> IO (Either TermsRefusal Text)
fetchTerms Contracts {ctChain, ctTermsTexts, ctFetchAfter, ctTermsBackoff} Terms {tmLink, tmHash} =
  allowedLink ctChain tmLink >>= \case
    Nothing -> pure $ Left TRLink
    Just (url, digest) -> do
      after <- readIORef ctFetchAfter
      now <- getCurrentTime
      if now < after
        then pure $ Left $ TRWait $ ceiling $ diffUTCTime after now
        else
          (>>= checked digest) <$> fetchText ctChain maxTermsBytes url >>= \case
            Right text -> Right text <$ modifyIORef' ctTermsTexts (M.insert tmHash text)
            Left (reason, retryAfter) -> do
              let wait = max ctTermsBackoff $ fromMaybe 0 retryAfter
              failedAt <- getCurrentTime
              writeIORef ctFetchAfter $ addUTCTime (fromIntegral wait) failedAt
              logWarn $ "names: the terms text of " <> tmLink <> " could not be fetched, " <> reason <> "; none is fetched for " <> tshow wait <> " seconds"
              pure $ Left $ TRWait wait
  where
    checked digest body
      | sha256Hash body /= digest = Left ("its SHA-256 is not the link's", Nothing)
      | otherwise = first (const ("not UTF-8", Nothing)) $ decodeUtf8' body

allowedLink :: ChainClient -> Text -> IO (Maybe (String, ByteString))
allowedLink c@ChainClient {ccConfig = ChainConfig {cChainId}} link = case termsLinkParts link of
  Just (False, parts) -> pure $ Just parts
  Just (True, parts)
    | cChainId == localChainId ->
        nodeChainId c <&> \case
          Right chainId | chainId == localChainId -> Just parts
          _ -> Nothing
  _ -> pure Nothing

termsLinkParts :: Text -> Maybe (Bool, (String, ByteString))
termsLinkParts link = do
  digest <- bytes32FromHex . ("0x" <>) =<< T.stripPrefix "#sha256=" fragment
  (loopback, path) <- (False,) <$> forgejoPath <|> (True,) <$> loopbackPath
  guard $ maybe False (not . T.null) (T.stripSuffix ".md" path) && all segment (T.splitOn "/" path)
  pure (loopback, (T.unpack url, digest))
  where
    (url, fragment) = T.breakOn "#" link
    forgejoPath = do
      (commit, path) <- T.splitAt commitChars <$> T.stripPrefix "https://git.simplex.chat/simplex-chat/simplex-chat/raw/commit/" url
      guard $ T.length commit == commitChars && T.all lowerHex commit
      T.stripPrefix "/" path
    loopbackPath = do
      (port, path) <- T.breakOn "/" <$> T.stripPrefix "http://127.0.0.1:" url
      guard $ maybe False (\n -> n >= 1 && n <= maxPort && tshow n == port) (readMaybe $ T.unpack port)
      T.stripPrefix "/" path
    segment s = not (T.null s) && s `notElem` [".", ".."] && T.all pathChar s
    pathChar c = isAsciiUpper c || isAsciiLower c || isDigit c || c `elem` ("_.-" :: String)
    lowerHex c = isDigit c || (c >= 'a' && c <= 'f')
    commitChars = 40
    maxPort = 65535 :: Int

policyNonce :: Contracts -> Address -> IO (Either ChainError Word64)
policyNonce ct owner = policyCall ct ("nonces(address)", addressWord owner) decodeWord64

-- | The terms hash the owner accepted for the commitment, all zeros when none.
acceptedTerms :: Contracts -> ByteString -> Address -> IO (Either ChainError ByteString)
acceptedTerms ct commitment owner = policyCall ct ("acceptedTerms(bytes32,address)", commitment <> addressWord owner) decodeBytes32

policyCall :: Contracts -> (ByteString, ByteString) -> (ByteString -> Either AbiError a) -> IO (Either ChainError a)
policyCall ct args decode = runExceptT $ do
  policy <- ExceptT $ namePolicy ct
  ExceptT $ call ct policy args decode

-- | The newest block at the configured confirmation depth, so a read there sees only what a reorg cannot undo.
confirmedTag :: Contracts -> IO (Either ChainError BlockTag)
confirmedTag Contracts {ctChain} = fmap (AtBlock . confirmedNumber ctChain . blockNumber) <$> latestBlock ctChain

-- | Nothing when nobody holds the name: @ownerOf@ then fails the registrar's bare @require@ on the expiry,
-- a revert without data. Any other revert is a failed read.
ownerOfToken :: Contracts -> BlockTag -> LabelHash -> IO (Either ChainError (Maybe Address))
ownerOfToken ct tag lh =
  callAt ct tag (baseRegistrar ct) ("ownerOf(uint256)", labelHashArg lh) decodeAddress >>= \case
    Left (ChainReverted (Just RNWithoutData)) -> pure $ Right Nothing
    r -> pure $ Just <$> r

nameExpires :: Contracts -> BlockTag -> LabelHash -> IO (Either ChainError Word64)
nameExpires ct tag lh = callAt ct tag (baseRegistrar ct) ("nameExpires(uint256)", labelHashArg lh) decodeWord64

-- | The block time of the commitment, Nothing when the controller holds none.
commitmentTime :: Contracts -> ByteString -> IO (Either ChainError (Maybe Word64))
commitmentTime ct commitment = fmap nonZero <$> call ct (controller ct) ("commitments(bytes32)", commitment) decodeWord64
  where
    nonZero t = if t == 0 then Nothing else Just t

controllerAvailable :: Contracts -> Text -> IO (Either ChainError Bool)
controllerAvailable ct label = call ct (controller ct) ("available(string)", abiTuple [Dynamic $ abiBytes $ encodeUtf8 label]) decodeBool

commitmentAges :: Contracts -> IO (Either ChainError CommitmentAges)
commitmentAges ct@Contracts {ctCommitmentAges} = readOnce ctCommitmentAges $ runExceptT $ do
  caMin <- ExceptT $ call ct (controller ct) ("minCommitmentAge()", "") decodeUint256
  caMax <- ExceptT $ call ct (controller ct) ("maxCommitmentAge()", "") decodeUint256
  pure CommitmentAges {caMin, caMax}

registrarConstants :: Contracts -> IO (Either ChainError RegistrarConstants)
registrarConstants ct@Contracts {ctRegistrar} = readOnce ctRegistrar $ runExceptT $ do
  rcRegistry <- ExceptT $ call ct (baseRegistrar ct) ("ens()", "") decodeAddress
  rcGracePeriod <- ExceptT $ call ct (baseRegistrar ct) ("GRACE_PERIOD()", "") decodeUint256
  pure RegistrarConstants {rcRegistry, rcGracePeriod}

-- | Concurrent first uses may each read; they read the same values, which the service keeps until it restarts.
readOnce :: IORef (Maybe a) -> IO (Either ChainError a) -> IO (Either ChainError a)
readOnce ref readChain =
  readIORef ref >>= \case
    Just a -> pure $ Right a
    Nothing -> readChain >>= either (pure . Left) (\a -> Right a <$ atomicWriteIORef ref (Just a))

-- | Reads run at once; the first failure in argument order is the answer.
together :: Compose Concurrently (Either ChainError) a -> IO (Either ChainError a)
together = runConcurrently . getCompose

concurrent :: IO (Either ChainError a) -> Compose Concurrently (Either ChainError) a
concurrent = Compose . Concurrently

withinDeadline :: Contracts -> Text -> IO (Either ChainError a) -> IO (Either ChainError a)
withinDeadline Contracts {ctDeadline} what action = fromMaybe (Left $ ChainUnavailable $ what <> ": timed out") <$> timeout ctDeadline action

call :: Contracts -> Address -> (ByteString, ByteString) -> (ByteString -> Either AbiError a) -> IO (Either ChainError a)
call ct = callAt ct Latest

-- | An answer that does not decode is a node failure, as an RPC error is.
callAt :: Contracts -> BlockTag -> Address -> (ByteString, ByteString) -> (ByteString -> Either AbiError a) -> IO (Either ChainError a)
callAt Contracts {ctChain} tag to (signature, args) decode =
  (>>= first malformed . decode) <$> ethCallAt ctChain tag to (selector signature <> args)
  where
    malformed e = ChainUnavailable $ "eth_call: " <> decodeLatin1 signature <> " answered " <> tshow e

labelHashArg :: LabelHash -> ByteString
labelHashArg (LabelHash h) = h

nodeArg :: Contracts -> LabelHash -> ByteString
nodeArg Contracts {ctTLD} = nodeBytes . labelNode ctTLD

chainConfig :: Contracts -> ChainConfig
chainConfig = ccConfig . ctChain

controller, resolver, baseRegistrar :: Contracts -> Address
controller = cController . chainConfig
resolver = cResolver . chainConfig
baseRegistrar = cBaseRegistrar . chainConfig

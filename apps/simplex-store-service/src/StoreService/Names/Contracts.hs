{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module StoreService.Names.Contracts
  ( Contracts (ctChain, ctDeadline, ctTermsBackoff, ctViewSlots),
    NameStatus (..),
    RegisteredName (..),
    NameRecords (..),
    CommitmentAges (..),
    Terms (..),
    TermsRefusal (..),
    RegistrarFunds (..),
    newContracts,
    secondsPerYear,
    nameStatus,
    nameRecords,
    textRecord,
    chainPriceCents,
    sponsorNonce,
    registrarFunds,
    resolverNonce,
    namePolicy,
    forgetNamePolicy,
    communityName,
    termsLinks,
    kindTerms,
    termsOf,
    termsText,
    fetchCurrentTerms,
    runTermsFetch,
    termsHeld,
    policyNonce,
    acceptedTerms,
    confirmedTag,
    ownerOfToken,
    nameExpires,
    disputeUntil,
    commitmentTime,
    controllerAvailable,
    commitmentAges,
    together,
    concurrent,
    callAt,
    textRecordAt,
    isZeroAddress,
  )
where

import Control.Applicative (optional, (<|>))
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (Concurrently (..))
import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import Control.Concurrent.STM (TVar, atomically, newTVarIO, readTVarIO)
import Control.Exception (try)
import Control.Logger.Simple (logError, logWarn)
import Control.Monad (forever, unless, when)
import Control.Monad.Except (ExceptT (..), runExceptT)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Attoparsec.ByteString.Char8 as A
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as LB
import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import Data.Functor ((<&>))
import Data.Functor.Compose (Compose (..))
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef, writeIORef)
import Data.Int (Int64)
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes, fromMaybe, isNothing)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeLatin1, decodeUtf8', encodeUtf8)
import Data.Time.Clock (UTCTime, addUTCTime, diffUTCTime, getCurrentTime)
import Data.Word (Word64)
import qualified Network.HTTP.Client as HTTP
import Network.HTTP.Types (statusCode)
import Simplex.Chat.Badges.Code (NameLength, nameLengthInt)
import Simplex.Messaging.Agent.Store.Common (DBStore, withTransaction)
import Simplex.Messaging.Crypto (keccak256, sha256Hash)
import Simplex.Messaging.Eth.Address (Address, unAddress)
import Simplex.Messaging.Names.Record (USDCents)
import Simplex.Messaging.Parsers (parseAll)
import Simplex.Messaging.SimplexName (LabelHash (..), SimplexTLD)
import Simplex.Messaging.SystemTime (RoundedSystemTime (..), SystemSeconds)
import Simplex.Messaging.TMap (TMap)
import qualified Simplex.Messaging.TMap as TM
import Simplex.Messaging.Util (catchOwn', catchThrow, eitherToMaybe, tshow)
import StoreService.Config (ChainConfig (..), boundedDecimal, localChainId)
import StoreService.Names.Abi (AbiError (..), AbiValue (..), abiBytes, abiTuple, abiWordSize, addressWord, decodeAddress, decodeBool, decodeBytes32, decodeSeconds, decodeString, decodeUint256, decodeWord64, lowerHexP, parseText, selector, uint256)
import StoreService.Names.Chain (Block (..), BlockTag (..), ChainClient (..), ChainError (..), Hex32 (..), confirmedNumber, ethCallAt, httpFailure, latestBlock, registrarBalance)
import StoreService.Names.Registration (Commitment, CommunityName, LinkKind (..), TermsHash, labelNode, nodeBytes, recordKey, termsKindText)
import StoreService.Names.Revert (RevertReason (..))
import StoreService.Names.Store (getTermsText, setTermsText)
import System.Timeout (timeout)
import Text.Read (readMaybe)

data Contracts = Contracts
  { ctChain :: ChainClient,
    ctStore :: DBStore,
    ctTLD :: SimplexTLD,
    ctMinPriceCentsPerYear :: Int,
    ctRegistrar :: IORef (Int, Maybe RegistrarConstants),
    ctCommitmentAges :: IORef (Int, Maybe CommitmentAges),
    ctNamePolicy :: IORef (Int, Maybe Address),
    ctTermsTexts :: TMap TermsHash Text,
    -- | the terms links as read at a block, which each search for an available name would read again
    ctTermsLinks :: IORef (Maybe (Word64, (Text, Text))),
    ctFetchAfter :: IORef UTCTime,
    ctFetching :: MVar (),
    -- | microseconds for all the reads of one name status, of one name's records, or of one registration view
    ctDeadline :: Int,
    ctTermsBackoff :: Int,
    ctViewSlots :: TVar Int
  }

data RegistrarConstants = RegistrarConstants {rcRegistry :: Address, rcGracePeriod :: Integer}

data NameStatus = NSAvailable | NSReserved | NSRegistered RegisteredName
  deriving (Eq, Show)

data RegisteredName = RegisteredName
  { rnOwner :: Address,
    rnResolver :: Address,
    rnExpiresAt :: SystemSeconds,
    rnGracePeriod :: Integer
  }
  deriving (Eq, Show)

-- | The default resolver's records ("" when unset) and the owner's nonce for the next edit.
data NameRecords = NameRecords {rdContact :: Text, rdChannel :: Text, rdNonce :: Word64}
  deriving (Eq, Show)

data CommitmentAges = CommitmentAges {caMin :: Word64, caMax :: Word64}
  deriving (Eq, Show)

data Terms = Terms {tmLink :: Text, tmHash :: TermsHash, tmFetch :: (String, Maybe ByteString)}
  deriving (Eq, Show)

data TermsRefusal = TRWait Int
  deriving (Eq, Show)

data RegistrarFunds = RegistrarFunds {rfBalanceWei :: Integer, rfAllowanceCents :: Integer}
  deriving (Eq, Show)

secondsPerYear :: Integer
secondsPerYear = 31536000

attoUsdPerCent :: Integer
attoUsdPerCent = 10 ^ (16 :: Int)

-- | One call's timeout, so a node that hangs fails a status or a records read once, not once per read.
nameReadsDeadline :: Int
nameReadsDeadline = 10000000

viewReadSlots :: Int
viewReadSlots = 16

termsBackoffSeconds :: Int
termsBackoffSeconds = 30

maxTermsBackoffSeconds :: Int
maxTermsBackoffSeconds = 3600

maxTermsBytes :: Int
maxTermsBytes = 256 * 1024

termsFetchInterval :: Int
termsFetchInterval = 3600 * 1000000

-- | Reads nothing: the registrar's constants, the commitment ages and the name policy are read on first use and kept once read.
newContracts :: DBStore -> SimplexTLD -> Int -> ChainClient -> IO Contracts
newContracts ctStore ctTLD ctMinPriceCentsPerYear ctChain = do
  ctRegistrar <- newIORef (0, Nothing)
  ctCommitmentAges <- newIORef (0, Nothing)
  ctNamePolicy <- newIORef (0, Nothing)
  ctTermsTexts <- TM.emptyIO
  ctTermsLinks <- newIORef Nothing
  ctFetchAfter <- newIORef =<< getCurrentTime
  ctFetching <- newMVar ()
  ctViewSlots <- newTVarIO viewReadSlots
  pure Contracts {ctChain, ctStore, ctTLD, ctMinPriceCentsPerYear, ctRegistrar, ctCommitmentAges, ctNamePolicy, ctTermsTexts, ctTermsLinks, ctFetchAfter, ctFetching, ctDeadline = nameReadsDeadline, ctTermsBackoff = termsBackoffSeconds, ctViewSlots}

-- | Registered while @nameExpires + GRACE_PERIOD@ is not before the latest block's time.
nameStatus :: Contracts -> LabelHash -> IO (Either ChainError NameStatus)
nameStatus ct@Contracts {ctChain} lh = withinDeadline ct "name status" $ runExceptT $ do
  (RegistrarConstants {rcRegistry, rcGracePeriod}, Block {blockNumber, blockTimestamp}) <-
    ExceptT $ together $ (,) <$> concurrent (registrarConstants ct) <*> concurrent (latestBlock ctChain)
  let at = AtBlock blockNumber
  expires <- ExceptT $ nameExpires ct at lh
  if toInteger (roundedSeconds expires) + rcGracePeriod >= toInteger (roundedSeconds blockTimestamp)
    then do
      let node = nodeArg ct lh
          registryRead signature = concurrent $ callAt ct at rcRegistry (signature, node) decodeAddress
      (rnOwner, rnResolver) <- ExceptT $ together $ (,) <$> registryRead "owner(bytes32)" <*> registryRead "resolver(bytes32)"
      pure $ NSRegistered RegisteredName {rnOwner, rnResolver, rnExpiresAt = expires, rnGracePeriod = rcGracePeriod}
    else do
      reason <- ExceptT $ policyCall ct at ("reservedNames(bytes32)", labelHashArg lh) decodeUint256
      pure $ if reason /= 0 then NSReserved else NSAvailable

-- | Nothing for a name on another resolver contract, or none: the service reads no records from a contract it does not know.
nameRecords :: Contracts -> LabelHash -> RegisteredName -> IO (Either ChainError (Maybe NameRecords))
nameRecords ct@Contracts {ctChain} lh RegisteredName {rnOwner, rnResolver}
  | rnResolver /= resolver ct = pure $ Right Nothing
  | otherwise = withinDeadline ct "name records" $ runExceptT $ do
      Block {blockNumber} <- ExceptT $ latestBlock ctChain
      let at = AtBlock blockNumber
          record = concurrent . textRecordAt ct at (nodeArg ct lh) . recordKey
      ExceptT $ together $
        (\rdContact rdChannel rdNonce -> Just NameRecords {rdContact, rdChannel, rdNonce})
          <$> record LKContact
          <*> record LKChannel
          <*> concurrent (resolverNonce ct at rnOwner)

-- | The default resolver's record, "" when unset.
textRecord :: Contracts -> LabelHash -> ByteString -> IO (Either ChainError Text)
textRecord ct lh = textRecordAt ct Latest (nodeArg ct lh)

textRecordAt :: Contracts -> BlockTag -> ByteString -> ByteString -> IO (Either ChainError Text)
textRecordAt ct tag node key = callAt ct tag (resolver ct) ("text(bytes32,string)", abiTuple [Static node, Dynamic $ abiBytes key]) decodeString

-- | Cents for a label of this length and these years. The oracle prices by length only,
-- so a placeholder label of that length keeps the label from the RPC node. A price under the floor for these years is refused: a node
-- answering one would sell names below it while the sponsor allowance pays their rent.
chainPriceCents :: Contracts -> NameLength -> Int -> IO (Either ChainError USDCents)
chainPriceCents ct@Contracts {ctMinPriceCentsPerYear} len years = fmap (>>= checkFloor) . call ct (controller ct) ("rentPriceUSD(string,uint256)", args) $ \ret -> do
  base <- decodeUint256 ret
  let cents = base `div` attoUsdPerCent
  if cents <= toInteger (maxBound :: Int64) then Right (fromInteger cents) else Left AERange
  where
    args = abiTuple [Dynamic $ abiBytes $ B.replicate (nameLengthInt len) 'a', Static $ uint256 $ toInteger years * secondsPerYear]
    checkFloor cents
      | toInteger cents >= toInteger ctMinPriceCentsPerYear * toInteger years = Right cents
      | otherwise = Left $ ChainUnavailable $ "rentPriceUSD answered " <> tshow (toInteger cents) <> " cents for " <> tshow years <> " years, under names.min_price_cents_per_year"

sponsorNonce :: Contracts -> Address -> IO (Either ChainError Word64)
sponsorNonce ct owner = call ct (cSponsor $ chainConfig ct) ("nonces(address)", addressWord owner) decodeWord64

registrarFunds :: Contracts -> IO (Either ChainError RegistrarFunds)
registrarFunds ct@Contracts {ctChain = c@ChainClient {ccRegistrar}} =
  together $ (\rfBalanceWei rfAllowanceCents -> RegistrarFunds {rfBalanceWei, rfAllowanceCents}) <$> concurrent (registrarBalance c) <*> concurrent allowance
  where
    allowance = call ct (cSponsor $ chainConfig ct) ("registrarAllowance(address)", addressWord ccRegistrar) $ fmap (`div` attoUsdPerCent) . decodeUint256

resolverNonce :: Contracts -> BlockTag -> Address -> IO (Either ChainError Word64)
resolverNonce ct tag owner = callAt ct tag (resolver ct) ("nonces(address)", addressWord owner) decodeWord64

-- | The policy holding the reserved and the community names, read once: as the service starts, or on first use after that
-- read failed, found none set, or was forgotten as a register reverted TermsNotAccepted.
namePolicy :: Contracts -> IO (Either ChainError Address)
namePolicy ct@Contracts {ctNamePolicy} = readOnce ctNamePolicy $ (>>= nonZero) <$> call ct (controller ct) ("namePolicy()", "") decodeAddress
  where
    nonZero policy = if isZeroAddress policy then Left (ChainUnavailable "the controller has no name policy yet") else Right policy

forgetNamePolicy :: Contracts -> IO ()
forgetNamePolicy Contracts {ctNamePolicy} = atomicModifyIORef' ctNamePolicy $ \(g, _) -> ((g + 1, Nothing), ())

communityName :: Contracts -> LabelHash -> IO (Either ChainError CommunityName)
communityName ct lh = policyCall ct Latest ("communityNames(bytes32)", labelHashArg lh) decodeBool

termsLinks :: Contracts -> IO (Either ChainError (Maybe Terms, Maybe Terms))
termsLinks ct@Contracts {ctChain, ctTermsLinks} = runExceptT $ do
  policy <- ExceptT $ namePolicy ct
  Block {blockNumber} <- ExceptT $ latestBlock ctChain
  liftIO (readIORef ctTermsLinks) >>= \case
    Just (at, (general, community)) | at == blockNumber -> pure (terms general, terms community)
    _ -> do
      let link signature = concurrent $ callAt ct (AtBlock blockNumber) policy (signature, "") decodeString
      (general, community) <- ExceptT $ together $ (,) <$> link "generalTermsLink()" <*> link "communityTermsLink()"
      liftIO $ do
        replaced <- atomicModifyIORef' ctTermsLinks (Just (blockNumber, (general, community)),)
        logRefused False (fst . snd <$> replaced) general
        logRefused True (snd . snd <$> replaced) community
      pure (terms general, terms community)
  where
    terms = termsOf $ cChainId $ chainConfig ct
    logRefused community before link =
      when (before /= Just link && isNothing (terms link) && not (T.null link)) $
        logWarn $ "names: the name policy's " <> termsKindText community <> " terms are not a link the service takes, so names under them cannot be ordered: " <> link

kindTerms :: CommunityName -> (Maybe Terms, Maybe Terms) -> Maybe Terms
kindTerms community (general, communityTerms) = if community then communityTerms else general

termsOf :: Word64 -> Text -> Maybe Terms
termsOf chainId link =
  terms $ case eitherToMaybe $ parseText linkP link of
    Just (loopback, fetch)
      | not loopback || chainId == localChainId -> Just fetch
    _ -> Nothing
  where
    terms = fmap $ \tmFetch -> Terms {tmLink = link, tmHash = Hex32 $ keccak256 $ encodeUtf8 link, tmFetch}
    linkP = do
      (url, loopback) <- A.match $ False <$ (commitP forgejoPrefix <|> commitP githubPrefix) <|> True <$ loopbackP
      digest <- optional $ A.string "#sha256=" *> lowerHexP abiWordSize
      if loopback && isNothing digest then fail "a loopback link without its SHA-256" else pure (loopback, (B.unpack url, digest))
    forgejoPrefix = "https://git.simplex.chat/simplex-chat/simplex-chat/raw/commit/"
    githubPrefix = "https://raw.githubusercontent.com/simplex-chat/simplex-chat/"
    commitP prefix = A.string prefix *> lowerHexP commitBytes *> pathP
    loopbackP = A.string "http://127.0.0.1:" *> portP *> pathP
    portP = A.takeWhile1 isDigit >>= \port -> unless (B.take 1 port /= "0" && B.length port <= 5 && maybe False (<= maxPort) (readMaybe $ B.unpack port)) $ fail "not a port"
    pathP = do
      (path, segments) <- A.match $ A.char '/' *> A.takeWhile1 pathChar `A.sepBy1` A.char '/'
      unless (all (`notElem` [".", ".."]) segments && ".md" `B.isSuffixOf` path && B.length path > B.length "/.md") $ fail "not one .md file"
    pathChar ch = isAsciiUpper ch || isAsciiLower ch || isDigit ch || ch `elem` ("_.-" :: String)
    commitBytes = 20
    maxPort = 65535 :: Int

termsText :: Contracts -> Terms -> IO (Either TermsRefusal Text)
termsText ct@Contracts {ctTermsTexts, ctFetching} t@Terms {tmHash} =
  kept >>= \case
    Just text -> pure $ Right text
    Nothing -> withMVar ctFetching $ \_ -> kept >>= maybe (fetchTerms ct t) (pure . Right)
  where
    kept = TM.lookupIO tmHash ctTermsTexts

-- | Each kind of terms the name policy links, with whether its text is held; nothing is fetched.
termsHeld :: Contracts -> IO (Either ChainError [(Text, Bool)])
termsHeld ct@Contracts {ctTermsTexts} = termsLinks ct >>= mapM held
  where
    held (general, community) = do
      texts <- readTVarIO ctTermsTexts
      pure [(termsKindText kind, M.member tmHash texts) | (kind, Just Terms {tmHash}) <- [(False, general), (True, community)]]

fetchCurrentTerms :: Contracts -> IO ()
fetchCurrentTerms ct =
  termsLinks ct >>= \case
    Right (general, community) -> mapM_ (termsText ct) $ catMaybes [general, community]
    Left e -> logWarn $ "names: the terms links could not be read to fetch their texts: " <> tshow e

runTermsFetch :: Contracts -> IO ()
runTermsFetch ct = forever $ (fetchCurrentTerms ct `catchOwn'` \e -> logError ("names terms fetch failed; the next pass will run: " <> tshow e)) >> threadDelay termsFetchInterval

fetchTerms :: Contracts -> Terms -> IO (Either TermsRefusal Text)
fetchTerms Contracts {ctChain, ctStore, ctTermsTexts, ctFetchAfter, ctTermsBackoff} Terms {tmLink, tmHash, tmFetch = (url, digest)} =
  withTransaction ctStore (`getTermsText` tmHash) >>= \case
    Just stored -> case checked stored of
      Right text -> keep text
      Left (reason, _) -> do
        logWarn $ "names: the stored terms text of " <> tmLink <> " is refused, " <> reason <> "; it is fetched again"
        fetchOrWait
    Nothing -> fetchOrWait
  where
    fetchOrWait = do
      after <- readIORef ctFetchAfter
      now <- getCurrentTime
      if now < after
        then pure $ Left $ TRWait $ ceiling $ diffUTCTime after now
        else
          (>>= checked) <$> fetchText ctChain maxTermsBytes url >>= \case
            Right text -> keep text <* store text
            Left (reason, retryAfter) -> do
              let wait = min maxTermsBackoffSeconds $ max ctTermsBackoff $ fromMaybe 0 retryAfter
              failedAt <- getCurrentTime
              writeIORef ctFetchAfter $ addUTCTime (fromIntegral wait) failedAt
              logWarn $ "names: the terms text of " <> tmLink <> " could not be fetched, " <> reason <> "; none is fetched for " <> tshow wait <> " seconds"
              pure $ Left $ TRWait wait
    keep text = Right text <$ atomically (TM.insert tmHash text ctTermsTexts)
    store text =
      withTransaction ctStore (\db -> setTermsText db tmHash text)
        `catchOwn'` \e -> logError ("names: the terms text of " <> tmLink <> " could not be stored: " <> tshow e)
    checked :: ByteString -> Either (Text, Maybe Int) Text
    checked body
      | any (sha256Hash body /=) digest = Left ("its SHA-256 is not the link's", Nothing)
      | otherwise = first (const ("not UTF-8", Nothing)) $ decodeUtf8' body

fetchText :: ChainClient -> Int -> String -> IO (Either (Text, Maybe Int) ByteString)
fetchText ChainClient {ccManager, ccTimeout} maxBytes url =
  timeout ccTimeout (runExceptT $ ExceptT (either failed id <$> try get) `catchThrow` const ("request failed", Nothing)) <&> \case
    Nothing -> Left ("timed out", Nothing)
    Just r -> r
  where
    failed (e :: HTTP.HttpException) = Left (httpFailure e, Nothing)
    get = do
      req <- HTTP.parseRequest url
      HTTP.withResponse req {HTTP.redirectCount = 0} ccManager $ \resp -> answer (HTTP.responseStatus resp) (HTTP.responseHeaders resp) (HTTP.responseBody resp)
    answer status headers body
      | statusCode status /= 200 = pure $ Left ("HTTP " <> tshow (statusCode status), retryAfter =<< lookup "Retry-After" headers)
      | otherwise = within <$> HTTP.brReadSome body (maxBytes + 1)
    within b
      | LB.length b > fromIntegral maxBytes = Left ("over " <> tshow maxBytes <> " bytes", Nothing)
      | otherwise = Right $ LB.toStrict b
    retryAfter = eitherToMaybe . parseAll (boundedDecimal 0 maxBound)

policyNonce :: Contracts -> Address -> IO (Either ChainError Word64)
policyNonce ct owner = policyCall ct Latest ("nonces(address)", addressWord owner) decodeWord64

-- | The terms hash the owner accepted for the commitment, all zeros when none.
acceptedTerms :: Contracts -> Commitment -> Address -> IO (Either ChainError TermsHash)
acceptedTerms ct (Hex32 commitment) owner = policyCall ct Latest ("acceptedTerms(bytes32,address)", commitment <> addressWord owner) (fmap Hex32 . decodeBytes32)

policyCall :: Contracts -> BlockTag -> (ByteString, ByteString) -> (ByteString -> Either AbiError a) -> IO (Either ChainError a)
policyCall ct tag args decode = runExceptT $ do
  policy <- ExceptT $ namePolicy ct
  ExceptT $ callAt ct tag policy args decode

-- | The newest block at the configured confirmation depth, so a read there sees only what a reorg cannot undo.
confirmedTag :: Contracts -> IO (Either ChainError BlockTag)
confirmedTag Contracts {ctChain} = fmap (AtBlock . confirmedNumber ctChain . blockNumber) <$> latestBlock ctChain

-- | Nothing when nobody holds the name: @ownerOf@ then fails the registrar's bare @require@ on the expiry,
-- a revert without data. Any other revert is a failed read.
ownerOfToken :: Contracts -> BlockTag -> LabelHash -> IO (Either ChainError (Maybe Address))
ownerOfToken ct tag lh =
  callAt ct tag (baseRegistrar ct) ("ownerOf(uint256)", labelHashArg lh) decodeAddress >>= \case
    Left (ChainReverted RRWithoutData) -> pure $ Right Nothing
    r -> pure $ Just <$> r

nameExpires :: Contracts -> BlockTag -> LabelHash -> IO (Either ChainError SystemSeconds)
nameExpires ct tag lh = callAt ct tag (baseRegistrar ct) ("nameExpires(uint256)", labelHashArg lh) decodeSeconds

disputeUntil :: Contracts -> BlockTag -> LabelHash -> IO (Either ChainError (Maybe SystemSeconds))
disputeUntil ct tag lh = fmap unlessZero <$> callAt ct tag (baseRegistrar ct) ("disputeUntil(uint256)", labelHashArg lh) decodeSeconds

-- | The block time of the commitment, Nothing when the controller holds none.
commitmentTime :: Contracts -> Commitment -> IO (Either ChainError (Maybe SystemSeconds))
commitmentTime ct (Hex32 commitment) = fmap unlessZero <$> call ct (controller ct) ("commitments(bytes32)", commitment) decodeSeconds

unlessZero :: SystemSeconds -> Maybe SystemSeconds
unlessZero t = if t == RoundedSystemTime 0 then Nothing else Just t

controllerAvailable :: Contracts -> BlockTag -> Text -> IO (Either ChainError Bool)
controllerAvailable ct tag label = callAt ct tag (controller ct) ("available(string)", abiTuple [Dynamic $ abiBytes $ encodeUtf8 label]) decodeBool

commitmentAges :: Contracts -> IO (Either ChainError CommitmentAges)
commitmentAges ct@Contracts {ctCommitmentAges} = readOnce ctCommitmentAges $ runExceptT $ do
  caMin <- ExceptT $ call ct (controller ct) ("minCommitmentAge()", "") decodeWord64
  caMax <- ExceptT $ call ct (controller ct) ("maxCommitmentAge()", "") decodeWord64
  pure CommitmentAges {caMin, caMax}

registrarConstants :: Contracts -> IO (Either ChainError RegistrarConstants)
registrarConstants ct@Contracts {ctRegistrar} = readOnce ctRegistrar $ runExceptT $ do
  rcRegistry <- ExceptT $ call ct (baseRegistrar ct) ("ens()", "") decodeAddress
  rcGracePeriod <- ExceptT $ call ct (baseRegistrar ct) ("GRACE_PERIOD()", "") decodeUint256
  pure RegistrarConstants {rcRegistry, rcGracePeriod}

readOnce :: IORef (Int, Maybe a) -> IO (Either ChainError a) -> IO (Either ChainError a)
readOnce ref readChain =
  readIORef ref >>= \case
    (_, Just a) -> pure $ Right a
    (generation, Nothing) ->
      readChain >>= traverse (\a -> a <$ atomicModifyIORef' ref (\kept@(g, _) -> (if g == generation then (g, Just a) else kept, ())))

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

isZeroAddress :: Address -> Bool
isZeroAddress = B.all (== '\NUL') . unAddress

chainConfig :: Contracts -> ChainConfig
chainConfig = ccConfig . ctChain

controller, resolver, baseRegistrar :: Contracts -> Address
controller = cController . chainConfig
resolver = cResolver . chainConfig
baseRegistrar = cBaseRegistrar . chainConfig

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
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
    RecordsSet (..),
    NameEntry (..),
    RegisteredNames (..),
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
    termsOf,
    termsLinkAllowed,
    termsText,
    fetchCurrentTerms,
    runTermsFetch,
    termsHeld,
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
    callAt,
    recordsLabel,
    registeredNames,
  )
where

import Control.Applicative ((<|>))
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (Concurrently (..), mapConcurrently)
import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import Control.Concurrent.STM (TVar, newTVarIO)
import Control.Logger.Simple (logError, logWarn)
import Control.Monad (forever, guard)
import Control.Monad.Except (ExceptT (..), runExceptT)
import Control.Monad.IO.Class (liftIO)
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import Data.Functor ((<&>))
import Data.Functor.Compose (Compose (..))
import Data.IORef (IORef, atomicModifyIORef', atomicWriteIORef, modifyIORef', newIORef, readIORef, writeIORef)
import qualified Data.List.NonEmpty as L
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, isJust, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeLatin1, decodeUtf8', encodeUtf8)
import Data.Time.Clock (UTCTime, addUTCTime, diffUTCTime, getCurrentTime)
import Data.Word (Word64)
import Simplex.Messaging.Agent.Store.Common (DBStore, withTransaction)
import Simplex.Messaging.Crypto (keccak256, sha256Hash)
import Simplex.Messaging.Eth.Address (Address, unAddress)
import Simplex.Messaging.SimplexName (LabelHash (..), SimplexTLD)
import Simplex.Messaging.Util (catchOwn', toChunks, tshow)
import StoreService.Config (ChainConfig (..), localChainId)
import StoreService.Names.Abi
import StoreService.Names.Chain (Block (..), BlockTag (..), ChainClient (..), ChainError (..), ChainSeconds, confirmedNumber, ethCallAt, fetchText, latestBlock, nodeChainId, registrarBalance)
import StoreService.Names.Registration (TermsHash, channelRecordKey, childNode, contactRecordKey, labelNode, nodeBytes)
import StoreService.Names.Revert (RevertReason (..))
import StoreService.Names.Store (getTermsText, setTermsText)
import StoreService.Safely (safely)
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
    ctTermsTexts :: IORef (Map ByteString Text),
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
    rnExpiresAt :: ChainSeconds,
    rnGracePeriod :: Integer
  }
  deriving (Eq, Show)

-- | The default resolver's records ("" when unset) and the owner's nonce for the next edit.
data NameRecords = NameRecords {rdContact :: Text, rdChannel :: Text, rdNonce :: Word64}
  deriving (Eq, Show)

data CommitmentAges = CommitmentAges {caMin :: ChainSeconds, caMax :: ChainSeconds}
  deriving (Eq, Show)

data Terms = Terms {tmLink :: Text, tmHash :: TermsHash}
  deriving (Eq, Show)

data TermsRefusal = TRLink | TRWait Int
  deriving (Eq, Show)

data RegistrarFunds = RegistrarFunds {rfBalanceWei :: Integer, rfAllowanceCents :: Integer}
  deriving (Eq, Show)

data RecordsSet = RNone | RContact | RChannel | RBoth | ROtherResolver
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | Level 2 is a name such as alice.simplex, 3 its subname; the length is in characters.
data NameEntry = NameEntry {enLevel :: Int, enLength :: Int, enRecords :: RecordsSet}
  deriving (Eq, Show)

data RegisteredNames = RegisteredNames {rgBlock :: Word64, rgNames :: [NameEntry]}
  deriving (Eq, Show)

data ReadsAt = ReadsAt
  { raContracts :: Contracts,
    raAt :: BlockTag,
    raRegistry :: Address,
    raSubnames :: Address
  }

type Node = (ByteString, Text)

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
  ctTermsTexts <- newIORef M.empty
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
  if toInteger expires + rcGracePeriod >= toInteger blockTimestamp
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
textRecord ct lh = textRecordAt ct Latest (nodeArg ct lh)

textRecordAt :: Contracts -> BlockTag -> ByteString -> ByteString -> IO (Either ChainError Text)
textRecordAt ct tag node key = callAt ct tag (resolver ct) ("text(bytes32,string)", abiTuple [Static node, Dynamic $ abiBytes key]) decodeString

-- | Cents for a label of this length and these years. The oracle prices by length only,
-- so a placeholder label of that length keeps the label from the RPC node. A price under the floor for these years is refused: a node
-- answering one would sell names below it while the sponsor allowance pays their rent.
chainPriceCents :: Contracts -> Int -> Int -> IO (Either ChainError Int)
chainPriceCents ct@Contracts {ctMinPriceCentsPerYear} len years = fmap (>>= checkFloor) . call ct (controller ct) ("rentPriceUSD(string,uint256)", args) $ \ret -> do
  base <- decodeUint256 ret
  let cents = base `div` attoUsdPerCent
  if cents <= toInteger (maxBound :: Int) then Right (fromInteger cents) else Left AERange
  where
    args = abiTuple [Dynamic $ abiBytes $ B.replicate len 'a', Static $ uint256 $ toInteger years * secondsPerYear]
    checkFloor cents
      | toInteger cents >= toInteger ctMinPriceCentsPerYear * toInteger years = Right cents
      | otherwise = Left $ ChainUnavailable $ "rentPriceUSD answered " <> tshow cents <> " cents for " <> tshow years <> " years, under names.min_price_cents_per_year"

sponsorNonce :: Contracts -> Address -> IO (Either ChainError Word64)
sponsorNonce ct owner = call ct (cSponsor $ chainConfig ct) ("nonces(address)", addressWord owner) decodeWord64

registrarFunds :: Contracts -> IO (Either ChainError RegistrarFunds)
registrarFunds ct@Contracts {ctChain = c@ChainClient {ccRegistrar}} =
  together $ (\rfBalanceWei rfAllowanceCents -> RegistrarFunds {rfBalanceWei, rfAllowanceCents}) <$> concurrent (registrarBalance c) <*> concurrent allowance
  where
    allowance = call ct (cSponsor $ chainConfig ct) ("registrarAllowance(address)", addressWord ccRegistrar) $ fmap (`div` attoUsdPerCent) . decodeUint256

resolverNonce :: Contracts -> Address -> IO (Either ChainError Word64)
resolverNonce ct owner = call ct (resolver ct) ("nonces(address)", addressWord owner) decodeWord64

-- | The policy holding the reserved and the community names, read once: as the service starts, or on first use after that
-- read failed, found none set, or was forgotten as a register reverted TermsNotAccepted.
namePolicy :: Contracts -> IO (Either ChainError Address)
namePolicy ct@Contracts {ctNamePolicy} = readOnce ctNamePolicy $ (>>= nonZero) <$> call ct (controller ct) ("namePolicy()", "") decodeAddress
  where
    nonZero policy = if isZeroAddress policy then Left (ChainUnavailable "the controller has no name policy yet") else Right policy

forgetNamePolicy :: Contracts -> IO ()
forgetNamePolicy Contracts {ctNamePolicy} = atomicModifyIORef' ctNamePolicy $ \(g, _) -> ((g + 1, Nothing), ())

communityName :: Contracts -> LabelHash -> IO (Either ChainError Bool)
communityName ct lh = policyCall ct Latest ("communityNames(bytes32)", labelHashArg lh) decodeBool

termsLinks :: Contracts -> IO (Either ChainError (Text, Text))
termsLinks ct@Contracts {ctChain, ctTermsLinks} = runExceptT $ do
  policy <- ExceptT $ namePolicy ct
  Block {blockNumber} <- ExceptT $ latestBlock ctChain
  liftIO (readIORef ctTermsLinks) >>= \case
    Just (at, links) | at == blockNumber -> pure links
    _ -> do
      let link signature = concurrent $ callAt ct (AtBlock blockNumber) policy (signature, "") decodeString
      links <- ExceptT $ together $ (,) <$> link "generalTermsLink()" <*> link "communityTermsLink()"
      liftIO $ atomicWriteIORef ctTermsLinks $ Just (blockNumber, links)
      pure links

termsOf :: Text -> Maybe Terms
termsOf link
  | T.null link = Nothing
  | otherwise = Just Terms {tmLink = link, tmHash = keccak256 $ encodeUtf8 link}

termsLinkAllowed :: Contracts -> Terms -> IO Bool
termsLinkAllowed Contracts {ctChain} Terms {tmLink} = isJust <$> allowedLink ctChain tmLink

termsText :: Contracts -> Terms -> IO (Either TermsRefusal Text)
termsText ct@Contracts {ctTermsTexts, ctFetching} t@Terms {tmHash} =
  kept >>= \case
    Just text -> pure $ Right text
    Nothing -> withMVar ctFetching $ \_ -> kept >>= maybe (fetchTerms ct t) (pure . Right)
  where
    kept = M.lookup tmHash <$> readIORef ctTermsTexts

-- | Each kind of terms the name policy links, with whether its text is held; nothing is fetched.
termsHeld :: Contracts -> IO (Either ChainError [(Text, Bool)])
termsHeld ct@Contracts {ctTermsTexts} = termsLinks ct >>= mapM held
  where
    held (generalLink, communityLink) = do
      texts <- readIORef ctTermsTexts
      pure [(kind, M.member tmHash texts) | (kind, Just Terms {tmHash}) <- [("general", termsOf generalLink), ("community", termsOf communityLink)]]

fetchCurrentTerms :: Contracts -> IO ()
fetchCurrentTerms ct =
  termsLinks ct >>= \case
    Right (generalLink, communityLink) -> mapM_ (termsText ct) $ mapMaybe termsOf [generalLink, communityLink]
    Left e -> logWarn $ "names: the terms links could not be read to fetch their texts: " <> tshow e

runTermsFetch :: Contracts -> IO ()
runTermsFetch ct = forever $ safely "names terms fetch" (fetchCurrentTerms ct) >> threadDelay termsFetchInterval

fetchTerms :: Contracts -> Terms -> IO (Either TermsRefusal Text)
fetchTerms Contracts {ctChain, ctStore, ctTermsTexts, ctFetchAfter, ctTermsBackoff} Terms {tmLink, tmHash} =
  allowedLink ctChain tmLink >>= \case
    Nothing -> pure $ Left TRLink
    Just (url, digest) ->
      withTransaction ctStore (`getTermsText` tmHash) >>= \case
        Just stored -> case checked digest stored of
          Right text -> keep text
          Left (reason, _) -> do
            logWarn $ "names: the stored terms text of " <> tmLink <> " is refused, " <> reason <> "; it is fetched again"
            fetchOrWait url digest
        Nothing -> fetchOrWait url digest
  where
    fetchOrWait url digest = do
      after <- readIORef ctFetchAfter
      now <- getCurrentTime
      if now < after
        then pure $ Left $ TRWait $ ceiling $ diffUTCTime after now
        else
          (>>= checked digest) <$> fetchText ctChain maxTermsBytes url >>= \case
            Right text -> keep text <* store text
            Left (reason, retryAfter) -> do
              let wait = min maxTermsBackoffSeconds $ max ctTermsBackoff $ fromMaybe 0 retryAfter
              failedAt <- getCurrentTime
              writeIORef ctFetchAfter $ addUTCTime (fromIntegral wait) failedAt
              logWarn $ "names: the terms text of " <> tmLink <> " could not be fetched, " <> reason <> "; none is fetched for " <> tshow wait <> " seconds"
              pure $ Left $ TRWait wait
    keep text = Right text <$ modifyIORef' ctTermsTexts (M.insert tmHash text)
    store text =
      withTransaction ctStore (\db -> setTermsText db tmHash text)
        `catchOwn'` \e -> logError ("names: the terms text of " <> tmLink <> " could not be stored: " <> tshow e)
    checked :: Maybe ByteString -> ByteString -> Either (Text, Maybe Int) Text
    checked digest body
      | any (sha256Hash body /=) digest = Left ("its SHA-256 is not the link's", Nothing)
      | otherwise = first (const ("not UTF-8", Nothing)) $ decodeUtf8' body

allowedLink :: ChainClient -> Text -> IO (Maybe (String, Maybe ByteString))
allowedLink c@ChainClient {ccConfig = ChainConfig {cChainId}} link = case termsLinkParts link of
  Just (False, parts) -> pure $ Just parts
  Just (True, parts)
    | cChainId == localChainId ->
        nodeChainId c <&> \case
          Right chainId | chainId == localChainId -> Just parts
          _ -> Nothing
  _ -> pure Nothing

termsLinkParts :: Text -> Maybe (Bool, (String, Maybe ByteString))
termsLinkParts link = do
  digest <- if T.null fragment then pure Nothing else Just <$> (bytes32FromHex . ("0x" <>) =<< T.stripPrefix "#sha256=" fragment)
  (loopback, path) <- (False,) <$> (commitPath forgejoPrefix <|> commitPath githubPrefix) <|> (True,) <$> loopbackPath
  guard $ (isJust digest || not loopback) && maybe False (not . T.null) (T.stripSuffix ".md" path) && all segment (T.splitOn "/" path)
  pure (loopback, (T.unpack url, digest))
  where
    (url, fragment) = T.breakOn "#" link
    forgejoPrefix = "https://git.simplex.chat/simplex-chat/simplex-chat/raw/commit/"
    githubPrefix = "https://raw.githubusercontent.com/simplex-chat/simplex-chat/"
    commitPath prefix = do
      (commit, path) <- T.splitAt commitChars <$> T.stripPrefix prefix url
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
policyNonce ct owner = policyCall ct Latest ("nonces(address)", addressWord owner) decodeWord64

-- | The terms hash the owner accepted for the commitment, all zeros when none.
acceptedTerms :: Contracts -> ByteString -> Address -> IO (Either ChainError ByteString)
acceptedTerms ct commitment owner = policyCall ct Latest ("acceptedTerms(bytes32,address)", commitment <> addressWord owner) decodeBytes32

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

nameExpires :: Contracts -> BlockTag -> LabelHash -> IO (Either ChainError ChainSeconds)
nameExpires ct tag lh = callAt ct tag (baseRegistrar ct) ("nameExpires(uint256)", labelHashArg lh) decodeWord64

-- | The block time of the commitment, Nothing when the controller holds none.
commitmentTime :: Contracts -> ByteString -> IO (Either ChainError (Maybe ChainSeconds))
commitmentTime ct commitment = fmap nonZero <$> call ct (controller ct) ("commitments(bytes32)", commitment) decodeWord64
  where
    nonZero t = if t == 0 then Nothing else Just t

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

-- | Concurrent first uses may each read; a read that started before the value was forgotten is not kept.
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

recordsLabel :: RecordsSet -> Text
recordsLabel r = case r of
  RNone -> "none"
  RContact -> "contact"
  RChannel -> "channel"
  RBoth -> "both"
  ROtherResolver -> "other_resolver"

readsAtOnce :: Int
readsAtOnce = 16

childrenPage :: Integer
childrenPage = 100

-- | Every live name and subname at the latest block: names while nameExpires is after the block's time, subnames while
-- the subname registrar names an owner. A name whose resolver is not the configured one is counted without its records.
registeredNames :: Contracts -> IO (Either ChainError RegisteredNames)
registeredNames ct@Contracts {ctChain} = runExceptT $ do
  Block {blockNumber, blockTimestamp} <- ExceptT $ latestBlock ctChain
  let at = AtBlock blockNumber
  registry <- ExceptT $ callAt ct at (baseRegistrar ct) ("ens()", "") decodeAddress
  subnames <- ExceptT $ callAt ct at (baseRegistrar ct) ("subnameHook()", "") decodeAddress
  base <- ExceptT $ callAt ct at (baseRegistrar ct) ("baseNode()", "") decodeBytes32
  total <- ExceptT $ callAt ct at (baseRegistrar ct) ("totalSupply()", "") decodeUint256
  let env = ReadsAt {raContracts = ct, raAt = at, raRegistry = registry, raSubnames = subnames}
      registrar :: ByteString -> Integer -> (ByteString -> Either AbiError a) -> IO (Either ChainError a)
      registrar signature i = readAt env (baseRegistrar ct) signature (uint256 i)
  ids <- readAll [registrar "tokenByIndex(uint256)" i decodeUint256 | i <- [0 .. total - 1]]
  expiries <- readAll [registrar "nameExpires(uint256)" i decodeUint256 | i <- ids]
  let live = [i | (i, expiry) <- zip ids expiries, expiry > toInteger blockTimestamp]
  labels <- readAll [registrar "labelOf(uint256)" i decodeString | i <- live]
  RegisteredNames blockNumber <$> levels env 2 [(childNode base (LabelHash $ uint256 i), label) | (i, label) <- zip live labels]

levels :: ReadsAt -> Int -> [Node] -> ExceptT ChainError IO [NameEntry]
levels env level nodes
  | null nodes = pure []
  | otherwise = do
      records <- recordsOf env nodes
      children <- childrenOf env nodes
      (zipWith (\(_, label) r -> NameEntry level (T.length label) r) nodes records <>) <$> levels env (level + 1) children

recordsOf :: ReadsAt -> [Node] -> ExceptT ChainError IO [RecordsSet]
recordsOf env@ReadsAt {raContracts = ct, raAt, raRegistry} nodes = do
  resolvers <- readAll [readAt env raRegistry "resolver(bytes32)" node decodeAddress | (node, _) <- nodes]
  readAll [if r == resolver ct then textsOf node else pure (Right ROtherResolver) | ((node, _), r) <- zip nodes resolvers]
  where
    textsOf node = do
      contact <- textRecordAt ct raAt node contactRecordKey
      channel <- textRecordAt ct raAt node channelRecordKey
      pure $ recordsOfTexts <$> contact <*> channel

recordsOfTexts :: Text -> Text -> RecordsSet
recordsOfTexts contact channel = case (T.null contact, T.null channel) of
  (True, True) -> RNone
  (False, True) -> RContact
  (True, False) -> RChannel
  (False, False) -> RBoth

-- | A subname the subname registrar no longer names an owner of, as of a lapsed name or a previous owner, is left out.
childrenOf :: ReadsAt -> [Node] -> ExceptT ChainError IO [Node]
childrenOf env@ReadsAt {raSubnames} parents = do
  counts <- readAll [readAt env raSubnames "childrenLength(bytes32)" node decodeUint256 | (node, _) <- parents]
  let pages = [(node, start) | ((node, _), count) <- zip parents counts, start <- [0, childrenPage .. count - 1]]
  listed <- readAll [readAt env raSubnames "getChildren(bytes32,uint256,uint256)" (node <> uint256 start <> uint256 childrenPage) decodeLabels | (node, start) <- pages]
  let found = [(childNode node (LabelHash labelHash), label) | ((node, _), children) <- zip pages listed, (labelHash, label) <- children]
  owners <- readAll [readAt env raSubnames "ownerOf(uint256)" node decodeAddress | (node, _) <- found]
  pure [child | (child, owner) <- zip found owners, not (isZeroAddress owner)]

readAt :: ReadsAt -> Address -> ByteString -> ByteString -> (ByteString -> Either AbiError a) -> IO (Either ChainError a)
readAt ReadsAt {raContracts, raAt} to signature args = callAt raContracts raAt to (signature, args)

readAll :: [IO (Either ChainError a)] -> ExceptT ChainError IO [a]
readAll calls = concatMap L.toList <$> mapM (ExceptT . fmap sequence . mapConcurrently id) (toChunks readsAtOnce calls)

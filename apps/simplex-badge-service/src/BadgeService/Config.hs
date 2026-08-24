{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | `badge_service.ini` deployment configuration (decision 4): the record, the parser and
--   startup validation, plus 'BadgeServiceEnv', the single runtime value every handler
--   receives. Secrets are always file paths named from the ini, never inline values, so the
--   ini can go into configuration management while the secrets do not.
module BadgeService.Config
  ( IssuerConfig (..),
    CodesConfig (..),
    WebConfig (..),
    BtcPayConfig (..),
    StripeConfig (..),
    ServiceConfig (..),
    ReconcileConfig (..),
    BucketLimits (..),
    ThrottleConfig (..),
    BadgeServiceConfig (..),
    readBadgeServiceConfig,
    TokenBucket (..),
    SignerBucketFamily (..),
    BadgeServiceEnv (..),
    newBadgeServiceEnv,
    checkFailureBuckets,
    debitFailureBuckets,
  )
where

import BadgeService.Codes (loadCodeSecret)
import BadgeService.Credentials (loadIssuerKey)
import Control.Concurrent.STM
import Data.ByteString (ByteString)
import Data.Ini (Ini, keys, lookupValue, readIniFile, sections)
import qualified Data.Map.Strict as M
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (UTCTime, diffUTCTime, getCurrentTime)
import Data.Word (Word32)
import Simplex.Messaging.Agent.Store.Common (DBStore)
import qualified Simplex.Messaging.Crypto as C
import Simplex.Messaging.Crypto.BBS (BBSSecretKey)
import Simplex.Messaging.Encoding.String (strEncode)
import Simplex.Messaging.Util (eitherToMaybe)
import System.Directory (doesFileExist)
import Text.Read (readMaybe)

data IssuerConfig = IssuerConfig
  { issuerKeyFile :: FilePath,
    issuerKeyIdx :: Int
  }
  deriving (Eq, Show)

data CodesConfig = CodesConfig
  { codesSecretFile :: FilePath,
    codesDefaultExpiryDays :: Int
  }
  deriving (Eq, Show)

data WebConfig = WebConfig
  { webPort :: Int,
    webHost :: Text,
    webBaseUrl :: Text,
    webSupportContact :: Text,
    webBehindProxy :: Bool,
    webDir :: Maybe FilePath
  }
  deriving (Eq, Show)

data BtcPayConfig = BtcPayConfig
  { btcPayUrl :: Text,
    btcPayStoreId :: Text,
    btcPayApiKeyFile :: FilePath,
    btcPayWebhookSecretFile :: FilePath,
    btcPayXmrMethodId :: Text,
    btcPayBtcExpiryMinutes :: Int,
    btcPayXmrExpiryMinutes :: Int
  }
  deriving (Eq, Show)

data StripeConfig = StripeConfig
  { stripeSecretKeyFile :: FilePath,
    stripeWebhookSecretFile :: FilePath
  }
  deriving (Eq, Show)

newtype ServiceConfig = ServiceConfig
  { serviceAddressFile :: Maybe FilePath
  }
  deriving (Eq, Show)

newtype ReconcileConfig = ReconcileConfig
  { reconcileIntervalSeconds :: Int
  }
  deriving (Eq, Show)

-- | One bucket family's fixed shape (B5 decision 5): capacity doubles as the hourly refill
-- rate -- every bucket this service specifies is "N per hour, burst N", so there is no case
-- that needs the two to differ -- and 'blStartTokens' is the token count a freshly created
-- bucket starts with. Both overridable via '[throttle]' below, so a test can shrink a bucket
-- to a size that empties predictably, or start it pre-drained, without waiting on real time.
-- Capacity 0 is meaningless (never refills, so 'retryAfter' has no finite answer) and is
-- never used by any caller.
data BucketLimits = BucketLimits
  { blCapacity :: Word32,
    blStartTokens :: Word32
  }
  deriving (Eq, Show)

-- | The three RPC token buckets' sizes (B5 decision 5): 'signerFailure' is keyed per signer
-- and only shapes an honest client's retries -- a purchase key is self-asserted and cheap to
-- mint. 'globalFailure' is the real control against a distributed guesser (the code's 95
-- bits of entropy is the load-bearing defence, this bucket only bounds the attempt rate).
-- 'catalog' bounds unsigned 'getBadgeCatalog', which has no signer to key on (B6). Defaults
-- match the plan exactly; '[throttle]' overrides them at startup for B5/B6/B10's tests.
data ThrottleConfig = ThrottleConfig
  { signerFailure :: BucketLimits,
    globalFailure :: BucketLimits,
    catalog :: BucketLimits
  }
  deriving (Eq, Show)

data BadgeServiceConfig = BadgeServiceConfig
  { issuer :: IssuerConfig,
    codes :: CodesConfig,
    web :: Maybe WebConfig,
    btcpay :: Maybe BtcPayConfig,
    stripe :: Maybe StripeConfig,
    service :: Maybe ServiceConfig,
    reconcile :: Maybe ReconcileConfig,
    throttle :: ThrottleConfig
  }
  deriving (Eq, Show)

-- | Read, parse and validate `badge_service.ini`. Returns 'Left' naming the file, and naming
--   the offending key where there is one, on: a missing file, an unparsable ini, an unknown
--   key in a known section, a missing `[issuer]` or `[codes]` section, or a half-configured
--   `[btcpay]`/`[stripe]` section.
readBadgeServiceConfig :: FilePath -> IO (Either String BadgeServiceConfig)
readBadgeServiceConfig path = do
  exists <- doesFileExist path
  if not exists
    then pure $ configError path "file not found"
    else do
      parsed <- readIniFile path
      pure $ case parsed of
        Left e -> configError path e
        Right ini -> parseBadgeServiceConfig path ini

parseBadgeServiceConfig :: FilePath -> Ini -> Either String BadgeServiceConfig
parseBadgeServiceConfig path ini = do
  issuerCfg <- parseIssuer path ini
  codesCfg <- parseCodes path ini
  webCfg <- parseWeb path ini
  btcPayCfg <- parseBtcPay path ini
  stripeCfg <- parseStripe path ini
  serviceCfg <- parseService path ini
  reconcileCfg <- parseReconcile path ini
  throttleCfg <- parseThrottle path ini
  if (isJust btcPayCfg || isJust stripeCfg) && not (isJust webCfg)
    then configError path "[web] section is required when a provider (btcpay or stripe) is configured"
    else
      pure
        BadgeServiceConfig
          { issuer = issuerCfg,
            codes = codesCfg,
            web = webCfg,
            btcpay = btcPayCfg,
            stripe = stripeCfg,
            service = serviceCfg,
            reconcile = reconcileCfg,
            throttle = throttleCfg
          }

issuerKeys :: [Text]
issuerKeys = ["key_file", "key_idx"]

parseIssuer :: FilePath -> Ini -> Either String IssuerConfig
parseIssuer path ini = do
  requiredSection path "issuer" ini
  checkKnownKeys path "issuer" issuerKeys ini
  keyFile <- T.unpack <$> requiredValue path "issuer" "key_file" ini
  keyIdx <- requiredInt path "issuer" "key_idx" ini
  pure IssuerConfig {issuerKeyFile = keyFile, issuerKeyIdx = keyIdx}

codesKeys :: [Text]
codesKeys = ["secret_file", "default_expiry_days"]

parseCodes :: FilePath -> Ini -> Either String CodesConfig
parseCodes path ini = do
  requiredSection path "codes" ini
  checkKnownKeys path "codes" codesKeys ini
  secretFile <- T.unpack <$> requiredValue path "codes" "secret_file" ini
  expiryDays <- requiredInt path "codes" "default_expiry_days" ini
  pure CodesConfig {codesSecretFile = secretFile, codesDefaultExpiryDays = expiryDays}

webKeys :: [Text]
webKeys = ["port", "host", "base_url", "support_contact", "behind_proxy", "web_dir"]

parseWeb :: FilePath -> Ini -> Either String (Maybe WebConfig)
parseWeb path ini
  | "web" `notElem` sections ini = Right Nothing
  | otherwise = do
      checkKnownKeys path "web" webKeys ini
      port <- requiredInt path "web" "port" ini
      baseUrl <- requiredValue path "web" "base_url" ini
      supportContact <- requiredValue path "web" "support_contact" ini
      let host = optionalValue "127.0.0.1" "web" "host" ini
          dir = T.unpack <$> optionalMaybeValue "web" "web_dir" ini
      behindProxy <- optionalBool path "web" "behind_proxy" False ini
      pure $
        Just
          WebConfig
            { webPort = port,
              webHost = host,
              webBaseUrl = baseUrl,
              webSupportContact = supportContact,
              webBehindProxy = behindProxy,
              webDir = dir
            }

btcPayKeys :: [Text]
btcPayKeys = ["url", "store_id", "api_key_file", "webhook_secret_file", "xmr_method_id", "btc_expiry_minutes", "xmr_expiry_minutes"]

btcPayRequiredKeys :: [Text]
btcPayRequiredKeys = ["url", "store_id", "api_key_file", "webhook_secret_file"]

parseBtcPay :: FilePath -> Ini -> Either String (Maybe BtcPayConfig)
parseBtcPay path ini
  | "btcpay" `notElem` sections ini = Right Nothing
  | otherwise = do
      checkKnownKeys path "btcpay" btcPayKeys ini
      case requiredKeysPresent "btcpay" btcPayRequiredKeys ini of
        [] -> Right Nothing
        present
          | length present == length btcPayRequiredKeys -> do
              url <- requiredValue path "btcpay" "url" ini
              storeId <- requiredValue path "btcpay" "store_id" ini
              apiKeyFile <- T.unpack <$> requiredValue path "btcpay" "api_key_file" ini
              webhookSecretFile <- T.unpack <$> requiredValue path "btcpay" "webhook_secret_file" ini
              let xmrMethodId = optionalValue "XMR-CHAIN" "btcpay" "xmr_method_id" ini
              btcExpiryMinutes <- optionalInt path "btcpay" "btc_expiry_minutes" 15 ini
              xmrExpiryMinutes <- optionalInt path "btcpay" "xmr_expiry_minutes" 60 ini
              pure $
                Just
                  BtcPayConfig
                    { btcPayUrl = url,
                      btcPayStoreId = storeId,
                      btcPayApiKeyFile = apiKeyFile,
                      btcPayWebhookSecretFile = webhookSecretFile,
                      btcPayXmrMethodId = xmrMethodId,
                      btcPayBtcExpiryMinutes = btcExpiryMinutes,
                      btcPayXmrExpiryMinutes = xmrExpiryMinutes
                    }
          | otherwise ->
              let missing = head $ filter (`notElem` present) btcPayRequiredKeys
               in halfConfiguredError path "btcpay" missing btcPayRequiredKeys

stripeKeys :: [Text]
stripeKeys = ["secret_key_file", "webhook_secret_file"]

parseStripe :: FilePath -> Ini -> Either String (Maybe StripeConfig)
parseStripe path ini
  | "stripe" `notElem` sections ini = Right Nothing
  | otherwise = do
      checkKnownKeys path "stripe" stripeKeys ini
      case requiredKeysPresent "stripe" stripeKeys ini of
        [] -> Right Nothing
        present
          | length present == length stripeKeys -> do
              secretKeyFile <- T.unpack <$> requiredValue path "stripe" "secret_key_file" ini
              webhookSecretFile <- T.unpack <$> requiredValue path "stripe" "webhook_secret_file" ini
              pure $ Just StripeConfig {stripeSecretKeyFile = secretKeyFile, stripeWebhookSecretFile = webhookSecretFile}
          | otherwise ->
              let missing = head $ filter (`notElem` present) stripeKeys
               in halfConfiguredError path "stripe" missing stripeKeys

parseService :: FilePath -> Ini -> Either String (Maybe ServiceConfig)
parseService path ini
  | "service" `notElem` sections ini = Right Nothing
  | otherwise = do
      checkKnownKeys path "service" ["address_file"] ini
      let addressFile = T.unpack <$> optionalMaybeValue "service" "address_file" ini
      pure $ Just ServiceConfig {serviceAddressFile = addressFile}

parseReconcile :: FilePath -> Ini -> Either String (Maybe ReconcileConfig)
parseReconcile path ini
  | "reconcile" `notElem` sections ini = Right Nothing
  | otherwise = do
      checkKnownKeys path "reconcile" ["interval_seconds"] ini
      intervalSeconds <- requiredInt path "reconcile" "interval_seconds" ini
      pure $ Just ReconcileConfig {reconcileIntervalSeconds = intervalSeconds}

defaultSignerFailureLimits :: BucketLimits
defaultSignerFailureLimits = BucketLimits {blCapacity = 10, blStartTokens = 10}

defaultGlobalFailureLimits :: BucketLimits
defaultGlobalFailureLimits = BucketLimits {blCapacity = 600, blStartTokens = 600}

defaultCatalogLimits :: BucketLimits
defaultCatalogLimits = BucketLimits {blCapacity = 600, blStartTokens = 600}

throttleKeys :: [Text]
throttleKeys =
  [ "signer_failure_capacity",
    "signer_failure_start_tokens",
    "global_failure_capacity",
    "global_failure_start_tokens",
    "catalog_capacity",
    "catalog_start_tokens"
  ]

-- | '[throttle]' is entirely optional, and so is every key within it: an absent section or
-- key falls back to the production default (B5 decision 5), so B5's own config files (which
-- never mention '[throttle]') get exactly those defaults, and only a test that wants a small
-- or pre-drained bucket needs to write this section at all.
parseThrottle :: FilePath -> Ini -> Either String ThrottleConfig
parseThrottle path ini
  | "throttle" `notElem` sections ini =
      Right ThrottleConfig {signerFailure = defaultSignerFailureLimits, globalFailure = defaultGlobalFailureLimits, catalog = defaultCatalogLimits}
  | otherwise = do
      checkKnownKeys path "throttle" throttleKeys ini
      signerCapacity <- optionalWord32 path "throttle" "signer_failure_capacity" (blCapacity defaultSignerFailureLimits) ini
      signerStart <- optionalWord32 path "throttle" "signer_failure_start_tokens" (blStartTokens defaultSignerFailureLimits) ini
      globalCapacity <- optionalWord32 path "throttle" "global_failure_capacity" (blCapacity defaultGlobalFailureLimits) ini
      globalStart <- optionalWord32 path "throttle" "global_failure_start_tokens" (blStartTokens defaultGlobalFailureLimits) ini
      catalogCapacity <- optionalWord32 path "throttle" "catalog_capacity" (blCapacity defaultCatalogLimits) ini
      catalogStart <- optionalWord32 path "throttle" "catalog_start_tokens" (blStartTokens defaultCatalogLimits) ini
      pure
        ThrottleConfig
          { signerFailure = BucketLimits {blCapacity = signerCapacity, blStartTokens = signerStart},
            globalFailure = BucketLimits {blCapacity = globalCapacity, blStartTokens = globalStart},
            catalog = BucketLimits {blCapacity = catalogCapacity, blStartTokens = catalogStart}
          }

-- Validation helpers ---------------------------------------------------------

configError :: FilePath -> String -> Either String a
configError path msg = Left (path <> ": " <> msg)

halfConfiguredError :: FilePath -> Text -> Text -> [Text] -> Either String a
halfConfiguredError path section missingKey allRequired =
  configError path $
    "section [" <> T.unpack section <> "] is missing key '" <> T.unpack missingKey
      <> "' ("
      <> T.unpack (T.intercalate ", " allRequired)
      <> " must all be set together)"

requiredKeysPresent :: Text -> [Text] -> Ini -> [Text]
requiredKeysPresent section requiredKeys ini =
  filter (\k -> either (const False) (const True) (lookupValue section k ini)) requiredKeys

requiredSection :: FilePath -> Text -> Ini -> Either String ()
requiredSection path name ini
  | name `elem` sections ini = Right ()
  | otherwise = configError path ("missing required section [" <> T.unpack name <> "]")

checkKnownKeys :: FilePath -> Text -> [Text] -> Ini -> Either String ()
checkKnownKeys path name allowed ini = case keys name ini of
  Left e -> configError path e
  Right ks -> case filter (`notElem` allowed) ks of
    [] -> Right ()
    (k : _) -> configError path ("unknown key '" <> T.unpack k <> "' in section [" <> T.unpack name <> "]")

requiredValue :: FilePath -> Text -> Text -> Ini -> Either String Text
requiredValue path section key ini = case lookupValue section key ini of
  Right v -> Right v
  Left _ -> configError path ("missing required key '" <> T.unpack key <> "' in section [" <> T.unpack section <> "]")

requiredInt :: FilePath -> Text -> Text -> Ini -> Either String Int
requiredInt path section key ini = do
  v <- requiredValue path section key ini
  case readMaybe (T.unpack v) of
    Just n -> Right n
    Nothing -> configError path ("key '" <> T.unpack key <> "' in section [" <> T.unpack section <> "] must be an integer, got: " <> T.unpack v)

optionalValue :: Text -> Text -> Text -> Ini -> Text
optionalValue def section key ini = either (const def) id (lookupValue section key ini)

optionalMaybeValue :: Text -> Text -> Ini -> Maybe Text
optionalMaybeValue section key ini = eitherToMaybe (lookupValue section key ini)

optionalInt :: FilePath -> Text -> Text -> Int -> Ini -> Either String Int
optionalInt path section key def ini = case lookupValue section key ini of
  Left _ -> Right def
  Right v -> case readMaybe (T.unpack v) of
    Just n -> Right n
    Nothing -> configError path ("key '" <> T.unpack key <> "' in section [" <> T.unpack section <> "] must be an integer, got: " <> T.unpack v)

-- | 'Word32's 'Read' instance wraps a negative literal instead of rejecting it (@"-1" ->
-- 4294967295@), so a leading '-' is rejected by hand before 'readMaybe' ever sees it.
optionalWord32 :: FilePath -> Text -> Text -> Word32 -> Ini -> Either String Word32
optionalWord32 path section key def ini = case lookupValue section key ini of
  Left _ -> Right def
  Right v
    | T.isPrefixOf "-" v -> badValue v
    | otherwise -> case readMaybe (T.unpack v) of
        Just n -> Right n
        Nothing -> badValue v
  where
    badValue v = configError path ("key '" <> T.unpack key <> "' in section [" <> T.unpack section <> "] must be a non-negative integer, got: " <> T.unpack v)

optionalBool :: FilePath -> Text -> Text -> Bool -> Ini -> Either String Bool
optionalBool path section key def ini = case lookupValue section key ini of
  Left _ -> Right def
  Right "on" -> Right True
  Right "off" -> Right False
  Right v -> configError path ("key '" <> T.unpack key <> "' in section [" <> T.unpack section <> "] must be 'on' or 'off', got: " <> T.unpack v)

-- Token buckets ---------------------------------------------------------

-- | An in-memory token bucket (B5 decision 5): the token count as of 'tbUpdatedAt', refilled
-- lazily from elapsed time whenever it is next read rather than ticked by a timer, so an idle
-- bucket costs nothing. 'tbCapacity' is carried alongside the mutable fields because it is
-- fixed per-bucket at creation (from that family's 'BucketLimits') and both the refill and the
-- 'retryAfter' calculation need it.
data TokenBucket = TokenBucket
  { tbCapacity :: Word32,
    tbTokens :: Double,
    tbUpdatedAt :: UTCTime
  }
  deriving (Eq, Show)

newTokenBucket :: BucketLimits -> UTCTime -> TokenBucket
newTokenBucket BucketLimits {blCapacity, blStartTokens} now' =
  TokenBucket {tbCapacity = blCapacity, tbTokens = fromIntegral blStartTokens, tbUpdatedAt = now'}

-- | Refills 'tb' up to 'now'', capped at capacity. Pure and total: capacity 0 simply never
-- adds anything (see 'bucketStatus' for where that would bite instead).
refillBucket :: UTCTime -> TokenBucket -> TokenBucket
refillBucket now' tb@TokenBucket {tbCapacity, tbTokens, tbUpdatedAt}
  | elapsedHours <= 0 = tb
  | otherwise = tb {tbTokens = min (fromIntegral tbCapacity) (tbTokens + elapsedHours * fromIntegral tbCapacity), tbUpdatedAt = now'}
  where
    elapsedHours = realToFrac (diffUTCTime now' tbUpdatedAt) / 3600

-- | Refills to 'now'' and reports whether >=1 token is available and, if not, the seconds
-- until one will be ('retryAfter'). Never debits -- only 'debitBucket' does that -- so calling
-- this to decide whether to proceed costs an honest, successful caller nothing.
bucketStatus :: UTCTime -> TokenBucket -> (Bool, Word32, TokenBucket)
bucketStatus now' tb0 =
  let tb@TokenBucket {tbCapacity, tbTokens} = refillBucket now' tb0
   in if tbTokens >= 1
        then (True, 0, tb)
        else (False, retryAfter tbCapacity tbTokens, tb)
  where
    -- capacity 0 never refills and has no finite retryAfter; BucketLimits' Haddock says no
    -- caller uses it, so this is an honest crash rather than a silently wrong number.
    retryAfter 0 _ = error "bucketStatus: capacity 0 bucket has no finite retryAfter"
    retryAfter capacity tokens = ceiling $ (1 - tokens) * 3600 / fromIntegral capacity

debitBucket :: TokenBucket -> TokenBucket
debitBucket tb@TokenBucket {tbTokens} = tb {tbTokens = max 0 (tbTokens - 1)}

-- | The per-signer failure-bucket family: every signer key gets its own 'TokenBucket', built
-- from 'sbLimits' the first time that key is seen. Keyed by the key's encoded bytes rather
-- than 'C.PublicKeyEd25519' itself, which has no 'Ord' instance.
data SignerBucketFamily = SignerBucketFamily
  { sbLimits :: BucketLimits,
    sbBuckets :: TVar (M.Map ByteString TokenBucket)
  }

newSignerBucketFamily :: BucketLimits -> IO SignerBucketFamily
newSignerBucketFamily limits = SignerBucketFamily limits <$> newTVarIO M.empty

peekSignerBucket :: UTCTime -> C.PublicKeyEd25519 -> SignerBucketFamily -> STM (Either Word32 ())
peekSignerBucket now' signerKey SignerBucketFamily {sbLimits, sbBuckets} = do
  buckets <- readTVar sbBuckets
  let keyBytes = strEncode signerKey
      tb0 = M.findWithDefault (newTokenBucket sbLimits now') keyBytes buckets
      (ok, retryAfter, tb') = bucketStatus now' tb0
  writeTVar sbBuckets $! M.insert keyBytes tb' buckets
  pure $ if ok then Right () else Left retryAfter

debitSignerBucket :: C.PublicKeyEd25519 -> SignerBucketFamily -> STM ()
debitSignerBucket signerKey SignerBucketFamily {sbBuckets} =
  modifyTVar' sbBuckets $ M.adjust debitBucket (strEncode signerKey)

peekGlobalBucket :: UTCTime -> TVar TokenBucket -> STM (Either Word32 ())
peekGlobalBucket now' var = do
  tb <- readTVar var
  let (ok, retryAfter, tb') = bucketStatus now' tb
  writeTVar var tb'
  pure $ if ok then Right () else Left retryAfter

-- Runtime environment ---------------------------------------------------------

-- | The single runtime value every badge service handler receives. A6 defines only the
--   parsed config, the store and the clock; later steps add the field they own (the decoded
--   code secret, the issuer key, RPC token buckets, provider clients, IP buckets).
data BadgeServiceEnv = BadgeServiceEnv
  { config :: BadgeServiceConfig,
    store :: DBStore,
    -- | Every service component reads the clock through this, and none calls
    --   'getCurrentTime' directly, so a test can advance service time without sleeping.
    now :: IO UTCTime,
    -- | Decoded once at startup from '[codes] secret_file' (rejected there if it fails to
    --   decode to at least 32 bytes): the long-lived HMAC key behind every order-derived
    --   redemption code. See 'BadgeService.Codes.deriveOrderCode' and 'loadCodeSecret'.
    codeSecret :: ByteString,
    -- | The issuer BBS secret key loaded from '[issuer] key_file' (B4): loaded once at
    --   startup, alongside 'config', so a malformed or absent key file fails fast.
    issuerKey :: BBSSecretKey,
    -- | B5 decision 5, keyed on the request's 'purchaseKey': 10 failed 'purchaseBadge{code}'
    -- redemptions per hour, burst 10. Shapes an honest client's retries only.
    signerFailureBucket :: SignerBucketFamily,
    -- | B5 decision 5: 600 failed redemptions per hour, burst 600, service-wide. The real
    -- control against a distributed guesser -- see 'signerFailureBucket''s Haddock.
    globalFailureBucket :: TVar TokenBucket,
    -- | B5 decision 5: bounds unsigned 'getBadgeCatalog' (no signer to key on), 600/hour,
    -- burst 600, service-wide. Unused until B6 wires 'getBadgeCatalog' itself.
    catalogBucket :: TVar TokenBucket
  }

newBadgeServiceEnv :: BadgeServiceConfig -> DBStore -> IO BadgeServiceEnv
newBadgeServiceEnv cfg st = do
  codeSecret <- loadCodeSecret (codesSecretFile (codes cfg))
  issuerKey <- loadIssuerKey (issuerKeyFile (issuer cfg)) (issuerKeyIdx (issuer cfg))
  now0 <- getCurrentTime
  signerFailureBucket <- newSignerBucketFamily (signerFailure (throttle cfg))
  globalFailureBucket <- newTVarIO (newTokenBucket (globalFailure (throttle cfg)) now0)
  catalogBucket <- newTVarIO (newTokenBucket (catalog (throttle cfg)) now0)
  pure BadgeServiceEnv {config = cfg, store = st, now = getCurrentTime, codeSecret, issuerKey, signerFailureBucket, globalFailureBucket, catalogBucket}

-- | The pre-processing gate for a signed 'purchaseBadge{code}' (B5 decision 5): peeks both
-- the caller's per-signer bucket and the service-wide failure budget, WITHOUT debiting either
-- -- only a failed redemption does that, via 'debitFailureBuckets'. 'Left' carries the
-- retryAfter of whichever bucket is empty (the signer bucket takes precedence when both are,
-- an arbitrary but deterministic choice).
checkFailureBuckets :: BadgeServiceEnv -> C.PublicKeyEd25519 -> IO (Either Word32 ())
checkFailureBuckets BadgeServiceEnv {now, signerFailureBucket, globalFailureBucket} signerKey = do
  now' <- now
  atomically $ do
    signerResult <- peekSignerBucket now' signerKey signerFailureBucket
    globalResult <- peekGlobalBucket now' globalFailureBucket
    pure $ case signerResult of
      Left _ -> signerResult
      Right () -> globalResult

-- | Debits one token from both failure buckets after a failed 'purchaseBadge{code}'
-- redemption (code_invalid / code_used / code_expired, including a checksum rejection that
-- never reached the database). Not called from B5: no code classifier exists yet, so no
-- redemption can fail here. B7 calls this after a failed classification; B10 asserts the
-- accounting.
debitFailureBuckets :: BadgeServiceEnv -> C.PublicKeyEd25519 -> IO ()
debitFailureBuckets BadgeServiceEnv {signerFailureBucket, globalFailureBucket} signerKey =
  atomically $ do
    debitSignerBucket signerKey signerFailureBucket
    modifyTVar' globalFailureBucket debitBucket

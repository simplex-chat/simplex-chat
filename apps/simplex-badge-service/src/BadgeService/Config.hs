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
    takeCatalogBucket,
    debitFailureBuckets,
    sweepSignerBuckets,
    sweepSignerBucketsIO,
  )
where

import BadgeService.Codes (loadCodeSecret)
import BadgeService.Credentials (loadIssuerKey)
import Control.Concurrent.STM
import Data.ByteString (ByteString)
import Data.Ini (Ini, keys, lookupValue, readIniFile, sections)
import qualified Data.Map.Strict as M
import Data.Maybe (isJust)
import Data.Functor (($>))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (UTCTime, diffUTCTime)
import Data.Word (Word32)
import qualified Network.HTTP.Client as HTTP
import Simplex.Messaging.Agent.Store.Common (DBStore)
import qualified Simplex.Messaging.Crypto as C
import Simplex.Messaging.Crypto.BBS (BBSSecretKey)
import Simplex.Messaging.Encoding.String (strEncode)
import Simplex.Messaging.Util (eitherToMaybe, safeDecodeUtf8)
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
      validateBaseUrl path baseUrl
      supportContact <- requiredValue path "web" "support_contact" ini
      validateSupportContact path supportContact
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

-- | The hosts '[web] base_url' may be reached at over plaintext http: the local mock stack (plan
-- \'10) runs everything on one of these. Compared case-insensitively, and with the brackets an
-- IPv6 literal carries in a URL stripped, because @[::1]@, @[::1]:8080@ and @LOCALHOST@ are the
-- same three hosts written differently and an operator should not have to guess the spelling.
loopbackHosts :: [Text]
loopbackHosts = ["localhost", "127.0.0.1", "::1"]

isLoopbackHost :: ByteString -> Bool
isLoopbackHost h = T.dropAround (\c -> c == '[' || c == ']') (T.toLower (safeDecodeUtf8 h)) `elem` loopbackHosts

-- | '[web] base_url' is the origin the site is reached at: it goes into a provider's return and
-- webhook URLs (E2, F1) and into the app's browser hand-off (G1), so a relative or scheme-less
-- value is not something to discover at the first payment. 'https' is required, because a card
-- return URL over plaintext is a real downgrade, EXCEPT on the loopback hosts.
validateBaseUrl :: FilePath -> Text -> Either String ()
validateBaseUrl path url = case HTTP.parseRequest (T.unpack url) :: Maybe HTTP.Request of
  Nothing -> bad "must be an absolute http:// or https:// URL"
  Just req
    | HTTP.host req == "" -> bad "must name a host"
    | HTTP.secure req -> Right ()
    | isLoopbackHost (HTTP.host req) -> Right ()
    | otherwise -> bad ("must use https unless its host is one of " <> T.unpack (T.intercalate ", " loopbackHosts))
  where
    bad why = configError path ("key 'base_url' in section [web] " <> why <> ", got: " <> T.unpack url)

-- | '[web] support_contact' is substituted into the page's one outbound link (D4). It is escaped
-- on the way in, which stops an operator's typo from breaking out of the @href@ attribute, but
-- escaping says nothing about the SCHEME: @javascript:@ survives it intact and becomes a live
-- link. The operator's own ini is inside the trust boundary, so this is a typo guard rather than
-- a defence -- but 'base_url' in this same section is validated and leaving its neighbour
-- unchecked is an arbitrary asymmetry. Allowed: an absolute @https@\/@http@ URL with a host, or a
-- @mailto:@ address, which is the one non-web way an operator plausibly publishes support.
validateSupportContact :: FilePath -> Text -> Either String ()
validateSupportContact path url
  | "mailto:" `T.isPrefixOf` T.toLower url = if T.length url > 7 then Right () else bad "names no address after mailto:"
  | otherwise = case HTTP.parseRequest (T.unpack url) :: Maybe HTTP.Request of
      Nothing -> bad "must be an absolute https:// or http:// URL, or a mailto: address"
      Just req
        | HTTP.host req == "" -> bad "must name a host"
        | otherwise -> Right ()
  where
    bad why = configError path ("key 'support_contact' in section [web] " <> why <> ", got: " <> T.unpack url)

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
      nonZeroCapacity path "signer_failure_capacity" signerCapacity
      nonZeroCapacity path "global_failure_capacity" globalCapacity
      nonZeroCapacity path "catalog_capacity" catalogCapacity
      pure
        ThrottleConfig
          { signerFailure = BucketLimits {blCapacity = signerCapacity, blStartTokens = signerStart},
            globalFailure = BucketLimits {blCapacity = globalCapacity, blStartTokens = globalStart},
            catalog = BucketLimits {blCapacity = catalogCapacity, blStartTokens = catalogStart}
          }

-- | Capacity 0 parses fine as a 'Word32' but is meaningless (a bucket that never refills has
-- no finite retryAfter, per 'BucketLimits'' Haddock) -- reject it at config-parse time, naming
-- the key, the same way every other malformed value in this file fails fast, rather than
-- letting it reach 'bucketStatus' and silently degrade one throttle to a spurious 'internal'
-- response via the catch-all.
nonZeroCapacity :: FilePath -> Text -> Word32 -> Either String ()
nonZeroCapacity path key 0 = configError path ("key '" <> T.unpack key <> "' in section [throttle] must be greater than 0 (a capacity of 0 never refills)")
nonZeroCapacity _ _ _ = Right ()

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

-- | The per-signer failure-bucket family: a signer key gets a 'TokenBucket' entry ONLY once
-- it has actually failed a redemption (via 'debitSignerBucket') -- never merely from being
-- checked ('peekSignerBucket'). This is the fix for an unbounded-memory hazard: 'purchaseKey'
-- is attacker-controlled and free to mint, 'purchaseBadge' deliberately skips the
-- pre-existing-record check (it's the normal first-purchase path), so every signed
-- 'purchaseBadge{code}' -- including one that never reaches processing, and one whose code
-- turns out valid -- reaches this bucket. A version that inserted on every peek let one cheap
-- keypair buy one permanent map entry, for free, before authentication. Because only a
-- classified failure debits (badges-rpc.md's "only a failed redemption debits a token", and
-- the B5 brief's "only a failed redemption debits a token"), and a debit also always spends
-- one token from the single shared 'globalFailureBucket' (see 'BadgeServiceEnv'), the number
-- of NEW entries creatable in any window is bounded by how many tokens that shared bucket can
-- grant in that window -- capped at its capacity (default 600) regardless of how many
-- distinct keys an attacker mints, because minting keys is free but making them each fail is
-- not. 'sweepSignerBuckets' additionally reclaims entries whose bucket has fully recovered,
-- keeping steady-state well under that cap. Keyed by the key's encoded bytes rather than
-- 'C.PublicKeyEd25519' itself, which has no 'Ord' instance.
data SignerBucketFamily = SignerBucketFamily
  { sbLimits :: BucketLimits,
    sbBuckets :: TVar (M.Map ByteString TokenBucket)
  }

newSignerBucketFamily :: BucketLimits -> IO SignerBucketFamily
newSignerBucketFamily limits = SignerBucketFamily limits <$> newTVarIO M.empty

-- | Read-only for a key with no entry yet: computes the check against an ephemeral bucket (as
-- if freshly created from 'sbLimits') WITHOUT inserting it -- a bare pre-processing check,
-- however many times repeated, from however many distinct keys, never grows the map (the
-- property 'SignerBucketFamily''s Haddock proves). A key that already has a real entry (from
-- a past debit) has its refill persisted back, which never grows the map either, only updates
-- an existing key.
peekSignerBucket :: UTCTime -> C.PublicKeyEd25519 -> SignerBucketFamily -> STM (Either Word32 ())
peekSignerBucket now' signerKey SignerBucketFamily {sbLimits, sbBuckets} = do
  buckets <- readTVar sbBuckets
  let keyBytes = strEncode signerKey
  case M.lookup keyBytes buckets of
    Nothing ->
      let (ok, retryAfter, _) = bucketStatus now' (newTokenBucket sbLimits now')
       in pure $ if ok then Right () else Left retryAfter
    Just tb0 -> do
      let (ok, retryAfter, tb') = bucketStatus now' tb0
      writeTVar sbBuckets $! M.insert keyBytes tb' buckets
      pure $ if ok then Right () else Left retryAfter

-- | The only thing that can insert a NEW key into the map (see 'SignerBucketFamily''s
-- Haddock): a first-ever failure starts that signer's bucket at 'sbLimits' (refilled to
-- 'now'', same as a fresh bucket would be) and immediately spends the one token this debit is
-- for; a key with an existing entry just has its own bucket refilled-then-debited.
debitSignerBucket :: UTCTime -> C.PublicKeyEd25519 -> SignerBucketFamily -> STM ()
debitSignerBucket now' signerKey SignerBucketFamily {sbLimits, sbBuckets} =
  modifyTVar' sbBuckets $ \buckets ->
    let keyBytes = strEncode signerKey
        tb0 = M.findWithDefault (newTokenBucket sbLimits now') keyBytes buckets
     in M.insert keyBytes (debitBucket (refillBucket now' tb0)) buckets

-- | Reclaims every signer entry whose bucket has fully recovered (refilled back to capacity)
-- as of 'now'' -- indistinguishable from a key that never failed, so safe to forget. Returns
-- the number evicted. Composes with, but does not replace, 'debitSignerBucket' never
-- inserting from a mere peek: this bounds steady-state size further, on top of the growth-rate
-- cap that holds even if this is never called.
sweepSignerBuckets :: UTCTime -> SignerBucketFamily -> STM Int
sweepSignerBuckets now' SignerBucketFamily {sbBuckets} = do
  buckets <- readTVar sbBuckets
  let recovered tb = tbTokens (refillBucket now' tb) >= fromIntegral (tbCapacity tb)
      kept = M.filter (not . recovered) buckets
  writeTVar sbBuckets kept
  pure (M.size buckets - M.size kept)

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
    -- burst 600, service-wide. Spent by 'BadgeService.Service.handleGetBadgeCatalog' (B6) via
    -- 'takeCatalogBucket', on every unsigned request.
    catalogBucket :: TVar TokenBucket
  }

-- | @clock@ becomes the env's 'now' and is also what the two service-wide buckets are
-- initialised against, so every instant the service reads -- a handler's, a bucket's refill
-- origin -- comes from one source. Production passes 'getCurrentTime'
-- ('BadgeService.Options.serviceClock'); a test passes a settable clock so it can advance
-- service time (a bucket's refill window, a month boundary) without sleeping.
newBadgeServiceEnv :: BadgeServiceConfig -> DBStore -> IO UTCTime -> IO BadgeServiceEnv
newBadgeServiceEnv cfg st clock = do
  codeSecret <- loadCodeSecret (codesSecretFile (codes cfg))
  issuerKey <- loadIssuerKey (issuerKeyFile (issuer cfg)) (issuerKeyIdx (issuer cfg))
  now0 <- clock
  signerFailureBucket <- newSignerBucketFamily (signerFailure (throttle cfg))
  globalFailureBucket <- newTVarIO (newTokenBucket (globalFailure (throttle cfg)) now0)
  catalogBucket <- newTVarIO (newTokenBucket (catalog (throttle cfg)) now0)
  pure BadgeServiceEnv {config = cfg, store = st, now = clock, codeSecret, issuerKey, signerFailureBucket, globalFailureBucket, catalogBucket}

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

-- | The gate for an UNSIGNED 'getBadgeCatalog' (B5 decision 5). Unlike the failure buckets,
-- this one is spent by every request, not only by failures: an unsigned catalog request has
-- no signer to key on and no notion of "failing", so the only thing that can bound it is the
-- request itself. Peek and debit are one STM transaction so two concurrent requests cannot
-- both see the last token. A signed request never reaches here -- it is bounded by having a
-- purchase row at all, which 'checkSignerRecord' already requires.
takeCatalogBucket :: BadgeServiceEnv -> IO (Either Word32 ())
takeCatalogBucket BadgeServiceEnv {now, catalogBucket} = do
  now' <- now
  atomically $ do
    tb <- readTVar catalogBucket
    let (ok, retryAfter, tb') = bucketStatus now' tb
    if ok
      then writeTVar catalogBucket (debitBucket tb') $> Right ()
      else writeTVar catalogBucket tb' $> Left retryAfter

-- | Debits one token from both failure buckets after a failed 'purchaseBadge{code}'
-- redemption (code_invalid / code_used / code_expired, including a checksum rejection that
-- never reached the database). Not called from B5: no code classifier exists yet, so no
-- redemption can fail here. B7 calls this after a failed classification; B10 asserts the
-- accounting.
debitFailureBuckets :: BadgeServiceEnv -> C.PublicKeyEd25519 -> IO ()
debitFailureBuckets BadgeServiceEnv {now, signerFailureBucket, globalFailureBucket} signerKey = do
  now' <- now
  atomically $ do
    debitSignerBucket now' signerKey signerFailureBucket
    modifyTVar' globalFailureBucket debitBucket

-- | Sweeps the per-signer bucket map (see 'SignerBucketFamily''s Haddock), using the env's
-- injectable clock rather than 'getCurrentTime' directly, so a test can prove eviction without
-- sleeping. Not run on a timer in B5: the map only ever gains an entry via 'debitFailureBuckets'
-- (see its Haddock), which nothing in B5 calls yet (no code classifier exists), so the map is
-- provably empty for the whole of this step regardless of sweeping. Exported so B7 (which
-- starts calling 'debitFailureBuckets' for real) or whichever step owns the service's
-- background-thread lifecycle can wire this on a timer without redoing the eviction logic.
sweepSignerBucketsIO :: BadgeServiceEnv -> IO Int
sweepSignerBucketsIO BadgeServiceEnv {now, signerFailureBucket} = do
  now' <- now
  atomically $ sweepSignerBuckets now' signerFailureBucket

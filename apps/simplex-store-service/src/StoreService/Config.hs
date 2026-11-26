{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module StoreService.Config
  ( ListenerConfig (..),
    BTCPayConfig (..),
    StripeConfig (..),
    SpeedPolicy (..),
    PollConfig (..),
    AppleStoreConfig (..),
    AppleServerApi (..),
    PlayStoreConfig (..),
    BadgeIssuerKey (..),
    GroupConfig (..),
    NamesConfig (..),
    ChainConfig (..),
    ServiceConfig (..),
    defaultExpiryMinutes,
    defaultSessionMinutes,
    providerTimeoutMicros,
    providerCheckoutTimeoutMicros,
    defaultLogLevel,
    localChainId,
    maxNameYears,
    isLoopbackHost,
    boundedDecimal,
    readServiceConfig,
    iniSections,
    unknownKeys,
  )
where

import qualified Control.Exception as E
import Control.Applicative ((<|>))
import Control.Logger.Simple (LogLevel (..), logNote, logWarn)
import Control.Monad (foldM, forM_, mfilter, unless, when)
import qualified Data.Attoparsec.ByteString.Char8 as A
import Data.Attoparsec.Text (char, endOfInput, option, parseOnly, satisfy, skipSpace, skipWhile, takeText, takeWhile1)
import Data.Bifunctor (first)
import qualified Data.ByteArray as BA
import Data.ByteArray.Encoding (Base (..), convertFromBase)
import qualified Data.ByteString.Char8 as B
import Data.Char (GeneralCategory (..), generalCategory, isAsciiLower, isAsciiUpper, isControl, isDigit, isHexDigit)
import Data.List (intercalate, isSuffixOf, nub, partition)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as L
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeLatin1, decodeUtf8', encodeUtf8)
import Data.Time.Clock (UTCTime, addUTCTime, getCurrentTime)
import Data.Time.Format.ISO8601 (iso8601ParseM)
import Data.Word (Word64)
import qualified Network.HTTP.Client as HTTP
import Simplex.Chat.Badges.Code (maxNameLength, minNameLength, minNameYears)
import Simplex.Chat.Library.Commands (mkValidName)
import Simplex.Messaging.Crypto.BBS (BBSSecretKey)
import Simplex.Messaging.Crypto.Secp256k1 (mkPrivateKey)
import Simplex.Messaging.Encoding.String (TextEncoding (..), strDecode)
import Simplex.Messaging.Eth.Address (Address)
import Simplex.Messaging.Parsers (parseAll)
import Simplex.Messaging.SimplexName (LabelHash, SimplexTLD (..), labelHash)
import Simplex.Messaging.Util (eitherToMaybe, tshow)
import StoreService.Names.Abi (abiWordSize, parseText)
import StoreService.Names.Chain (ChainConfig (..))
import StoreService.Names.Label (LabelError (..), validLabel)
import System.IO.Error (ioeGetErrorString)
import Text.Read (readMaybe)

data ListenerConfig = ListenerConfig
  { lHost :: Text,
    lBadgesPort :: Int,
    lBadgesSite :: Maybe FilePath,
    lTrustForwardedFor :: Bool,
    lNamesSite :: Maybe (Int, FilePath),
    lMetricsPort :: Maybe Int
  }
  deriving (Eq, Show)

-- | Sent by name, since BTCPay's numbering is not in speed order.
data SpeedPolicy = HighSpeed | MediumSpeed | LowMediumSpeed | LowSpeed
  deriving (Bounded, Enum, Eq, Show)

speedPolicies :: [SpeedPolicy]
speedPolicies = [minBound .. maxBound]

instance TextEncoding SpeedPolicy where
  textEncode = \case
    HighSpeed -> "HighSpeed"
    MediumSpeed -> "MediumSpeed"
    LowMediumSpeed -> "LowMediumSpeed"
    LowSpeed -> "LowSpeed"
  textDecode = \case
    "HighSpeed" -> Just HighSpeed
    "MediumSpeed" -> Just MediumSpeed
    "LowMediumSpeed" -> Just LowMediumSpeed
    "LowSpeed" -> Just LowSpeed
    _ -> Nothing

data BTCPayConfig = BTCPayConfig
  { bHost :: Text,
    bApiKey :: Text,
    bStoreId :: Text,
    bWebhookSecret :: Text,
    bExpiryMinutes :: Int,
    bSpeedPolicy :: SpeedPolicy,
    bPaymentTolerance :: Double,
    bTimeoutMicros :: Int,
    bCheckoutTimeoutMicros :: Int
  }
  deriving (Eq)

-- Hand-written to keep the api key and webhook secret out of logs.
instance Show BTCPayConfig where
  show BTCPayConfig {bHost, bStoreId} = "btcpay " <> T.unpack bHost <> " store " <> T.unpack bStoreId

data StripeConfig = StripeConfig
  { sSecretKey :: Text,
    sPublishableKey :: Text,
    sWebhookSecret :: Text,
    sSessionMinutes :: Int,
    sHost :: Text,
    sTimeoutMicros :: Int,
    sCheckoutTimeoutMicros :: Int
  }
  deriving (Eq)

-- Hand-written to keep the restricted key and signing secret out of logs.
instance Show StripeConfig where
  show StripeConfig {sHost} = "stripe " <> T.unpack sHost

data PollConfig = PollConfig {pWaitingSeconds :: Int, pIdleSeconds :: Int}
  deriving (Eq, Show)

data AppleStoreConfig = AppleStoreConfig
  { aBundleId :: Text,
    aRootCertificates :: NonEmpty FilePath,
    aServerApi :: Maybe AppleServerApi -- without it, App Store subscriptions are not verified
  }
  deriving (Eq, Show)

-- | The App Store Server API key, which subscription checks sign their requests with.
data AppleServerApi = AppleServerApi
  { aKeyId :: Text,
    aIssuerId :: Text,
    aPrivateKeyFile :: FilePath,
    aApiHost :: Text
  }
  deriving (Eq, Show)

data PlayStoreConfig = PlayStoreConfig
  { gPackageName :: Text,
    gServiceAccountFile :: FilePath,
    gApiHost :: Text,
    gTokenUrl :: Text
  }
  deriving (Eq, Show)

data BadgeIssuerKey = BadgeIssuerKey
  { keyIdx :: Int,
    secretKey :: BBSSecretKey
  }
  deriving (Eq)

-- BBSSecretKey derives Show, so this is written out to keep the secret out of logs and errors
instance Show BadgeIssuerKey where
  show BadgeIssuerKey {keyIdx} = "issuer key " <> show keyIdx

data GroupConfig = GroupConfig
  { gDisplayName :: Text,
    gDescription :: Maybe Text,
    gNameCodesPerDay :: Int
  }
  deriving (Eq, Show)

data NamesConfig = NamesConfig
  { nTLD :: SimplexTLD,
    nYears :: Int,
    nTermStart :: UTCTime,
    nTermSlackSeconds :: Int,
    nRevealMarginSeconds :: Int,
    nMaxDryRunFailures :: Int,
    nMaxSentFailures :: Int,
    -- | Names are registered only with prepaid codes: every order that would take money for one is refused.
    nCodesOnly :: Bool,
    -- | The owner record edits relayed for one name in any 365 days, each a transaction whose gas the service pays.
    nRecordEditsPerYear :: Int,
    nMinPriceCentsPerYear :: Int,
    -- | What each community name is for, keyed by the label hash, as search receives a name.
    nCommunity :: Map LabelHash Text,
    nChain :: ChainConfig
  }
  deriving (Eq)

data ServiceConfig = ServiceConfig
  { listener :: ListenerConfig,
    btcpay :: Maybe BTCPayConfig,
    stripe :: Maybe StripeConfig,
    poll :: PollConfig,
    appleStore :: Maybe AppleStoreConfig,
    playStore :: Maybe PlayStoreConfig,
    issuer :: Maybe BadgeIssuerKey,
    -- | Trusted beside the clients' keys; only with names on the local chain.
    devIssuerKeys :: [BadgeIssuerKey],
    group :: Maybe GroupConfig,
    serviceLogLevel :: LogLevel,
    names :: Maybe NamesConfig,
    -- Local testing only; anyone who can reach the service can mint badges with any well-formed receipt.
    devAcceptUnverifiedStoreReceipts :: Bool,
    -- Local testing only; every issue request for a subscription asks its store and banks the months it charges.
    devCheckStoreDuringCoverage :: Bool,
    -- Local testing only; credits a purchase the store itself calls a test, which no real buyer pays for.
    devCreditTestPurchases :: Bool
  }
  deriving (Eq)

defaultExpiryMinutes :: Int
defaultExpiryMinutes = 60

defaultStripeHost :: Text
defaultStripeHost = "https://api.stripe.com"

defaultSessionMinutes :: Int
defaultSessionMinutes = 60

providerTimeoutMicros, providerCheckoutTimeoutMicros :: Int
providerTimeoutMicros = 10 * 1000000
providerCheckoutTimeoutMicros = 30 * 1000000

defaultLogLevel :: LogLevel
defaultLogLevel = LogNote

logLevels :: [(Text, LogLevel)]
logLevels = [("trace", LogTrace), ("debug", LogDebug), ("info", LogInfo), ("note", LogNote), ("warn", LogWarn), ("error", LogError)]

defaultPlayApiHost :: Text
defaultPlayApiHost = "https://androidpublisher.googleapis.com"

-- | Production: it answers 404 for a Sandbox transaction, which a test deployment reaches through api_host.
defaultAppleApiHost :: Text
defaultAppleApiHost = "https://api.storekit.itunes.apple.com"

appleSandboxHost :: Text
appleSandboxHost = "https://api.storekit-sandbox.itunes.apple.com"

defaultPlayTokenUrl :: Text
defaultPlayTokenUrl = "https://oauth2.googleapis.com/token"

-- | At most one day, so a card order expires long before the poller stops checking it at 72 hours.
minSessionMinutes, maxSessionMinutes :: Int
minSessionMinutes = 1
maxSessionMinutes = 1440

isLoopbackHost :: Text -> Bool
isLoopbackHost h = h `elem` ["127.0.0.1", "::1", "localhost"]

httpUrl :: Text -> Maybe HTTP.Request
httpUrl = HTTP.parseRequest . T.unpack

onLoopback :: HTTP.Request -> Bool
onLoopback = isLoopbackHost . T.toLower . T.dropAround (\c -> c == '[' || c == ']') . decodeLatin1 . HTTP.host

httpsUrl :: String -> Text -> Either String Text
httpsUrl k v
  | maybe False HTTP.secure (httpUrl v) = Right v
  | otherwise = Left (k <> " must be an absolute https URL")

publishableKey :: Text -> Bool
publishableKey k = case T.stripPrefix "pk_live_" k <|> T.stripPrefix "pk_test_" k of
  Just rest -> not (T.null rest) && T.all (\c -> isAsciiUpper c || isAsciiLower c || isDigit c) rest
  Nothing -> False

testSecretKey :: Text -> Bool
testSecretKey k = any (`T.isPrefixOf` k) ["sk_test_", "rk_test_"]

testPublishableKey :: Text -> Bool
testPublishableKey = T.isPrefixOf "pk_test_"

maxCommunityTextLength :: Int
maxCommunityTextLength = 200

defaultNameCodesPerDay :: Int
defaultNameCodesPerDay = 10

defaultRecordEditsPerYear, minRecordEditsPerYear, maxRecordEditsPerYear :: Int
defaultRecordEditsPerYear = 3
minRecordEditsPerYear = 1
maxRecordEditsPerYear = 100

defaultMinPriceCentsPerYear :: Int
defaultMinPriceCentsPerYear = 5000

-- | anvil's chain id, the only chain [issuer] dev_keys are accepted on.
localChainId :: Word64
localChainId = 31337

sepoliaChainId :: Word64
sepoliaChainId = 11155111

-- | 5 gwei.
defaultMaxFeePerGas :: Word64
defaultMaxFeePerGas = 5_000_000_000

defaultTermStart :: String
defaultTermStart = "2027-01-01T00:00:00Z"

maxNameYears :: Int
maxNameYears = 10

-- | The builds' paths in the Docker image; a run outside it sets both.
defaultBadgesStaticDir, defaultNamesStaticDir :: FilePath
defaultBadgesStaticDir = "/srv/badges"
defaultNamesStaticDir = "/srv/names"

defaultBadgesPort, defaultNamesPort :: Int
defaultBadgesPort = 8080
defaultNamesPort = 8081

-- | Keys of earlier releases, each refused with what replaced it.
retiredListenerKeys :: [(Text, String)]
retiredListenerKeys =
  [ ("port", "listener.port is now listener.badges_port"),
    ("static_dir", "listener.static_dir is now listener.badges_static_dir"),
    ("webapp_export_dir", exportRemoved "webapp_export_dir"),
    ("badges_export_dir", exportRemoved "badges_export_dir"),
    ("names_export_dir", exportRemoved "names_export_dir"),
    ("names_hosts", "listener.names_hosts is replaced by listener.names_port: serve the names site on its own port")
  ]

exportRemoved :: Text -> String
exportRemoved k = "listener." <> T.unpack k <> " is removed: the service writes no site, as the pages read their settings from /api/config; serve the build as it is"

-- | The [listener] keys only the names site reads.
namesListenerKeys :: [Text]
namesListenerKeys = ["names_static_dir", "names_port"]

-- | Well below 100, where BTCPay settles an invoice for one satoshi.
maxTolerance :: Double
maxTolerance = 10

-- | A Sandbox receipt verifies like a real one, offline too, so test purchases are credited only beside the Sandbox's
-- server API; and the Sandbox charges no one, so without them only the production host can credit a subscription.
checkAppleHost :: Bool -> AppleStoreConfig -> Either String ()
checkAppleHost creditTestPurchases AppleStoreConfig {aServerApi} = case (creditTestPurchases, aApiHost <$> aServerApi) of
  (False, Nothing) -> Right ()
  (False, Just h) | h == defaultAppleApiHost -> Right ()
  (True, Just h) | h == appleSandboxHost -> Right ()
  (False, _) -> Left $ "apple.api_host must be " <> T.unpack defaultAppleApiHost <> " unless [dev] credit_test_purchases = on, as no other host credits a real subscription"
  (True, _) -> Left $ "[dev] credit_test_purchases = on needs [apple]'s server API key with api_host = " <> T.unpack appleSandboxHost <> ", as a Sandbox receipt would otherwise be credited as a real purchase"

boundedDecimal :: Integral a => a -> a -> A.Parser a
boundedDecimal lowest highest = do
  n <- A.decimal
  if n >= toInteger lowest && n <= toInteger highest then pure (fromInteger n) else fail "out of bounds"

readServiceConfig :: FilePath -> IO (Either String ServiceConfig)
readServiceConfig path =
  -- read as UTF-8 whatever the locale, which is ASCII in a container with none set
  E.try (B.readFile path) >>= \case
    Left (e :: E.IOException) -> pure (Left (ioeGetErrorString e))
    Right bytes -> either (const $ pure $ Left "is not UTF-8") readText $ decodeUtf8' bytes
  where
    readText text = case iniSections text of
      Left e -> pure (Left e)
      Right written -> do
        now <- getCurrentTime
        let (sales, ignored) = partition salesKey (unknownKeys written)
            cfg
              | null sales = parseConfig now $ M.fromList $ map (fmap M.fromList) written
              | otherwise = Left ("nothing reads " <> T.unpack (T.intercalate ", " sales) <> ", and a mistyped [names] or [chain] setting would change what is sold: correct or remove it")
        mapM_ (\k -> logWarn (T.pack path <> ": nothing reads " <> k <> ", so it was ignored")) ignored
        forM_ (eitherToMaybe cfg >>= names) $ \NamesConfig {nCommunity} ->
          logNote ("names: " <> tshow (M.size nCommunity) <> " community names loaded")
        pure cfg

iniSections :: Text -> Either String [(Text, [(Text, Text)])]
iniSections text = written <$> foldM addLine (("", []), []) (zip [1 :: Int ..] (T.lines text))
  where
    written (current, earlier) = reverse $ map (fmap reverse) (current : earlier)
    addLine (current@(section, settings), earlier) (n, line)
      | Right () <- parseOnly lineEndP line = Right (current, earlier)
      | Right name <- parseOnly headerP line =
          if name `elem` map fst (current : earlier)
            then Left ("[" <> T.unpack name <> "] is written twice")
            else Right ((name, []), current : earlier)
      | Right (key, value) <- parseOnly settingP line =
          if key `elem` map fst settings
            then Left (T.unpack (if T.null section then key else section <> "." <> key) <> " is written twice")
            else Right ((section, (key, value) : settings), earlier)
      | otherwise = Left ("line " <> show n <> " is malformed: write a [section] header, a key = value setting or a ; comment")
    headerP = skipSpace *> char '[' *> mfilter (not . T.null) (T.strip <$> takeWhile1 (\c -> c /= '[' && c /= ']')) <* char ']' <* lineEndP
    settingP = (,) <$> (skipSpace *> (T.strip <$> takeWhile1 (\c -> c /= '=' && c /= ':'))) <* char '=' <*> (T.strip <$> takeText)
    lineEndP = skipSpace <* option () (satisfy (\c -> c == ';' || c == '#') *> skipWhile (const True)) <* endOfInput

knownSettings :: [(Text, [Text])]
knownSettings =
  -- The retired keys are known, so parseConfig refuses them rather than a warning calling them ignored.
  [ ("listener", ["host", "badges_port", "badges_static_dir", "serve_webapp", "trust_forwarded_for", "metrics_port"] <> namesListenerKeys <> map fst retiredListenerKeys),
    ("btcpay", ["host", "api_key", "store_id", "webhook_secret", "expiry_minutes", "speed_policy", "payment_tolerance"]),
    ("stripe", ["secret_key", "publishable_key", "webhook_secret", "session_minutes"]),
    ("poll", ["waiting_seconds", "idle_seconds"]),
    ("apple", ["bundle_id", "root_certificates", "api_key_id", "api_issuer_id", "api_private_key_file", "api_host"]),
    ("google", ["package_name", "service_account_file"]),
    ("group", ["display_name", "description", "name_codes_per_day"]),
    ("dev", ["accept_unverified_store_receipts", "check_store_during_coverage", "credit_test_purchases"]),
    ("issuer", ["index", "private_key", "dev_keys"]),
    ("log", ["level"]),
    ("names", ["tld", "years", "term_start", "term_slack_seconds", "reveal_margin_seconds", "max_dry_run_failures", "max_sent_failures", "codes_only", "record_edits_per_year", "min_price_cents_per_year"]),
    ("chain", ["rpc_url", "chain_id", "controller", "sponsor", "base_registrar", "resolver", "service_key", "confirmations", "gas_limit_cap", "max_fee_per_gas"])
  ]

-- | A mistyped [names] or [chain] setting, such as codes_only, would change what is sold, so it is refused rather than ignored.
salesKey :: Text -> Bool
salesKey k = any (`T.isPrefixOf` k) ["names.", "chain."]

unknownKeys :: [(Text, [(Text, Text)])] -> [Text]
unknownKeys written = beforeAnySection <> unknownSections <> settings
  where
    beforeAnySection = [k <> ", written above the first section header" | ("", kvs) <- written, (k, _) <- kvs]
    -- [community] keys are labels, which parseConfig checks.
    unknownSections = ["[" <> s <> "]" | (s, _) <- written, not (T.null s), s `notElem` ("community" : map fst knownSettings)]
    settings =
      [ section <> "." <> key
        | (section, known) <- knownSettings,
          Just kvs <- [lookup section written],
          (key, _) <- kvs,
          key `notElem` known
      ]

parseConfig :: UTCTime -> Map Text (Map Text Text) -> Either String ServiceConfig
parseConfig now ini = do
  case [e | (k, e) <- retiredListenerKeys, isJust (look "listener" k)] of
    [] -> pure ()
    es -> Left (intercalate "; " es)
  lHost <- optional "listener" "host" "127.0.0.1"
  lBadgesPort <- listenerPort "badges_port" defaultBadgesPort
  serveWebapp <- bool "listener" "serve_webapp" True
  let lBadgesSite = if serveWebapp then Just (maybe defaultBadgesStaticDir T.unpack (present "listener" "badges_static_dir")) else Nothing
  lTrustForwardedFor <- bool "listener" "trust_forwarded_for" False
  lNamesSite <- namesSite serveWebapp lBadgesPort
  lMetricsPort <- optionalPort "metrics_port"
  when (any (\p -> p == lBadgesPort || Just p == (fst <$> lNamesSite)) lMetricsPort) $
    Left "listener.metrics_port must differ from listener.badges_port and listener.names_port"
  btc <- btcpaySection
  str <- stripeSection
  iss <- issuerSection
  grp <- groupSection
  serviceLogLevel <- logLevel
  nms <- namesSection
  when (isJust nms && fmap bSpeedPolicy btc == Just HighSpeed) $
    Left "btcpay.speed_policy = HighSpeed settles an invoice with no confirmation, which a double spend would turn into a free name, so it is refused with [names]"
  devKeys <- devKeysSetting
  unless (null devKeys || maybe False onLocalChain nms) $
    Left ("issuer.dev_keys is accepted only with [names] on the local chain, [chain] chain_id = " <> show localChainId)
  let stripeTestKeys = testSecretKey . sSecretKey <$> str
  unless (null devKeys) $ do
    when (stripeTestKeys == Just False) $
      Left "issuer.dev_keys is refused with live Stripe keys: stripe.secret_key and stripe.publishable_key must be test keys"
    forM_ btc $ \BTCPayConfig {bHost} ->
      unless (maybe False onLoopback (httpUrl bHost)) $
        Left "issuer.dev_keys is refused with a remote BTCPay: btcpay.host must be on loopback, 127.0.0.1, ::1 or localhost"
  when (stripeTestKeys == Just True && maybe False (not . onTestChain) nms) $
    Left ("Stripe test keys are accepted with [names] only on a test chain, [chain] chain_id = " <> show localChainId <> " or " <> show sepoliaChainId)
  pWaitingSeconds <- num "poll" "waiting_seconds" 1 maxBound 3
  pIdleSeconds <- num "poll" "idle_seconds" 1 maxBound 60
  apple <- appleSection
  google <- googleSection
  devUnverifiedReceipts <- bool "dev" "accept_unverified_store_receipts" False
  when (devUnverifiedReceipts && (isJust apple || isJust google)) $
    Left "[dev] accept_unverified_store_receipts must be off where [apple] or [google] verifies store receipts"
  devCheckStoreDuringCoverage <- bool "dev" "check_store_during_coverage" False
  devCreditTestPurchases <- bool "dev" "credit_test_purchases" False
  mapM_ (checkAppleHost devCreditTestPurchases) apple
  when (devCheckStoreDuringCoverage && not (devUnverifiedReceipts || devCreditTestPurchases)) $
    Left "[dev] check_store_during_coverage needs accept_unverified_store_receipts or credit_test_purchases = on, as only a dev deployment may ask a store while coverage runs"
  pure
    ServiceConfig
      { listener = ListenerConfig {lHost, lBadgesPort, lBadgesSite, lTrustForwardedFor, lNamesSite, lMetricsPort},
        btcpay = btc,
        stripe = str,
        poll = PollConfig {pWaitingSeconds, pIdleSeconds},
        appleStore = apple,
        playStore = google,
        issuer = iss,
        devIssuerKeys = devKeys,
        group = grp,
        serviceLogLevel,
        names = nms,
        devAcceptUnverifiedStoreReceipts = devUnverifiedReceipts,
        devCheckStoreDuringCoverage,
        devCreditTestPurchases
      }
  where
    hasSection s = M.member s ini
    look s k = M.lookup s ini >>= M.lookup k
    present s k = mfilter (not . T.null) (T.strip <$> look s k)
    required s k = maybe (Left (T.unpack s <> "." <> T.unpack k <> " is required")) Right (present s k)
    optional s k d = Right (fromMaybe d (present s k))
    listenerPort k d = fromMaybe d <$> optionalPort k
    optionalPort k = optionalNum "listener" k 1 65535
    unusedListenerKey k why = Left ("listener." <> T.unpack k <> " is set but " <> why <> ", so nothing would use it")
    namesSite serve badgesPort
      | not (hasSection "names") = case filter (isJust . look "listener") namesListenerKeys of
          k : _ -> unusedListenerKey k "[names] is absent"
          [] -> Right Nothing
      | not serve = Nothing <$ when (isJust (look "listener" "names_port")) (unusedListenerKey "names_port" "serve_webapp is off")
      | otherwise = do
          namesPort <- listenerPort "names_port" defaultNamesPort
          when (namesPort == badgesPort) $ Left "listener.names_port must differ from listener.badges_port"
          Right (Just (namesPort, maybe defaultNamesStaticDir T.unpack (present "listener" "names_static_dir")))
    num :: (Integral a, Bounded a, Show a) => Text -> Text -> a -> a -> a -> Either String a
    num s k lowest highest d = fromMaybe d <$> optionalNum s k lowest highest
    optionalNum :: (Integral a, Bounded a, Show a) => Text -> Text -> a -> a -> Either String (Maybe a)
    optionalNum s k lowest highest = mapM (boundedSetting (T.unpack s <> "." <> T.unpack k) lowest highest) (look s k)
    boundedSetting :: (Integral a, Bounded a, Show a) => String -> a -> a -> Text -> Either String a
    boundedSetting name lowest highest v =
      first (const (name <> " must be a whole number " <> range)) $ parseAll (boundedDecimal lowest highest) (encodeUtf8 v)
      where
        range
          | highest == maxBound && not overflow = "of at least " <> show lowest
          | otherwise = "from " <> show lowest <> " to " <> show highest
        overflow = either (const False) (> toInteger highest) (parseAll (A.signed A.decimal) (encodeUtf8 v) :: Either String Integer)
    bool s k d = case fmap (T.toLower . T.strip) (look s k) of
      Nothing -> Right d
      Just "on" -> Right True
      Just "off" -> Right False
      Just other -> Left (T.unpack k <> " must be on or off, not " <> T.unpack other)
    issuerSection
      | not (hasSection "issuer") = Right Nothing
      | otherwise = do
          keyIdx <- issuerIndex
          secretKey <- issuerSecret
          pure (Just BadgeIssuerKey {keyIdx, secretKey})
    issuerIndex = required "issuer" "index" >>= boundedSetting "issuer.index" 1 maxBound
    issuerSecret = required "issuer" "private_key" >>= keySecret "issuer.private_key"
    keySecret k v =
      either (const (Left (k <> " is not a valid issuer secret; use the value from `simplex-chat badge keygen`"))) Right $
        strDecode (B.pack (T.unpack v))
    -- No error quotes an entry, since each holds a secret.
    devKeysSetting = do
      ks <- mapM devKey $ filter (not . T.null) $ map T.strip $ maybe [] (T.splitOn ",") (present "issuer" "dev_keys")
      let idxs = map (\BadgeIssuerKey {keyIdx} -> keyIdx) ks
      unless (length (nub idxs) == length idxs) $ Left "issuer.dev_keys names an index twice"
      pure ks
    devKey entry = case T.breakOn ":" entry of
      (i, sk) | not (T.null sk) -> do
        keyIdx <- boundedSetting "issuer.dev_keys index" 1 maxBound (T.strip i)
        secretKey <- keySecret "issuer.dev_keys secret" (T.strip (T.drop 1 sk))
        pure BadgeIssuerKey {keyIdx, secretKey}
      _ -> Left "issuer.dev_keys must be <index>:<secret> entries separated by commas"
    onLocalChain NamesConfig {nChain = ChainConfig {cChainId}} = cChainId == localChainId
    onTestChain NamesConfig {nChain = ChainConfig {cChainId}} = cChainId `elem` [localChainId, sepoliaChainId]
    btcpaySection
      | not (hasSection "btcpay") = Right Nothing
      | otherwise = do
          bHost <- required "btcpay" "host" >>= httpsUrl "btcpay.host"
          bApiKey <- required "btcpay" "api_key"
          bStoreId <- required "btcpay" "store_id"
          bWebhookSecret <- required "btcpay" "webhook_secret"
          bExpiryMinutes <- num "btcpay" "expiry_minutes" 1 maxBound defaultExpiryMinutes
          bSpeedPolicy <- speedPolicy
          bPaymentTolerance <- tolerance
          pure (Just BTCPayConfig {bHost, bApiKey, bStoreId, bWebhookSecret, bExpiryMinutes, bSpeedPolicy, bPaymentTolerance, bTimeoutMicros = providerTimeoutMicros, bCheckoutTimeoutMicros = providerCheckoutTimeoutMicros})
    speedPolicy = case look "btcpay" "speed_policy" of
      Nothing -> Right MediumSpeed
      Just v -> case textDecode (T.strip v) of
        Just p -> Right p
        Nothing ->
          Left
            ( "btcpay.speed_policy must be one of "
                <> T.unpack (T.intercalate ", " (map textEncode speedPolicies))
                <> ", not "
                <> T.unpack (T.strip v)
            )
    tolerance = case look "btcpay" "payment_tolerance" of
      Nothing -> Right 0.5
      Just v -> case readMaybe (T.unpack (T.strip v)) of
        Just d | d >= 0 && d <= maxTolerance -> Right d
        _ -> Left ("btcpay.payment_tolerance must be a percentage between 0 and " <> show maxTolerance)
    groupSection
      | not (hasSection "group") = Right Nothing
      | otherwise = do
          gDisplayName <- required "group" "display_name" >>= validGroupName
          gNameCodesPerDay <- num "group" "name_codes_per_day" 0 maxBound defaultNameCodesPerDay
          pure (Just GroupConfig {gDisplayName, gDescription = present "group" "description", gNameCodesPerDay})
    -- The core refuses a group name that mkValidName would change, so it is rejected here.
    validGroupName n =
      let valid = T.pack (mkValidName (T.unpack n))
       in if n == valid
            then Right n
            else Left ("group.display_name \"" <> T.unpack n <> "\" is not a valid group name" <> closest valid)
    closest valid
      | T.null valid = ""
      | otherwise = ", the closest valid name is \"" <> T.unpack valid <> "\""
    logLevel = case look "log" "level" of
      Nothing -> Right defaultLogLevel
      Just v -> case lookup (T.strip v) logLevels of
        Just l -> Right l
        Nothing -> Left ("log.level must be one of " <> T.unpack (T.intercalate ", " (map fst logLevels)) <> ", not " <> T.unpack (T.strip v))
    stripeSection
      | not (hasSection "stripe") = Right Nothing
      | otherwise = do
          sSecretKey <- required "stripe" "secret_key"
          sPublishableKey <- required "stripe" "publishable_key"
          -- it is written into the sites' HTML as it is
          unless (publishableKey sPublishableKey) $ Left "stripe.publishable_key must be pk_live_ or pk_test_ followed by letters and digits"
          when (testSecretKey sSecretKey /= testPublishableKey sPublishableKey) $
            Left "stripe.secret_key and stripe.publishable_key must be both live keys or both test keys"
          sWebhookSecret <- required "stripe" "webhook_secret"
          sSessionMinutes <- num "stripe" "session_minutes" minSessionMinutes maxSessionMinutes defaultSessionMinutes
          let sHost = defaultStripeHost
          pure (Just StripeConfig {sSecretKey, sPublishableKey, sWebhookSecret, sSessionMinutes, sHost, sTimeoutMicros = providerTimeoutMicros, sCheckoutTimeoutMicros = providerCheckoutTimeoutMicros})
    appleSection
      | not (hasSection "apple") = Right Nothing
      | otherwise = do
          aBundleId <- required "apple" "bundle_id"
          aRootCertificates <- rootCertificates
          aServerApi <- appleServerApi
          pure (Just AppleStoreConfig {aBundleId, aRootCertificates, aServerApi})
    rootCertificates = do
      v <- required "apple" "root_certificates"
      maybe (Left "apple.root_certificates must list at least one file") Right $
        L.nonEmpty $ map T.unpack $ filter (not . T.null) $ map T.strip $ T.splitOn "," v
    -- the key is three settings, and one or two of them would leave subscriptions silently unverified
    appleServerApi = case (present "apple" "api_key_id", present "apple" "api_issuer_id", present "apple" "api_private_key_file") of
      (Nothing, Nothing, Nothing) -> Right Nothing
      (Just aKeyId, Just aIssuerId, Just keyFile) -> do
        aApiHost <- optional "apple" "api_host" defaultAppleApiHost >>= httpsUrl "apple.api_host" . T.toLower . T.dropWhileEnd (== '/')
        pure (Just AppleServerApi {aKeyId, aIssuerId, aPrivateKeyFile = T.unpack keyFile, aApiHost})
      _ -> Left "apple.api_key_id, apple.api_issuer_id and apple.api_private_key_file are required together"
    googleSection
      | not (hasSection "google") = Right Nothing
      | otherwise = do
          gPackageName <- required "google" "package_name"
          gServiceAccountFile <- T.unpack <$> required "google" "service_account_file"
          pure (Just PlayStoreConfig {gPackageName, gServiceAccountFile, gApiHost = defaultPlayApiHost, gTokenUrl = defaultPlayTokenUrl})
    namesSection
      | not (hasSection "names") = case filter hasSection ["chain", "community"] of
          s : _ -> Left ("[" <> T.unpack s <> "] is set but [names] is absent, so nothing would use it")
          [] -> Right Nothing
      | not (hasSection "chain") = Left "[names] needs a [chain] section"
      | otherwise = do
          nTLD <- tldSetting
          nYears <- num "names" "years" minNameYears maxNameYears minNameYears
          nTermStart <- termStart
          nTermSlackSeconds <- num "names" "term_slack_seconds" 0 maxBound 3600
          when (addUTCTime (fromIntegral nTermSlackSeconds) nTermStart > addUTCTime (365 * 86400) now) $
            Left "names.term_start, with names.term_slack_seconds, must be at most 365 days ahead, as the controller registers a name for at most 4015 days, a community name for 1095"
          nRevealMarginSeconds <- num "names" "reveal_margin_seconds" 0 maxBound 900
          nMaxDryRunFailures <- num "names" "max_dry_run_failures" 1 maxBound 5
          nMaxSentFailures <- num "names" "max_sent_failures" 1 maxBound 5
          nCodesOnly <- bool "names" "codes_only" False
          nRecordEditsPerYear <- num "names" "record_edits_per_year" minRecordEditsPerYear maxRecordEditsPerYear defaultRecordEditsPerYear
          nMinPriceCentsPerYear <- num "names" "min_price_cents_per_year" 1 maxBound defaultMinPriceCentsPerYear
          nCommunity <- communityList nTLD
          nChain <- chainSection
          pure (Just NamesConfig {nTLD, nYears, nTermStart, nTermSlackSeconds, nRevealMarginSeconds, nMaxDryRunFailures, nMaxSentFailures, nCodesOnly, nRecordEditsPerYear, nMinPriceCentsPerYear, nCommunity, nChain})
    tldSetting =
      required "names" "tld" >>= \case
        "simplex" -> Right TLDSimplex
        "testing" -> Right TLDTesting
        other -> Left ("names.tld must be simplex or testing, not " <> T.unpack other)
    termStart = do
      let raw = maybe defaultTermStart T.unpack (present "names" "term_start")
          refuse = Left ("names.term_start must be an ISO 8601 UTC time such as " <> defaultTermStart)
      -- time-1.15 parses a bare date and a zoneless datetime. A term boundary
      -- is an instant, so the text needs a time and an explicit UTC designator.
      if utcInstant raw
        then maybe refuse Right (iso8601ParseM raw :: Maybe UTCTime)
        else refuse
    utcInstant s = case break (\c -> c == 'T' || c == 't') s of
      (_, _ : time) -> not (null time) && utcSuffix time
      _ -> False
    utcSuffix time = any (`isSuffixOf` time) ["Z", "z", "+00:00", "-00:00"]
    communityList tld = M.fromList <$> mapM (communityEntry tld) (maybe [] M.keys (M.lookup "community" ini))
    communityEntry tld label = do
      let k = "community." <> T.unpack label
          text = fromMaybe "" (look "community" label)
      valid <- first (labelRefusal k) (validLabel tld label)
      when (T.null text || T.length text > maxCommunityTextLength) $ Left (k <> " must be 1 to " <> show maxCommunityTextLength <> " characters")
      when (T.any invisible text) $ Left (k <> " has a control, format or line separator character")
      pure (labelHash valid, text)
    invisible c = isControl c || generalCategory c `elem` [Format, LineSeparator, ParagraphSeparator]
    labelRefusal k = \case
      LELength -> k <> " must be " <> show minNameLength <> " to " <> show maxNameLength <> " characters"
      LEChars -> k <> " must be lower-case letters and digits, with single hyphens between them"
    -- No error below quotes a value, since the URL and the key are secrets.
    chainSection = do
      cRpcUrl <- rpcUrl
      cChainId <- required "chain" "chain_id" >>= boundedSetting "chain.chain_id" 1 maxBound
      cController <- address "controller"
      cSponsor <- address "sponsor"
      cBaseRegistrar <- address "base_registrar"
      cResolver <- address "resolver"
      cServiceKey <- serviceKey
      cConfirmations <- num "chain" "confirmations" 1 maxBound 3
      cGasLimitCap <- num "chain" "gas_limit_cap" 1 maxBound 8000000
      cMaxFeePerGas <- num "chain" "max_fee_per_gas" 1 maxBound defaultMaxFeePerGas
      pure ChainConfig {cRpcUrl, cChainId, cController, cSponsor, cBaseRegistrar, cResolver, cServiceKey, cConfirmations, cGasLimitCap, cMaxFeePerGas}
    -- http only to a node on the same machine, as the URL can carry a provider's API key
    rpcUrl = do
      v <- required "chain" "rpc_url"
      case httpUrl v of
        Just req | HTTP.secure req || onLoopback req -> Right (T.unpack v)
        _ -> Left "chain.rpc_url must be an https URL, or http to a node on loopback: 127.0.0.1, ::1 or localhost"
    address k = do
      v <- required "chain" k
      either (const (Left ("chain." <> T.unpack k <> " is not an Ethereum address"))) Right (strDecode (encodeUtf8 v) :: Either String Address)
    serviceKey = do
      v <- required "chain" "service_key"
      let bad = Left "chain.service_key must be 0x and 64 hex digits of a valid secp256k1 key"
      case parseText serviceKeyP v of
        Right bytes -> either (const bad) Right (mkPrivateKey (BA.convert bytes))
        Left _ -> bad
    serviceKeyP :: A.Parser B.ByteString
    serviceKeyP = A.string "0x" *> A.count (2 * abiWordSize) (A.satisfy isHexDigit) >>= either fail pure . convertFromBase Base16 . B.pack

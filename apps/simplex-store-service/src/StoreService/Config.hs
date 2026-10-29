{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module StoreService.Config
  ( ListenerConfig (..),
    BTCPayConfig (..),
    StripeConfig (..),
    SpeedPolicy (..),
    speedPolicyName,
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
    minNameYears,
    localChainId,
    isLoopbackHost,
    readServiceConfig,
    unknownKeys,
  )
where

import qualified Control.Exception as E
import StoreService.Log (logInfo, logWarn)
import Control.Applicative (many)
import Control.Monad (forM_, mfilter, unless, when)
import Data.Attoparsec.Combinator (lookAhead)
import Data.Attoparsec.Text (Parser, endOfInput, isEndOfLine, parseOnly, satisfy, skipMany, skipSpace, skipWhile)
import Data.Bifunctor (first)
import qualified Data.ByteArray as BA
import Data.ByteArray.Encoding (Base (Base16), convertFromBase)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Char (GeneralCategory (..), generalCategory, isControl)
import Data.Ini (Ini, iniGlobals, iniParser, keyValueParser, keys, lookupValue, sectionParser, sections)
import Data.List (nub, (\\))
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as L
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text.IO as TIO
import Data.Time.Clock (UTCTime)
import Data.Time.Format.ISO8601 (iso8601ParseM)
import Data.Word (Word64)
import Simplex.Chat.Badges.Code (maxNameLength, minNameLength)
import Simplex.Chat.Library.Commands (mkValidName)
import Simplex.Messaging.Crypto.BBS (BBSSecretKey)
import Simplex.Messaging.Crypto.Secp256k1 (Secp256k1PrivateKey, mkPrivateKey)
import Simplex.Messaging.Encoding.String (strDecode)
import Simplex.Messaging.Eth.Address (Address)
import Simplex.Messaging.SimplexName (LabelHash (..), SimplexTLD (..), labelHash)
import Simplex.Messaging.Util (tshow)
import StoreService.Names.Label (LabelError (..), validLabel)
import System.IO.Error (ioeGetErrorString)
import Text.Read (readMaybe)

data ListenerConfig = ListenerConfig
  { lHost :: Text,
    lBadgesPort :: Int,
    lBadgesStaticDir :: FilePath,
    lServeWebapp :: Bool,
    lBadgesExportDir :: Maybe FilePath,
    lTrustForwardedFor :: Bool,
    -- | @Nothing@ without @[names]@.
    lNamesStaticDir :: Maybe FilePath,
    lNamesExportDir :: Maybe FilePath,
    -- | @Nothing@ unless the names site is served, which takes @[names]@ and @serve_webapp@ on.
    lNamesPort :: Maybe Int
  }
  deriving (Eq, Show)

-- | Sent by name, since BTCPay's numbering is not in speed order.
data SpeedPolicy = HighSpeed | MediumSpeed | LowMediumSpeed | LowSpeed
  deriving (Bounded, Enum, Eq, Show)

speedPolicies :: [SpeedPolicy]
speedPolicies = [minBound .. maxBound]

speedPolicyName :: SpeedPolicy -> Text
speedPolicyName = \case
  HighSpeed -> "HighSpeed"
  MediumSpeed -> "MediumSpeed"
  LowMediumSpeed -> "LowMediumSpeed"
  LowSpeed -> "LowSpeed"

data BTCPayConfig = BTCPayConfig
  { bHost :: Text,
    bApiKey :: Text,
    bStoreId :: Text,
    bWebhookSecret :: Text,
    bExpiryMinutes :: Int,
    bSpeedPolicy :: SpeedPolicy,
    bPaymentTolerance :: Double
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
    sHost :: Text
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
    gDescription :: Maybe Text
  }
  deriving (Eq, Show)

data NamesConfig = NamesConfig
  { nTLD :: SimplexTLD,
    nYears :: Int,
    nTermStart :: UTCTime,
    nTermSlackSeconds :: Int,
    nRevealMarginSeconds :: Int,
    nMaxOrderMoves :: Int,
    -- | What each community name is for, keyed by the label hash's bytes, as search receives a name.
    nCommunity :: Map ByteString Text,
    nChain :: ChainConfig
  }
  deriving (Eq, Show)

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
    names :: Maybe NamesConfig,
    -- Local testing only; anyone who can reach the service can mint badges with any well-formed receipt.
    devAcceptUnverifiedStoreReceipts :: Bool,
    -- Local testing only; every issue request for a subscription asks its store and banks the months it charges.
    devCheckStoreDuringCoverage :: Bool,
    -- Local testing only; credits a purchase the store itself calls a test, which no real buyer pays for.
    devCreditTestPurchases :: Bool
  }
  deriving (Eq, Show)

defaultExpiryMinutes :: Int
defaultExpiryMinutes = 60

defaultStripeHost :: Text
defaultStripeHost = "https://api.stripe.com"

defaultSessionMinutes :: Int
defaultSessionMinutes = 60

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

-- | The host of an absolute URL, lower-cased, without user info, port or an IPv6 literal's brackets.
urlHost :: Text -> Text
urlHost url = T.toLower $ case T.stripPrefix "[" hostPort of
  Just literal -> T.takeWhile (/= ']') literal
  Nothing -> T.takeWhile (/= ':') hostPort
  where
    authority = T.takeWhile (`notElem` ['/', '?', '#']) (T.drop 2 (snd (T.breakOn "//" url)))
    hostPort = snd (T.breakOnEnd "@" authority)

-- | An http host would carry the API key in the clear on every call.
requireHttps :: Text -> Either String Text
requireHttps u
  | "https://" `T.isPrefixOf` u = Right u
  | otherwise = Left "btcpay.host must be an absolute https URL"

maxCommunityTextLength :: Int
maxCommunityTextLength = 200

-- | The controller refuses a registration under 730 days.
minNameYears :: Int
minNameYears = 2

-- | anvil's chain id, the only chain [issuer] dev_keys are accepted on.
localChainId :: Word64
localChainId = 31337

-- | 100 gwei.
defaultMaxFeePerGas :: Word64
defaultMaxFeePerGas = 100000000000

defaultTermStart :: String
defaultTermStart = "2027-01-01T00:00:00Z"

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
    ("webapp_export_dir", "listener.webapp_export_dir is now listener.badges_export_dir"),
    ("names_hosts", "listener.names_hosts is replaced by listener.names_port: serve the names site on its own port")
  ]

-- | The [listener] keys only the names site reads.
namesListenerKeys :: [Text]
namesListenerKeys = ["names_static_dir", "names_export_dir", "names_port"]

serviceKeyBytes :: Int
serviceKeyBytes = 32

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

readServiceConfig :: FilePath -> IO (Either String ServiceConfig)
readServiceConfig path =
  E.try (TIO.readFile path) >>= \case
    Left (e :: E.IOException) -> pure (Left (ioeGetErrorString e))
    -- Data.Ini stops at the first unparseable line and keeps what it has, so a missing `=` silently drops every section below.
    Right text -> case parseOnly ((,) <$> lookAhead (many keyValueParser *> many sectionParser) <*> iniParser <* trailingNoise <* endOfInput) text of
      Left _ -> pure (Left "could not be read as an ini file: a line is malformed")
      Right (written, ini) -> case repeatedSetting written of
        Just e -> pure (Left e)
        Nothing -> do
          mapM_ (\k -> logWarn (T.pack path <> ": nothing reads " <> k <> ", so it was ignored")) (unknownKeys ini)
          let cfg = parseConfig ini
          forM_ (either (const Nothing) names cfg) $ \NamesConfig {nCommunity} ->
            logInfo ("names: " <> tshow (M.size nCommunity) <> " community names loaded")
          pure cfg

-- | Data.Ini keeps only the last section of a name and the first value of a key in a section,
-- so the sections are read in order to refuse a repeated one.
repeatedSetting :: [(Text, [(Text, Text)])] -> Maybe String
repeatedSetting written = case repeated (map fst written) of
  h : _ -> Just ("[" <> T.unpack h <> "] is written twice, and only the last of them would be read")
  [] -> case [(s, k) | (s, kvs) <- written, k <- repeated (map fst kvs)] of
    (s, k) : _ -> Just (T.unpack s <> "." <> T.unpack k <> " is written twice, and only the first of them would be read")
    [] -> Nothing

repeated :: Eq a => [a] -> [a]
repeated xs = nub (xs \\ nub xs)

-- | iniParser stops before a trailing comment, so endOfInput alone would reject a valid file.
trailingNoise :: Parser ()
trailingNoise = skipSpace *> skipMany (comment *> skipSpace)
  where
    comment = satisfy (\c -> c == ';' || c == '#') *> skipWhile (not . isEndOfLine)

knownSettings :: [(Text, [Text])]
knownSettings =
  -- The retired keys are known, so parseConfig refuses them rather than a warning calling them ignored.
  [ ("listener", ["host", "badges_port", "badges_static_dir", "serve_webapp", "badges_export_dir", "trust_forwarded_for"] <> namesListenerKeys <> map fst retiredListenerKeys),
    ("btcpay", ["host", "api_key", "store_id", "webhook_secret", "expiry_minutes", "speed_policy", "payment_tolerance"]),
    ("stripe", ["secret_key", "publishable_key", "webhook_secret", "session_minutes"]),
    ("poll", ["waiting_seconds", "idle_seconds"]),
    ("apple", ["bundle_id", "root_certificates", "api_key_id", "api_issuer_id", "api_private_key_file", "api_host"]),
    ("google", ["package_name", "service_account_file"]),
    ("group", ["display_name", "description"]),
    ("dev", ["accept_unverified_store_receipts", "check_store_during_coverage", "credit_test_purchases"]),
    ("issuer", ["index", "private_key", "dev_keys"]),
    ("names", ["tld", "years", "term_start", "term_slack_seconds", "reveal_margin_seconds", "max_order_moves"]),
    ("chain", ["rpc_url", "chain_id", "controller", "sponsor", "base_registrar", "resolver", "service_key", "confirmations", "gas_limit_cap", "max_fee_per_gas"])
  ]

unknownKeys :: Ini -> [Text]
unknownKeys ini = beforeAnySection <> unknownSections <> settings
  where
    beforeAnySection = [k <> ", written above the first section header" | (k, _) <- iniGlobals ini]
    -- [community] keys are labels, which parseConfig checks.
    unknownSections = ["[" <> s <> "]" | s <- sections ini, T.strip s `notElem` ("community" : map fst knownSettings)]
    settings =
      [ section <> "." <> key
        | (section, known) <- knownSettings,
          key <- either (const []) id (keys section ini),
          T.strip key `notElem` known
      ]

parseConfig :: Ini -> Either String ServiceConfig
parseConfig ini = do
  forM_ retiredListenerKeys $ \(k, e) -> when (isJust (look "listener" k)) (Left e)
  lHost <- optional "listener" "host" "127.0.0.1"
  lBadgesPort <- listenerPort "badges_port" defaultBadgesPort
  let lBadgesStaticDir = maybe defaultBadgesStaticDir T.unpack (present "listener" "badges_static_dir")
  lServeWebapp <- bool "listener" "serve_webapp" True
  let lBadgesExportDir = T.unpack <$> present "listener" "badges_export_dir"
  lTrustForwardedFor <- bool "listener" "trust_forwarded_for" False
  (lNamesStaticDir, lNamesExportDir, lNamesPort) <- namesSite lServeWebapp lBadgesPort
  btc <- btcpaySection
  str <- stripeSection
  iss <- issuerSection
  grp <- groupSection
  nms <- namesSection
  devKeys <- devKeysSetting
  unless (null devKeys || maybe False onLocalChain nms) $
    Left ("issuer.dev_keys is accepted only with [names] on the local chain, [chain] chain_id = " <> show localChainId)
  unless (null devKeys) $ do
    forM_ str $ \StripeConfig {sSecretKey, sPublishableKey} -> do
      unless (any (`T.isPrefixOf` sSecretKey) ["sk_test_", "rk_test_"]) $
        Left "issuer.dev_keys is refused with a live Stripe key: stripe.secret_key must be a test key, sk_test_ or rk_test_"
      unless ("pk_test_" `T.isPrefixOf` sPublishableKey) $
        Left "issuer.dev_keys is refused with a live Stripe key: stripe.publishable_key must be a test key, pk_test_"
    forM_ btc $ \BTCPayConfig {bHost} ->
      unless (isLoopbackHost (urlHost bHost)) $
        Left "issuer.dev_keys is refused with a remote BTCPay: btcpay.host must be on loopback, 127.0.0.1, ::1 or localhost"
  pWaitingSeconds <- cadence "waiting_seconds" 3
  pIdleSeconds <- cadence "idle_seconds" 60
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
      { listener = ListenerConfig {lHost, lBadgesPort, lBadgesStaticDir, lServeWebapp, lBadgesExportDir, lTrustForwardedFor, lNamesStaticDir, lNamesExportDir, lNamesPort},
        btcpay = btc,
        stripe = str,
        poll = PollConfig {pWaitingSeconds, pIdleSeconds},
        appleStore = apple,
        playStore = google,
        issuer = iss,
        devIssuerKeys = devKeys,
        group = grp,
        names = nms,
        devAcceptUnverifiedStoreReceipts = devUnverifiedReceipts,
        devCheckStoreDuringCoverage,
        devCreditTestPurchases
      }
  where
    hasSection s = s `elem` sections ini
    look s k = either (const Nothing) Just (lookupValue s k ini)
    present s k = mfilter (not . T.null) (T.strip <$> look s k)
    required s k = maybe (Left (T.unpack s <> "." <> T.unpack k <> " is required")) Right (present s k)
    optional s k d = Right (fromMaybe d (present s k))
    listenerPort k d = do
      p <- num "listener" k d
      if 1 <= p && p <= 65535 then Right p else Left ("listener." <> T.unpack k <> " must be between 1 and 65535")
    unusedListenerKey k why = Left ("listener." <> T.unpack k <> " is set but " <> why <> ", so nothing would use it")
    namesSite serve badgesPort
      | not (hasSection "names") = case filter (isJust . look "listener") namesListenerKeys of
          k : _ -> unusedListenerKey k "[names] is absent"
          [] -> Right (Nothing, Nothing, Nothing)
      | otherwise = do
          when (not serve && isJust (look "listener" "names_port")) $ unusedListenerKey "names_port" "serve_webapp is off"
          namesPort <- if serve then Just <$> listenerPort "names_port" defaultNamesPort else Right Nothing
          when (namesPort == Just badgesPort) $ Left "listener.names_port must differ from listener.badges_port"
          let staticDir = maybe defaultNamesStaticDir T.unpack (present "listener" "names_static_dir")
          Right (Just staticDir, T.unpack <$> present "listener" "names_export_dir", namesPort)
    -- Integer, because readMaybe at Int wraps silently, reading 2^64+4 as 4.
    num s k d = case look s k of
      Nothing -> Right d
      Just v -> case readMaybe (T.unpack (T.strip v)) of
        Just n | n >= toInteger (minBound :: Int), n <= toInteger (maxBound :: Int) -> Right (fromInteger n)
        _ -> Left (T.unpack k <> " must be a whole number")
    bool s k d = case fmap (T.toLower . T.strip) (look s k) of
      Nothing -> Right d
      Just "on" -> Right True
      Just "off" -> Right False
      Just other -> Left (T.unpack k <> " must be on or off, not " <> T.unpack other)
    -- num accepts 0 and negatives, and a zero cadence is a busy loop.
    cadence k d = do
      v <- num "poll" k d
      if v >= 1 then Right v else Left ("poll." <> T.unpack k <> " must be at least 1 second")
    issuerSection
      | not (hasSection "issuer") = Right Nothing
      | otherwise = do
          keyIdx <- issuerIndex
          secretKey <- issuerSecret
          pure (Just BadgeIssuerKey {keyIdx, secretKey})
    issuerIndex = required "issuer" "index" >>= keyIndex "issuer.index"
    issuerSecret = required "issuer" "private_key" >>= keySecret "issuer.private_key"
    keyIndex k v = case readMaybe (T.unpack v) of
      Just n | n >= 1, n <= toInteger (maxBound :: Int) -> Right (fromInteger n)
      _ -> Left (k <> " must be a positive whole number")
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
        keyIdx <- keyIndex "issuer.dev_keys index" (T.strip i)
        secretKey <- keySecret "issuer.dev_keys secret" (T.strip (T.drop 1 sk))
        pure BadgeIssuerKey {keyIdx, secretKey}
      _ -> Left "issuer.dev_keys must be <index>:<secret> entries separated by commas"
    onLocalChain NamesConfig {nChain = ChainConfig {cChainId}} = cChainId == localChainId
    btcpaySection
      | not (hasSection "btcpay") = Right Nothing
      | otherwise = do
          bHost <- required "btcpay" "host" >>= requireHttps
          bApiKey <- required "btcpay" "api_key"
          bStoreId <- required "btcpay" "store_id"
          bWebhookSecret <- required "btcpay" "webhook_secret"
          bExpiryMinutes <- expiryMinutes
          bSpeedPolicy <- speedPolicy
          bPaymentTolerance <- tolerance
          pure (Just BTCPayConfig {bHost, bApiKey, bStoreId, bWebhookSecret, bExpiryMinutes, bSpeedPolicy, bPaymentTolerance})
    -- A negative window puts BTCPay's startDate in the future, so every poll comes back empty.
    expiryMinutes = do
      v <- num "btcpay" "expiry_minutes" defaultExpiryMinutes
      if v >= 1 then Right v else Left "btcpay.expiry_minutes must be at least 1 minute"
    speedPolicy = case look "btcpay" "speed_policy" of
      Nothing -> Right MediumSpeed
      Just v -> case lookup (T.strip v) [(speedPolicyName p, p) | p <- speedPolicies] of
        Just p -> Right p
        Nothing ->
          Left
            ( "btcpay.speed_policy must be one of "
                <> T.unpack (T.intercalate ", " (map speedPolicyName speedPolicies))
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
          pure (Just GroupConfig {gDisplayName, gDescription = present "group" "description"})
    -- The core refuses a group name that mkValidName would change, so it is rejected here.
    validGroupName n =
      let valid = T.pack (mkValidName (T.unpack n))
       in if n == valid
            then Right n
            else Left ("group.display_name \"" <> T.unpack n <> "\" is not a valid group name" <> closest valid)
    closest valid
      | T.null valid = ""
      | otherwise = ", the closest valid name is \"" <> T.unpack valid <> "\""
    stripeSection
      | not (hasSection "stripe") = Right Nothing
      | otherwise = do
          sSecretKey <- required "stripe" "secret_key"
          sPublishableKey <- required "stripe" "publishable_key"
          sWebhookSecret <- required "stripe" "webhook_secret"
          sSessionMinutes <- sessionMinutes
          let sHost = defaultStripeHost
          pure (Just StripeConfig {sSecretKey, sPublishableKey, sWebhookSecret, sSessionMinutes, sHost})
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
        aApiHost <- T.toLower . T.dropWhileEnd (== '/') <$> optional "apple" "api_host" defaultAppleApiHost
        unless ("https://" `T.isPrefixOf` aApiHost) $ Left "apple.api_host must be an absolute https URL"
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
          nYears <- num "names" "years" minNameYears
          when (nYears < minNameYears) $ Left ("names.years must be at least " <> show minNameYears)
          nTermStart <- termStart
          nTermSlackSeconds <- atLeast 0 "names" "term_slack_seconds" 3600
          nRevealMarginSeconds <- atLeast 0 "names" "reveal_margin_seconds" 900
          nMaxOrderMoves <- atLeast 0 "names" "max_order_moves" 3
          nCommunity <- communityList nTLD
          nChain <- chainSection
          pure (Just NamesConfig {nTLD, nYears, nTermStart, nTermSlackSeconds, nRevealMarginSeconds, nMaxOrderMoves, nCommunity, nChain})
    tldSetting =
      required "names" "tld" >>= \case
        "simplex" -> Right TLDSimplex
        "testing" -> Right TLDTesting
        other -> Left ("names.tld must be simplex or testing, not " <> T.unpack other)
    termStart =
      maybe (Left ("names.term_start must be an ISO 8601 UTC time such as " <> defaultTermStart)) Right $
        iso8601ParseM (maybe defaultTermStart T.unpack (present "names" "term_start"))
    communityList tld = M.fromList <$> mapM (communityEntry tld) (either (const []) id (keys "community" ini))
    communityEntry tld label = do
      let k = "community." <> T.unpack label
          text = fromMaybe "" (look "community" label)
      valid <- first (labelRefusal k) (validLabel tld label)
      when (T.null text || T.length text > maxCommunityTextLength) $ Left (k <> " must be 1 to " <> show maxCommunityTextLength <> " characters")
      when (T.any invisible text) $ Left (k <> " has a control, format or line separator character")
      let LabelHash h = labelHash valid
      pure (h, text)
    invisible c = isControl c || generalCategory c `elem` [Format, LineSeparator, ParagraphSeparator]
    labelRefusal k = \case
      LELength -> k <> " must be " <> show minNameLength <> " to " <> show maxNameLength <> " characters"
      LEChars -> k <> " must be lower-case letters and digits, with single hyphens between them"
    atLeast lowest s k d = do
      v <- num s k d
      if v >= lowest then Right v else Left (T.unpack s <> "." <> T.unpack k <> " must be at least " <> show lowest)
    -- No error below quotes a value, since the URL and the key are secrets.
    chainSection = do
      cRpcUrl <- rpcUrl
      cChainId <- positiveWord "chain_id" Nothing
      cController <- address "controller"
      cSponsor <- address "sponsor"
      cBaseRegistrar <- address "base_registrar"
      cResolver <- address "resolver"
      cServiceKey <- serviceKey
      cConfirmations <- atLeast 0 "chain" "confirmations" 3
      cGasLimitCap <- positiveWord "gas_limit_cap" (Just 8000000)
      cMaxFeePerGas <- positiveWord "max_fee_per_gas" (Just defaultMaxFeePerGas)
      pure ChainConfig {cRpcUrl, cChainId, cController, cSponsor, cBaseRegistrar, cResolver, cServiceKey, cConfirmations, cGasLimitCap, cMaxFeePerGas}
    rpcUrl = do
      v <- required "chain" "rpc_url"
      unless (any (`T.isPrefixOf` v) ["http://", "https://"]) $ Left "chain.rpc_url must be an http or https URL"
      Right (T.unpack v)
    positiveWord k d = case (present "chain" k, d) of
      (Nothing, Just v) -> Right v
      (Nothing, Nothing) -> Left ("chain." <> T.unpack k <> " is required")
      (Just v, _) -> case readMaybe (T.unpack v) :: Maybe Integer of
        Just n | n >= 1, n <= toInteger (maxBound :: Word64) -> Right (fromInteger n)
        _ -> Left ("chain." <> T.unpack k <> " must be a positive whole number")
    address k = do
      v <- required "chain" k
      either (const (Left ("chain." <> T.unpack k <> " is not an Ethereum address"))) Right (strDecode (encodeUtf8 v) :: Either String Address)
    serviceKey = do
      v <- required "chain" "service_key"
      let bad = Left "chain.service_key must be 0x and 64 hex digits of a valid secp256k1 key"
      case T.stripPrefix "0x" v of
        Just h | T.length h == 2 * serviceKeyBytes, Right bytes <- convertFromBase Base16 (encodeUtf8 h) -> either (const bad) Right (mkPrivateKey (BA.convert (bytes :: B.ByteString)))
        _ -> bad
    sessionMinutes = do
      v <- num "stripe" "session_minutes" defaultSessionMinutes
      if v >= minSessionMinutes && v <= maxSessionMinutes
        then Right v
        else Left ("stripe.session_minutes must be between " <> show minSessionMinutes <> " and " <> show maxSessionMinutes <> " minutes")

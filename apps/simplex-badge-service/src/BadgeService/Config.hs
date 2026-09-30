{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module BadgeService.Config
  ( ListenerConfig (..),
    BTCPayConfig (..),
    StripeConfig (..),
    SpeedPolicy (..),
    speedPolicyName,
    PollConfig (..),
    BadgeIssuerKey (..),
    GroupConfig (..),
    ServiceConfig (..),
    defaultExpiryMinutes,
    defaultSessionMinutes,
    readServiceConfig,
    unknownKeys,
  )
where

import qualified Control.Exception as E
import BadgeService.Log (logWarn)
import Control.Monad (mfilter)
import Data.Attoparsec.Text (Parser, endOfInput, isEndOfLine, parseOnly, satisfy, skipMany, skipSpace, skipWhile)
import qualified Data.ByteString.Char8 as B
import Data.Ini (Ini, iniGlobals, iniParser, keys, lookupValue, sections)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Simplex.Chat.Library.Commands (mkValidName)
import Simplex.Messaging.Crypto.BBS (BBSSecretKey)
import Simplex.Messaging.Encoding.String (strDecode)
import System.IO.Error (ioeGetErrorString)
import Text.Read (readMaybe)

data ListenerConfig = ListenerConfig
  { lHost :: Text,
    lPort :: Int,
    lStaticDir :: FilePath,
    lServeWebapp :: Bool,
    lWebappExportDir :: Maybe FilePath,
    lTrustForwardedFor :: Bool
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

data ServiceConfig = ServiceConfig
  { listener :: ListenerConfig,
    btcpay :: Maybe BTCPayConfig,
    stripe :: Maybe StripeConfig,
    poll :: PollConfig,
    issuer :: Maybe BadgeIssuerKey,
    group :: Maybe GroupConfig
  }
  deriving (Eq, Show)

defaultExpiryMinutes :: Int
defaultExpiryMinutes = 60

defaultStripeHost :: Text
defaultStripeHost = "https://api.stripe.com"

defaultSessionMinutes :: Int
defaultSessionMinutes = 60

-- | At most one day, so a card order expires long before the poller stops checking it at 72 hours.
minSessionMinutes, maxSessionMinutes :: Int
minSessionMinutes = 1
maxSessionMinutes = 1440

-- | An http host would carry the API key in the clear on every call.
requireHttps :: Text -> Either String Text
requireHttps u
  | "https://" `T.isPrefixOf` u = Right u
  | otherwise = Left "btcpay.host must be an absolute https URL"

-- | Well below 100, where BTCPay settles an invoice for one satoshi.
maxTolerance :: Double
maxTolerance = 10

readServiceConfig :: FilePath -> IO (Either String ServiceConfig)
readServiceConfig path =
  E.try (TIO.readFile path) >>= \case
    Left (e :: E.IOException) -> pure (Left (ioeGetErrorString e))
    -- Data.Ini stops at the first unparseable line and keeps what it has, so a missing `=` silently drops every section below.
    Right text -> case parseOnly (iniParser <* trailingNoise <* endOfInput) text of
      Left _ -> pure (Left "could not be read as an ini file: a line is malformed")
      Right ini -> do
        mapM_ (\k -> logWarn (T.pack path <> ": nothing reads " <> k <> ", so it was ignored")) (unknownKeys ini)
        pure (parseConfig ini)

-- | iniParser stops before a trailing comment, so endOfInput alone would reject a valid file.
trailingNoise :: Parser ()
trailingNoise = skipSpace *> skipMany (comment *> skipSpace)
  where
    comment = satisfy (\c -> c == ';' || c == '#') *> skipWhile (not . isEndOfLine)

knownSettings :: [(Text, [Text])]
knownSettings =
  [ ("listener", ["host", "port", "static_dir", "serve_webapp", "webapp_export_dir", "trust_forwarded_for"]),
    ("btcpay", ["host", "api_key", "store_id", "webhook_secret", "expiry_minutes", "speed_policy", "payment_tolerance"]),
    ("stripe", ["secret_key", "publishable_key", "webhook_secret", "session_minutes"]),
    ("poll", ["waiting_seconds", "idle_seconds"]),
    ("group", ["display_name", "description"]),
    ("issuer", ["index", "private_key"])
  ]

unknownKeys :: Ini -> [Text]
unknownKeys ini = beforeAnySection <> unknownSections <> settings
  where
    beforeAnySection = [k <> ", written above the first section header" | (k, _) <- iniGlobals ini]
    unknownSections = ["[" <> s <> "]" | s <- sections ini, T.strip s `notElem` map fst knownSettings]
    settings =
      [ section <> "." <> key
        | (section, known) <- knownSettings,
          key <- either (const []) id (keys section ini),
          T.strip key `notElem` known
      ]

parseConfig :: Ini -> Either String ServiceConfig
parseConfig ini = do
  lStaticDir <- T.unpack <$> required "listener" "static_dir"
  lHost <- optional "listener" "host" "127.0.0.1"
  lPort <- do
    p <- num "listener" "port" 8080
    if 1 <= p && p <= 65535 then Right p else Left "listener.port must be between 1 and 65535"
  lServeWebapp <- bool "listener" "serve_webapp" True
  let lWebappExportDir = T.unpack <$> present "listener" "webapp_export_dir"
  lTrustForwardedFor <- bool "listener" "trust_forwarded_for" False
  btc <- btcpaySection
  str <- stripeSection
  iss <- issuerSection
  grp <- groupSection
  pWaitingSeconds <- cadence "waiting_seconds" 3
  pIdleSeconds <- cadence "idle_seconds" 60
  pure
    ServiceConfig
      { listener = ListenerConfig {lHost, lPort, lStaticDir, lServeWebapp, lWebappExportDir, lTrustForwardedFor},
        btcpay = btc,
        stripe = str,
        poll = PollConfig {pWaitingSeconds, pIdleSeconds},
        issuer = iss,
        group = grp
      }
  where
    hasSection s = s `elem` sections ini
    look s k = either (const Nothing) Just (lookupValue s k ini)
    present s k = mfilter (not . T.null) (T.strip <$> look s k)
    required s k = maybe (Left (T.unpack s <> "." <> T.unpack k <> " is required")) Right (present s k)
    optional s k d = Right (fromMaybe d (present s k))
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
    issuerIndex = do
      v <- required "issuer" "index"
      case readMaybe (T.unpack v) of
        Just n | n >= 1, n <= toInteger (maxBound :: Int) -> Right (fromInteger n)
        _ -> Left "issuer.index must be a positive whole number"
    issuerSecret = do
      v <- required "issuer" "private_key"
      either (const (Left "issuer.private_key is not a valid issuer secret; use the value from `simplex-chat badge keygen`")) Right $
        strDecode (B.pack (T.unpack v))
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
    sessionMinutes = do
      v <- num "stripe" "session_minutes" defaultSessionMinutes
      if v >= minSessionMinutes && v <= maxSessionMinutes
        then Right v
        else Left ("stripe.session_minutes must be between " <> show minSessionMinutes <> " and " <> show maxSessionMinutes <> " minutes")

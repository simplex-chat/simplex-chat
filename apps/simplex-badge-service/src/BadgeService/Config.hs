{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module BadgeService.Config
  ( ListenerConfig (..),
    BTCPayConfig (..),
    SpeedPolicy (..),
    speedPolicyName,
    PollConfig (..),
    IssuerConfig (..),
    ServiceConfig (..),
    defaultExpiryMinutes,
    readServiceConfig,
    unknownKeys,
  )
where

import qualified Control.Exception as E
import Control.Logger.Simple (logWarn)
import Data.Attoparsec.Text (Parser, endOfInput, isEndOfLine, parseOnly, satisfy, skipMany, skipSpace, skipWhile)
import qualified Data.ByteString.Char8 as B
import Data.Ini (Ini, iniGlobals, iniParser, keys, lookupValue, sections)
import Data.List (sort)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Simplex.Messaging.Crypto.BBS (BBSSecretKey)
import Simplex.Messaging.Encoding.String (strDecode)
import System.IO.Error (ioeGetErrorString)
import Text.Read (readMaybe)

data ListenerConfig = ListenerConfig
  { lHost :: Text,
    lPort :: Int,
    lStaticDir :: FilePath,
    lTrustForwardedFor :: Bool
  }
  deriving (Eq, Show)

-- | Sent by name: BTCPay's numbering is not in speed order, so 2 would mean six
-- confirmations rather than two.
data SpeedPolicy = HighSpeed | MediumSpeed | LowMediumSpeed | LowSpeed
  deriving (Bounded, Enum, Eq, Show)

-- | BTCPay's own names, which are also what the ini accepts and what the create body sends.
-- Derived from the type, so a policy added there cannot be one the ini refuses.
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

-- written out to keep the api key and the webhook secret out of logs and errors
instance Show BTCPayConfig where
  show BTCPayConfig {bHost, bStoreId} = "btcpay " <> T.unpack bHost <> " store " <> T.unpack bStoreId

data PollConfig = PollConfig {pWaitingSeconds :: Int, pIdleSeconds :: Int}
  deriving (Eq, Show)

-- | The issuer keys this service may sign with, by the index clients verify against.
-- Only 'iDefaultIdx' signs; the rest are listed so rotating is a config change.
data IssuerConfig = IssuerConfig
  { iKeys :: Map Int BBSSecretKey,
    iDefaultIdx :: Int
  }
  deriving (Eq)

-- BBSSecretKey derives Show from ByteString, so this is written out to keep the secrets
-- out of logs and errors.
instance Show IssuerConfig where
  show IssuerConfig {iKeys, iDefaultIdx} =
    "issuer keys " <> show (M.keys iKeys) <> ", signing with " <> show iDefaultIdx

data ServiceConfig = ServiceConfig
  { listener :: ListenerConfig,
    btcpay :: Maybe BTCPayConfig,
    poll :: PollConfig,
    issuer :: Maybe IssuerConfig,
    -- signs a credential for anyone who asks over chat, with a master key this service
    -- generated and so can link: for local testing, never for a deployment
    devChatRedeem :: Bool
  }
  deriving (Eq, Show)

defaultExpiryMinutes :: Int
defaultExpiryMinutes = 60

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
    -- the caller prefixes the path, so this says only what went wrong with it
    Left (e :: E.IOException) -> pure (Left (ioeGetErrorString e))
    -- `readIniFile` stops at the first line the grammar cannot read and keeps what it has, so
    -- a missing `=` in [listener] silently takes every section below it, the provider included.
    Right text -> case parseOnly (iniParser <* trailingNoise <* endOfInput) text of
      Left _ -> pure (Left "could not be read as an ini file: a line is malformed")
      Right ini -> do
        -- a setting whose name is wrong is a setting that silently took its default, and two of
        -- them decide when money counts as received. [issuer] refuses outright, since signing
        -- with a key nobody meant is worse than not starting.
        mapM_ (\k -> logWarn (T.pack path <> ": nothing reads " <> k <> ", so it was ignored")) (unknownKeys ini)
        pure (parseConfig ini)

-- | `iniParser` stops before a comment that follows the last setting, so `endOfInput` alone
-- would call a perfectly good file malformed. Commenting out the last section is enough.
trailingNoise :: Parser ()
trailingNoise = skipSpace *> skipMany (comment *> skipSpace)
  where
    comment = satisfy (\c -> c == ';' || c == '#') *> skipWhile (not . isEndOfLine)

-- | Every setting `parseConfig` reads, by section. `[issuer]` is absent because its keys are
-- `key_<n>` and it does its own, stricter check.
knownSettings :: [(Text, [Text])]
knownSettings =
  [ ("listener", ["host", "port", "static_dir", "trust_forwarded_for"]),
    ("btcpay", ["host", "api_key", "store_id", "webhook_secret", "expiry_minutes", "speed_policy", "payment_tolerance"]),
    ("poll", ["waiting_seconds", "idle_seconds"]),
    ("dev", ["chat_redeem"])
  ]

-- | Everything in the file that nothing reads: a setting under a section that is read, and a
-- section header that is not one of ours. A mistyped `[btcpay]` disables the provider as surely
-- as a mistyped key takes a default.
unknownKeys :: Ini -> [Text]
unknownKeys ini = beforeAnySection <> unknownSections <> settings
  where
    -- the parser keeps these, and nothing else ever looks at them
    beforeAnySection = [k <> ", written above the first section header" | (k, _) <- iniGlobals ini]
    ours = map fst knownSettings <> ["issuer", "stripe"]
    unknownSections = ["[" <> s <> "]" | s <- sections ini, T.strip s `notElem` ours]
    settings =
      [ section <> "." <> key
        | (section, known) <- knownSettings,
          key <- either (const []) id (keys section ini),
          T.strip key `notElem` known
      ]

parseConfig :: Ini -> Either String ServiceConfig
parseConfig ini = do
  refuseStripe
  lStaticDir <- T.unpack <$> required "listener" "static_dir"
  lHost <- optional "listener" "host" "127.0.0.1"
  lPort <- num "listener" "port" 8080
  lTrustForwardedFor <- bool "listener" "trust_forwarded_for" False
  btc <- btcpaySection
  iss <- issuerSection
  pWaitingSeconds <- cadence "waiting_seconds" 3
  pIdleSeconds <- cadence "idle_seconds" 60
  devRedeem <- bool "dev" "chat_redeem" False
  pure
    ServiceConfig
      { listener = ListenerConfig {lHost, lPort, lStaticDir, lTrustForwardedFor},
        btcpay = btc,
        poll = PollConfig {pWaitingSeconds, pIdleSeconds},
        issuer = iss,
        devChatRedeem = devRedeem
      }
  where
    hasSection s = s `elem` sections ini
    look s k = either (const Nothing) Just (lookupValue s k ini)
    required s k = case look s k of
      Just v | not (T.null (T.strip v)) -> Right (T.strip v)
      _ -> Left (T.unpack s <> "." <> T.unpack k <> " is required")
    optional s k d = case fmap T.strip (look s k) of
      Just v | not (T.null v) -> Right v
      _ -> Right d
    -- Integer, because readMaybe at Int wraps silently: 2^64+4 would be read as 4
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
    -- `num` accepts 0 and negatives, and a zero cadence is a loop with no wait in it
    cadence k d = do
      v <- num "poll" k d
      if v >= 1 then Right v else Left ("poll." <> T.unpack k <> " must be at least 1 second")
    refuseStripe
      | hasSection "stripe" =
          Left "the [stripe] section is not supported: card payments are not implemented in this build"
      | otherwise = Right ()
    issuerSection
      | not (hasSection "issuer") = Right Nothing
      | otherwise = do
          entries <- either (const (Left "[issuer] could not be read")) Right (keys "issuer" ini)
          let named = sort [T.strip e | e <- entries]
          mapM_ knownEntry named
          ks <- M.fromList <$> mapM issuerKey [e | e <- named, e /= "default"]
          if M.null ks
            then Left "[issuer] lists no key_<n>, so the service has nothing to sign with"
            else do
              d <- required "issuer" "default"
              idx <- keyIndex d
              if M.member idx ks
                then Right (Just IssuerConfig {iKeys = ks, iDefaultIdx = idx})
                else Left ("issuer.default names " <> T.unpack d <> ", which is not listed in [issuer]")
    -- refuse anything else in the section, so a mistyped key_1 is a boot failure rather
    -- than a service signing with a key nobody meant
    knownEntry e
      | e == "default" = Right ()
      | "key_" `T.isPrefixOf` e = () <$ keyIndex e
      | otherwise = Left ("[issuer] has no setting " <> T.unpack e <> "; expected default or key_<n>")
    -- The index has one spelling: `key_01` and `key_1` would otherwise both read as 1 and collapse
    -- in the map, dropping a secret the operator wrote and never checking it.
    keyIndex :: Text -> Either String Int
    keyIndex e = case T.stripPrefix "key_" e of
      Just written
        | Just n <- readMaybe (T.unpack written),
          n > 0,
          n <= toInteger (maxBound :: Int),
          T.pack (show n) == written ->
            Right (fromInteger n)
      _ -> Left ("[issuer] " <> T.unpack e <> " must be named key_<n>, with n a positive whole number")
    issuerKey e = do
      idx <- keyIndex e
      raw <- required "issuer" e
      case strDecode (B.pack (T.unpack raw)) of
        Right sk -> Right (idx, sk)
        Left _ -> Left ("issuer." <> T.unpack e <> " is not a valid issuer secret; use the value from `simplex-chat badge keygen`")
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
    -- a negative window puts BTCPay's startDate in the future, so every poll comes back
    -- empty and we silently stop detecting payments
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
        -- 100 settles an invoice for one satoshi, which a typo for 1.00 would reach
        Just d | d >= 0 && d <= maxTolerance -> Right d
        _ -> Left ("btcpay.payment_tolerance must be a percentage between 0 and " <> show maxTolerance)

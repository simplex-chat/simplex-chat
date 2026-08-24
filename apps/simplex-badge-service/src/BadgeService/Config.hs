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
    BadgeServiceConfig (..),
    readBadgeServiceConfig,
    BadgeServiceEnv (..),
    newBadgeServiceEnv,
  )
where

import BadgeService.Codes (loadCodeSecret)
import BadgeService.Credentials (loadIssuerKey)
import Data.ByteString (ByteString)
import Data.Ini (Ini, keys, lookupValue, readIniFile, sections)
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (UTCTime, getCurrentTime)
import Simplex.Messaging.Agent.Store.Common (DBStore)
import Simplex.Messaging.Crypto.BBS (BBSSecretKey)
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

data BadgeServiceConfig = BadgeServiceConfig
  { issuer :: IssuerConfig,
    codes :: CodesConfig,
    web :: Maybe WebConfig,
    btcpay :: Maybe BtcPayConfig,
    stripe :: Maybe StripeConfig,
    service :: Maybe ServiceConfig,
    reconcile :: Maybe ReconcileConfig
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
            reconcile = reconcileCfg
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

optionalBool :: FilePath -> Text -> Text -> Bool -> Ini -> Either String Bool
optionalBool path section key def ini = case lookupValue section key ini of
  Left _ -> Right def
  Right "on" -> Right True
  Right "off" -> Right False
  Right v -> configError path ("key '" <> T.unpack key <> "' in section [" <> T.unpack section <> "] must be 'on' or 'off', got: " <> T.unpack v)

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
    issuerKey :: BBSSecretKey
  }

newBadgeServiceEnv :: BadgeServiceConfig -> DBStore -> IO BadgeServiceEnv
newBadgeServiceEnv cfg st = do
  codeSecret <- loadCodeSecret (codesSecretFile (codes cfg))
  issuerKey <- loadIssuerKey (issuerKeyFile (issuer cfg)) (issuerKeyIdx (issuer cfg))
  pure BadgeServiceEnv {config = cfg, store = st, now = getCurrentTime, codeSecret, issuerKey}

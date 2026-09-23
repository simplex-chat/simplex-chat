{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Bots.BadgeConfigTests where

import BadgeService.Config
import qualified Data.ByteString.Char8 as B
import Data.Either (isLeft)
import Data.Ini (readIniFile)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Simplex.Messaging.Encoding.String (strDecode)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import Test.Hspec
import UnliftIO.Temporary (withTempDirectory)

badgeConfigTests :: Spec
badgeConfigTests = describe "badge service config" $ do
  it "applies every documented default" testDefaults
  it "falls back to the default host when the value is blank" testBlankHost
  it "disables a provider whose section is absent" testAbsentSection
  it "refuses an incomplete provider section, naming the key" testIncompleteSection
  it "refuses a missing static_dir" testAbsentStaticDir
  it "reads serve_webapp off and a webapp_export_dir" testWebappSplitDeployment
  it "applies every documented stripe default" testStripeDefaults
  it "disables stripe when the section is absent" testStripeAbsent
  it "refuses an incomplete stripe section, naming the key" testStripeIncomplete
  it "bounds stripe session_minutes to 31-1439" testStripeSessionMinutesRange
  it "accepts HighSpeed" (testSpeedPolicyAccepted "HighSpeed" HighSpeed)
  it "accepts LowMediumSpeed" (testSpeedPolicyAccepted "LowMediumSpeed" LowMediumSpeed)
  it "accepts LowSpeed" (testSpeedPolicyAccepted "LowSpeed" LowSpeed)
  it "refuses a numeric speed policy" testSpeedPolicyName
  it "accepts a poll cadence and refuses a zero or negative one" testPollCadence
  it "accepts an expiry window and refuses a zero or negative one" testExpiryMinutes
  it "accepts a payment tolerance and refuses one that settles for a satoshi" testPaymentTolerance
  it "refuses a host that would send the api key in the clear" testHostMustBeHttps
  it "names a setting nothing reads, in every section it parses" testUnknownKeysAreNamed
  it "refuses a file one malformed line would silently truncate" testMalformedLineRefused
  it "accepts a comment or blank line after the last setting" testTrailingCommentIsAccepted
  it "reports a missing file rather than throwing" testMissingFileIsReported
  it "has no issuer key when the section is absent" testIssuerAbsent
  it "reads the index and the private key it signs with" testIssuerKey
  it "refuses a section without an index" testIssuerIndexMissing
  it "refuses a section without a private key" testIssuerSecretMissing
  it "refuses an index that is not a positive whole number" testIssuerIndexInvalid
  it "refuses a private key that is not a valid issuer secret" testIssuerBadSecret
  it "names the old default and key_<n> settings, then refuses the boot" testIssuerOldFormat
  it "leaves chat redemption off when the dev section is absent" testDevRedeemAbsent
  it "reads chat_redeem = on" testDevRedeemOn
  it "reads chat_redeem = off" testDevRedeemOff
  it "refuses a chat_redeem that is not on or off" testDevRedeemNotBoolean

fullIni :: T.Text
fullIni =
  T.unlines
    [ "[listener]",
      "static_dir = /srv/badges",
      "[btcpay]",
      "host = https://btcpay.example.org",
      "api_key = token-value",
      "store_id = store-value",
      "webhook_secret = secret-value"
    ]

withIni :: T.Text -> (FilePath -> IO a) -> IO a
withIni t f = do
  createDirectoryIfMissing True "tests/tmp"
  withTempDirectory "tests/tmp" "badge-ini" $ \d -> do
    let p = d </> "badge_service.ini"
    T.writeFile p t
    f p

testDefaults :: IO ()
testDefaults = withIni fullIni $ \p -> do
  Right cfg <- readServiceConfig p
  let ListenerConfig {lHost, lPort, lStaticDir, lServeWebapp, lWebappExportDir, lTrustForwardedFor} = listener cfg
  lHost `shouldBe` "127.0.0.1"
  lPort `shouldBe` 8080
  lStaticDir `shouldBe` "/srv/badges"
  lServeWebapp `shouldBe` True
  lWebappExportDir `shouldBe` Nothing
  lTrustForwardedFor `shouldBe` False
  let PollConfig {pWaitingSeconds, pIdleSeconds} = poll cfg
  pWaitingSeconds `shouldBe` 3
  pIdleSeconds `shouldBe` 60
  case btcpay cfg of
    Nothing -> expectationFailure "the btcpay section was present"
    Just BTCPayConfig {bExpiryMinutes, bSpeedPolicy, bPaymentTolerance} -> do
      bExpiryMinutes `shouldBe` 60
      bSpeedPolicy `shouldBe` MediumSpeed
      bPaymentTolerance `shouldBe` 0.5

testBlankHost :: IO ()
testBlankHost =
  withIni (T.replace "[listener]\n" "[listener]\nhost = \n" fullIni) $ \p -> do
    Right cfg <- readServiceConfig p
    lHost (listener cfg) `shouldBe` "127.0.0.1"

testAbsentSection :: IO ()
testAbsentSection = withIni (T.unlines (takeWhile (/= "[btcpay]") (T.lines fullIni))) $ \p -> do
  Right cfg <- readServiceConfig p
  btcpay cfg `shouldBe` Nothing

testIncompleteSection :: IO ()
testIncompleteSection =
  withIni (T.replace "webhook_secret = secret-value" "" fullIni) $ \p -> do
    r <- readServiceConfig p
    case r of
      Left e -> e `shouldContain` "webhook_secret"
      Right _ -> expectationFailure "an incomplete btcpay section must fail at boot"

testAbsentStaticDir :: IO ()
testAbsentStaticDir =
  withIni (T.replace "static_dir = /srv/badges\n" "" fullIni) $ \p ->
    readServiceConfig p >>= (`shouldSatisfy` isLeft)

testWebappSplitDeployment :: IO ()
testWebappSplitDeployment =
  withIni (T.replace "static_dir = /srv/badges\n" "static_dir = /srv/badges\nserve_webapp = off\nwebapp_export_dir = /srv/web\n" fullIni) $ \p -> do
    Right cfg <- readServiceConfig p
    let ListenerConfig {lServeWebapp, lWebappExportDir} = listener cfg
    lServeWebapp `shouldBe` False
    lWebappExportDir `shouldBe` Just "/srv/web"

fullStripeIni :: T.Text
fullStripeIni =
  fullIni
    <> T.unlines
      [ "[stripe]",
        "secret_key = rk_test_x",
        "publishable_key = pk_test_x",
        "webhook_secret = whsec_x"
      ]

testStripeDefaults :: IO ()
testStripeDefaults = withIni fullStripeIni $ \p -> do
  Right cfg <- readServiceConfig p
  case stripe cfg of
    Nothing -> expectationFailure "the stripe section was present"
    Just StripeConfig {sSessionMinutes, sHost} -> do
      sSessionMinutes `shouldBe` 60
      sHost `shouldBe` "https://api.stripe.com"

testStripeAbsent :: IO ()
testStripeAbsent = withIni fullIni $ \p -> do
  Right cfg <- readServiceConfig p
  stripe cfg `shouldBe` Nothing

testStripeIncomplete :: IO ()
testStripeIncomplete =
  withIni (T.replace "webhook_secret = whsec_x" "" fullStripeIni) $ \p -> do
    r <- readServiceConfig p
    case r of
      Left e -> e `shouldContain` "webhook_secret"
      Right _ -> expectationFailure "an incomplete stripe section must fail at boot"

testStripeSessionMinutesRange :: IO ()
testStripeSessionMinutesRange = do
  refuses "session_minutes = 5\n"
  refuses "session_minutes = 30\n"
  refuses "session_minutes = 1440\n"
  refuses "session_minutes = 2000\n"
  accepts "session_minutes = 31\n" 31
  accepts "session_minutes = 1439\n" 1439
  where
    refuses value =
      withIni (fullStripeIni <> value) $ \p -> do
        r <- readServiceConfig p
        case r of
          Left e -> e `shouldContain` "session_minutes"
          Right _ -> expectationFailure ("stripe." <> T.unpack (T.strip value) <> " is outside 31-1439 and must fail")
    accepts value expected =
      withIni (fullStripeIni <> value) $ \p -> do
        r <- readServiceConfig p
        case r of
          Right cfg -> fmap sSessionMinutes (stripe cfg) `shouldBe` Just expected
          Left e -> expectationFailure ("stripe." <> T.unpack (T.strip value) <> " is inside 31-1439 and must parse, but: " <> e)

testSpeedPolicyAccepted :: T.Text -> SpeedPolicy -> IO ()
testSpeedPolicyAccepted name expected =
  withIni (fullIni <> "speed_policy = " <> name <> "\n") $ \p -> do
    Right cfg <- readServiceConfig p
    case btcpay cfg of
      Nothing -> expectationFailure "the btcpay section was present"
      Just BTCPayConfig {bSpeedPolicy} -> bSpeedPolicy `shouldBe` expected

testSpeedPolicyName :: IO ()
testSpeedPolicyName =
  withIni (fullIni <> "speed_policy = 2\n") $ \p ->
    readServiceConfig p >>= (`shouldSatisfy` isLeft)

testPollCadence :: IO ()
testPollCadence = do
  withPoll "waiting_seconds = 1\nidle_seconds = 5\n" $ \r -> case r of
    Right cfg -> poll cfg `shouldBe` PollConfig {pWaitingSeconds = 1, pIdleSeconds = 5}
    Left e -> expectationFailure ("a one-second cadence is legal: " <> e)
  mapM_
    (\v -> withPoll v (`shouldSatisfy` isLeft))
    [ "waiting_seconds = 0\n",
      "idle_seconds = 0\n",
      "waiting_seconds = -3\n",
      "idle_seconds = -1\n",
      -- 2^64 + 4 would wrap to a legal 4 seconds on a machine-width read.
      "idle_seconds = 18446744073709551620\n"
    ]
  where
    withPoll keys act = withIni (fullIni <> "[poll]\n" <> keys) $ \p -> readServiceConfig p >>= act

testExpiryMinutes :: IO ()
testExpiryMinutes = do
  withExpiry "expiry_minutes = 1\n" $ \r -> case r of
    Right cfg -> (bExpiryMinutes <$> btcpay cfg) `shouldBe` Just 1
    Left e -> expectationFailure ("a one-minute window is legal: " <> e)
  mapM_
    ( \v ->
        withExpiry v $ \r -> case r of
          Left e -> e `shouldContain` "expiry_minutes"
          Right _ -> expectationFailure ("btcpay." <> T.unpack (T.strip v) <> " must not boot")
    )
    ["expiry_minutes = 0\n", "expiry_minutes = -60\n"]
  where
    withExpiry key act = withIni (fullIni <> key) $ \p -> readServiceConfig p >>= act

testUnknownKeysAreNamed :: IO ()
testUnknownKeysAreNamed =
  withIni (fullIni <> "speed_polcy = LowSpeed\ntrust_forwaded_for = on\n[poll]\nwaiting_secnds = 5\n[dev]\nchat_redem = on\n") $ \p -> do
    Right ini <- readIniFile p
    unknownKeys ini `shouldMatchList` ["btcpay.speed_polcy", "btcpay.trust_forwaded_for", "poll.waiting_secnds", "dev.chat_redem"]
    withIni (T.replace "[btcpay]" "[btcpai]" fullIni) $ \wrongSection -> do
      Right sectionIni <- readIniFile wrongSection
      unknownKeys sectionIni `shouldContain` ["[btcpai]"]
    withIni ("chat_redeem = on\n" <> fullIni) $ \stray -> do
      Right strayIni <- readIniFile stray
      unknownKeys strayIni `shouldContain` ["chat_redeem, written above the first section header"]
    withIni fullIni $ \clean -> do
      Right cleanIni <- readIniFile clean
      unknownKeys cleanIni `shouldBe` []

-- | The ini parser stops at the first line it cannot read and keeps what it has, so without this a
-- missing `=` in [listener] would silently drop every section below it, and the provider with it.
testMalformedLineRefused :: IO ()
testMalformedLineRefused =
  withIni (T.replace "static_dir = /srv/badges" "static_dir = /srv/badges\ntrust_forwarded_for on" fullIni) $ \p ->
    readServiceConfig p >>= \r -> case r of
      Left e -> e `shouldContain` "malformed"
      Right cfg -> expectationFailure ("a truncated file must not boot, and this one kept " <> show (btcpay cfg))

-- | The strictness that refuses a truncated file must not refuse a trailing comment or blank line,
-- since `iniParser` stops before one and commenting out the last section is enough to produce it.
testTrailingCommentIsAccepted :: IO ()
testTrailingCommentIsAccepted = do
  accepts (fullIni <> "; rotated the api key on 2026-09-01\n")
  accepts (fullIni <> "\n\n")
  accepts (fullIni <> "[dev]\n; chat_redeem = on\n")
  accepts (fullIni <> "# a hash comment, with no newline after it")
  where
    accepts t =
      withIni t $ \p ->
        readServiceConfig p >>= \r -> case r of
          Right _ -> pure ()
          Left e -> expectationFailure ("a legal file was refused: " <> e)

-- | The caller prints the reason under the path it was asked for, so naming the file here as well
-- would put it in the line twice.
testMissingFileIsReported :: IO ()
testMissingFileIsReported =
  readServiceConfig "tests/tmp/no-such-badge_service.ini" >>= \r -> case r of
    Left e -> do
      e `shouldContain` "does not exist"
      e `shouldNotContain` "no-such-badge_service.ini"
    Right _ -> expectationFailure "a file that is not there cannot be read"

testPaymentTolerance :: IO ()
testPaymentTolerance = do
  withTolerance "payment_tolerance = 2.5\n" $ \r -> case r of
    Right cfg -> (bPaymentTolerance <$> btcpay cfg) `shouldBe` Just 2.5
    Left e -> expectationFailure ("two and a half percent is legal: " <> e)
  mapM_
    ( \v ->
        withTolerance v $ \r -> case r of
          Left e -> e `shouldContain` "payment_tolerance"
          Right _ -> expectationFailure ("btcpay." <> T.unpack (T.strip v) <> " must not boot")
    )
    ["payment_tolerance = 100\n", "payment_tolerance = -1\n", "payment_tolerance = half\n"]
  where
    withTolerance key act = withIni (fullIni <> key) $ \p -> readServiceConfig p >>= act

testHostMustBeHttps :: IO ()
testHostMustBeHttps =
  withIni (T.replace "https://" "http://" fullIni) $ \p ->
    readServiceConfig p >>= \r -> case r of
      Left e -> e `shouldContain` "https"
      Right _ -> expectationFailure "an http host carries the api key in the clear"

-- A real 32-byte base64url secret, as `simplex-chat badge keygen` prints it.
issuerSecret :: T.Text
issuerSecret = "Ea5wG-J2mQjPBu9YfSJRKPnGnzoIdEE-8VaMh_wY2Bg="

withIssuer :: [T.Text] -> (FilePath -> IO a) -> IO a
withIssuer ls = withIni (fullIni <> T.unlines ("[issuer]" : ls))

issuerRefusal :: [T.Text] -> IO String
issuerRefusal ls =
  withIssuer ls readServiceConfig >>= \r -> case r of
    Left e -> pure e
    Right cfg -> expectationFailure ("[issuer] " <> show ls <> " must not boot, it read " <> show (issuer cfg)) >> pure ""

testIssuerAbsent :: IO ()
testIssuerAbsent = withIni fullIni $ \p -> do
  Right cfg <- readServiceConfig p
  issuer cfg `shouldBe` Nothing

testIssuerKey :: IO ()
testIssuerKey =
  withIssuer ["index = 3", "private_key = " <> issuerSecret] $ \p -> do
    Right cfg <- readServiceConfig p
    Right sk <- pure (strDecode (B.pack (T.unpack issuerSecret)))
    issuer cfg `shouldBe` Just BadgeIssuerKey {keyIdx = 3, secretKey = sk}

testIssuerIndexMissing :: IO ()
testIssuerIndexMissing =
  issuerRefusal ["private_key = " <> issuerSecret] `shouldReturn` "issuer.index is required"

testIssuerSecretMissing :: IO ()
testIssuerSecretMissing =
  issuerRefusal ["index = 1"] `shouldReturn` "issuer.private_key is required"

testIssuerIndexInvalid :: IO ()
testIssuerIndexInvalid =
  mapM_
    (\v -> issuerRefusal ["index = " <> v, "private_key = " <> issuerSecret] `shouldReturn` "issuer.index must be a positive whole number")
    ["0", "-1", "one", "1.5", "key_1", "18446744073709551617"]

testIssuerBadSecret :: IO ()
testIssuerBadSecret =
  issuerRefusal ["index = 1", "private_key = not-a-key"]
    `shouldReturn` "issuer.private_key is not a valid issuer secret; use the value from `simplex-chat badge keygen`"

testIssuerOldFormat :: IO ()
testIssuerOldFormat = do
  let old = ["default = key_1", "key_1 = " <> issuerSecret]
  withIssuer old $ \p -> do
    Right ini <- readIniFile p
    unknownKeys ini `shouldMatchList` ["issuer.default", "issuer.key_1"]
  issuerRefusal old `shouldReturn` "issuer.index is required"

testDevRedeemAbsent :: IO ()
testDevRedeemAbsent = withIni fullIni $ \p -> do
  Right cfg <- readServiceConfig p
  devChatRedeem cfg `shouldBe` False

testDevRedeemOn :: IO ()
testDevRedeemOn = withDev "chat_redeem = on\n" $ \r -> case r of
  Right cfg -> devChatRedeem cfg `shouldBe` True
  Left e -> expectationFailure ("[dev] chat_redeem = on is legal: " <> e)

testDevRedeemOff :: IO ()
testDevRedeemOff = withDev "chat_redeem = off\n" $ \r -> case r of
  Right cfg -> devChatRedeem cfg `shouldBe` False
  Left e -> expectationFailure ("[dev] chat_redeem = off is legal: " <> e)

testDevRedeemNotBoolean :: IO ()
testDevRedeemNotBoolean = withDev "chat_redeem = true\n" $ \r -> case r of
  Left e -> e `shouldContain` "chat_redeem"
  Right _ -> expectationFailure "only on and off are accepted, so a typo cannot silently disarm the gate"

withDev :: T.Text -> (Either String ServiceConfig -> IO a) -> IO a
withDev keys act = withIni (fullIni <> "[dev]\n" <> keys) $ \p -> readServiceConfig p >>= act

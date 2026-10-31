{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Bots.StoreService.ConfigTests where

import StoreService.Config
import Control.Monad (forM_)
import qualified Data.ByteArray as BA
import qualified Data.ByteString.Char8 as B
import Data.Either (isLeft)
import Data.Ini (readIniFile)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime (..))
import Simplex.Messaging.Crypto.Secp256k1 (mkPrivateKey)
import Simplex.Messaging.Encoding.String (strDecode)
import Simplex.Messaging.Eth.Address (Address)
import Simplex.Messaging.SimplexName (SimplexTLD (..))
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import Test.Hspec
import UnliftIO.Temporary (withTempDirectory)

badgeConfigTests :: Spec
badgeConfigTests = describe "store service config" $ do
  it "applies every documented default" testDefaults
  it "falls back to the default host when the value is blank" testBlankHost
  it "disables a provider whose section is absent" testAbsentSection
  it "refuses an incomplete provider section, naming the key" testIncompleteSection
  it "reads the badges_port and badges_static_dir written out, as a local run sets them" testBadgesListenerKeys
  it "refuses the listener keys of earlier releases, naming the key that replaced each, all at once" testRetiredListenerKeys
  it "refuses a badges_port outside 1-65535" testBadgesPortRange
  it "reads serve_webapp off and a badges_export_dir" testWebappSplitDeployment
  it "applies every documented stripe default" testStripeDefaults
  it "disables stripe when the section is absent" testStripeAbsent
  it "refuses an incomplete stripe section, naming the key" testStripeIncomplete
  it "bounds stripe session_minutes to 1-1440" testStripeSessionMinutesRange
  it "accepts HighSpeed" (testSpeedPolicyAccepted "HighSpeed" HighSpeed)
  it "accepts LowMediumSpeed" (testSpeedPolicyAccepted "LowMediumSpeed" LowMediumSpeed)
  it "accepts LowSpeed" (testSpeedPolicyAccepted "LowSpeed" LowSpeed)
  it "refuses a numeric speed policy" testSpeedPolicyName
  it "accepts a poll cadence and refuses a zero or negative one" testPollCadence
  it "accepts an expiry window and refuses a zero or negative one" testExpiryMinutes
  it "accepts a payment tolerance and refuses one that settles for a satoshi" testPaymentTolerance
  it "refuses a host that would send the api key in the clear" testHostMustBeHttps
  it "names a setting nothing reads, in every section it parses" testUnknownKeysAreNamed
  it "ignores a section nothing reads rather than refusing the file" testUnknownSectionStillBoots
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
  it "verifies store receipts when the dev section is absent" testDevUnverifiedReceiptsAbsent
  it "reads accept_unverified_store_receipts = on" testDevUnverifiedReceiptsOn
  it "reads accept_unverified_store_receipts = off" testDevUnverifiedReceiptsOff
  it "refuses an accept_unverified_store_receipts that is not on or off" testDevUnverifiedReceiptsNotBoolean
  it "defaults check_store_during_coverage to off" testDevStoreChecksDefault
  it "reads check_store_during_coverage = on and off beside accept_unverified_store_receipts = on" testDevStoreChecksOnOff
  it "refuses check_store_during_coverage = on without accept_unverified_store_receipts or credit_test_purchases on" testDevStoreChecksWithoutMock
  it "defaults credit_test_purchases to off" testDevCreditTestPurchasesDefault
  it "reads credit_test_purchases = on and off" testDevCreditTestPurchasesOnOff
  it "refuses a credit_test_purchases that is not on or off" testDevCreditTestPurchasesNotBoolean
  it "reads credit_test_purchases = on beside [google] and beside the App Store Sandbox, however its host is spelled" testDevCreditTestPurchasesBesideStores
  it "refuses credit_test_purchases = on beside [apple] without the App Store Sandbox's server API, however production is spelled" testDevCreditTestPurchasesBesideApple
  it "reads check_store_during_coverage = on beside credit_test_purchases = on, with no store, beside the App Store Sandbox or [google]" testDevStoreChecksWithCredit
  it "configures no store verifier when [apple] and [google] are absent" testStoresAbsent
  it "reads [apple] and [google], defaulting Google's endpoints" testStoresRead
  it "refuses an incomplete [apple] or [google], naming the key" testStoresIncomplete
  it "reads the App Store Server API key, defaulting to the production host, and a host in any case with a trailing slash" testAppleServerApi
  it "refuses an App Store Server API key missing a setting, or an http host" testAppleServerApiIncomplete
  it "refuses the App Store Sandbox or any other host but production with credit_test_purchases off" testAppleHostWithoutCredit
  it "refuses accept_unverified_store_receipts beside a real store verifier, also with credit_test_purchases = on" testDevUnverifiedBesideVerifier
  groupConfigTests

fullIni :: T.Text
fullIni =
  T.unlines
    [ "[listener]",
      -- [group] stays before [btcpay], since tests append btcpay keys to this fixture.
      "[group]",
      "display_name = SimpleX Badges",
      "description = badge ops desk",
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
    let p = d </> "store_service.ini"
    T.writeFile p t
    f p

testDefaults :: IO ()
testDefaults = withIni fullIni $ \p -> do
  Right cfg <- readServiceConfig p
  let ListenerConfig {lHost, lBadgesPort, lBadgesStaticDir, lServeWebapp, lBadgesExportDir, lTrustForwardedFor, lNamesStaticDir, lNamesExportDir, lNamesPort} = listener cfg
  lHost `shouldBe` "127.0.0.1"
  lBadgesPort `shouldBe` 8080
  lBadgesStaticDir `shouldBe` "/srv/badges"
  lServeWebapp `shouldBe` True
  lBadgesExportDir `shouldBe` Nothing
  lTrustForwardedFor `shouldBe` False
  (lNamesStaticDir, lNamesExportDir, lNamesPort) `shouldBe` (Nothing, Nothing, Nothing)
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

-- | fullIni with these lines first in [listener].
withListenerKeys :: [T.Text] -> T.Text
withListenerKeys ks = T.replace "[listener]\n" ("[listener]\n" <> T.unlines ks) fullIni

testBadgesListenerKeys :: IO ()
testBadgesListenerKeys = do
  Right cfg <- parseIni (withListenerKeys ["badges_port = 9080", "badges_static_dir = ./apps/simplex-store-service/web/dist"])
  let ListenerConfig {lBadgesPort, lBadgesStaticDir} = listener cfg
  (lBadgesPort, lBadgesStaticDir) `shouldBe` (9080, "./apps/simplex-store-service/web/dist")

testRetiredListenerKeys :: IO ()
testRetiredListenerKeys = do
  refused "port = 8080" "listener.port is now listener.badges_port"
  refused "static_dir = /srv/badges" "listener.static_dir is now listener.badges_static_dir"
  refused "static_dir =" "listener.static_dir is now listener.badges_static_dir"
  refused "webapp_export_dir = /srv/badges-export" "listener.webapp_export_dir is now listener.badges_export_dir"
  refused "names_hosts = simplex.domains" "listener.names_hosts is replaced by listener.names_port: serve the names site on its own port"
  refusedAll ["port = 8080", "static_dir = /srv/badges"] "listener.port is now listener.badges_port; listener.static_dir is now listener.badges_static_dir"
  withIni (withListenerKeys ["static_dir = /srv/badges", "webapp_export_dir = /srv/badges-export", "names_hosts = simplex.domains", "port = 8080"]) $ \p -> do
    Right ini <- readIniFile p
    unknownKeys ini `shouldBe` []
  where
    refused key = refusedAll [key]
    refusedAll keys e = parseIni (withListenerKeys keys) >>= (`shouldBe` e) . either id (const "parsed")

testBadgesPortRange :: IO ()
testBadgesPortRange = do
  forM_ ["0", "65536", "-1"] $ \v ->
    parseIni (withListenerKeys ["badges_port = " <> v]) >>= (`shouldBe` "listener.badges_port must be between 1 and 65535") . either id (const "parsed")
  Right cfg <- parseIni (withListenerKeys ["badges_port = 65535"])
  lBadgesPort (listener cfg) `shouldBe` 65535

testWebappSplitDeployment :: IO ()
testWebappSplitDeployment =
  withIni (withListenerKeys ["serve_webapp = off", "badges_export_dir = /srv/web"]) $ \p -> do
    Right cfg <- readServiceConfig p
    let ListenerConfig {lServeWebapp, lBadgesExportDir} = listener cfg
    lServeWebapp `shouldBe` False
    lBadgesExportDir `shouldBe` Just "/srv/web"

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
  refuses "session_minutes = -5\n"
  refuses "session_minutes = 0\n"
  refuses "session_minutes = 1441\n"
  refuses "session_minutes = 2000\n"
  accepts "session_minutes = 1\n" 1
  accepts "session_minutes = 1440\n" 1440
  where
    refuses value =
      withIni (fullStripeIni <> value) $ \p -> do
        r <- readServiceConfig p
        case r of
          Left e -> e `shouldContain` "session_minutes"
          Right _ -> expectationFailure ("stripe." <> T.unpack (T.strip value) <> " is outside 1-1440 and must fail")
    accepts value expected =
      withIni (fullStripeIni <> value) $ \p -> do
        r <- readServiceConfig p
        case r of
          Right cfg -> fmap sSessionMinutes (stripe cfg) `shouldBe` Just expected
          Left e -> expectationFailure ("stripe." <> T.unpack (T.strip value) <> " is inside 1-1440 and must parse, but: " <> e)

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
  withIni (fullIni <> "speed_polcy = LowSpeed\ntrust_forwaded_for = on\n[poll]\nwaiting_secnds = 5\n[stripe]\nsecret_ky = rk_test_x\n") $ \p -> do
    Right ini <- readIniFile p
    unknownKeys ini `shouldMatchList` ["btcpay.speed_polcy", "btcpay.trust_forwaded_for", "poll.waiting_secnds", "stripe.secret_ky"]
    withIni (T.replace "[btcpay]" "[btcpai]" fullIni) $ \wrongSection -> do
      Right sectionIni <- readIniFile wrongSection
      unknownKeys sectionIni `shouldContain` ["[btcpai]"]
    withIni ("badges_static_dir = /srv/badges\n" <> fullIni) $ \stray -> do
      Right strayIni <- readIniFile stray
      unknownKeys strayIni `shouldContain` ["badges_static_dir, written above the first section header"]
    withIni fullIni $ \clean -> do
      Right cleanIni <- readIniFile clean
      unknownKeys cleanIni `shouldBe` []

testUnknownSectionStillBoots :: IO ()
testUnknownSectionStillBoots =
  parseIni (fullIni <> "[legacy]\nsetting = on\n") >>= \r -> case r of
    Right cfg -> lBadgesStaticDir (listener cfg) `shouldBe` "/srv/badges"
    Left e -> expectationFailure ("a section nothing reads must be ignored, not refused: " <> e)

-- | The ini parser stops at the first line it cannot read and keeps what it has, so without this a
-- missing `=` in [listener] would silently drop every section below it, and the provider with it.
testMalformedLineRefused :: IO ()
testMalformedLineRefused =
  withIni (withListenerKeys ["trust_forwarded_for on"]) $ \p ->
    readServiceConfig p >>= \r -> case r of
      Left e -> e `shouldContain` "malformed"
      Right cfg -> expectationFailure ("a truncated file must not boot, and this one kept " <> show (btcpay cfg))

-- | The strictness that refuses a truncated file must not refuse a trailing comment or blank line,
-- since `iniParser` stops before one and commenting out the last section is enough to produce it.
testTrailingCommentIsAccepted :: IO ()
testTrailingCommentIsAccepted = do
  accepts (fullIni <> "; rotated the api key on 2026-09-01\n")
  accepts (fullIni <> "\n\n")
  accepts (fullIni <> "[poll]\n; idle_seconds = 5\n")
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
  readServiceConfig "tests/tmp/no-such-store_service.ini" >>= \r -> case r of
    Left e -> do
      e `shouldContain` "does not exist"
      e `shouldNotContain` "no-such-store_service.ini"
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

testDevUnverifiedReceiptsAbsent :: IO ()
testDevUnverifiedReceiptsAbsent = withIni fullIni $ \p -> do
  Right cfg <- readServiceConfig p
  devAcceptUnverifiedStoreReceipts cfg `shouldBe` False

testDevUnverifiedReceiptsOn :: IO ()
testDevUnverifiedReceiptsOn = withDev "accept_unverified_store_receipts = on\n" $ \r -> case r of
  Right cfg -> devAcceptUnverifiedStoreReceipts cfg `shouldBe` True
  Left e -> expectationFailure ("[dev] accept_unverified_store_receipts = on is legal: " <> e)

testDevUnverifiedReceiptsOff :: IO ()
testDevUnverifiedReceiptsOff = withDev "accept_unverified_store_receipts = off\n" $ \r -> case r of
  Right cfg -> devAcceptUnverifiedStoreReceipts cfg `shouldBe` False
  Left e -> expectationFailure ("[dev] accept_unverified_store_receipts = off is legal: " <> e)

testDevUnverifiedReceiptsNotBoolean :: IO ()
testDevUnverifiedReceiptsNotBoolean = withDev "accept_unverified_store_receipts = yes\n" $ \r -> case r of
  Left e -> e `shouldContain` "accept_unverified_store_receipts"
  Right _ -> expectationFailure "only on and off are accepted, so a typo cannot silently arm the mock"

testDevStoreChecksDefault :: IO ()
testDevStoreChecksDefault = do
  withIni fullIni $ \p -> do
    Right cfg <- readServiceConfig p
    devCheckStoreDuringCoverage cfg `shouldBe` False
  withDev "accept_unverified_store_receipts = on\n" $ \r -> devCheckStoreDuringCoverage <$> r `shouldBe` Right False

testDevStoreChecksOnOff :: IO ()
testDevStoreChecksOnOff =
  forM_ [("on", True), ("off", False)] $ \(v, expected) ->
    withDev ("accept_unverified_store_receipts = on\ncheck_store_during_coverage = " <> v <> "\n") $ \r ->
      devCheckStoreDuringCoverage <$> r `shouldBe` Right expected

testDevStoreChecksWithoutMock :: IO ()
testDevStoreChecksWithoutMock =
  forM_ ["", "accept_unverified_store_receipts = off\n", "accept_unverified_store_receipts = off\ncredit_test_purchases = off\n"] $ \devFlags ->
    withDev (devFlags <> "check_store_during_coverage = on\n") $ \case
      Left e -> e `shouldBe` "[dev] check_store_during_coverage needs accept_unverified_store_receipts or credit_test_purchases = on, as only a dev deployment may ask a store while coverage runs"
      Right _ -> expectationFailure "only a dev deployment may ask a store while coverage runs"

testDevCreditTestPurchasesDefault :: IO ()
testDevCreditTestPurchasesDefault = do
  withIni fullIni $ \p -> do
    Right cfg <- readServiceConfig p
    devCreditTestPurchases cfg `shouldBe` False
  withDev "accept_unverified_store_receipts = on\n" $ \r -> devCreditTestPurchases <$> r `shouldBe` Right False

testDevCreditTestPurchasesOnOff :: IO ()
testDevCreditTestPurchasesOnOff =
  forM_ [("on", True), ("off", False)] $ \(v, expected) ->
    withDev ("credit_test_purchases = " <> v <> "\n") $ \r ->
      devCreditTestPurchases <$> r `shouldBe` Right expected

testDevCreditTestPurchasesNotBoolean :: IO ()
testDevCreditTestPurchasesNotBoolean = withDev "credit_test_purchases = yes\n" $ \case
  Left e -> e `shouldContain` "credit_test_purchases"
  Right _ -> expectationFailure "only on and off are accepted, so a typo cannot silently credit test purchases"

testDevCreditTestPurchasesBesideStores :: IO ()
testDevCreditTestPurchasesBesideStores =
  forM_ [googleIni, appleSandboxIni, appleApiHostIni "HTTPS://API.STOREKIT-SANDBOX.ITUNES.APPLE.COM/"] $ \stores ->
    withDevBeside stores "credit_test_purchases = on\n" $ \r ->
      devCreditTestPurchases <$> r `shouldBe` Right True

testDevCreditTestPurchasesBesideApple :: IO ()
testDevCreditTestPurchasesBesideApple =
  forM_ (appleIni : appleIni <> appleApiIni : map appleApiHostIni notSandbox) $ \stores ->
    forM_ ["", "check_store_during_coverage = on\n"] $ \storeChecks ->
      withDevBeside stores ("credit_test_purchases = on\n" <> storeChecks) $ \case
        Left e -> e `shouldContain` "[dev] credit_test_purchases = on needs [apple]'s server API key with api_host = https://api.storekit-sandbox.itunes.apple.com"
        Right _ -> expectationFailure "test purchases must be credited only beside the App Store Sandbox"
  where
    notSandbox = ["https://api.storekit.itunes.apple.com", "HTTPS://API.STOREKIT.ITUNES.APPLE.COM", "https://api.storekit.itunes.apple.com/", "https://example.com"]

testDevStoreChecksWithCredit :: IO ()
testDevStoreChecksWithCredit =
  forM_ ["", appleSandboxIni, googleIni] $ \stores ->
    withDevBeside stores "credit_test_purchases = on\ncheck_store_during_coverage = on\n" $ \r ->
      (\cfg -> (devCheckStoreDuringCoverage cfg, devCreditTestPurchases cfg)) <$> r `shouldBe` Right (True, True)

withDev :: T.Text -> (Either String ServiceConfig -> IO a) -> IO a
withDev = withDevBeside ""

withDevBeside :: T.Text -> T.Text -> (Either String ServiceConfig -> IO a) -> IO a
withDevBeside stores keys act = withIni (fullIni <> stores <> "[dev]\n" <> keys) $ \p -> readServiceConfig p >>= act

appleIni, googleIni :: T.Text
appleIni = "[apple]\nbundle_id = chat.simplex.app\nroot_certificates = /etc/badges/AppleRootCA-G3.cer ,/etc/badges/AppleRootCA-G4.cer,\n"
googleIni = "[google]\npackage_name = chat.simplex.app\nservice_account_file = /etc/badges/play.json\n"

testStoresAbsent :: IO ()
testStoresAbsent = withIni fullIni $ \p -> do
  Right cfg <- readServiceConfig p
  appleStore cfg `shouldBe` Nothing
  playStore cfg `shouldBe` Nothing

testStoresRead :: IO ()
testStoresRead = withIni (fullIni <> appleIni <> googleIni) $ \p -> do
  Right cfg <- readServiceConfig p
  appleStore cfg `shouldBe` Just AppleStoreConfig {aBundleId = "chat.simplex.app", aRootCertificates = "/etc/badges/AppleRootCA-G3.cer" :| ["/etc/badges/AppleRootCA-G4.cer"], aServerApi = Nothing}
  playStore cfg
    `shouldBe` Just
      PlayStoreConfig
        { gPackageName = "chat.simplex.app",
          gServiceAccountFile = "/etc/badges/play.json",
          gApiHost = "https://androidpublisher.googleapis.com",
          gTokenUrl = "https://oauth2.googleapis.com/token"
        }

testStoresIncomplete :: IO ()
testStoresIncomplete = do
  refused (T.replace "bundle_id = chat.simplex.app\n" "" appleIni) "apple.bundle_id"
  refused (T.replace "root_certificates = /etc/badges/AppleRootCA-G3.cer ,/etc/badges/AppleRootCA-G4.cer,\n" "" appleIni) "apple.root_certificates"
  refused (T.replace "package_name = chat.simplex.app\n" "" googleIni) "google.package_name"
  refused (T.replace "service_account_file = /etc/badges/play.json\n" "" googleIni) "google.service_account_file"
  where
    refused section key =
      withIni (fullIni <> section) readServiceConfig >>= \case
        Left e -> e `shouldContain` key
        Right _ -> expectationFailure ("an incomplete section must fail at boot, naming " <> key)

appleApiIni :: T.Text
appleApiIni = "api_key_id = 2X9R4HXF34\napi_issuer_id = 57246542-96fe-1a63-e053-0824d011072a\napi_private_key_file = /etc/badges/AuthKey_2X9R4HXF34.p8\n"

appleApiHostIni :: T.Text -> T.Text
appleApiHostIni host = appleIni <> appleApiIni <> "api_host = " <> host <> "\n"

appleSandboxIni :: T.Text
appleSandboxIni = appleApiHostIni "https://api.storekit-sandbox.itunes.apple.com"

testAppleServerApi :: IO ()
testAppleServerApi = do
  withIni (fullIni <> appleIni <> appleApiIni) $ \p -> do
    Right cfg <- readServiceConfig p
    (appleStore cfg >>= aServerApi)
      `shouldBe` Just
        AppleServerApi
          { aKeyId = "2X9R4HXF34",
            aIssuerId = "57246542-96fe-1a63-e053-0824d011072a",
            aPrivateKeyFile = "/etc/badges/AuthKey_2X9R4HXF34.p8",
            aApiHost = "https://api.storekit.itunes.apple.com"
          }
  withIni (fullIni <> appleApiHostIni "HTTPS://API.STOREKIT.ITUNES.APPLE.COM//") $ \p -> do
    Right cfg <- readServiceConfig p
    (aApiHost <$> (appleStore cfg >>= aServerApi)) `shouldBe` Just "https://api.storekit.itunes.apple.com"
  forM_ [appleSandboxIni, appleApiHostIni "https://api.storekit-sandbox.itunes.apple.com/"] $ \stores ->
    withDevBeside stores "credit_test_purchases = on\n" $ \r ->
      (\cfg -> aApiHost <$> (appleStore cfg >>= aServerApi)) <$> r `shouldBe` Right (Just "https://api.storekit-sandbox.itunes.apple.com")

-- | One or two of the key's settings would leave subscriptions unverified without a word, so they are refused.
testAppleServerApiIncomplete :: IO ()
testAppleServerApiIncomplete = do
  forM_ ["api_key_id", "api_issuer_id", "api_private_key_file"] $ \key ->
    refused (T.unlines $ filter (not . (key `T.isPrefixOf`)) $ T.lines appleApiIni) "apple.api_key_id, apple.api_issuer_id and apple.api_private_key_file"
  refused (appleApiIni <> "api_host = http://api.storekit.itunes.apple.com\n") "apple.api_host"
  where
    refused keys message =
      withIni (fullIni <> appleIni <> keys) readServiceConfig >>= \case
        Left e -> e `shouldContain` message
        Right _ -> expectationFailure ("an incomplete API key must fail at boot, naming " <> message)

testAppleHostWithoutCredit :: IO ()
testAppleHostWithoutCredit =
  forM_ ["https://api.storekit-sandbox.itunes.apple.com", "HTTPS://API.STOREKIT-SANDBOX.ITUNES.APPLE.COM/", "https://api.storekit.itunes.apple.com/inApps", "https://example.com"] $ \host ->
    forM_ ["", "[dev]\ncredit_test_purchases = off\n"] $ \dev ->
      withIni (fullIni <> appleApiHostIni host <> dev) readServiceConfig >>= \case
        Left e -> e `shouldContain` "apple.api_host must be https://api.storekit.itunes.apple.com unless [dev] credit_test_purchases = on"
        Right _ -> expectationFailure "a deployment that credits no test purchases must ask the production App Store"

testDevUnverifiedBesideVerifier :: IO ()
testDevUnverifiedBesideVerifier =
  forM_ [appleIni, googleIni] $ \stores ->
    forM_ ["", "credit_test_purchases = on\n"] $ \credit ->
      withDevBeside stores ("accept_unverified_store_receipts = on\n" <> credit) $ \case
        Left e -> e `shouldContain` "accept_unverified_store_receipts"
        Right _ -> expectationFailure "a service that verifies store receipts must not also accept unverified ones"

parseIni :: T.Text -> IO (Either String ServiceConfig)
parseIni t = withIni t readServiceConfig

groupConfigTests :: Spec
groupConfigTests = describe "group config" $ do
  it "parses display_name and description" testGroupNameAndDescription
  it "defaults description to Nothing" testGroupNoDescription
  it "treats a blank description as absent" testGroupBlankDescription
  it "refuses a display_name no group can be created under" testGroupInvalidName
  it "suggests no name when no character of display_name is valid" testGroupNoValidName
  it "requires display_name when the section is present" testGroupMissingName
  it "leaves group Nothing when the section is absent" testGroupAbsent

listenerIni :: T.Text
listenerIni = "[listener]\nbadges_static_dir = /srv/web\n"

groupIni :: T.Text -> IO (Either String ServiceConfig)
groupIni body = parseIni (listenerIni <> "[group]\n" <> body)

testGroupNameAndDescription :: IO ()
testGroupNameAndDescription = do
  r <- groupIni "display_name = SimpleX Badges\ndescription = Welcome\n"
  fmap group r `shouldBe` Right (Just GroupConfig {gDisplayName = "SimpleX Badges", gDescription = Just "Welcome"})

testGroupNoDescription :: IO ()
testGroupNoDescription = do
  r <- groupIni "display_name = X\n"
  fmap group r `shouldBe` Right (Just GroupConfig {gDisplayName = "X", gDescription = Nothing})

testGroupBlankDescription :: IO ()
testGroupBlankDescription = do
  r <- groupIni "display_name = X\ndescription =    \n"
  fmap group r `shouldBe` Right (Just GroupConfig {gDisplayName = "X", gDescription = Nothing})

testGroupInvalidName :: IO ()
testGroupInvalidName = do
  r <- groupIni "display_name = Значки (staging)\n"
  case r of
    Left e -> e `shouldBe` "group.display_name \"Значки (staging)\" is not a valid group name, the closest valid name is \"Значки staging\""
    Right cfg -> expectationFailure ("a name the core refuses must not boot, and this one kept " <> show (group cfg))

testGroupNoValidName :: IO ()
testGroupNoValidName = do
  r <- groupIni "display_name = !!!\n"
  fmap group r `shouldBe` Left "group.display_name \"!!!\" is not a valid group name"

testGroupMissingName :: IO ()
testGroupMissingName = do
  r <- groupIni "description = hi\n"
  fmap group r `shouldBe` Left "group.display_name is required"

testGroupAbsent :: IO ()
testGroupAbsent = do
  r <- parseIni listenerIni
  fmap group r `shouldBe` Right Nothing

namesConfigTests :: Spec
namesConfigTests = describe "StoreService names: config" $ do
  it "reads [names] and [chain], applying every documented default" testNamesDefaults
  it "reads every [names] and [chain] setting written out" testNamesWrittenOut
  it "reads the names keys of [listener], defaulting to the image's build and port 8081" testNamesListenerKeys
  it "refuses the names keys of [listener] without [names]" testNamesSiteOnlyWithNames
  it "refuses a names_port outside 1-65535, on the badges_port, or with serve_webapp off" testNamesPortRules
  it "leaves the names side off when [names] is absent" testNamesAbsent
  it "refuses [names] without [chain], and [chain] without [names]" testNamesNeedsChain
  it "refuses years below 2" testNamesYearsBelowTwo
  it "refuses a term_start that is not an ISO 8601 UTC time" testNamesBadTermStart
  it "refuses a TLD other than simplex or testing" testNamesBadTLD
  it "refuses a bad or wrongly checksummed contract address, naming the key" testNamesBadAddress
  it "refuses a bad service key without quoting it" testNamesBadServiceKey
  it "keeps the RPC URL and the service key out of errors and the shown config" testNamesSecretsHidden
  it "names a [names] or [chain] setting nothing reads, and still boots" testNamesUnknownKeys
  it "reads codes_only on and off as a known setting, off when absent, and refuses any other value" testNamesCodesOnly
  it "reads record_edits_per_year as a known setting, 3 when absent, and refuses a value outside 1-100" testNamesRecordEdits
  it "accepts [issuer] dev_keys on chain 31337 only, with [names] on" testDevKeysLocalChain
  it "refuses a malformed dev_keys entry or an index named twice, quoting no secret" testDevKeysMalformed
  it "accepts [issuer] dev_keys beside Stripe test keys and a BTCPay on loopback" testDevKeysTestPayments
  it "refuses [issuer] dev_keys beside a live Stripe key or a remote BTCPay, naming which and quoting no key" testDevKeysLivePayments

rpcUrl :: T.Text
rpcUrl = "https://eth.example.org/v2/rpc-token-value"

-- anvil's first dev account, a real secp256k1 key
serviceKeyHex :: T.Text
serviceKeyHex = "0xac0974bec39a17e36ba4a6b4d238ff944bacb478cbed5efcae784d7bf4f2ff80"

controllerAddr, sponsorAddr, registrarAddr, resolverAddr :: T.Text
controllerAddr = "0x5aAeb6053F3E94C9b9A09f33669435E7Ef1BeAed"
sponsorAddr = "0x0B306BF915C4d645ff596e518fAf3F9669b97016"
registrarAddr = "0x014f9c09CcEeDEF1B713110d04D3a07d5B5D9b73"
resolverAddr = "0x8A791620dd6260079BF849Dc5567aDC3F2FdC318"

termsVersion, termsUrl :: T.Text
termsVersion = "2026-10-23"
termsUrl = "https://simplex.chat/"

namesSection :: [T.Text]
namesSection = ["[names]", "tld = testing", "terms_version = " <> termsVersion, "terms_url = " <> termsUrl]

chainSection :: [T.Text]
chainSection =
  [ "[chain]",
    "rpc_url = " <> rpcUrl,
    "chain_id = 31337",
    "controller = " <> controllerAddr,
    "sponsor = " <> sponsorAddr,
    "base_registrar = " <> registrarAddr,
    "resolver = " <> resolverAddr,
    "service_key = " <> serviceKeyHex
  ]

namesIni :: [T.Text] -> [T.Text] -> T.Text
namesIni ns cs = fullIni <> T.unlines (ns <> cs)

namesRefusal :: [T.Text] -> [T.Text] -> IO String
namesRefusal ns cs =
  parseIni (namesIni ns cs) >>= \r -> case r of
    Left e -> pure e
    Right cfg -> expectationFailure ("must not boot, it read " <> show (names cfg)) >> pure ""

parsedNames :: [T.Text] -> [T.Text] -> IO NamesConfig
parsedNames ns cs =
  parseIni (namesIni ns cs) >>= \r -> case r of
    Right cfg -> maybe (fail "[names] was present but not read") pure (names cfg)
    Left e -> fail ("must boot, but: " <> e)

address :: HasCallStack => T.Text -> Address
address t = either error id (strDecode (B.pack (T.unpack t)))

testNamesDefaults :: IO ()
testNamesDefaults = do
  NamesConfig {nTLD, nYears, nTermStart, nTermSlackSeconds, nRevealMarginSeconds, nMaxOrderMoves, nChain} <- parsedNames namesSection chainSection
  nTLD `shouldBe` TLDTesting
  nYears `shouldBe` 2
  nTermStart `shouldBe` UTCTime (fromGregorian 2027 1 1) 0
  nTermSlackSeconds `shouldBe` 3600
  nRevealMarginSeconds `shouldBe` 900
  nMaxOrderMoves `shouldBe` 3
  let ChainConfig {cRpcUrl, cChainId, cController, cSponsor, cBaseRegistrar, cResolver, cServiceKey, cConfirmations, cGasLimitCap, cMaxFeePerGas} = nChain
  cRpcUrl `shouldBe` T.unpack rpcUrl
  cChainId `shouldBe` 31337
  (cController, cSponsor, cBaseRegistrar, cResolver) `shouldBe` (address controllerAddr, address sponsorAddr, address registrarAddr, address resolverAddr)
  Right expectedKey <- pure (mkPrivateKey (BA.pack [0xac, 0x09, 0x74, 0xbe, 0xc3, 0x9a, 0x17, 0xe3, 0x6b, 0xa4, 0xa6, 0xb4, 0xd2, 0x38, 0xff, 0x94, 0x4b, 0xac, 0xb4, 0x78, 0xcb, 0xed, 0x5e, 0xfc, 0xae, 0x78, 0x4d, 0x7b, 0xf4, 0xf2, 0xff, 0x80]))
  (cServiceKey == expectedKey) `shouldBe` True
  cConfirmations `shouldBe` 3
  cGasLimitCap `shouldBe` 8000000
  cMaxFeePerGas `shouldBe` 100000000000

testNamesWrittenOut :: IO ()
testNamesWrittenOut = do
  let ns = ["[names]", "tld = simplex", "terms_version = 2027-01.b", "terms_url = https://simplex.chat/names/terms", "years = 5", "term_start = 2027-03-01T12:00:00Z", "term_slack_seconds = 60", "reveal_margin_seconds = 0", "max_order_moves = 0"]
      cs = chainSection <> ["confirmations = 0", "gas_limit_cap = 500000", "max_fee_per_gas = 30000000000"]
  NamesConfig {nTLD, nTerms, nYears, nTermStart, nTermSlackSeconds, nRevealMarginSeconds, nMaxOrderMoves, nChain = ChainConfig {cConfirmations, cGasLimitCap, cMaxFeePerGas}} <- parsedNames ns cs
  nTLD `shouldBe` TLDSimplex
  nTerms `shouldBe` TermsConfig {tVersion = "2027-01.b", tUrl = "https://simplex.chat/names/terms"}
  nYears `shouldBe` 5
  nTermStart `shouldBe` UTCTime (fromGregorian 2027 3 1) (12 * 3600)
  (nTermSlackSeconds, nRevealMarginSeconds, nMaxOrderMoves) `shouldBe` (60, 0, 0)
  (cConfirmations, cGasLimitCap, cMaxFeePerGas) `shouldBe` (0, 500000, 30000000000)
  namesRefusal ns (chainSection <> ["confirmations = -1"]) `shouldReturn` "chain.confirmations must be at least 0"
  namesRefusal ns (chainSection <> ["gas_limit_cap = 0"]) `shouldReturn` "chain.gas_limit_cap must be a positive whole number"
  namesRefusal ns (chainSection <> ["max_fee_per_gas = 0"]) `shouldReturn` "chain.max_fee_per_gas must be a positive whole number"
  namesRefusal ns (chainSection <> ["max_fee_per_gas = 18446744073709551616"]) `shouldReturn` "chain.max_fee_per_gas must be a positive whole number"
  namesRefusal ns (filter (not . ("chain_id" `T.isPrefixOf`)) chainSection) `shouldReturn` "chain.chain_id is required"
  namesRefusal (namesSection <> ["max_order_moves = -1"]) chainSection `shouldReturn` "names.max_order_moves must be at least 0"

testNamesListenerKeys :: IO ()
testNamesListenerKeys = do
  Right cfg <- parseIni (namesIni namesSection chainSection)
  let ListenerConfig {lNamesStaticDir, lNamesExportDir, lNamesPort} = listener cfg
  (lNamesStaticDir, lNamesExportDir, lNamesPort) `shouldBe` (Just "/srv/names", Nothing, Just 8081)
  Right written <- parseIni (namesIniWith ["names_static_dir = ./names/dist", "names_export_dir = /srv/names-out", "names_port = 9081"])
  let ListenerConfig {lNamesStaticDir = dir, lNamesExportDir = out, lNamesPort = port} = listener written
  (dir, out, port) `shouldBe` (Just "./names/dist", Just "/srv/names-out", Just 9081)
  Right split <- parseIni (namesIniWith ["serve_webapp = off", "names_export_dir = /srv/names-out"])
  let ListenerConfig {lNamesStaticDir = splitDir, lNamesExportDir = splitOut, lNamesPort = splitPort} = listener split
  (splitDir, splitOut, splitPort) `shouldBe` (Just "/srv/names", Just "/srv/names-out", Nothing)

testNamesSiteOnlyWithNames :: IO ()
testNamesSiteOnlyWithNames =
  forM_ ["names_static_dir = /srv/names", "names_export_dir = /srv/names-out", "names_port = 8081"] $ \key -> do
    r <- parseIni (withListenerKeys [key])
    either id (const "parsed") r `shouldBe` ("listener." <> T.unpack (T.takeWhile (/= ' ') key) <> " is set but [names] is absent, so nothing would use it")

testNamesPortRules :: IO ()
testNamesPortRules = do
  forM_ ["0", "65536"] $ \v -> refused ["names_port = " <> v] "listener.names_port must be between 1 and 65535"
  refused ["names_port = 8080"] "listener.names_port must differ from listener.badges_port"
  refused ["badges_port = 8081"] "listener.names_port must differ from listener.badges_port"
  refused ["serve_webapp = off", "names_port = 8081"] "listener.names_port is set but serve_webapp is off, so nothing would use it"
  Right moved <- parseIni (namesIniWith ["badges_port = 8081", "names_port = 8080"])
  let ListenerConfig {lBadgesPort, lNamesPort} = listener moved
  (lBadgesPort, lNamesPort) `shouldBe` (8081, Just 8080)
  where
    refused ks e = parseIni (namesIniWith ks) >>= (`shouldBe` e) . either id (const "parsed")

-- | namesIni with these lines first in [listener].
namesIniWith :: [T.Text] -> T.Text
namesIniWith ks = withListenerKeys ks <> T.unlines (namesSection <> chainSection)

testNamesAbsent :: IO ()
testNamesAbsent = do
  Right cfg <- parseIni fullIni
  names cfg `shouldBe` Nothing

testNamesNeedsChain :: IO ()
testNamesNeedsChain = do
  namesRefusal namesSection [] `shouldReturn` "[names] needs a [chain] section"
  namesRefusal [] chainSection `shouldReturn` "[chain] is set but [names] is absent, so nothing would use it"

testNamesYearsBelowTwo :: IO ()
testNamesYearsBelowTwo = do
  mapM_ (\v -> namesRefusal (namesSection <> ["years = " <> v]) chainSection `shouldReturn` "names.years must be at least 2") ["1", "0", "-2"]
  NamesConfig {nYears} <- parsedNames (namesSection <> ["years = 2"]) chainSection
  nYears `shouldBe` 2

testNamesBadTermStart :: IO ()
testNamesBadTermStart =
  mapM_
    (\v -> namesRefusal (namesSection <> ["term_start = " <> v]) chainSection >>= (`shouldContain` "names.term_start must be an ISO 8601 UTC time"))
    ["2027-01-01", "2027-01-01T00:00:00", "2027-13-01T00:00:00Z", "1 January 2027"]

testNamesBadTLD :: IO ()
testNamesBadTLD = do
  namesRefusal ["[names]", "tld = simplex.chat"] chainSection `shouldReturn` "names.tld must be simplex or testing, not simplex.chat"
  namesRefusal ["[names]", "years = 2"] chainSection `shouldReturn` "names.tld is required"

testNamesBadAddress :: IO ()
testNamesBadAddress = do
  let withSponsor v = map (\l -> if "sponsor =" `T.isPrefixOf` l then "sponsor = " <> v else l) chainSection
      refused v = namesRefusal namesSection (withSponsor v) `shouldReturn` "chain.sponsor is not an Ethereum address"
  -- the mixed-case form of sponsorAddr with one letter's case flipped fails its EIP-55 checksum
  refused "0x0b306BF915C4d645ff596e518fAf3F9669b97016"
  refused "0x0B306BF915C4d645ff596e518fAf3F9669b970"
  refused "0x0B306BF915C4d645ff596e518fAf3F9669b97016ff"
  refused "not-an-address"
  NamesConfig {nChain = ChainConfig {cSponsor}} <- parsedNames namesSection (withSponsor (T.toLower sponsorAddr))
  cSponsor `shouldBe` address sponsorAddr

testNamesBadServiceKey :: IO ()
testNamesBadServiceKey = do
  let withKey v = map (\l -> if "service_key =" `T.isPrefixOf` l then "service_key = " <> v else l) chainSection
      refused v = do
        e <- namesRefusal namesSection (withKey v)
        e `shouldBe` "chain.service_key must be 0x and 64 hex digits of a valid secp256k1 key"
  refused (T.drop 2 serviceKeyHex)
  refused (T.dropEnd 2 serviceKeyHex)
  refused ("0x" <> T.replicate 64 "0")
  -- the curve order n is not a valid key
  refused "0xfffffffffffffffffffffffffffffffebaaedce6af48a03bbfd25e8cd0364141"
  refused "0xzz0974bec39a17e36ba4a6b4d238ff944bacb478cbed5efcae784d7bf4f2ff80"

testNamesSecretsHidden :: IO ()
testNamesSecretsHidden = do
  let secretUrl = "ftp://eth.example.org/v2/rpc-token-value"
      withUrl v = map (\l -> if "rpc_url =" `T.isPrefixOf` l then "rpc_url = " <> v else l) chainSection
  e <- namesRefusal namesSection (withUrl secretUrl)
  e `shouldBe` "chain.rpc_url must be an http or https URL"
  e `shouldNotContain` "rpc-token-value"
  other <- namesRefusal namesSection (chainSection <> ["confirmations = many"])
  other `shouldNotContain` "rpc-token-value"
  other `shouldNotContain` T.unpack (T.drop 2 serviceKeyHex)
  Right cfg <- parseIni (namesIni namesSection chainSection)
  show cfg `shouldNotContain` "rpc-token-value"
  show cfg `shouldNotContain` T.unpack (T.drop 2 serviceKeyHex)
  show cfg `shouldContain` "chain 31337"

testNamesUnknownKeys :: IO ()
testNamesUnknownKeys = do
  let t = namesIni (namesSection <> ["yeers = 3"]) (chainSection <> ["rpc = x"])
  withIni t $ \p -> do
    Right ini <- readIniFile p
    unknownKeys ini `shouldMatchList` ["names.yeers", "chain.rpc"]
  NamesConfig {nYears} <- parsedNames (namesSection <> ["yeers = 3"]) (chainSection <> ["rpc = x"])
  nYears `shouldBe` 2

testNamesCodesOnly :: IO ()
testNamesCodesOnly = do
  NamesConfig {nCodesOnly = absent} <- parsedNames namesSection chainSection
  absent `shouldBe` False
  forM_ [("on", True), ("off", False)] $ \(value, expected) -> do
    let ns = namesSection <> ["codes_only = " <> value]
    withIni (namesIni ns chainSection) $ \p -> do
      Right ini <- readIniFile p
      unknownKeys ini `shouldBe` []
    NamesConfig {nCodesOnly} <- parsedNames ns chainSection
    (value, nCodesOnly) `shouldBe` (value, expected)
  namesRefusal (namesSection <> ["codes_only = yes"]) chainSection `shouldReturn` "codes_only must be on or off, not yes"

testNamesRecordEdits :: IO ()
testNamesRecordEdits = do
  NamesConfig {nRecordEditsPerYear = absent} <- parsedNames namesSection chainSection
  absent `shouldBe` 3
  forM_ [1, 100] $ \n -> do
    let ns = namesSection <> ["record_edits_per_year = " <> T.pack (show n)]
    withIni (namesIni ns chainSection) $ \p -> do
      Right ini <- readIniFile p
      unknownKeys ini `shouldBe` []
    NamesConfig {nRecordEditsPerYear} <- parsedNames ns chainSection
    (n, nRecordEditsPerYear) `shouldBe` (n, n)
  forM_ ["0", "101", "-3"] $ \v ->
    namesRefusal (namesSection <> ["record_edits_per_year = " <> v]) chainSection `shouldReturn` "names.record_edits_per_year must be between 1 and 100"
  namesRefusal (namesSection <> ["record_edits_per_year = three"]) chainSection `shouldReturn` "record_edits_per_year must be a whole number"

-- | Names on the local chain with no payment provider, as a run with dev keys has.
localNamesIni :: [T.Text] -> T.Text
localNamesIni cs = T.unlines (["[listener]"] <> namesSection <> cs)

devKeysIssuer :: T.Text -> [T.Text]
devKeysIssuer keys = ["[issuer]", "index = 1", "private_key = " <> issuerSecret, "dev_keys = " <> keys]

withChainId :: T.Text -> [T.Text]
withChainId v = map (\l -> if "chain_id =" `T.isPrefixOf` l then "chain_id = " <> v else l) chainSection

testDevKeysLocalChain :: IO ()
testDevKeysLocalChain = do
  Right sk <- pure (strDecode (B.pack (T.unpack issuerSecret)))
  Right local <- parseIni (localNamesIni chainSection <> T.unlines (devKeysIssuer ("9:" <> issuerSecret <> ", 12:" <> issuerSecret)))
  devIssuerKeys local `shouldBe` [BadgeIssuerKey {keyIdx = 9, secretKey = sk}, BadgeIssuerKey {keyIdx = 12, secretKey = sk}]
  Right without <- parseIni (namesIni namesSection chainSection)
  devIssuerKeys without `shouldBe` []
  let refused = "issuer.dev_keys is accepted only with [names] on the local chain, [chain] chain_id = 31337" :: String
  forM_ ["1", "11155111", "31338"] $ \chainId ->
    parseIni (localNamesIni (withChainId chainId) <> T.unlines (devKeysIssuer ("9:" <> issuerSecret))) `shouldReturn` Left refused
  parseIni (T.unlines ["[listener]"] <> T.unlines (devKeysIssuer ("9:" <> issuerSecret))) `shouldReturn` Left refused

testDevKeysMalformed :: IO ()
testDevKeysMalformed = do
  forM_ [issuerSecret, "9=" <> issuerSecret] $ \keys -> do
    e <- refusal keys
    e `shouldBe` "issuer.dev_keys must be <index>:<secret> entries separated by commas"
    e `shouldNotContain` T.unpack issuerSecret
  refusal ("0:" <> issuerSecret) `shouldReturn` "issuer.dev_keys index must be a positive whole number"
  refusal "9:not-a-key" `shouldReturn` "issuer.dev_keys secret is not a valid issuer secret; use the value from `simplex-chat badge keygen`"
  refusal ("9:" <> issuerSecret <> ",9:" <> issuerSecret) `shouldReturn` "issuer.dev_keys names an index twice"
  where
    refusal :: T.Text -> IO String
    refusal keys =
      parseIni (localNamesIni chainSection <> T.unlines (devKeysIssuer keys))
        >>= either pure (\_ -> "" <$ expectationFailure (T.unpack keys <> " must not boot"))

devKeysBTCPay :: T.Text -> [T.Text]
devKeysBTCPay host = ["[btcpay]", "host = " <> host, "api_key = token-value", "store_id = store-value", "webhook_secret = secret-value"]

devKeysStripe :: T.Text -> T.Text -> [T.Text]
devKeysStripe secret publishable = ["[stripe]", "secret_key = " <> secret, "publishable_key = " <> publishable, "webhook_secret = whsec_x"]

devKeysWith :: [T.Text] -> IO (Either String ServiceConfig)
devKeysWith provider = parseIni (localNamesIni (chainSection <> provider) <> T.unlines (devKeysIssuer ("9:" <> issuerSecret)))

testDevKeysTestPayments :: IO ()
testDevKeysTestPayments = do
  let loopbacks = ["https://127.0.0.1:8443", "https://localhost/", "https://[::1]:8443/btcpay", "https://LOCALHOST"]
      stripes = [devKeysStripe "sk_test_x" "pk_test_x", devKeysStripe "rk_test_x" "pk_test_x"]
  forM_ (map devKeysBTCPay loopbacks <> stripes <> [devKeysBTCPay "https://127.0.0.1" <> devKeysStripe "rk_test_x" "pk_test_x"]) $ \provider ->
    devKeysWith provider >>= \case
      Right cfg -> length (devIssuerKeys cfg) `shouldBe` 1
      Left e -> expectationFailure (show provider <> " must boot with dev_keys, got: " <> e)
  parseIni (localNamesIni (withChainId "1" <> devKeysStripe "rk_test_x" "pk_test_x") <> T.unlines (devKeysIssuer ("9:" <> issuerSecret)))
    `shouldReturn` Left "issuer.dev_keys is accepted only with [names] on the local chain, [chain] chain_id = 31337"

testDevKeysLivePayments :: IO ()
testDevKeysLivePayments = do
  let liveSecret = "issuer.dev_keys is refused with a live Stripe key: stripe.secret_key must be a test key, sk_test_ or rk_test_" :: String
      livePublishable = "issuer.dev_keys is refused with a live Stripe key: stripe.publishable_key must be a test key, pk_test_" :: String
      remote = "issuer.dev_keys is refused with a remote BTCPay: btcpay.host must be on loopback, 127.0.0.1, ::1 or localhost" :: String
  forM_ ["sk_live_secretvalue", "rk_live_secretvalue", "sk_test", "pk_test_x"] $ \secret ->
    devKeysWith (devKeysStripe secret "pk_test_x") `shouldReturn` Left liveSecret
  devKeysWith (devKeysStripe "rk_test_x" "pk_live_x") `shouldReturn` Left livePublishable
  forM_ ["https://btcpay.example.org", "https://127.0.0.2", "https://localhost.example.org", "https://user@btcpay.example.org", "https://[::2]:8443"] $ \host ->
    devKeysWith (devKeysBTCPay host) `shouldReturn` Left remote
  devKeysWith (devKeysBTCPay "https://127.0.0.1" <> devKeysStripe "sk_live_secretvalue" "pk_test_x") `shouldReturn` Left liveSecret
  devKeysWith (devKeysStripe "sk_live_secretvalue" "pk_test_x") >>= either (`shouldNotContain` "secretvalue") (const (expectationFailure "a live key must not boot"))
  parseIni (localNamesIni (chainSection <> devKeysBTCPay "https://btcpay.example.org" <> devKeysStripe "sk_live_x" "pk_live_x")) >>= (`shouldSatisfy` either (const False) (const True))

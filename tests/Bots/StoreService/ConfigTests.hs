{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Bots.StoreService.ConfigTests where

import StoreService.Config
import Control.Monad (forM_)
import qualified Data.ByteString.Char8 as B
import Data.Either (isLeft)
import Data.Ini (readIniFile)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Simplex.Messaging.Encoding.String (strDecode)
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
  it "refuses a missing static_dir" testAbsentStaticDir
  it "reads serve_webapp off and a webapp_export_dir" testWebappSplitDeployment
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
      "static_dir = /srv/badges",
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
    withIni ("static_dir = /srv/badges\n" <> fullIni) $ \stray -> do
      Right strayIni <- readIniFile stray
      unknownKeys strayIni `shouldContain` ["static_dir, written above the first section header"]
    withIni fullIni $ \clean -> do
      Right cleanIni <- readIniFile clean
      unknownKeys cleanIni `shouldBe` []

testUnknownSectionStillBoots :: IO ()
testUnknownSectionStillBoots =
  parseIni (fullIni <> "[legacy]\nsetting = on\n") >>= \r -> case r of
    Right cfg -> lStaticDir (listener cfg) `shouldBe` "/srv/badges"
    Left e -> expectationFailure ("a section nothing reads must be ignored, not refused: " <> e)

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
listenerIni = "[listener]\nstatic_dir = /srv/web\n"

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

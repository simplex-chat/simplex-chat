{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Bots.BadgeConfigTests where

import BadgeService.Config
import Data.Either (isLeft)
import Data.Ini (readIniFile)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.IO as T
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
  it "refuses a stripe section in this build" testStripeRefused
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
  it "has no issuer keys when the section is absent" testIssuerAbsent
  it "reads every key_<n> and the default that names one" testIssuerKeys
  it "refuses a default that names no listed key" testIssuerDefaultMissing
  it "refuses a key that is not named key_<n>" testIssuerKeyName
  it "refuses a padded index, which would collapse onto another key" testIssuerKeyPadded
  it "refuses a setting the section does not define" testIssuerUnknownSetting
  it "refuses a secret that is not a valid issuer key" testIssuerBadSecret
  it "refuses a section with a default and no keys" testIssuerNoKeys
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
  let ListenerConfig {lHost, lPort, lStaticDir, lTrustForwardedFor} = listener cfg
  lHost `shouldBe` "127.0.0.1"
  lPort `shouldBe` 8080
  lStaticDir `shouldBe` "/srv/badges"
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

testStripeRefused :: IO ()
testStripeRefused =
  withIni (fullIni <> "[stripe]\nsecret_key = rk_live_x\n") $ \p -> do
    r <- readServiceConfig p
    case r of
      Left e -> e `shouldContain` "card payments"
      Right _ -> expectationFailure "a stripe section must fail this build"

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
      -- 2^64 + 4, which a machine-width read would wrap to a legal 4 seconds
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

-- | A misspelled setting takes its default in silence, and two of them decide when money
-- counts as received. Naming them is what the operator gets instead.
testUnknownKeysAreNamed :: IO ()
testUnknownKeysAreNamed =
  withIni (fullIni <> "speed_polcy = LowSpeed\ntrust_forwaded_for = on\n[poll]\nwaiting_secnds = 5\n[dev]\nchat_redem = on\n") $ \p -> do
    Right ini <- readIniFile p
    -- the typo is in [btcpay], which `fullIni` leaves open, and one in each other section
    unknownKeys ini `shouldMatchList` ["btcpay.speed_polcy", "btcpay.trust_forwaded_for", "poll.waiting_secnds", "dev.chat_redem"]
    -- and a section header nobody reads, which disables the provider just as quietly
    withIni (T.replace "[btcpay]" "[btcpai]" fullIni) $ \wrongSection -> do
      Right sectionIni <- readIniFile wrongSection
      unknownKeys sectionIni `shouldContain` ["[btcpai]"]
    -- and a setting written above the first header, which the parser keeps and nothing reads
    withIni ("chat_redeem = on\n" <> fullIni) $ \stray -> do
      Right strayIni <- readIniFile stray
      unknownKeys strayIni `shouldContain` ["chat_redeem, written above the first section header"]
    -- and a file with nothing misspelled names nothing
    withIni fullIni $ \clean -> do
      Right cleanIni <- readIniFile clean
      unknownKeys cleanIni `shouldBe` []

-- | The ini parser stops at the first line it cannot read and keeps what it has, so without
-- this a missing `=` in [listener] takes every section below it, and the provider with it.
testMalformedLineRefused :: IO ()
testMalformedLineRefused =
  withIni (T.replace "static_dir = /srv/badges" "static_dir = /srv/badges\ntrust_forwarded_for on" fullIni) $ \p ->
    readServiceConfig p >>= \r -> case r of
      Left e -> e `shouldContain` "malformed"
      Right cfg -> expectationFailure ("a truncated file must not boot, and this one kept " <> show (btcpay cfg))

-- | Comments and blank lines after the last setting are not content. `iniParser` stops before
-- a trailing comment, so the strictness that refuses a truncated file must not refuse this:
-- commenting out the last section is enough to produce one.
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

-- | The reason only: the caller prints it under the path it was asked for, and naming the file
-- here as well put it in the line twice.
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

-- a real 32-byte secret, base64url, as `simplex-chat badge keygen` prints it
issuerSecret1, issuerSecret2 :: T.Text
issuerSecret1 = "Ea5wG-J2mQjPBu9YfSJRKPnGnzoIdEE-8VaMh_wY2Bg="
issuerSecret2 = "Zm9vYmFyYmF6cXV1eDEyMzQ1Njc4OTBhYmNkZWZnaGk="

withIssuer :: [T.Text] -> (FilePath -> IO a) -> IO a
withIssuer ls = withIni (fullIni <> T.unlines ("[issuer]" : ls))

testIssuerAbsent :: IO ()
testIssuerAbsent = withIni fullIni $ \p -> do
  Right cfg <- readServiceConfig p
  issuer cfg `shouldBe` Nothing

testIssuerKeys :: IO ()
testIssuerKeys =
  withIssuer ["default = key_3", "key_1 = " <> issuerSecret1, "key_3 = " <> issuerSecret2] $ \p -> do
    Right cfg <- readServiceConfig p
    case issuer cfg of
      Nothing -> expectationFailure "the issuer section was present"
      Just IssuerConfig {iKeys, iDefaultIdx} -> do
        M.keys iKeys `shouldBe` [1, 3]
        iDefaultIdx `shouldBe` 3

testIssuerDefaultMissing :: IO ()
testIssuerDefaultMissing =
  withIssuer ["default = key_2", "key_1 = " <> issuerSecret1] $ \p -> do
    r <- readServiceConfig p
    case r of
      Left e -> e `shouldContain` "key_2"
      Right _ -> expectationFailure "a default naming no listed key must fail at boot"

-- `key_01` reads as 1, so it would land on `key_1` in the map and one of the two secrets would
-- be dropped without ever being checked. One spelling per index, refused at boot.
testIssuerKeyPadded :: IO ()
testIssuerKeyPadded =
  withIssuer ["default = key_1", "key_1 = " <> issuerSecret1, "key_01 = " <> issuerSecret2] $ \p -> do
    r <- readServiceConfig p
    case r of
      Left e -> e `shouldContain` "key_01"
      Right _ -> expectationFailure "two spellings of one index must fail at boot"

testIssuerKeyName :: IO ()
testIssuerKeyName =
  withIssuer ["default = key_1", "key_1 = " <> issuerSecret1, "key_x = " <> issuerSecret2] $ \p -> do
    r <- readServiceConfig p
    case r of
      Left e -> e `shouldContain` "key_x"
      Right _ -> expectationFailure "a key that is not key_<n> must fail at boot"

testIssuerUnknownSetting :: IO ()
testIssuerUnknownSetting =
  withIssuer ["default = key_1", "key_1 = " <> issuerSecret1, "rotate = yes"] $ \p -> do
    r <- readServiceConfig p
    case r of
      Left e -> e `shouldContain` "rotate"
      Right _ -> expectationFailure "an unknown setting must fail rather than be ignored"

testIssuerBadSecret :: IO ()
testIssuerBadSecret =
  withIssuer ["default = key_1", "key_1 = not-a-key"] $ \p ->
    readServiceConfig p >>= (`shouldSatisfy` isLeft)

testIssuerNoKeys :: IO ()
testIssuerNoKeys =
  withIssuer ["default = key_1"] $ \p -> do
    r <- readServiceConfig p
    case r of
      Left e -> e `shouldContain` "key_<n>"
      Right _ -> expectationFailure "a section with no keys must fail at boot"

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

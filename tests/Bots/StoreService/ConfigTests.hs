{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Bots.StoreService.ConfigTests where

import StoreService.Config
import Control.Monad (forM_)
import qualified Data.ByteArray as BA
import qualified Data.ByteString.Char8 as B
import Data.Either (isLeft)
import Data.Ini (readIniFile)
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

namesConfigTests :: Spec
namesConfigTests = describe "StoreService names: config" $ do
  it "reads [names] and [chain], applying every documented default" testNamesDefaults
  it "reads every [names] and [chain] setting written out" testNamesWrittenOut
  it "reads the names keys of [listener]" testNamesListenerKeys
  it "refuses names_hosts or names_export_dir without names_static_dir" testNamesSiteNeedsItsBuild
  it "reads names_hosts entries without a port or the root's trailing dot, and IPv6 literals whole" testNamesHostsNormalised
  it "leaves the names side off when [names] is absent" testNamesAbsent
  it "refuses [names] without [chain], and [chain] without [names]" testNamesNeedsChain
  it "refuses years below 2" testNamesYearsBelowTwo
  it "refuses a term_start that is not an ISO 8601 UTC time" testNamesBadTermStart
  it "refuses a TLD other than simplex or testing" testNamesBadTLD
  it "refuses a bad or wrongly checksummed contract address, naming the key" testNamesBadAddress
  it "refuses a bad service key without quoting it" testNamesBadServiceKey
  it "keeps the RPC URL and the service key out of errors and the shown config" testNamesSecretsHidden
  it "names a [names] or [chain] setting nothing reads, and still boots" testNamesUnknownKeys
  it "accepts [issuer] dev_keys on chain 31337 only, with [names] on" testDevKeysLocalChain
  it "refuses a malformed dev_keys entry or an index named twice, quoting no secret" testDevKeysMalformed
  it "refuses [issuer] dev_keys beside [btcpay] or [stripe]" testDevKeysNoPayments

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

namesSection :: [T.Text]
namesSection = ["[names]", "tld = testing"]

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
  let ns = ["[names]", "tld = simplex", "years = 5", "term_start = 2027-03-01T12:00:00Z", "term_slack_seconds = 60", "reveal_margin_seconds = 0", "max_order_moves = 0"]
      cs = chainSection <> ["confirmations = 0", "gas_limit_cap = 500000", "max_fee_per_gas = 30000000000"]
  NamesConfig {nTLD, nYears, nTermStart, nTermSlackSeconds, nRevealMarginSeconds, nMaxOrderMoves, nChain = ChainConfig {cConfirmations, cGasLimitCap, cMaxFeePerGas}} <- parsedNames ns cs
  nTLD `shouldBe` TLDSimplex
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
  let withKeys = T.replace "static_dir = /srv/badges\n" "static_dir = /srv/badges\nnames_static_dir = /srv/names\nnames_export_dir = /srv/names-out\nnames_hosts = simplex.domains, WWW.simplex.domains ,\n" fullIni
  Right cfg <- parseIni withKeys
  let ListenerConfig {lNamesStaticDir, lNamesExportDir, lNamesHosts} = listener cfg
  lNamesStaticDir `shouldBe` Just "/srv/names"
  lNamesExportDir `shouldBe` Just "/srv/names-out"
  lNamesHosts `shouldBe` ["simplex.domains", "www.simplex.domains"]
  Right plain <- parseIni fullIni
  let ListenerConfig {lNamesStaticDir = noDir, lNamesExportDir = noExport, lNamesHosts = noHosts} = listener plain
  (noDir, noExport, noHosts) `shouldBe` (Nothing, Nothing, [])

testNamesSiteNeedsItsBuild :: IO ()
testNamesSiteNeedsItsBuild =
  forM_ ["names_hosts = simplex.domains", "names_export_dir = /srv/names-out"] $ \key -> do
    let withKey = T.replace "static_dir = /srv/badges\n" ("static_dir = /srv/badges\n" <> key <> "\n") fullIni
    r <- parseIni withKey
    either id (const "parsed") r `shouldBe` "listener.names_static_dir is required with names_hosts or names_export_dir"

testNamesHostsNormalised :: IO ()
testNamesHostsNormalised = do
  let withHosts = T.replace "static_dir = /srv/badges\n" "static_dir = /srv/badges\nnames_static_dir = /srv/names\nnames_hosts = simplex.domains:8443, Names.Example., www.simplex.domains.:443\n" fullIni
  Right cfg <- parseIni withHosts
  lNamesHosts (listener cfg) `shouldBe` ["simplex.domains", "names.example", "www.simplex.domains"]
  let withLiterals = T.replace "static_dir = /srv/badges\n" "static_dir = /srv/badges\nnames_static_dir = /srv/names\nnames_hosts = [2001:DB8::1], 2001:db8::2, [::1]:8443\n" fullIni
  Right literals <- parseIni withLiterals
  lNamesHosts (listener literals) `shouldBe` ["[2001:db8::1]", "[2001:db8::2]", "[::1]"]

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

-- | Names on the local chain with no payment provider, as a run with dev keys has.
localNamesIni :: [T.Text] -> T.Text
localNamesIni cs = T.unlines (["[listener]", "static_dir = /srv/badges"] <> namesSection <> cs)

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
  parseIni (T.unlines ["[listener]", "static_dir = /srv/badges"] <> T.unlines (devKeysIssuer ("9:" <> issuerSecret))) `shouldReturn` Left refused

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

testDevKeysNoPayments :: IO ()
testDevKeysNoPayments = do
  let devKeys = T.unlines (devKeysIssuer ("9:" <> issuerSecret))
      btcpay = ["[btcpay]", "host = https://btcpay.example.org", "api_key = token-value", "store_id = store-value", "webhook_secret = secret-value"]
      stripe = ["[stripe]", "secret_key = rk_test_x", "publishable_key = pk_test_x", "webhook_secret = whsec_x"]
      refused = "issuer.dev_keys is refused with [btcpay] or [stripe], since a run with dev keys takes no payments" :: String
  forM_ [btcpay, stripe, btcpay <> stripe] $ \provider ->
    parseIni (localNamesIni (chainSection <> provider) <> devKeys) `shouldReturn` Left refused
  parseIni (localNamesIni (chainSection <> btcpay)) >>= (`shouldSatisfy` either (const False) (const True))

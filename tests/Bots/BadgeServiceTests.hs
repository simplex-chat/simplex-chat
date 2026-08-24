{-# LANGUAGE CPP #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Bots.BadgeServiceTests where

import BadgeService.Catalog (catalogTotals, defaultCatalog, offerTotal, seedCatalog)
import BadgeService.Config (BadgeServiceConfig (..), readBadgeServiceConfig)
import BadgeService.Options
import BadgeService.Service
import BadgeService.Store
import ChatClient
import ChatTests.DBUtils
import ChatTests.Utils
import Control.Concurrent (forkIO, killThread, threadDelay)
import Control.Concurrent.STM (atomically)
import Control.Exception (SomeException, evaluate, finally, try)
import Crypto.Random (getRandomBytes)
import qualified Data.Aeson as J
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as BC
import Data.List (find, isInfixOf)
import Data.Maybe (fromJust, isJust)
import Data.String (fromString)
import Data.Text (Text)
import Data.Time.Clock (addUTCTime, diffUTCTime, getCurrentTime, nominalDay)
import Data.Word (Word32)
import Simplex.Chat.Badges (BadgeMasterKey (..), BadgeType (..))
import Simplex.Chat.Badges.Service (BadgeCatalog (..), BadgeOffer (..), BadgePrice (..))
import Simplex.Chat.Badges.Types
  ( BadgeItemStatus (..),
    BadgeLedgerEntry (..),
    BadgeOfferId (..),
    BadgePurchaseStatus (..),
    LedgerCreditType (..),
    LedgerDebitType (..),
    LedgerEntryType (..),
    OfferDiscount (..),
  )
import Simplex.Chat.Controller (ChatConfig)
import Simplex.Chat.Options (CoreChatOpts (..))
import Simplex.Chat.Options.DB
import Simplex.Chat.PaymentService.Types (CurrencyAmount (..))
import Simplex.Chat.Types (ChatPeerType (..), Profile (..))
import Simplex.Messaging.Agent.Store.Common (DBStore, withConnection)
import qualified Simplex.Messaging.Agent.Store.DB as DB
import Simplex.Messaging.Agent.Store.Interface (closeDBStore, createDBStore)
import Simplex.Messaging.Agent.Store.Shared (Migration (..), MigrationConfig (..), MigrationConfirmation (..), MigrationsToRun (..), toDownMigration)
import qualified Simplex.Messaging.Crypto as C
import Simplex.Messaging.Crypto.BBS (BBSPublicKey (..), BBSSecretKey (..), bbsKeyGen)
import Simplex.Messaging.Encoding.String (strEncode)
import System.FilePath ((</>))
import Test.Hspec hiding (it)
#if defined(dbPostgres)
import BadgeService.Store.Postgres.Migrations (badgeServiceSchemaMigrations)
import Database.PostgreSQL.Simple (Only (..))
import qualified Simplex.Messaging.Agent.Store.Postgres.Migrations as Migrations
#else
import BadgeService.Store.SQLite.Migrations (badgeServiceSchemaMigrations)
import Database.SQLite.Simple (Only (..))
import qualified Simplex.Messaging.Agent.Store.SQLite.Migrations as Migrations
#endif

badgeServiceTests :: SpecWith TestParams
badgeServiceTests = do
  it "should respond with unsupported_version to redeem" testBadgeServiceRedeemUnsupported
  it "should migrate web_orders, codes and provider_events up and down" testBadgeServiceWebOrderSchemaMigration
  it "should seed the catalog idempotently and preserve a deprecated price" testBadgeServiceCatalogSeeding
  it "should price 3 months at 2x and 12 months at 6x the monthly price" testBadgeCatalogOfferTotal
  it "should fill total for every seeded offer" testBadgeCatalogTotalsFillsSeededOffers
  it "should reject an offer with freeMonths >= months instead of wrapping" testBadgeCatalogOfferTotalRejectsBadFreeMonths
  it "should encode BadgeItemStatus on the wire as active/deprecated/disabled" testBadgeItemStatusJsonWireFormat
  it "should fail to start on a missing config file, naming the file" testBadgeServiceConfigMissingFile
  it "should fail to start on an unparsable value, naming the key" testBadgeServiceConfigUnparsableValue
  it "should fail to start on an unknown key in a known section" testBadgeServiceConfigUnknownKey
  it "should fail to start on a missing [issuer] section" testBadgeServiceConfigMissingIssuerSection
  it "should fail to start on a missing [codes] section" testBadgeServiceConfigMissingCodesSection
  it "should fail to start on a half-configured [btcpay] section" testBadgeServiceConfigHalfConfiguredBtcPay
  it "should fail to start on a half-configured [stripe] section" testBadgeServiceConfigHalfConfiguredStripe
  it "should fail to start when a provider is configured without [web]" testBadgeServiceConfigProviderRequiresWeb
  it "should start with just [issuer] and [codes], no provider section" testBadgeServiceConfigMinimalStarts
  it "should start the service from a complete config with web and both providers" testBadgeServiceCompleteConfigStarts
  it "should create a purchase and append ledger entries readable back in order" testBadgeStorePurchaseAndLedger
  it "should disable a price out of the active catalog while both stay reachable by id" testBadgeStoreSetPriceStatusDisabled
  it "should return the redeeming purchase key from getCodeByHash" testBadgeStoreGetCodeByHashRedeemer
  it "should clear both redemption columns and set unredeemed_at" testBadgeStoreUnredeemCode

badgeProfile :: Profile
badgeProfile = Profile {displayName = "SimpleX Badges", fullName = "", shortDescr = Nothing, description = Nothing, image = Nothing, contactLink = Nothing, peerType = Just CPTBot, preferences = Nothing, badge = Nothing, contactDomain = Nothing}

serviceDbPrefix :: FilePath
serviceDbPrefix = "badge_service"

mkBadgeServiceOpts :: TestParams -> BadgeServiceOpts
mkBadgeServiceOpts TestParams {tmpPath = ps} =
  BadgeServiceOpts
    { coreOptions =
        testCoreOpts
          { dbOptions =
              (dbOptions testCoreOpts)
#if defined(dbPostgres)
                {dbSchemaPrefix = "client_" <> serviceDbPrefix}
#else
                {dbFilePrefix = ps </> serviceDbPrefix}
#endif
          },
      serviceName = "SimpleX Badges",
      clientService = True,
      noAddress = False,
      runCLI = False,
      testing = True,
      configFile = badgeServiceConfigPath ps
    }

badgeServiceConfigPath :: FilePath -> FilePath
badgeServiceConfigPath tmpPath = tmpPath </> "badge_service.ini"

-- Generates a real issuer key file (the two-line `badge keygen` output: "secret ..\npublic
-- ..") and a real 32-byte code secret (base64-encoded), at fixed names under tmpPath. A6
-- doesn't read either file's contents -- that's B3 and B4 -- but the harness must still hand
-- every later step real files at real paths, per the config keys naming secrets as files.
writeTestBadgeServiceSecrets :: FilePath -> IO (FilePath, FilePath)
writeTestBadgeServiceSecrets tmpPath = do
  let issuerKeyFile = tmpPath </> "badge-issuer.keys"
      codeSecretFile = tmpPath </> "badge-code.secret"
  Right (BBSPublicKey pk, BBSSecretKey sk) <- bbsKeyGen
  writeFile issuerKeyFile $ "secret " <> BC.unpack (strEncode sk) <> "\npublic " <> BC.unpack (strEncode pk) <> "\n"
  codeSecret <- getRandomBytes 32
  writeFile codeSecretFile $ BC.unpack (B64.encode codeSecret) <> "\n"
  pure (issuerKeyFile, codeSecretFile)

issuerCodesIniLines :: FilePath -> FilePath -> [String]
issuerCodesIniLines issuerKeyFile codeSecretFile =
  [ "[issuer]",
    "key_file = " <> issuerKeyFile,
    "key_idx = 1",
    "",
    "[codes]",
    "secret_file = " <> codeSecretFile,
    "default_expiry_days = 365"
  ]

-- Writes a complete but minimal badge_service.ini (required sections only, no provider
-- section) at the path mkBadgeServiceOpts points BadgeServiceOpts's configFile at. Provider
-- sections are omitted until E2 and F1 add them.
writeTestBadgeServiceConfig :: TestParams -> IO ()
writeTestBadgeServiceConfig TestParams {tmpPath} = do
  (issuerKeyFile, codeSecretFile) <- writeTestBadgeServiceSecrets tmpPath
  writeFile (badgeServiceConfigPath tmpPath) $ unlines (issuerCodesIniLines issuerKeyFile codeSecretFile)

withBadgeService :: HasCallStack => TestParams -> (TestCC -> String -> IO ()) -> IO ()
withBadgeService ps = withBadgeServiceConfig ps (writeTestBadgeServiceConfig ps)

-- Shared by withBadgeService and testBadgeServiceCompleteConfigStarts: the two-phase startup
-- dance (CreateMyAddress, then ShowMyAddress) is the same regardless of what the config looks
-- like, as long as it's valid; writeConfig is what varies.
withBadgeServiceConfig :: HasCallStack => TestParams -> IO () -> (TestCC -> String -> IO ()) -> IO ()
withBadgeServiceConfig ps writeConfig test = do
  let opts = mkBadgeServiceOpts ps
  writeConfig
  withNewTestChatCfg ps testCfg serviceDbPrefix badgeProfile $ \_ -> pure ()
  -- First start: badge service takes the CreateMyAddress branch.
  runBadgeService testCfg opts (pure ())
  -- Reopen the DB to read the link the service created.
  bsLink <- withTestChat ps serviceDbPrefix $ \bs -> do
    bs <## "subscribed 1 connections on server localhost"
    bs ##> "/sa"
    (sLink, _) <- getContactLinks bs False
    bs <## "auto_accept off"
    pure sLink
  -- Second start: badge service takes the ShowMyAddress branch, then serves the test body.
  runBadgeService testCfg opts $
    withNewTestChatCfg ps testCfg "client" bobProfile $ \client ->
      test client bsLink

runBadgeService :: ChatConfig -> BadgeServiceOpts -> IO () -> IO ()
runBadgeService cfg opts action = do
  t <- forkIO $ badgeService opts cfg
  threadDelay 500000
  action `finally` killThread t

testBadgeServiceRedeemUnsupported :: HasCallStack => TestParams -> IO ()
testBadgeServiceRedeemUnsupported ps =
  withBadgeService ps $ \client bsLink -> do
    let redeemReq =
          "{\"version\":1,\"request\":{\"type\":\"purchaseBadge\",\"payment\":{\"type\":\"code\",\"code\":\"TEST-CODE\"}}}"
    client ##> ("/_service_request 1 " <> bsLink <> " " <> redeemReq)
    client <## "service response: {\"code\":\"unsupported_version\",\"type\":\"error\"}"

-- Applies every migration except 20260821_badge_service_web, then exercises that one
-- migration's up/down/up cycle directly, checking the three new tables appear and
-- disappear as expected.
testBadgeServiceWebOrderSchemaMigration :: HasCallStack => TestParams -> IO ()
testBadgeServiceWebOrderSchemaMigration ps = do
  let dbOpts = toDBOpts (dbOptions $ coreOptions $ mkBadgeServiceOpts ps) chatSuffix False chatDBFunctions
      priorMigrations = filter ((/= migrationName) . name) badgeServiceSchemaMigrations
      webOrderMigration = fromJust $ find ((== migrationName) . name) badgeServiceSchemaMigrations
      downMigration = fromJust $ toDownMigration webOrderMigration
  Right st <- createDBStore dbOpts priorMigrations (MigrationConfig MCError Nothing)
  mapM_ (assertTableMissing st) newTables
  runMigrationsToRun st $ MTRUp [webOrderMigration]
  mapM_ (assertTableExists st) newTables
  runMigrationsToRun st $ MTRDown [downMigration]
  mapM_ (assertTableMissing st) newTables
  runMigrationsToRun st $ MTRUp [webOrderMigration]
  mapM_ (assertTableExists st) newTables
  closeDBStore st
  where
    migrationName = "20260821_badge_service_web"
    newTables = ["sx_badge_service_web_orders", "sx_badge_service_codes", "sx_badge_service_provider_events"]
    assertTableExists st tbl =
      withConnection st (\db -> DB.query_ db (tableCountQuery tbl)) `shouldReturn` [Only (0 :: Int)]
    assertTableMissing st tbl = do
      r <- try (withConnection st (\db -> DB.query_ db (tableCountQuery tbl))) :: IO (Either SomeException [Only Int])
      case r of
        Left _ -> pure ()
        Right rows -> expectationFailure $ tbl <> " should not exist after down migration, got: " <> show rows
    tableCountQuery tbl = fromString ("SELECT count(*) FROM " <> tbl)

-- Seeds a fresh database, seeds again, and asserts price/offer row counts are unchanged;
-- then deprecates one price directly with SQL (B1's setPriceStatus doesn't exist yet) and
-- asserts a further re-seed leaves it deprecated rather than reviving it.
testBadgeServiceCatalogSeeding :: HasCallStack => TestParams -> IO ()
testBadgeServiceCatalogSeeding ps = do
  let dbOpts = toDBOpts (dbOptions $ coreOptions $ mkBadgeServiceOpts ps) chatSuffix False chatDBFunctions
  Right st <- createDBStore dbOpts badgeServiceSchemaMigrations (MigrationConfig MCError Nothing)
  seedCatalog st
  pricesAfterFirstSeed <- rowCount st "sx_badge_service_badge_prices"
  offersAfterFirstSeed <- rowCount st "sx_badge_service_badge_offers"
  pricesAfterFirstSeed `shouldBe` 2
  offersAfterFirstSeed `shouldBe` 4
  [Only deprecatedPriceId] <-
    withConnection st (\db -> DB.query_ db "SELECT price_id FROM sx_badge_service_badge_prices LIMIT 1") :: IO [Only Text]
  withConnection
    st
    (\db -> DB.execute db "UPDATE sx_badge_service_badge_prices SET status = 'deprecated' WHERE price_id = ?" (Only deprecatedPriceId))
  seedCatalog st
  pricesAfterSecondSeed <- rowCount st "sx_badge_service_badge_prices"
  offersAfterSecondSeed <- rowCount st "sx_badge_service_badge_offers"
  pricesAfterSecondSeed `shouldBe` pricesAfterFirstSeed
  offersAfterSecondSeed `shouldBe` offersAfterFirstSeed
  [Only statusAfterReseed] <-
    withConnection st (\db -> DB.query db "SELECT status FROM sx_badge_service_badge_prices WHERE price_id = ?" (Only deprecatedPriceId)) :: IO [Only Text]
  statusAfterReseed `shouldBe` "deprecated"
  closeDBStore st
  where
    rowCount :: DBStore -> String -> IO Int
    rowCount st tbl = do
      [Only n] <- withConnection st (\db -> DB.query_ db (fromString ("SELECT count(*) FROM " <> tbl)))
      pure n

-- offerTotal must price 3 months at exactly 2x the monthly price and 12 months at exactly
-- 6x, for both badge types (UX §6.12).
testBadgeCatalogOfferTotal :: HasCallStack => TestParams -> IO ()
testBadgeCatalogOfferTotal _ps = do
  now <- getCurrentTime
  let BadgeCatalog {prices, offers} = defaultCatalog now
      priceFor pid = fromJust $ find (\BadgePrice {priceId = pid'} -> pid' == pid) prices
  mapM_ (assertOfferTotal priceFor) offers
  where
    assertOfferTotal priceFor offer@BadgeOffer {months, priceId = Just pid} = do
      let BadgePrice {monthPrice = CurrencyAmount monthly} = priceFor pid
          CurrencyAmount total = offerTotal (priceFor pid) (Just offer)
          multiplier = if months == 3 then 2 else 6 :: Word32
      total `shouldBe` monthly * multiplier
    assertOfferTotal _ BadgeOffer {priceId = Nothing} =
      expectationFailure "seeded offer must be pinned to a price"

-- catalogTotals must fill total for all four seeded offers.
testBadgeCatalogTotalsFillsSeededOffers :: HasCallStack => TestParams -> IO ()
testBadgeCatalogTotalsFillsSeededOffers _ps = do
  now <- getCurrentTime
  let BadgeCatalog {offers} = catalogTotals (defaultCatalog now)
  length offers `shouldBe` 4
  all (\BadgeOffer {total} -> isJust total) offers `shouldBe` True

-- A Word8 subtraction of freeMonths from months is unsigned and unguarded: an offer with
-- freeMonths >= months (a typo, a future repricing) would wrap silently
-- (3 - 12 :: Word8 == 247) and hand out a wildly wrong charge. offerTotal must instead fail
-- loudly, naming the offer, before it ever reaches that subtraction.
testBadgeCatalogOfferTotalRejectsBadFreeMonths :: HasCallStack => TestParams -> IO ()
testBadgeCatalogOfferTotalRejectsBadFreeMonths _ps = do
  now <- getCurrentTime
  let BadgeCatalog {prices} = defaultCatalog now
      price@BadgePrice {priceId} = fromJust $ find (\BadgePrice {badgeType} -> badgeType == BTSupporter) prices
      badOffer =
        BadgeOffer
          { offerId = BadgeOfferId "test-bad-offer-freeMonths-ge-months",
            priceId = Just priceId,
            months = 3,
            discount = ODFreeMonths 12,
            status = BISActive,
            createdAt = now,
            total = Nothing
          }
  result <- try (evaluate (offerTotal price (Just badOffer))) :: IO (Either SomeException CurrencyAmount)
  case result of
    Left _ -> pure ()
    Right (CurrencyAmount total) ->
      expectationFailure $ "offerTotal should reject freeMonths >= months, got: " <> show total

-- BadgeItemStatus's JSON crosses the wire (BadgePrice/BadgeOffer.status), so pinning finding
-- 2's TextEncoding-derived encoding to what the earlier TH-derived instance produced proves
-- the change is invisible on the wire, not just asserted to be.
testBadgeItemStatusJsonWireFormat :: HasCallStack => TestParams -> IO ()
testBadgeItemStatusJsonWireFormat _ps = do
  J.encode BISActive `shouldBe` "\"active\""
  J.encode BISDeprecated `shouldBe` "\"deprecated\""
  J.encode BISDisabled `shouldBe` "\"disabled\""

testBadgeServiceConfigMissingFile :: HasCallStack => TestParams -> IO ()
testBadgeServiceConfigMissingFile TestParams {tmpPath} = do
  let path = tmpPath </> "missing.ini"
  Left err <- readBadgeServiceConfig path
  err `shouldSatisfy` (path `isInfixOf`)

testBadgeServiceConfigUnparsableValue :: HasCallStack => TestParams -> IO ()
testBadgeServiceConfigUnparsableValue TestParams {tmpPath} = do
  (issuerKeyFile, codeSecretFile) <- writeTestBadgeServiceSecrets tmpPath
  let path = tmpPath </> "unparsable.ini"
  writeFile path $
    unlines
      [ "[issuer]",
        "key_file = " <> issuerKeyFile,
        "key_idx = not-a-number",
        "",
        "[codes]",
        "secret_file = " <> codeSecretFile,
        "default_expiry_days = 365"
      ]
  Left err <- readBadgeServiceConfig path
  err `shouldSatisfy` ("key_idx" `isInfixOf`)

testBadgeServiceConfigUnknownKey :: HasCallStack => TestParams -> IO ()
testBadgeServiceConfigUnknownKey TestParams {tmpPath} = do
  (issuerKeyFile, codeSecretFile) <- writeTestBadgeServiceSecrets tmpPath
  let path = tmpPath </> "unknown-key.ini"
  writeFile path $
    unlines $
      issuerCodesIniLines issuerKeyFile codeSecretFile
        ++ [ "",
             "[web]",
             "port = 8080",
             "base_url = https://badges.example.org",
             "support_contact = https://simplex.chat/contact",
             "bogus_key = 1"
           ]
  Left err <- readBadgeServiceConfig path
  err `shouldSatisfy` ("bogus_key" `isInfixOf`)

testBadgeServiceConfigMissingIssuerSection :: HasCallStack => TestParams -> IO ()
testBadgeServiceConfigMissingIssuerSection TestParams {tmpPath} = do
  (_issuerKeyFile, codeSecretFile) <- writeTestBadgeServiceSecrets tmpPath
  let path = tmpPath </> "no-issuer.ini"
  writeFile path $ unlines ["[codes]", "secret_file = " <> codeSecretFile, "default_expiry_days = 365"]
  Left err <- readBadgeServiceConfig path
  err `shouldSatisfy` ("[issuer]" `isInfixOf`)

testBadgeServiceConfigMissingCodesSection :: HasCallStack => TestParams -> IO ()
testBadgeServiceConfigMissingCodesSection TestParams {tmpPath} = do
  (issuerKeyFile, _codeSecretFile) <- writeTestBadgeServiceSecrets tmpPath
  let path = tmpPath </> "no-codes.ini"
  writeFile path $ unlines ["[issuer]", "key_file = " <> issuerKeyFile, "key_idx = 1"]
  Left err <- readBadgeServiceConfig path
  err `shouldSatisfy` ("[codes]" `isInfixOf`)

testBadgeServiceConfigHalfConfiguredBtcPay :: HasCallStack => TestParams -> IO ()
testBadgeServiceConfigHalfConfiguredBtcPay TestParams {tmpPath} = do
  (issuerKeyFile, codeSecretFile) <- writeTestBadgeServiceSecrets tmpPath
  let path = tmpPath </> "half-btcpay.ini"
  writeFile path $
    unlines $
      issuerCodesIniLines issuerKeyFile codeSecretFile
        ++ [ "",
             "[web]",
             "port = 8080",
             "base_url = https://badges.example.org",
             "support_contact = https://simplex.chat/contact",
             "",
             "[btcpay]",
             "url = https://btcpay.example.org",
             "store_id = teststore"
           ]
  Left err <- readBadgeServiceConfig path
  err `shouldSatisfy` ("btcpay" `isInfixOf`)

testBadgeServiceConfigHalfConfiguredStripe :: HasCallStack => TestParams -> IO ()
testBadgeServiceConfigHalfConfiguredStripe TestParams {tmpPath} = do
  (issuerKeyFile, codeSecretFile) <- writeTestBadgeServiceSecrets tmpPath
  let stripeKeyFile = tmpPath </> "stripe.key"
      path = tmpPath </> "half-stripe.ini"
  writeFile path $
    unlines $
      issuerCodesIniLines issuerKeyFile codeSecretFile
        ++ [ "",
             "[web]",
             "port = 8080",
             "base_url = https://badges.example.org",
             "support_contact = https://simplex.chat/contact",
             "",
             "[stripe]",
             "secret_key_file = " <> stripeKeyFile
           ]
  Left err <- readBadgeServiceConfig path
  err `shouldSatisfy` ("stripe" `isInfixOf`)

testBadgeServiceConfigProviderRequiresWeb :: HasCallStack => TestParams -> IO ()
testBadgeServiceConfigProviderRequiresWeb TestParams {tmpPath} = do
  (issuerKeyFile, codeSecretFile) <- writeTestBadgeServiceSecrets tmpPath
  let apiKeyFile = tmpPath </> "btcpay-api.key"
      webhookFile = tmpPath </> "btcpay-webhook.secret"
      path = tmpPath </> "provider-no-web.ini"
  writeFile apiKeyFile "api-key\n"
  writeFile webhookFile "webhook-secret\n"
  writeFile path $
    unlines $
      issuerCodesIniLines issuerKeyFile codeSecretFile
        ++ [ "",
             "[btcpay]",
             "url = https://btcpay.example.org",
             "store_id = teststore",
             "api_key_file = " <> apiKeyFile,
             "webhook_secret_file = " <> webhookFile
           ]
  Left err <- readBadgeServiceConfig path
  err `shouldSatisfy` ("[web]" `isInfixOf`)

testBadgeServiceConfigMinimalStarts :: HasCallStack => TestParams -> IO ()
testBadgeServiceConfigMinimalStarts TestParams {tmpPath} = do
  (issuerKeyFile, codeSecretFile) <- writeTestBadgeServiceSecrets tmpPath
  let path = tmpPath </> "minimal.ini"
  writeFile path $ unlines (issuerCodesIniLines issuerKeyFile codeSecretFile)
  result <- readBadgeServiceConfig path
  case result of
    Right BadgeServiceConfig {web} -> web `shouldBe` Nothing
    Left err -> expectationFailure $ "expected a minimal config to parse, got: " <> err

-- Proves a fully populated ini -- issuer, codes, web and both providers -- starts the real
-- service end to end, not just that readBadgeServiceConfig accepts it.
testBadgeServiceCompleteConfigStarts :: HasCallStack => TestParams -> IO ()
testBadgeServiceCompleteConfigStarts ps@TestParams {tmpPath} =
  withBadgeServiceConfig ps writeCompleteConfig $ \client bsLink -> do
    let redeemReq =
          "{\"version\":1,\"request\":{\"type\":\"purchaseBadge\",\"payment\":{\"type\":\"code\",\"code\":\"TEST-CODE\"}}}"
    client ##> ("/_service_request 1 " <> bsLink <> " " <> redeemReq)
    client <## "service response: {\"code\":\"unsupported_version\",\"type\":\"error\"}"
  where
    writeCompleteConfig = do
      (issuerKeyFile, codeSecretFile) <- writeTestBadgeServiceSecrets tmpPath
      let apiKeyFile = tmpPath </> "btcpay-api.key"
          btcWebhookFile = tmpPath </> "btcpay-webhook.secret"
          stripeKeyFile = tmpPath </> "stripe.key"
          stripeWebhookFile = tmpPath </> "stripe-webhook.secret"
      writeFile apiKeyFile "btcpay-api-key\n"
      writeFile btcWebhookFile "btcpay-webhook-secret\n"
      writeFile stripeKeyFile "sk_test_123\n"
      writeFile stripeWebhookFile "whsec_test_123\n"
      writeFile (badgeServiceConfigPath tmpPath) $
        unlines $
          issuerCodesIniLines issuerKeyFile codeSecretFile
            ++ [ "",
                 "[web]",
                 "port = 0",
                 "base_url = https://badges.example.org",
                 "support_contact = https://simplex.chat/contact",
                 "",
                 "[btcpay]",
                 "url = https://btcpay.example.org",
                 "store_id = teststore",
                 "api_key_file = " <> apiKeyFile,
                 "webhook_secret_file = " <> btcWebhookFile,
                 "",
                 "[stripe]",
                 "secret_key_file = " <> stripeKeyFile,
                 "webhook_secret_file = " <> stripeWebhookFile
               ]

-- B1 store layer -------------------------------------------------------------

-- A fresh, migrated badge-service database, independent of any running bot: these tests
-- exercise BadgeService.Store directly against BadgeServiceTests' own store, the same way
-- testBadgeServiceCatalogSeeding does.
withFreshBadgeStore :: TestParams -> (DBStore -> IO a) -> IO a
withFreshBadgeStore ps test = do
  let dbOpts = toDBOpts (dbOptions $ coreOptions $ mkBadgeServiceOpts ps) chatSuffix False chatDBFunctions
  Right st <- createDBStore dbOpts badgeServiceSchemaMigrations (MigrationConfig MCError Nothing)
  test st `finally` closeDBStore st

mkTestKeyPair :: IO (C.PublicKeyEd25519, C.PrivateKeyEd25519)
mkTestKeyPair = do
  drg <- C.newRandom
  (pub, priv :: C.PrivateKeyEd25519) <- atomically $ C.generateKeyPair drg
  pure (pub, priv)

expectRight :: HasCallStack => IO (Either ServiceError a) -> IO a
expectRight action =
  action >>= \case
    Right a -> pure a
    Left e -> expectationFailure ("expected Right, got: " <> show e) >> error "unreachable"

-- Creates a purchase, appends three ledger entries (an opening credit and two badge debits)
-- and asserts getLedgerSince returns all three in ascending entry_id order with the expected
-- changeMonths, and getLastLedgerEntry returns exactly the last one appended.
testBadgeStorePurchaseAndLedger :: HasCallStack => TestParams -> IO ()
testBadgeStorePurchaseAndLedger ps =
  withFreshBadgeStore ps $ \st -> do
    (purchaseKey, _) <- mkTestKeyPair
    masterKey <- BadgeMasterKey <$> getRandomBytes 32
    now <- getCurrentTime
    BadgePurchaseRow {badgePurchaseId, status} <-
      expectRight $ withServiceTransaction st $ \db -> createPurchase db purchaseKey masterKey BTSupporter now
    status `shouldBe` PSIssued
    let mkEntry changeMonths entryType balanceMonths =
          BadgeLedgerEntry
            { entryId = 0,
              entryUuid = "test-entry-" <> tshow' (badgePurchaseId, changeMonths, balanceMonths),
              badgePurchaseId,
              changeMonths,
              balanceMonths,
              balanceStartTs = now,
              balanceBadgeType = BTSupporter,
              wasPausedSince = Nothing,
              serviceCreatedAt = now,
              createdAt = now,
              entryType
            }
        tshow' = fromString . show
    entry1 <- expectRight $ withServiceTransaction st $ \db -> appendLedgerEntry db (mkEntry 3 (LECredit CTOpening) 3)
    entry2 <- expectRight $ withServiceTransaction st $ \db -> appendLedgerEntry db (mkEntry (-1) (LEDebit DTBadge) 2)
    entry3 <- expectRight $ withServiceTransaction st $ \db -> appendLedgerEntry db (mkEntry (-1) (LEDebit DTBadge) 1)
    entryId entry1 < entryId entry2 && entryId entry2 < entryId entry3 `shouldBe` True
    allEntries <- expectRight $ withServiceTransaction st $ \db -> getLedgerSince db badgePurchaseId Nothing
    map entryId allEntries `shouldBe` [entryId entry1, entryId entry2, entryId entry3]
    map changeMonths allEntries `shouldBe` [3, -1, -1]
    lastEntry <- expectRight $ withServiceTransaction st $ \db -> getLastLedgerEntry db badgePurchaseId
    (entryId <$> lastEntry) `shouldBe` Just (entryId entry3)
    sinceFirst <- expectRight $ withServiceTransaction st $ \db -> getLedgerSince db badgePurchaseId (Just (entryId entry1))
    map entryId sinceFirst `shouldBe` [entryId entry2, entryId entry3]

-- setPriceStatus BISDisabled removes the price and every offer pinned to it from
-- getActiveCatalog, while getPriceById and getOfferById still resolve both.
testBadgeStoreSetPriceStatusDisabled :: HasCallStack => TestParams -> IO ()
testBadgeStoreSetPriceStatusDisabled ps =
  withFreshBadgeStore ps $ \st -> do
    seedCatalog st
    BadgeCatalog {prices = seededPrices} <- expectRight $ withServiceTransaction st getActiveCatalog
    let BadgePrice {priceId = targetPriceId} = head seededPrices
    pinnedOfferIds <- expectRight $ withServiceTransaction st $ \db -> do
      BadgeCatalog {offers} <- getActiveCatalog db
      pure [offerId | BadgeOffer {offerId, priceId = Just pid} <- offers, pid == targetPriceId]
    length pinnedOfferIds > 0 `shouldBe` True
    _ <- expectRight $ withServiceTransaction st $ \db -> setPriceStatus db targetPriceId BISDisabled
    BadgeCatalog {prices = pricesAfter, offers = offersAfter} <- expectRight $ withServiceTransaction st getActiveCatalog
    any (\BadgePrice {priceId} -> priceId == targetPriceId) pricesAfter `shouldBe` False
    any (\BadgeOffer {priceId} -> priceId == Just targetPriceId) offersAfter `shouldBe` False
    priceStillById <- expectRight $ withServiceTransaction st $ \db -> getPriceById db targetPriceId
    case priceStillById of
      Just BadgePrice {status} -> status `shouldBe` BISDisabled
      Nothing -> expectationFailure "disabled price should still resolve by id"
    mapM_
      ( \oid -> do
          offerStillById <- expectRight $ withServiceTransaction st $ \db -> getOfferById db oid
          isJust offerStillById `shouldBe` True
      )
      pinnedOfferIds

-- Inserts a code, marks it redeemed by one purchase's key, and asserts getCodeByHash returns
-- that same purchase key alongside the code row.
testBadgeStoreGetCodeByHashRedeemer :: HasCallStack => TestParams -> IO ()
testBadgeStoreGetCodeByHashRedeemer ps =
  withFreshBadgeStore ps $ \st -> do
    (purchaseKey, _) <- mkTestKeyPair
    masterKey <- BadgeMasterKey <$> getRandomBytes 32
    now <- getCurrentTime
    BadgePurchaseRow {badgePurchaseId} <-
      expectRight $ withServiceTransaction st $ \db -> createPurchase db purchaseKey masterKey BTSupporter now
    codeHash <- getRandomBytes 32
    let expiresAt = addUTCTime (30 * nominalDay) now
        newCode = NewBadgeCode {codeHash, badgeType = BTSupporter, months = 3, batch = "test-batch", expiresAt}
    _ <- expectRight $ withServiceTransaction st $ \db -> insertCodes db [newCode] now
    beforeRedeem <- expectRight $ withServiceTransaction st $ \db -> getCodeByHash db codeHash
    case beforeRedeem of
      Just (_, redeemer) -> redeemer `shouldBe` Nothing
      Nothing -> expectationFailure "code should exist before redemption"
    _ <- expectRight $ withServiceTransaction st $ \db -> markCodeRedeemed db codeHash badgePurchaseId now
    afterRedeem <- expectRight $ withServiceTransaction st $ \db -> getCodeByHash db codeHash
    case afterRedeem of
      Just (BadgeCode {redeemedPurchaseId}, redeemer) -> do
        redeemedPurchaseId `shouldBe` Just badgePurchaseId
        redeemer `shouldBe` Just purchaseKey
      Nothing -> expectationFailure "code should exist after redemption"

-- unredeemCode clears redeemed_purchase_id and redeemed_at and sets unredeemed_at, re-opening
-- the code for another redemption.
testBadgeStoreUnredeemCode :: HasCallStack => TestParams -> IO ()
testBadgeStoreUnredeemCode ps =
  withFreshBadgeStore ps $ \st -> do
    (purchaseKey, _) <- mkTestKeyPair
    masterKey <- BadgeMasterKey <$> getRandomBytes 32
    now <- getCurrentTime
    BadgePurchaseRow {badgePurchaseId} <-
      expectRight $ withServiceTransaction st $ \db -> createPurchase db purchaseKey masterKey BTSupporter now
    codeHash <- getRandomBytes 32
    let expiresAt = addUTCTime (30 * nominalDay) now
        newCode = NewBadgeCode {codeHash, badgeType = BTSupporter, months = 3, batch = "test-batch", expiresAt}
    _ <- expectRight $ withServiceTransaction st $ \db -> insertCodes db [newCode] now
    _ <- expectRight $ withServiceTransaction st $ \db -> markCodeRedeemed db codeHash badgePurchaseId now
    unredeemAt <- getCurrentTime
    _ <- expectRight $ withServiceTransaction st $ \db -> unredeemCode db codeHash unredeemAt
    Just (BadgeCode {redeemedPurchaseId, redeemedAt, unredeemedAt}, redeemer) <-
      expectRight $ withServiceTransaction st $ \db -> getCodeByHash db codeHash
    redeemedPurchaseId `shouldBe` Nothing
    redeemedAt `shouldBe` Nothing
    -- Postgres TIMESTAMPTZ truncates to microseconds, so an exact Haskell UTCTime round-trip
    -- isn't guaranteed on that backend; comparing within a second confirms the column was
    -- actually set to the given time without depending on sub-microsecond precision surviving.
    case unredeemedAt of
      Just storedUnredeemedAt -> abs (diffUTCTime storedUnredeemedAt unredeemAt) < 1 `shouldBe` True
      Nothing -> expectationFailure "unredeemed_at should be set after unredeemCode"
    redeemer `shouldBe` Nothing

#if defined(dbPostgres)
runMigrationsToRun :: DBStore -> MigrationsToRun -> IO ()
runMigrationsToRun st = Migrations.run st Nothing
#else
runMigrationsToRun :: DBStore -> MigrationsToRun -> IO ()
runMigrationsToRun st = Migrations.run st Nothing True
#endif

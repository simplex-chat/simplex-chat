{-# LANGUAGE CPP #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Bots.BadgeServiceTests where

import BadgeService.Catalog (catalogTotals, defaultCatalog, offerTotal, seedCatalog)
import BadgeService.Options
import BadgeService.Service
import ChatClient
import ChatTests.DBUtils
import ChatTests.Utils
import Control.Concurrent (forkIO, killThread, threadDelay)
import Control.Exception (SomeException, finally, try)
import Data.List (find)
import Data.Maybe (fromJust, isJust)
import Data.String (fromString)
import Data.Text (Text)
import Data.Time.Clock (getCurrentTime)
import Data.Word (Word32)
import Simplex.Chat.Badges.Service (BadgeCatalog (..), BadgeOffer (..), BadgePrice (..))
import Simplex.Chat.Controller (ChatConfig)
import Simplex.Chat.Options (CoreChatOpts (..))
import Simplex.Chat.Options.DB
import Simplex.Chat.PaymentService.Types (CurrencyAmount (..))
import Simplex.Chat.Types (ChatPeerType (..), Profile (..))
import Simplex.Messaging.Agent.Store.Common (DBStore, withConnection)
import qualified Simplex.Messaging.Agent.Store.DB as DB
import Simplex.Messaging.Agent.Store.Interface (closeDBStore, createDBStore)
import Simplex.Messaging.Agent.Store.Shared (Migration (..), MigrationConfig (..), MigrationConfirmation (..), MigrationsToRun (..), toDownMigration)
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
      testing = True
    }

withBadgeService :: HasCallStack => TestParams -> (TestCC -> String -> IO ()) -> IO ()
withBadgeService ps test = do
  let opts = mkBadgeServiceOpts ps
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

#if defined(dbPostgres)
runMigrationsToRun :: DBStore -> MigrationsToRun -> IO ()
runMigrationsToRun st = Migrations.run st Nothing
#else
runMigrationsToRun :: DBStore -> MigrationsToRun -> IO ()
runMigrationsToRun st = Migrations.run st Nothing True
#endif

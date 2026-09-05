{-# LANGUAGE CPP #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Bots.BadgeWebTests (badgeWebTests) where

import BadgeService.Catalog (catalogCurrency, defaultCatalog)
import BadgeService.Config (BTCPayConfig (..), ListenerConfig (..), PollConfig (..), ServiceConfig (..), SpeedPolicy (..))
import BadgeService.Orders (codeLifetime, settleOrder)
import BadgeService.Poller
import BadgeService.Providers
import BadgeService.Providers.BTCPay (btcpayProvider, listPageSize, maxListPages)
import BadgeService.Store.Invoices
import BadgeService.Waiters (awaitStatus, newWaiters, publish, waitingCount)
import BadgeService.Web.Server
import Bots.BadgeCatalogTests (WebOffer (..), WebPrice (..), parseCatalogSource)
import Bots.FakeBTCPay
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.Async (async, wait)
import qualified Control.Concurrent.Async as Async
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.STM (atomically, readTVarIO)
import qualified Control.Exception as E
import Control.Monad (join, replicateM, replicateM_, void, when)
import Data.Aeson ((.=))
import qualified Data.Aeson as J
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64.URL as B64U
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy.Char8 as LB
import Data.Char (toLower)
import Data.Either (isLeft)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.List (sort, sortOn)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust, isNothing, fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text.IO as T
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (NominalDiffTime, UTCTime (..), addUTCTime, diffUTCTime, getCurrentTime, picosecondsToDiffTime, secondsToDiffTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import Data.Word (Word32, Word8)
import Network.HTTP.Client (Manager, Request (..), RequestBody (..), Response, defaultManagerSettings, httpLbs, newManager, parseRequest, responseBody, responseHeaders, responseStatus, responseTimeoutMicro)
import Network.HTTP.Types (Header, HeaderName, hCacheControl, hContentType)
import Network.HTTP.Types.Status (statusCode)
import qualified Network.Wai.Handler.Warp as Warp
import Simplex.Chat.Badges (BadgeType (..))
import Simplex.Chat.Badges.Service (BadgeOffer (..), BadgePrice (..))
import Simplex.Chat.Badges.Types (BadgeCodePaymentStatus (..), BadgeItemStatus (..), BadgeOfferId (..), BadgePriceId (..), OfferDiscount (..))
import Simplex.Chat.PaymentService.Types (CryptoCurrency (..), CurrencyAmount (..), InvoiceId (..), InvoiceStatus (..), PaymentProvider (..), PaymentStatus (..), ServicePaymentDestination (..), ServicePaymentMethod (..))
import Simplex.Messaging.Agent.Store.Common (DBStore (..), withConnection, withTransaction)
import qualified Simplex.Messaging.Agent.Store.DB as DB
import Simplex.Messaging.Agent.Store.Interface
import Simplex.Messaging.Agent.Store.Shared (MigrationConfig (..), MigrationConfirmation (..))
import qualified Simplex.Messaging.Crypto as C
import Simplex.Messaging.Encoding.String (textDecode, textEncode)
import Simplex.Messaging.Util (safeDecodeUtf8, tshow)
import System.Directory (createDirectoryIfMissing, createFileLink, doesFileExist, listDirectory)
import System.FilePath ((</>))
import System.IO.Unsafe (unsafePerformIO)
import System.Timeout (timeout)
import Test.Hspec
import Text.Read (readMaybe)
import UnliftIO.Temporary (withTempDirectory)

#if defined(dbPostgres)
import BadgeService.Store.Postgres.Migrations (badgeServiceSchemaMigrations)
import ChatClient (testDBConnectInfo, testDBConnstr)
import Database.PostgreSQL.Simple (Only (..))
import Simplex.Messaging.Agent.Store.Postgres.Util (createDBAndUserIfNotExists, dropDatabaseAndUser)
#else
import BadgeService.Store.SQLite.Migrations (badgeServiceSchemaMigrations)
import Data.String (fromString)
import Database.SQLite.Simple (Only (..))
import qualified Database.SQLite.Simple as SQL
import Simplex.Messaging.Agent.Store.DB (TrackQueries (..))
#endif

#if defined(dbPostgres)
withServiceStore :: (DBStore -> IO a) -> IO a
withServiceStore action =
  E.bracket_
    (dropDatabaseAndUser testDBConnectInfo >> createDBAndUserIfNotExists testDBConnectInfo)
    (dropDatabaseAndUser testDBConnectInfo)
    $ do
      Right st <- createDBStore serviceDBOpts badgeServiceSchemaMigrations (MigrationConfig MCError Nothing)
      action st `E.finally` closeDBStore st
  where
    serviceDBOpts =
      DBOpts
        { connstr = BC.pack testDBConnstr,
          schema = "sx_badge_service_web_test",
          poolSize = 4,
          createSchema = True
        }
#else
withServiceStore :: (DBStore -> IO a) -> IO a
withServiceStore action = do
  createDirectoryIfMissing True "tests/tmp"
  withTempDirectory "tests/tmp" "badge-web" $ \dir -> do
    Right st <- createDBStore (DBOpts (dir </> "badge_service_test.db") [] "" False True TQOff) badgeServiceSchemaMigrations (MigrationConfig MCError Nothing)
    action st `E.finally` closeDBStore st
#endif

columnsOf :: DBStore -> Text -> IO [Text]
columnsOf st table = withConnection st $ \db ->
#if defined(dbPostgres)
  map fromOnly
    <$> DB.query
      db
      "SELECT column_name FROM information_schema.columns WHERE table_schema = current_schema() AND table_name = ?"
      (Only table)
#else
  map fromOnly
    <$> DB.query_ db (fromString ("SELECT name FROM pragma_table_info('" ++ T.unpack table ++ "')"))
#endif

badgeWebTests :: Spec
badgeWebTests = do
  describe "badge service schema" $ do
    it "carries the five service-only columns" testServiceColumns
    it "refuses a duplicate provider_ref" testProviderRefUnique
  describe "badge service store" $ do
    it "writes the invoice, its code and their link atomically" testCreationIsAtomic
    it "newInvoiceId is 128 CSPRNG bits, base64url, and two calls differ" testNewInvoiceIdRandom
    it "codeHashExists is true for a hash already written" testCodeHashExists
    it "expireOverdue moves only open, past-expiry rows, and names them" testExpireOverdueMovesOnlyQualifying
    it "expireOverdue never sweeps an invoice that has been paid into" testExpireOverdueSparesAFundedInvoice
    it "expireOverdue spares an invoice funded by dust, or by the verdict alone" testExpireOverdueSparesAZeroAmount
    it "readCatalogRows drops every disabled row" testReadCatalogRowsDropsDisabled
    it "every timestamp round-trips to the second" testTimestampRoundTrip
  describe "badge service catalog seed" $ do
    it "writes the compiled-in catalog into an empty database" testSeedWritesTheCatalog
    it "leaves exactly one row per id when the service starts twice" testSeedIsIdempotent
    it "leaves a price and an offer an operator withdrew withdrawn" testSeedNeverResurrectsAWithdrawnRow
    it "seeds exactly what web/src/catalog.ts compiles into the page" testSeedMatchesWebCatalog
    it "prices a btc checkout, and answers card provider_unavailable" testSeededCatalogSellsSomething
  describe "BadgeService.Providers stub" $ do
    it "constructs a Provider record and records every call, in order" testStubProviderRecordsCalls
    it "makes pCreateInvoice fail on demand, so provider_unavailable is exercisable" testStubProviderCreateFailsOnDemand
    it "records nothing when the caller never reaches the provider" testStubProviderNotCalledWhenSkipped
  describe "badge service web" $ do
    it "serves the shell at / and a hashed asset under /assets" testServesTheBuild
    it "caches by where the file is, not by how the request spelled it" testCachingFollowsTheResolvedPath
    it "serves the built web app, exactly as it is shipped" testServesBuiltWebApp
    it "refuses every traversal spelling, by canonicalisation" testTraversalRefused
    it "answers the read fields, with cryptoCurrency lowercase" testInvoiceView
    it "names the confirmations settlement needs, from the speed policy" testViewNamesTheConfirmationsSettlementNeeds
    it "omits the confirmations where no provider is configured" testViewOmitsConfirmationsWithoutBTCPay
    it "carries the provider's paid-in-full verdict, which the amounts cannot give" testViewCarriesTheProvidersPaidVerdict
    it "never withdraws a paid-in-full verdict on a later read" testPaidVerdictIsNotWithdrawnByALaterRead
    it "answers an unknown id with 404 not_found and nothing else" testUnknownInvoiceIsOpaque
    it "releases a held wait when a payment lands without settling" testHeldWaitWakesOnAPaymentThatDoesNotSettle
    it "leaves a held wait alone when a pass writes nothing" testHeldWaitIsNotWokenByAPassThatWroteNothing
    it "is not woken by the same figures arriving again" testARepeatedSignalDoesNotWakeAHold
    it "carries Cache-Control: no-store on every API response" testApiIsNeverCached
    it "answers a known path reached with the wrong method with 405" testWrongMethodIs405
    it "answers a path this task does not implement with 404" testUnroutedPathIs404
    it "answers ?wait= at once when it is terminal, stale or unparseable" testWaitAnswersAtOnce
    it "answers at once when it holds a payment the page has not seen" testHoldAnswersAPaymentThePageHasNotSeen
    it "answers a verdict that arrived with no figure to go with it" testHoldAnswersAVerdictWithNoFigure
    it "holds ?wait=open, and is woken by publish rather than polled" testHoldIsWokenNotPolled
    it "refuses over sixty reads a minute from one IP" testReadRateLimit
    it "takes the client from X-Forwarded-For only when it is trusted" testForwardedForOnlyWhenTrusted
    it "uses a forwarded client only when it parses as an IP address" testForwardedForMustBeAnAddress
    it "holds the bucket map under its cap through a flood of distinct clients" testBucketsStayBounded
    it "answers a hold from the row it read, not from the status it woke on" testHoldReportsTheRowNotTheWake
    it "answers a hold that timed out with a change nothing published" testHoldTimeoutReportsAnUnpublishedChange
    it "answers a handler exception with 500 internal, and keeps serving" testHandlerExceptionIsContained
  describe "badge service settlement" $ do
    it "settles an open invoice: paid, a settled payment row, and the code" testSettlesAnOpenInvoice
    it "settles an expired invoice, because late settlement is routine" testLateSettlementIsLegal
    it "records what arrived without moving an unsettled invoice" testFundedRecordsWithoutMoving
    it "expires an open invoice whose window closed, recording what arrived" testClosedExpiresAnOpenInvoice
    it "writes no payment for a window that closed on nothing" testClosedWithNothingWritesNoPayment
    it "rewrites the same values when a closed window is redelivered" testClosedReplayIsIdempotent
    it "changes nothing for any signal against a paid invoice" testPaidRefusesEverySignal
    it "takes the larger of the stored and the reported amount" testAmountIsMonotonic
    it "measures the code deadline from the first settlement" testDeadlineIsFromTheFirstSettlement
    it "leaves an invoice another transaction moved first alone" testStatusGuardRefusesAStaleObservation
    it "publishes what it found when it loses the status guard" testLosingTheStatusGuardStillWakesTheHold
    it "never downgrades a settled payment row" testSettledPaymentIsNotDowngraded
    it "fills in a crypto amount that only arrived later" testCryptoAmountFillsInFromNull
    it "answers Left for an invoice this service does not hold" testUnknownInvoiceSettlesNothing
    it "publishes after the commit, so a woken reader finds the row" testPublishIsAfterCommit
    it "wakes a request the listener is holding, from settleOrder itself" testSettlementWakesAHeldRequest
    it "reports settledAt from the payment row, not from the invoice" testSettledAtIsThePaymentRow
    it "refuses a settlement instant the provider cannot have meant" testAbsurdSettledInstantIsRefused
    it "refuses a settlement instant in the future, as a millisecond clock gives" testFutureSettledInstantIsRefused
  describe "badge service poller" $ do
    it "settles a payment with no webhook delivered anywhere" testSettlesWithNoWebhookAtAll
    it "reads only the invoices it is waiting on, and lists on the stray cadence" testPassReadsWhatItAwaits
    it "asks the provider nothing when it is waiting on nothing" testIdlePassAsksNothing
    it "lists instead of reading once too many invoices are open" testManyOpenInvoicesList
    it "sweeps nothing when no provider can account for the rows" testNoProviderAccountsForNothing
    it "holds the sweep for one row a configured provider does not own" testMixedProvidersHoldTheSweep
    it "sends one list for a bulk pass, and none for the next one that reads" testBulkListIsTheOnlyList
    it "hands every signal the pass returned to settleOrder, unless the row answers it" testEverySignalSettles
    it "passes over a listed invoice this service does not hold" testForeignRefIsPassedOver
    it "changes nothing when the provider fails, and settles on the next pass" testProviderFailureLosesNothing
    it "reads the cadence fresh from the waiters each pass" testCadenceFollowsTheWaiters
    it "cuts an idle sleep short when a browser arrives during it" testWaiterCutsTheIdleSleepShort
    it "serves a hint batch without postponing a pass that falls due" testHintsDoNotPostponeThePass
    it "floors the cadence, so a zero in the ini is not a busy loop" testCadenceHasAFloor
    it "wakes a request the listener is holding, from a poller pass" testPollerWakesAHeldRequest
    it "expires an open invoice past the ten-minute grace, and reads before it does" testSweepExpiresPastTheGrace
    it "writes status alone, so a later SigClosed still records what arrived" testSweepWritesStatusAlone
    it "wakes a request held on an invoice it expires" testSweepWakesAHeldRequest
    it "reports a skipped invoice once, and again only after the interval" testSkipWarningsAreRateLimited
    it "warns once for a provider that stays down, not once a pass" testOutageWarnsOnceNotEveryPass
    it "holds the skip log under its cap when every reason is fresh" testSkipReasonsStayBounded
    it "raises a skip naming an invoice this service sold" testSkipNamingOurInvoiceIsRaised
    it "holds the sweep back until a pass has accounted for every invoice" testSweepWaitsForAPassThatSawEverything
    it "settles the rest of the pass around an invoice that throws" testOneBadInvoiceDoesNotStopThePass
    it "pages past a hundred open invoices, a request per hundred" testListPaginates
    it "stops paging at its ceiling rather than looping on a server that ignores take" testPagingStopsAtTheCeiling
  describe "badge service webhook" $ do
    it "verifies a real BTCPay signature over the bytes as received, and queues" testWebhookVerifiesARealSignature
    it "hands the adapter the raw body, byte for byte" testWebhookPassesTheRawBytes
    it "queues a read and answers 200 empty, reaching no provider" testWebhookQueuesARead
    it "settles by the poller's own path, so there is one settlement lane" testWebhookSettlesByThePollerPath
    it "answers at once even when a read at the provider takes a second" testWebhookDoesNotWaitOnTheProvider
    it "answers 400 empty, with no detail, for a signature that does not verify" testWebhookRefusesASignature
    it "answers 200 for an unhandled type, an unknown ref and the other lane's ref" testWebhookIgnoresWhatItCannotActOn
    it "refuses a body over the 64 KB cap before parsing or verifying it" testWebhookRefusesAnOversizedBody
    it "drops a hint rather than blocking when the queue is full" testWebhookDropsAHintWhenFull
    it "answers 200 rather than any 5xx when the adapter or the store throws" testWebhookNeverAnswers5xx
  describe "badge service checkout" $ do
    it "answers the checkout fields and writes the three rows" testCreateInvoice
    it "carries no code and no code hash, over the raw bytes" testCreateCarriesNoCode
    it "tells the provider xmr rather than the chain another request used" testXmrReachesTheProviderAsXmr
    it "derives badgeType, months and amount from the catalog, never from the body" testCreateDerivesFromCatalog
    it "refuses every catalog_changed condition before the provider" testCatalogRefusalCostsNothing
    it "refuses a code_hash already sold before the provider" testCodeConflictCostsNothing
    it "maps a code_hash unique violation that raced the check to 409, not 500" testRacingCodeHashIsConflict
    it "answers 500 and names the abandoned invoice when the store is lost" testStoreLostAfterTheProviderCreated
    it "refuses a malformed body, an unknown method and a malformed codeHash" testBadRequestCostsNothing
    it "refuses a body over the size cap before reading it all" testOversizedBodyCostsNothing
    it "answers method card with provider_unavailable while Stripe is out of scope" testCardIsProviderUnavailable
    it "answers a provider that failed to create with provider_unavailable, writing nothing" testProviderFailureWritesNothing
    it "refuses the sixth create in a minute, without reaching the provider" testCreateRateLimit
  describe "badge service cancel" $ do
    it "expires the invoice here and invalidates it at the provider" testCancelClosesTheInvoiceAtBothEnds
    it "wakes a hold another tab is sitting on" testCancelWakesAHeldRequest
    it "refuses to cancel an invoice that is no longer open" testCancelIsRefusedOnceItIsNotOpen
    it "refuses to cancel an invoice awaiting confirmation" testCancelIsRefusedOnceItIsFunded
    it "leaves the invoice open when the provider refuses" testCancelLeavesTheInvoiceOpenWhenTheProviderFails
    it "expires an invoice a payment landed on while the provider was being told" testCancelExpiresAFundedInvoice
    it "answers an unknown id with the same 404 the read does" testCancelIsOpaqueForAnUnknownInvoice
    it "answers GET with 405 and Allow: POST" testCancelRefusesOtherMethods
  describe "badge service scenarios" $ do
    it "buys a code: create, pay at BTCPay, one pass, paid" scenarioPaidPurchase
    it "wakes a wait held from before the settlement, within a second of it" scenarioHeldWaitWakes
    it "reports a partial payment and leaves the invoice open" scenarioPartPaymentIsReported
    it "reports what an expiry received, and sells no code for it" scenarioExpiryReportsWhatArrived
    it "settles an expired invoice late, because confirmation after expiry is routine" scenarioLateSettlement
    it "changes nothing when the same settlement is seen a second time" scenarioReplayChangesNothing
    it "closes an invalid invoice as expired, on the same screen" scenarioInvalidClosesAsExpired
    it "refuses a second invoice for one code hash, creating none at BTCPay" scenarioCodeConflictCreatesNothing
    it "settles with no webhook delivered, by reading the invoice it is waiting on" scenarioNoWebhookAnywhere

testServiceColumns :: IO ()
testServiceColumns = withServiceStore $ \st -> do
  columnsOf st "sx_badge_service_badge_code_invoices"
    >>= (`shouldSatisfy` \cs -> all (`elem` cs) ["code_hash", "provider_ref"])
  columnsOf st "sx_badge_service_payments" >>= (`shouldSatisfy` elem "crypto_amount")
  columnsOf st "sx_badge_service_badge_codes"
    >>= (`shouldSatisfy` \cs -> all (`elem` cs) ["expires_at", "revoked_at"])

testProviderRefUnique :: IO ()
testProviderRefUnique = withServiceStore $ \st -> do
  seedBadgePrice st "price1"
  seedInvoice st "inv1"
  seedInvoice st "inv2"
  insertBadgeCodeInvoice st "inv1" "price1" "same-provider-ref"
  insertBadgeCodeInvoice st "inv2" "price1" "same-provider-ref"
    `shouldThrow` anyException

seedInvoice :: DBStore -> Text -> IO ()
seedInvoice st invoiceId = withConnection st $ \db ->
  DB.execute
    db
    "INSERT INTO sx_badge_service_invoices (invoice_id, provider, price, amount, currency, expires_at, status, created_at, updated_at) VALUES (?,?,?,?,?,?,?,?,?)"
    (invoiceId, "btcpay" :: Text, 500 :: Int, 500 :: Int, "usd" :: Text, "2030-01-01T00:00:00Z" :: Text, "open" :: Text, "2026-08-31T00:00:00Z" :: Text, "2026-08-31T00:00:00Z" :: Text)

seedBadgePrice :: DBStore -> Text -> IO ()
seedBadgePrice st priceId = withConnection st $ \db ->
  DB.execute
    db
    "INSERT INTO sx_badge_service_badge_prices (price_id, badge_type, month_price, currency, status, created_at) VALUES (?,?,?,?,?,?)"
    (priceId, "supporter" :: Text, 500 :: Int, "usd" :: Text, "active" :: Text, "2026-08-31T00:00:00Z" :: Text)

insertBadgeCodeInvoice :: DBStore -> Text -> Text -> Text -> IO ()
insertBadgeCodeInvoice st invoiceId priceId providerRef = withConnection st $ \db ->
  DB.execute
    db
    "INSERT INTO sx_badge_service_badge_code_invoices (invoice_id, price_id, months, created_at, provider_ref) VALUES (?,?,?,?,?)"
    (invoiceId, priceId, 1 :: Int, "2026-08-31T00:00:00Z" :: Text, providerRef)

someExpiry :: UTCTime
someExpiry = UTCTime (fromGregorian 2030 1 1) 0

someCreated :: UTCTime
someCreated = UTCTime (fromGregorian 2026 8 31) 0

-- | 32 bytes, all above 0x7f, so the value is not valid UTF-8. An ASCII hash would prove
-- nothing: postgresql-simple writes a plain ByteString as escaped text, so at one point
-- every store test passed on both backends while every checkout 500'd on Postgres.
digestFixture :: Word8 -> ByteString
digestFixture n = BS.pack [0x80 + ((n * 7 + i) `mod` 0x80) | i <- [0 .. 31]]

sampleInvoice :: NewInvoice
sampleInvoice =
  NewInvoice
    { niInvoiceId = InvoiceId "inv1",
      niProviderRef = "provider-ref-1",
      niCodeHash = digestFixture 1,
      niPriceId = BadgePriceId "price1",
      niOfferId = Nothing,
      niBadgeType = BTSupporter,
      niMonths = 1,
      niPrice = CurrencyAmount 500,
      niAmount = CurrencyAmount 500,
      niCurrency = "usd",
      niProvider = PPCrypto,
      niDestination = SPDCrypto CCBtc "bc1qexampleaddress" "0.00050000",
      niExpiresAt = someExpiry,
      niCreatedAt = someCreated
    }

codeInvoiceRow :: DBStore -> InvoiceId -> IO (Maybe Text)
codeInvoiceRow st (InvoiceId invId) = withConnection st $ \db -> do
  rows <- DB.query db "SELECT invoice_id FROM sx_badge_service_badge_code_invoices WHERE invoice_id = ?" (Only invId) :: IO [Only Text]
  pure $ case rows of
    [] -> Nothing
    (Only r : _) -> Just r

markPaid :: DBStore -> InvoiceId -> IO ()
markPaid st (InvoiceId invId) = withConnection st $ \db ->
  DB.execute db "UPDATE sx_badge_service_invoices SET status = 'paid' WHERE invoice_id = ?" (Only invId)

insertPrice :: DBStore -> Text -> Text -> Int -> Text -> IO ()
insertPrice st priceId badgeType monthPrice status = withConnection st $ \db ->
  DB.execute
    db
    "INSERT INTO sx_badge_service_badge_prices (price_id, badge_type, month_price, currency, status, created_at) VALUES (?,?,?,?,?,?)"
    (priceId, badgeType, monthPrice, "usd" :: Text, status, "2026-08-31T00:00:00Z" :: Text)

insertOffer :: DBStore -> Text -> Maybe Text -> Int -> Int -> Text -> IO ()
insertOffer st offerId priceId months discount status = withConnection st $ \db ->
  DB.execute
    db
    "INSERT INTO sx_badge_service_badge_offers (offer_id, price_id, months, discount, status, created_at) VALUES (?,?,?,?,?,?)"
    (offerId, priceId, months, discount, status, "2026-08-31T00:00:00Z" :: Text)

testCreationIsAtomic :: IO ()
testCreationIsAtomic = withServiceStore $ \st -> do
  seedBadgePrice st "price1"
  let duplicateHash = digestFixture 9
      planted = sampleInvoice {niInvoiceId = InvoiceId "other", niProviderRef = "p-other", niCodeHash = duplicateHash}
      ni = sampleInvoice {niCodeHash = duplicateHash}
  planted' <- createInvoiceRows st planted
  planted' `shouldBe` Right ()
  r <- createInvoiceRows st ni
  r `shouldSatisfy` isLeft
  r `shouldBe` Left CECodeConflict
  getInvoice st (niInvoiceId ni) `shouldReturn` Nothing
  codeInvoiceRow st (niInvoiceId ni) `shouldReturn` Nothing

testNewInvoiceIdRandom :: IO ()
testNewInvoiceIdRandom = do
  InvoiceId a <- newInvoiceId
  InvoiceId b <- newInvoiceId
  a `shouldNotBe` b
  T.length a `shouldBe` 22
  T.any (== '=') a `shouldBe` False

testCodeHashExists :: IO ()
testCodeHashExists = withServiceStore $ \st -> do
  seedBadgePrice st "price1"
  createInvoiceRows st sampleInvoice `shouldReturn` Right ()
  codeHashExists st (niCodeHash sampleInvoice) `shouldReturn` True
  codeHashExists st "no-such-hash" `shouldReturn` False

testExpireOverdueMovesOnlyQualifying :: IO ()
testExpireOverdueMovesOnlyQualifying = withServiceStore $ \st -> do
  seedBadgePrice st "price1"
  now <- getCurrentTime
  let pastExpiry = addUTCTime (-3600) now
      futureExpiry = addUTCTime 3600 now
      overdue = sampleInvoice {niExpiresAt = pastExpiry}
      notYet = sampleInvoice {niInvoiceId = InvoiceId "inv-notyet", niProviderRef = "p-notyet", niCodeHash = digestFixture 13, niExpiresAt = futureExpiry}
      alreadyPaid = sampleInvoice {niInvoiceId = InvoiceId "inv-paid", niProviderRef = "p-paid", niCodeHash = digestFixture 14, niExpiresAt = pastExpiry}
  createInvoiceRows st overdue `shouldReturn` Right ()
  createInvoiceRows st notYet `shouldReturn` Right ()
  createInvoiceRows st alreadyPaid `shouldReturn` Right ()
  markPaid st (niInvoiceId alreadyPaid)
  moved <- expireOverdue st now
  moved `shouldBe` [niInvoiceId overdue]
  Just overdueRow <- getInvoice st (niInvoiceId overdue)
  irStatus overdueRow `shouldBe` ISExpired
  Just notYetRow <- getInvoice st (niInvoiceId notYet)
  irStatus notYetRow `shouldBe` ISOpen
  Just paidRow <- getInvoice st (niInvoiceId alreadyPaid)
  irStatus paidRow `shouldBe` ISPaid

testReadCatalogRowsDropsDisabled :: IO ()
testReadCatalogRowsDropsDisabled = withServiceStore $ \st -> do
  insertPrice st "price-active" "supporter" 500 "active"
  insertPrice st "price-disabled" "supporter" 500 "disabled"
  insertOffer st "offer-active" (Just "price-active") 3 10 "active"
  insertOffer st "offer-disabled" (Just "price-active") 3 10 "disabled"
  (prices, offers) <- readCatalogRows st
  map (\BadgePrice {priceId} -> priceId) prices `shouldBe` [BadgePriceId "price-active"]
  map (\BadgeOffer {offerId} -> offerId) offers `shouldBe` [BadgeOfferId "offer-active"]

seedDefaultCatalog :: DBStore -> UTCTime -> IO (Int, Int)
seedDefaultCatalog st at = uncurry (seedCatalog st) (defaultCatalog at)

priceFacts :: BadgePrice -> (Text, Text, Word32, Text, BadgeItemStatus)
priceFacts BadgePrice {priceId = BadgePriceId pId, badgeType = bType, monthPrice = CurrencyAmount mPrice, currency = cur, status = pStatus} =
  (pId, textEncode bType, mPrice, cur, pStatus)

offerFacts :: BadgeOffer -> (Text, Maybe Text, Word8, OfferDiscount, BadgeItemStatus)
offerFacts BadgeOffer {offerId = BadgeOfferId oId, priceId = oPriceId, months = oMonths, discount = oDiscount, status = oStatus} =
  (oId, (\(BadgePriceId p) -> p) <$> oPriceId, oMonths, oDiscount, oStatus)

allPriceRows :: DBStore -> IO [(Text, Text, Word32, Text, Text, UTCTime)]
allPriceRows st = withConnection st $ \db ->
  DB.query_ db "SELECT price_id, badge_type, month_price, currency, status, created_at FROM sx_badge_service_badge_prices ORDER BY price_id"

allOfferRows :: DBStore -> IO [(Text, Maybe Text, Word32, Maybe Word32, Maybe Word32, Text, UTCTime)]
allOfferRows st = withConnection st $ \db ->
  DB.query_ db "SELECT offer_id, price_id, months, free_months, discount, status, created_at FROM sx_badge_service_badge_offers ORDER BY offer_id"

testSeedWritesTheCatalog :: IO ()
testSeedWritesTheCatalog = withServiceStore $ \st -> do
  readCatalogRows st `shouldReturn'` (0, 0)
  seedDefaultCatalog st someCreated `shouldReturn` (2, 4)
  (prices, offers) <- readCatalogRows st
  sortOn (\(i, _, _, _, _) -> i) (map priceFacts prices)
    `shouldBe` [ ("price_legend", "legend", 7000, "usd", BISActive),
                 ("price_supporter", "supporter", 700, "usd", BISActive)
               ]
  sortOn (\(i, _, _, _, _) -> i) (map offerFacts offers)
    `shouldBe` [ ("offer_12m", Just "price_legend", 12, ODDiscount 50, BISActive),
                 ("offer_12m_s", Just "price_supporter", 12, ODDiscount 50, BISActive),
                 ("offer_3m", Just "price_legend", 3, ODFreeMonths 1, BISActive),
                 ("offer_3m_s", Just "price_supporter", 3, ODFreeMonths 1, BISActive)
               ]
  where
    shouldReturn' action expected = do
      (ps, os) <- action
      (length ps, length os) `shouldBe` expected

testSeedIsIdempotent :: IO ()
testSeedIsIdempotent = withServiceStore $ \st -> do
  seedDefaultCatalog st someCreated `shouldReturn` (2, 4)
  let restartedAt = addUTCTime 86400 someCreated
  seedDefaultCatalog st restartedAt `shouldReturn` (0, 0)
  priceRows <- allPriceRows st
  offerRows <- allOfferRows st
  map (\(i, _, _, _, _, _) -> i) priceRows `shouldBe` ["price_legend", "price_supporter"]
  map (\(i, _, _, _, _, _, _) -> i) offerRows `shouldBe` ["offer_12m", "offer_12m_s", "offer_3m", "offer_3m_s"]
  map (\(_, _, _, _, _, at) -> at) priceRows `shouldBe` replicate 2 someCreated
  map (\(_, _, _, _, _, _, at) -> at) offerRows `shouldBe` replicate 4 someCreated

testSeedNeverResurrectsAWithdrawnRow :: IO ()
testSeedNeverResurrectsAWithdrawnRow = withServiceStore $ \st -> do
  insertPrice st "price_supporter" "supporter" 900 "deprecated"
  insertPrice st "price_legend" "legend" 9000 "disabled"
  insertOffer st "offer_12m" (Just "price_legend") 12 25 "disabled"
  seedDefaultCatalog st someCreated `shouldReturn` (0, 3)
  priceRows <- allPriceRows st
  map (\(i, _, mp, _, s, _) -> (i, mp, s)) priceRows
    `shouldBe` [("price_legend", 9000, "disabled"), ("price_supporter", 900, "deprecated")]
  offerRows <- allOfferRows st
  map (\(i, _, m, _, d, s, _) -> (i, m, d, s)) offerRows
    `shouldBe` [ ("offer_12m", 12, Just 25, "disabled"),
                 ("offer_12m_s", 12, Just 50, "active"),
                 ("offer_3m", 3, Nothing, "active"),
                 ("offer_3m_s", 3, Nothing, "active")
               ]
  (prices, offers) <- readCatalogRows st
  sortOn (\(i, _, _, _, _) -> i) (map priceFacts prices)
    `shouldBe` [("price_supporter", "supporter", 900, "usd", BISDeprecated)]
  map (\(i, _, _, _, _) -> i) (sortOn (\(i, _, _, _, _) -> i) (map offerFacts offers))
    `shouldBe` ["offer_12m_s", "offer_3m", "offer_3m_s"]

testSeedMatchesWebCatalog :: IO ()
testSeedMatchesWebCatalog = withServiceStore $ \st -> do
  src <- T.readFile "apps/simplex-badge-service/web/src/catalog.ts"
  (webPrices, webOffers) <- case parseCatalogSource src of
    Nothing -> failWith "could not parse CATALOG out of web/src/catalog.ts -- its shape has changed"
    Just parsed -> pure parsed
  webPrices `shouldNotBe` []
  webOffers `shouldNotBe` []
  seedDefaultCatalog st someCreated `shouldReturn` (length webPrices, length webOffers)
  (prices, offers) <- readCatalogRows st
  sortOn (\(i, _, _, _) -> i) (map (\(i, t, mp, cur, _) -> (i, t, mp, cur)) (map priceFacts prices))
    `shouldBe` sortOn (\(i, _, _, _) -> i) (map (\WebPrice {wpPriceId, wpBadgeType, wpMonthPrice, wpCurrency} -> (wpPriceId, wpBadgeType, wpMonthPrice, wpCurrency)) webPrices)
  sortOn (\(i, _, _, _) -> i) (map (\(i, p, m, d, _) -> (i, p, m, d)) (map offerFacts offers))
    `shouldBe` sortOn (\(i, _, _, _) -> i) (map (\WebOffer {woOfferId, woPriceId, woMonths, woDiscount} -> (woOfferId, Just woPriceId, woMonths, woDiscount)) webOffers)

testSeededCatalogSellsSomething :: IO ()
testSeededCatalogSellsSomething = bounded "seeded catalog sells" $ withSeededCheckout $ \client -> do
  card <- postCreateAs client 1 (createBody "price_legend" (Just "offer_12m") "card" (codeHashText sampleCode))
  statusOf card `shouldBe` 503
  responseBody card `shouldBe` errorBody "provider_unavailable"
  priced <- postCreateAs client 2 (createBody "price_legend" (Just "offer_12m") "btc" (codeHashText sampleCode))
  statusOf priced `shouldBe` 200
  o <- jsonObject priced
  fieldOf o "badgeType" `shouldBe` Just (J.String "legend")
  fieldOf o "months" `shouldBe` Just (J.Number 12)
  fieldOf o "amount" `shouldBe` Just (J.Number 42000)
  fieldOf o "currency" `shouldBe` Just (J.String catalogCurrency)

withSeededCheckout :: (WebClient -> IO a) -> IO a
withSeededCheckout action =
  withServiceStore $ \st -> do
    createDirectoryIfMissing True "tests/tmp"
    withTempDirectory "tests/tmp" "badge-seeded" $ \root -> do
      staticDir <- prepareStaticDir root
      _ <- seedDefaultCatalog st someCreated
      ref <- newIORef (newStubState (Right sampleProviderInvoice))
      let cfg = (testServiceConfig staticDir True) {btcpay = Just testBTCPayConfig}
      withListener [stubProvider ref] True holdMicros st cfg (const action)

testTimestampRoundTrip :: IO ()
testTimestampRoundTrip = withServiceStore $ \st -> do
  seedBadgePrice st "price1"
  let subSecond = UTCTime (fromGregorian 2026 8 31) (picosecondsToDiffTime 12500000000000) -- 12.5s
      truncated = UTCTime (fromGregorian 2026 8 31) (picosecondsToDiffTime 12000000000000)
      ni = sampleInvoice {niExpiresAt = subSecond, niCreatedAt = subSecond}
  createInvoiceRows st ni `shouldReturn` Right ()
  Just row <- getInvoice st (niInvoiceId ni)
  irExpiresAt row `shouldBe` truncated
  irCreatedAt row `shouldBe` truncated

data StubCall
  = StubCreate ServicePaymentMethod OrderDraft
  | StubRead Text
  | StubCancel Text
  | StubListOpen
  deriving (Eq, Show)

data StubState = StubState
  { ssCalls :: [StubCall],
    ssCreateResult :: Either ProviderError ProviderInvoice,
    ssInvoices :: Map.Map Text PaymentSignal,
    ssCancelError :: Maybe ProviderError,
    ssListError :: Maybe ProviderError,
    ssReadError :: Maybe ProviderError,
    ssSkipped :: [(Maybe Text, Text)],
    ssVerifyResult :: Either WebhookError (Maybe Text),
    ssVerifyThrows :: Bool,
    ssWebhooks :: [([Header], ByteString)],
    ssReadDelay :: Int
  }

newStubState :: Either ProviderError ProviderInvoice -> StubState
newStubState ssCreateResult =
  StubState
    { ssCalls = [],
      ssCreateResult,
      ssInvoices = Map.empty,
      ssCancelError = Nothing,
      ssListError = Nothing,
      ssReadError = Nothing,
      ssSkipped = [],
      ssVerifyResult = Right Nothing,
      ssVerifyThrows = False,
      ssWebhooks = [],
      ssReadDelay = 0
    }

record :: IORef StubState -> StubCall -> IO ()
record ref call = atomicModifyIORef' ref $ \s -> (s {ssCalls = ssCalls s ++ [call]}, ())

stubProvider :: IORef StubState -> Provider
stubProvider ref =
  Provider
    { pProvider = PPCrypto,
      pCreateInvoice = \method draft -> do
        record ref (StubCreate method draft)
        ssCreateResult <$> readIORef ref,
      pReadInvoice = \providerRef -> do
        record ref (StubRead providerRef)
        stub <- readIORef ref
        threadDelay (ssReadDelay stub)
        pure $ maybe (Right (Map.lookup providerRef (ssInvoices stub))) Left (ssReadError stub),
      pCancelInvoice = \providerRef -> do
        record ref (StubCancel providerRef)
        maybe (Right ()) Left . ssCancelError <$> readIORef ref,
      pListOpen = do
        record ref StubListOpen
        stub <- readIORef ref
        pure $ case ssListError stub of
          Just e -> Left e
          Nothing -> Right ListPass {lpMoved = Map.toList (ssInvoices stub), lpSkipped = ssSkipped stub},
      pVerifyWebhook = verifyRecording ref
    }

-- | 'unsafePerformIO' because 'pVerifyWebhook' is pure by design and a pure function
-- cannot record what it was given. Safe here: the effect cannot be duplicated, inlined
-- or floated out, and every assertion is an exact list of calls, so a lost or repeated
-- effect fails the test rather than passing it.
verifyRecording :: IORef StubState -> [Header] -> ByteString -> Either WebhookError (Maybe Text)
verifyRecording ref hdrs body = unsafePerformIO $ do
  atomicModifyIORef' ref $ \s -> (s {ssWebhooks = ssWebhooks s ++ [(hdrs, body)]}, ())
  stub <- readIORef ref
  if ssVerifyThrows stub
    then E.throwIO (userError "the adapter threw while verifying")
    else pure (ssVerifyResult stub)
{-# NOINLINE verifyRecording #-}

stubCalls :: IORef StubState -> IO [StubCall]
stubCalls ref = ssCalls <$> readIORef ref

stubWebhooks :: IORef StubState -> IO [([Header], ByteString)]
stubWebhooks ref = ssWebhooks <$> readIORef ref

setVerifyResult :: IORef StubState -> Either WebhookError (Maybe Text) -> IO ()
setVerifyResult ref r = atomicModifyIORef' ref $ \s -> (s {ssVerifyResult = r}, ())

setReadDelay :: IORef StubState -> Int -> IO ()
setReadDelay ref micros = atomicModifyIORef' ref $ \s -> (s {ssReadDelay = micros}, ())

setVerifyThrows :: IORef StubState -> Bool -> IO ()
setVerifyThrows ref throws = atomicModifyIORef' ref $ \s -> (s {ssVerifyThrows = throws}, ())

sampleDraft :: OrderDraft
sampleDraft = OrderDraft {odAmount = CurrencyAmount 500, odCurrency = "usd"}

sampleProviderInvoice :: ProviderInvoice
sampleProviderInvoice = ProviderInvoice {piProviderRef = "pref-1", piDestination = SPDCrypto CCBtc "bc1qexampleaddress" "0.00050000"}

testStubProviderRecordsCalls :: IO ()
testStubProviderRecordsCalls = do
  ref <- newIORef (newStubState (Right sampleProviderInvoice))
  let provider = stubProvider ref
  created <- pCreateInvoice provider (SPMCrypto CCBtc) sampleDraft
  created `shouldBe` Right sampleProviderInvoice
  let pref = piProviderRef sampleProviderInvoice
  pReadInvoice provider pref `shouldReturn` Right Nothing
  let signal = SigFunded (rcv 500 (Just "0.00050000")) PaidInPart
  atomicModifyIORef' ref $ \s -> (s {ssInvoices = Map.insert pref signal (ssInvoices s)}, ())
  pReadInvoice provider pref `shouldReturn` Right (Just signal)
  pListOpen provider `shouldReturn` Right ListPass {lpMoved = [(pref, signal)], lpSkipped = []}
  calls <- stubCalls ref
  calls `shouldBe` [StubCreate (SPMCrypto CCBtc) sampleDraft, StubRead pref, StubRead pref, StubListOpen]

testStubProviderCreateFailsOnDemand :: IO ()
testStubProviderCreateFailsOnDemand = do
  let err = ProviderError "connection refused"
  ref <- newIORef (newStubState (Left err))
  let provider = stubProvider ref
  pCreateInvoice provider (SPMCrypto CCBtc) sampleDraft `shouldReturn` Left err
  stubCalls ref `shouldReturn` [StubCreate (SPMCrypto CCBtc) sampleDraft]

testStubProviderNotCalledWhenSkipped :: IO ()
testStubProviderNotCalledWhenSkipped = do
  ref <- newIORef (newStubState (Right sampleProviderInvoice))
  stubCalls ref `shouldReturn` []

-- | Every test below is bounded. This suite has hung rather than failed before, and a
-- hang reports a truncated count instead of an error. Shorter than the 30s hold, so a
-- test that waits when it should not fails here instead of passing slowly.
exampleCeiling :: Int
exampleCeiling = 20 * 1000000

bounded :: HasCallStack => String -> IO a -> IO a
bounded label action =
  timeout exampleCeiling action >>= maybe (failWith (label <> ": did not finish within 20s")) pure

clientTimeout :: Int
clientTimeout = 60 * 1000000

failWith :: HasCallStack => String -> IO a
failWith msg = expectationFailure msg >> error msg

outsideMarker :: LB.ByteString
outsideMarker = "SECRET-OUTSIDE-STATIC-DIR"

shellHtml :: LB.ByteString
shellHtml = "<!doctype html><title>SimpleX badges</title>"

assetJs :: LB.ByteString
assetJs = "export const build = \"d95503da54ee228f\";"

workerJs :: LB.ByteString
workerJs = "self.addEventListener(\"install\", () => {});"

buildHash :: FilePath
buildHash = "d95503da54ee228f"

notFoundBody :: LB.ByteString
notFoundBody = "{\"error\":\"not_found\"}"

prepareStaticDir :: FilePath -> IO FilePath
prepareStaticDir root = do
  let staticDir = root </> "dist"
      assetDir = staticDir </> "assets" </> buildHash
  createDirectoryIfMissing True assetDir
  LB.writeFile (root </> "secret.txt") outsideMarker
  LB.writeFile (staticDir </> "index.html") shellHtml
  LB.writeFile (staticDir </> "sw.js") workerJs
  LB.writeFile (assetDir </> "main.js") assetJs
  createFileLink (".." </> ".." </> "secret.txt") (staticDir </> "assets" </> "escape.txt")
  pure staticDir

testServiceConfig :: FilePath -> Bool -> ServiceConfig
testServiceConfig staticDir trustForwarded =
  ServiceConfig
    { listener = ListenerConfig {lHost = "127.0.0.1", lPort = 0, lStaticDir = staticDir, lTrustForwardedFor = trustForwarded},
      btcpay = Nothing,
      poll = PollConfig {pWaitingSeconds = 3, pIdleSeconds = 60},
      issuer = Nothing,
      devChatRedeem = False
    }

type WebClient = (Manager, String)

withWebApp :: (WebEnv -> WebClient -> IO a) -> IO a
withWebApp = withWebAppWith False True holdMicros

withWebAppForwarded :: Bool -> (WebEnv -> WebClient -> IO a) -> IO a
withWebAppForwarded trustForwarded = withWebAppWith trustForwarded True holdMicros

withWebAppHolding :: Int -> (WebEnv -> WebClient -> IO a) -> IO a
withWebAppHolding hold = withWebAppWith False True hold

withWebAppExpectingThrow :: (WebEnv -> WebClient -> IO a) -> IO a
withWebAppExpectingThrow = withWebAppWith False False holdMicros

withWebAppWith :: Bool -> Bool -> Int -> (WebEnv -> WebClient -> IO a) -> IO a
withWebAppWith trustForwarded rethrow hold action =
  withServiceStore $ \st -> do
    createDirectoryIfMissing True "tests/tmp"
    withTempDirectory "tests/tmp" "badge-static" $ \root -> do
      staticDir <- prepareStaticDir root
      withListener [] rethrow hold st (testServiceConfig staticDir trustForwarded) action

withListener :: [Provider] -> Bool -> Int -> DBStore -> ServiceConfig -> (WebEnv -> WebClient -> IO a) -> IO a
withListener providers rethrow hold st cfg action = do
  let runner = if rethrow then Warp.testWithApplicationSettings else Warp.withApplicationSettings
  waiters <- newWaiters
  hints <- newReadHints
  built <- newWebEnv st cfg waiters hints providers
  let env = built {weHoldMicros = hold}
  mgr <- newManager defaultManagerSettings
  runner (webSettings (listener cfg)) (pure (webApp env)) $ \prt ->
    action env (mgr, "http://127.0.0.1:" <> show prt)

webRequest :: WebClient -> ByteString -> String -> [Header] -> IO (Response LB.ByteString)
webRequest client verb target hdrs = webRequestBody client verb target hdrs ""

webRequestBody :: WebClient -> ByteString -> String -> [Header] -> LB.ByteString -> IO (Response LB.ByteString)
webRequestBody (mgr, base) verb target hdrs body = do
  req <- parseRequest base
  let (rawPath, rawQuery) = break (== '?') target
  httpLbs
    req
      { method = verb,
        path = BC.pack rawPath,
        queryString = BC.pack rawQuery,
        requestHeaders = hdrs,
        requestBody = RequestBodyLBS body,
        responseTimeout = responseTimeoutMicro clientTimeout
      }
    mgr

webGet :: WebClient -> String -> IO (Response LB.ByteString)
webGet client target = webRequest client "GET" target []

statusOf :: Response LB.ByteString -> Int
statusOf = statusCode . responseStatus

headerOf :: Response LB.ByteString -> HeaderName -> Maybe ByteString
headerOf r name = lookup name (responseHeaders r)

jsonObject :: HasCallStack => Response LB.ByteString -> IO J.Object
jsonObject r = case J.decode (responseBody r) of
  Just (J.Object o) -> pure o
  _ -> failWith ("not a JSON object: " <> LB.unpack (responseBody r))

fieldOf :: J.Object -> Text -> Maybe J.Value
fieldOf o k = KM.lookup (K.fromText k) o

timedGet :: WebClient -> String -> IO (NominalDiffTime, Response LB.ByteString)
timedGet client target = do
  started <- getCurrentTime
  r <- webGet client target
  elapsed <- (`diffUTCTime` started) <$> getCurrentTime
  pure (elapsed, r)

seedOpenInvoice :: HasCallStack => WebEnv -> IO InvoiceId
seedOpenInvoice WebEnv {weStore} = do
  seedBadgePrice weStore "price1"
  createInvoiceRows weStore sampleInvoice `shouldReturn` Right ()
  pure (niInvoiceId sampleInvoice)

-- | A read that must park, then be released by the trigger and not by the timer. Parking first
-- is the point: an answer that arrives without the trigger proves nothing about the wake-up.
wokenBy :: HasCallStack => WebClient -> InvoiceId -> IO () -> Text -> IO J.Object
wokenBy client iid trigger expected = do
  started <- getCurrentTime
  held <- async $ webGet client (invoicePath iid <> "?wait=open")
  threadDelay holdParkDelay
  parked <- Async.poll held
  parked `shouldSatisfy` isNothing
  trigger
  r <- wait held
  elapsed <- (`diffUTCTime` started) <$> getCurrentTime
  statusOf r `shouldBe` 200
  o <- jsonObject r
  fieldOf o "status" `shouldBe` Just (J.String expected)
  elapsed `shouldSatisfy` (< 1)
  pure o

-- | Long enough for the request to reach the hold, short enough to leave the whole check
-- well inside the one second a woken answer is allowed.
holdParkDelay :: Int
holdParkDelay = 100000

invoicePath :: InvoiceId -> String
invoicePath (InvoiceId iid) = "/api/invoice/" <> T.unpack iid

markPaidAndPublish :: WebEnv -> InvoiceId -> IO ()
markPaidAndPublish WebEnv {weStore, weWaiters} iid = do
  markPaid weStore iid
  atomically $ publish weWaiters iid ISPaid

expireRow :: DBStore -> InvoiceId -> IO ()
expireRow st (InvoiceId iid) = withConnection st $ \db ->
  DB.execute db "UPDATE sx_badge_service_invoices SET status = 'expired' WHERE invoice_id = ?" (Only iid)

breakStore :: DBStore -> IO ()
breakStore st = withConnection st $ \db ->
  DB.execute_ db "DROP TABLE sx_badge_service_badge_code_invoices"

readsPerMinute :: Int
readsPerMinute = lmPerMinute readLimit

-- | `/assets/<hash>/%2e%2e/%2e%2e/index.html` resolves to the shell. Reading the cache rule off
-- the request would hand a shared cache the page for a year, which is the exact skew `sw.js`
-- exists to prevent: a pinned shell goes on asking for a build that is no longer served.
testCachingFollowsTheResolvedPath :: IO ()
testCachingFollowsTheResolvedPath = bounded "cache by resolved path" $ withWebApp $ \_ client -> do
  asset <- webGet client ("/assets/" <> buildHash <> "/main.js")
  headerOf asset hCacheControl `shouldBe` Just "public, max-age=31536000, immutable"
  shell <- webGet client "/"
  headerOf shell hCacheControl `shouldBe` Just "no-cache"
  escaped <- webGet client ("/assets/" <> buildHash <> "/%2e%2e/%2e%2e/index.html")
  statusOf escaped `shouldBe` 200
  responseBody escaped `shouldBe` shellHtml
  headerOf escaped hCacheControl `shouldBe` Just "no-cache"

testServesTheBuild :: IO ()
testServesTheBuild = bounded "serves the build" $ withWebApp $ \_ client -> do
  shell <- webGet client "/"
  statusOf shell `shouldBe` 200
  responseBody shell `shouldBe` shellHtml
  headerOf shell hContentType `shouldBe` Just "text/html; charset=utf-8"
  asset <- webGet client ("/assets/" <> buildHash <> "/main.js")
  statusOf asset `shouldBe` 200
  responseBody asset `shouldBe` assetJs
  headerOf asset hContentType `shouldBe` Just "text/javascript; charset=utf-8"
  worker <- webGet client "/sw.js"
  statusOf worker `shouldBe` 200
  responseBody worker `shouldBe` workerJs
  headerOf worker hContentType `shouldBe` Just "text/javascript; charset=utf-8"
  headerOf worker hCacheControl `shouldBe` Just "no-cache"
  missing <- webGet client ("/assets/" <> buildHash <> "/absent.js")
  statusOf missing `shouldBe` 404

-- | `npm run build` writes this, and it is what `static_dir` points at in a deployment. It is
-- a build artefact and not in the repository, so a checkout that has not been built has nothing
-- to serve and this test says so rather than failing over a file it never expected to find.
builtSiteDir :: FilePath
builtSiteDir = "apps" </> "simplex-badge-service" </> "web" </> "dist"

testServesBuiltWebApp :: IO ()
testServesBuiltWebApp = bounded "built web app" $ withServiceStore $ \st -> do
  built <- doesFileExist (builtSiteDir </> "index.html")
  if not built
    then pendingWith ("no web build at " <> builtSiteDir <> ": run `npm run build` in apps/simplex-badge-service/web")
    else withListener [] True holdMicros st (testServiceConfig builtSiteDir False) $ \_ client -> do
      shell <- webGet client "/"
      statusOf shell `shouldBe` 200
      LB.map toLower (LB.take 9 (responseBody shell)) `shouldBe` "<!doctype"
      hashes <- listDirectory (builtSiteDir </> "assets")
      hashes `shouldSatisfy` not . null
      assets <- concat <$> mapM (\h -> map (h </>) <$> listDirectory (builtSiteDir </> "assets" </> h)) hashes
      assets `shouldSatisfy` not . null
      served <- mapM (\a -> (a,) . statusOf <$> webGet client ("/assets/" <> a)) assets
      served `shouldSatisfy` all ((== 200) . snd)
      worker <- webGet client "/sw.js"
      statusOf worker `shouldBe` 200
      responseBody worker `shouldSatisfy` not . LB.null

traversalSpellings :: [String]
traversalSpellings =
  [ "/assets/../secret.txt",
    "/assets/%2e%2e/secret.txt",
    "/assets/%2e%2e%2fsecret.txt",
    "/assets/..%2fsecret.txt",
    "/assets/./../secret.txt",
    "/assets//../secret.txt",
    "/assets/" <> buildHash <> "/../../../secret.txt",
    "/assets/%2e%2e%2f%2e%2e%2fsecret.txt",
    "/assets/%2fetc%2fpasswd",
    "/assets/%00/../secret.txt",
    "/assets/escape.txt",
    "/%2e%2e%2fsecret.txt"
  ]

testTraversalRefused :: IO ()
testTraversalRefused = bounded "traversal" $ withWebApp $ \_ client -> do
  control <- webGet client ("/assets/" <> buildHash <> "/main.js")
  statusOf control `shouldBe` 200
  mapM_
    ( \target -> do
        r <- webGet client target
        (target, statusOf r) `shouldBe` (target, 404)
        (target, responseBody r) `shouldBe` (target, notFoundBody)
    )
    traversalSpellings

testInvoiceView :: IO ()
testInvoiceView = bounded "invoice view" $ withWebApp $ \env client -> do
  iid <- seedOpenInvoice env
  r <- webGet client (invoicePath iid)
  statusOf r `shouldBe` 200
  headerOf r hContentType `shouldBe` Just "application/json"
  o <- jsonObject r
  sort (map K.toText (KM.keys o))
    `shouldBe` sort ["status", "badgeType", "months", "amount", "currency", "expiresAt", "address", "cryptoAmount", "cryptoCurrency"]
  fieldOf o "status" `shouldBe` Just (J.String "open")
  fieldOf o "badgeType" `shouldBe` Just (J.String "supporter")
  fieldOf o "months" `shouldBe` Just (J.Number 1)
  fieldOf o "amount" `shouldBe` Just (J.Number 500)
  fieldOf o "currency" `shouldBe` Just (J.String "usd")
  fieldOf o "address" `shouldBe` Just (J.String "bc1qexampleaddress")
  fieldOf o "cryptoAmount" `shouldBe` Just (J.String "0.00050000")
  fieldOf o "cryptoCurrency" `shouldBe` Just (J.String "btc")
  case fieldOf o "expiresAt" of
    Just (J.String t) -> T.unpack t `shouldStartWith` "2030-01-01T00:00:00"
    other -> expectationFailure ("expiresAt is " <> show other)
  settle (weStore env) iid (SigSettled (rcv 500 (Just "0.00050000")) settleAt) detectedAt `shouldReturn` Right ISPaid
  settled <- webGet client (invoicePath iid)
  po <- jsonObject settled
  fieldOf po "status" `shouldBe` Just (J.String "paid")
  fieldOf po "settledAt" `shouldBe` Just (J.toJSON settleAt)
  fieldOf po "amountPaid" `shouldBe` Just (J.Number 500)
  fieldOf po "cryptoAmountPaid" `shouldBe` Just (J.String "0.00050000")
  fieldOf po "clientSecret" `shouldBe` Nothing

testUnknownInvoiceIsOpaque :: IO ()
testUnknownInvoiceIsOpaque = bounded "unknown id" $ withWebApp $ \env client -> do
  iid <- seedOpenInvoice env
  guessed <- webGet client "/api/invoice/no-such-invoice"
  statusOf guessed `shouldBe` 404
  responseBody guessed `shouldBe` notFoundBody
  nearMiss <- webGet client (invoicePath iid <> "x")
  statusOf nearMiss `shouldBe` 404
  responseBody nearMiss `shouldBe` responseBody guessed

testApiIsNeverCached :: IO ()
testApiIsNeverCached = bounded "no-store" $ withWebApp $ \env client -> do
  iid <- seedOpenInvoice env
  ok <- webGet client (invoicePath iid)
  statusOf ok `shouldBe` 200
  headerOf ok hCacheControl `shouldBe` Just "no-store"
  missing <- webGet client "/api/invoice/no-such-invoice"
  statusOf missing `shouldBe` 404
  headerOf missing hCacheControl `shouldBe` Just "no-store"
  refused <- exhaustReadLimit client "/api/invoice/no-such-invoice" []
  statusOf refused `shouldBe` 429
  headerOf refused hCacheControl `shouldBe` Just "no-store"

exhaustReadLimit :: HasCallStack => WebClient -> String -> [Header] -> IO (Response LB.ByteString)
exhaustReadLimit client target hdrs = go (readsPerMinute + 5)
  where
    go :: Int -> IO (Response LB.ByteString)
    go 0 = failWith ("no read of " <> target <> " was refused within 65 requests")
    go n = do
      r <- webRequest client "GET" target hdrs
      if statusOf r == 429 then pure r else go (n - 1)

testWrongMethodIs405 :: IO ()
testWrongMethodIs405 = bounded "wrong method" $ withWebApp $ \env client -> do
  iid <- seedOpenInvoice env
  postShell <- webRequest client "POST" "/" []
  statusOf postShell `shouldBe` 405
  headerOf postShell "Allow" `shouldBe` Just "GET"
  postWorker <- webRequest client "POST" "/sw.js" []
  statusOf postWorker `shouldBe` 405
  deleteInvoice <- webRequest client "DELETE" (invoicePath iid) []
  statusOf deleteInvoice `shouldBe` 405
  headerOf deleteInvoice hCacheControl `shouldBe` Just "no-store"
  getCreate <- webRequest client "GET" "/api/invoice" []
  statusOf getCreate `shouldBe` 405
  headerOf getCreate "Allow" `shouldBe` Just "POST"

testUnroutedPathIs404 :: IO ()
testUnroutedPathIs404 = bounded "unrouted" $ withWebApp $ \_ client -> do
  nowhere <- webGet client "/nowhere"
  statusOf nowhere `shouldBe` 404
  responseBody nowhere `shouldBe` notFoundBody
  webhook <- webRequest client "POST" "/webhooks/btcpay" []
  statusOf webhook `shouldBe` 400
  responseBody webhook `shouldBe` ""
  wrongMethod <- webGet client "/webhooks/btcpay"
  statusOf wrongMethod `shouldBe` 405
  headerOf wrongMethod "Allow" `shouldBe` Just "POST"

testWaitAnswersAtOnce :: IO ()
testWaitAnswersAtOnce = bounded "wait answers at once" $ withWebApp $ \env client -> do
  iid <- seedOpenInvoice env
  (staleElapsed, stale) <- timedGet client (invoicePath iid <> "?wait=expired")
  statusOf stale `shouldBe` 200
  staleObject <- jsonObject stale
  fieldOf staleObject "status" `shouldBe` Just (J.String "open")
  staleElapsed `shouldSatisfy` (< 1)
  (junkElapsed, junk) <- timedGet client (invoicePath iid <> "?wait=not-a-status")
  junkObject <- jsonObject junk
  fieldOf junkObject "status" `shouldBe` Just (J.String "open")
  junkElapsed `shouldSatisfy` (< 1)
  markPaid (weStore env) iid
  (terminalElapsed, terminalR) <- timedGet client (invoicePath iid <> "?wait=paid")
  terminalObject <- jsonObject terminalR
  fieldOf terminalObject "status" `shouldBe` Just (J.String "paid")
  terminalElapsed `shouldSatisfy` (< 1)

-- | The counter a hold watches starts at zero, so a payment recorded before the request arrived
-- can never wake it. The browser says what it has rendered; anything else answers at once.
testHoldAnswersAPaymentThePageHasNotSeen :: IO ()
testHoldAnswersAPaymentThePageHasNotSeen = bounded "hold sees the payment" $ withWebApp $ \env client -> do
  iid <- seedOpenInvoice env
  settle (weStore env) iid (SigFunded (rcv 200 (Just "0.00020000")) PaidInPart) settleAt `shouldReturn` Right ISOpen
  (staleElapsed, stale) <- timedGet client (invoicePath iid <> "?wait=open&seenPaid=")
  statusOf stale `shouldBe` 200
  staleElapsed `shouldSatisfy` (< 1)
  staleObject <- jsonObject stale
  fieldOf staleObject "cryptoAmountPaid" `shouldBe` Just (J.String "0.00020000")
  -- and a page that has already rendered that figure still waits for the next change
  held <- async $ webGet client (invoicePath iid <> "?wait=open&seenPaid=0.00020000")
  threadDelay 100000
  Async.poll held >>= (`shouldSatisfy` isNothing)
  markPaidAndPublish env iid
  statusOf <$> wait held `shouldReturn` 200

-- | Monero reports an invoice as confirming while its figures are still zero, so the verdict
-- arrives with no figure to go with it. It is the whole difference between the payment screen
-- and the confirming one, and a hold that ignored it would sit on the wrong screen.
testHoldAnswersAVerdictWithNoFigure :: IO ()
testHoldAnswersAVerdictWithNoFigure = bounded "hold sees the verdict" $ withWebApp $ \env client -> do
  iid <- seedOpenInvoice env
  settle (weStore env) iid (SigFunded (rcv 0 Nothing) PaidInFull) settleAt `shouldReturn` Right ISOpen
  (elapsed, r) <- timedGet client (invoicePath iid <> "?wait=open&seenPaid=&seenFull=0")
  statusOf r `shouldBe` 200
  elapsed `shouldSatisfy` (< 1)
  o <- jsonObject r
  fieldOf o "paidInFull" `shouldBe` Just (J.Bool True)
  fieldOf o "cryptoAmountPaid" `shouldBe` Nothing
  -- and a page that has already rendered the verdict waits for whatever comes next
  held <- async $ webGet client (invoicePath iid <> "?wait=open&seenPaid=&seenFull=1")
  threadDelay 100000
  Async.poll held >>= (`shouldSatisfy` isNothing)
  markPaidAndPublish env iid
  statusOf <$> wait held `shouldReturn` 200

testHoldIsWokenNotPolled :: IO ()
testHoldIsWokenNotPolled = bounded "hold is woken" $ withWebApp $ \env client -> do
  iid <- seedOpenInvoice env
  _ <- wokenBy client iid (markPaidAndPublish env iid) "paid"
  pure ()

testReadRateLimit :: IO ()
testReadRateLimit = bounded "read rate limit" $ withWebApp $ \_ client -> do
  let target = "/api/invoice/no-such-invoice"
  allowed <- mapM (\_ -> statusOf <$> webGet client target) [1 .. readsPerMinute]
  allowed `shouldSatisfy` all (== 404)
  refused <- webGet client target
  statusOf refused `shouldBe` 429
  responseBody refused `shouldBe` "{\"error\":\"rate_limited\"}"
  case headerOf refused "Retry-After" >>= (readMaybe . BC.unpack) of
    Just seconds -> seconds `shouldSatisfy` \s -> s >= (1 :: Int) && s <= 60
    Nothing -> expectationFailure ("Retry-After is " <> show (headerOf refused "Retry-After"))

testForwardedForOnlyWhenTrusted :: IO ()
testForwardedForOnlyWhenTrusted = bounded "forwarded-for" $ do
  let target = "/api/invoice/no-such-invoice"
      -- what a caller can write, then what the proxy appends: nginx's
      -- proxy_add_x_forwarded_for puts the peer it saw last
      forgedBy i = ("X-Forwarded-For", BC.pack ("203.0.113." <> show (i :: Int)) <> ", 10.0.0.1")
      proxiedFor i = ("X-Forwarded-For", BC.pack ("10.0.0." <> show (i :: Int)))
  withWebApp $ \_ client -> do
    forged <- mapM (\i -> statusOf <$> webRequest client "GET" target [forgedBy i]) [1 .. readsPerMinute]
    forged `shouldSatisfy` all (== 404)
    refused <- webRequest client "GET" target [forgedBy (readsPerMinute + 1)]
    statusOf refused `shouldBe` 429
  -- trusted, and the caller varies every entry but the last: one bucket, so the limit holds
  withWebAppForwarded True $ \_ client -> do
    forged <- mapM (\i -> statusOf <$> webRequest client "GET" target [forgedBy i]) [1 .. readsPerMinute]
    forged `shouldSatisfy` all (== 404)
    refused <- webRequest client "GET" target [forgedBy (readsPerMinute + 1)]
    statusOf refused `shouldBe` 429
  -- and two clients the proxy really saw apart are counted apart
  withWebAppForwarded True $ \_ client -> do
    first' <- mapM (\_ -> statusOf <$> webRequest client "GET" target [proxiedFor 1]) [1 .. readsPerMinute]
    first' `shouldSatisfy` all (== 404)
    statusOf <$> webRequest client "GET" target [proxiedFor 1] `shouldReturn` 429
    statusOf <$> webRequest client "GET" target [proxiedFor 2] `shouldReturn` 404

testForwardedForMustBeAnAddress :: IO ()
testForwardedForMustBeAnAddress = bounded "forwarded-for is an address" $ withWebAppForwarded True $ \_ client -> do
  let target = "/api/invoice/no-such-invoice"
      forged i = ("X-Forwarded-For", BC.pack ("not-an-address-" <> show (i :: Int)))
  minted <- mapM (\i -> statusOf <$> webRequest client "GET" target [forged i]) [1 .. readsPerMinute]
  minted `shouldSatisfy` all (== 404)
  refused <- webRequest client "GET" target [forged (readsPerMinute + 1)]
  statusOf refused `shouldBe` 429
  mapM_
    ( \v -> do
        r <- webRequest client "GET" target [("X-Forwarded-For", v)]
        (v, statusOf r) `shouldBe` (v, 429)
    )
    ["", "not-an-address", "010.0.0.1", "1.2.3.4.5", "256.1.1.1", "1.2.3", "::1%eth0", "1:2:3:4:5:6:7", "[2001:db8::1]", "1.2.3.4:5678", "gggg::1", "12345::1"]
  mapM_
    ( \v -> do
        r <- webRequest client "GET" target [("X-Forwarded-For", v)]
        (v, statusOf r) `shouldBe` (v, 404)
    )
    ["192.0.2.7", "2001:db8::1", "::1", "::", "::ffff:192.0.2.1", "1:2:3:4:5:6:7:8"]

testBucketsStayBounded :: IO ()
testBucketsStayBounded = bounded "bucket cap" $ withWebApp $ \env _ -> do
  let clients = map (\i -> T.pack ("198.51." <> show (i `div` 256) <> "." <> show (i `mod` 256))) [1 .. maxBuckets * 2 + 100]
  mapM_ (takeToken env readLimit) clients
  held <- Map.size <$> readTVarIO (weBuckets env)
  held `shouldSatisfy` \n -> n > 0 && n <= maxBuckets
  answers <- mapM (\_ -> takeToken env readLimit "203.0.113.9") [1 .. readsPerMinute + 1]
  length (filter isJust answers) `shouldBe` 1

  -- A refusal has to write the reclaimed map back too, or every refused request redoes the
  -- filtering over a map that never shrinks: under a flood the limiter becomes the amplifier.
  -- The refusal has to land while the map is over the cap, which is why the flood comes second.
  -- `reclaim` runs before the insert, so an allowed call never leaves the map above the cap.
  -- Filling to exactly the cap makes the refusal below the call that has to do the reclaiming.
  let fill i = do
        n <- Map.size <$> readTVarIO (weBuckets env)
        when (n < maxBuckets) $ do
          _ <- takeToken env readLimit (T.pack ("192.0." <> show (i `div` 256) <> "." <> show (i `mod` 256)))
          fill (i + 1)
  fill (1 :: Int)
  atTheCap <- Map.size <$> readTVarIO (weBuckets env)
  atTheCap `shouldBe` maxBuckets
  refusedAgain <- takeToken env readLimit "203.0.113.9"
  refusedAgain `shouldSatisfy` isJust
  -- the half, not merely one fewer: dropping a single bucket per refusal would still sort the
  -- whole map on every refused request, which is the cost this write-back exists to stop paying
  afterARefusal <- Map.size <$> readTVarIO (weBuckets env)
  afterARefusal `shouldSatisfy` (<= atTheCap `div` 2 + 1)

testHoldReportsTheRowNotTheWake :: IO ()
testHoldReportsTheRowNotTheWake = bounded "hold reports the row" $ withWebApp $ \env client -> do
  iid <- seedOpenInvoice env
  held <- async $ webGet client (invoicePath iid <> "?wait=open")
  threadDelay 100000
  markPaid (weStore env) iid
  atomically $ publish (weWaiters env) iid ISExpired
  r <- wait held
  statusOf r `shouldBe` 200
  o <- jsonObject r
  fieldOf o "status" `shouldBe` Just (J.String "paid")

testHold :: Int
testHold = 2000000

testHoldTimeoutReportsAnUnpublishedChange :: IO ()
testHoldTimeoutReportsAnUnpublishedChange = bounded "hold timeout" $ withWebAppHolding testHold $ \env client -> do
  iid <- seedOpenInvoice env
  started <- getCurrentTime
  held <- async $ webGet client (invoicePath iid <> "?wait=open")
  threadDelay 50000
  expireRow (weStore env) iid
  r <- wait held
  elapsed <- (`diffUTCTime` started) <$> getCurrentTime
  statusOf r `shouldBe` 200
  o <- jsonObject r
  fieldOf o "status" `shouldBe` Just (J.String "expired")
  elapsed `shouldSatisfy` (>= 1.95)

testHandlerExceptionIsContained :: IO ()
testHandlerExceptionIsContained = bounded "handler exception" $ withWebAppExpectingThrow $ \env client -> do
  iid <- seedOpenInvoice env
  breakStore (weStore env)
  r <- webGet client (invoicePath iid)
  statusOf r `shouldBe` 500
  responseBody r `shouldBe` "{\"error\":\"internal\"}"
  headerOf r hCacheControl `shouldBe` Just "no-store"
  shell <- webGet client "/"
  statusOf shell `shouldBe` 200
  responseBody shell `shouldBe` shellHtml

createsPerMinute :: Int
createsPerMinute = 5

testExpiryMinutes :: Int
testExpiryMinutes = 45

testBTCPayConfig :: BTCPayConfig
testBTCPayConfig =
  BTCPayConfig
    { bHost = "https://btcpay.example",
      bApiKey = "api-key",
      bStoreId = "store-id",
      bWebhookSecret = "webhook-secret",
      bExpiryMinutes = testExpiryMinutes,
      bSpeedPolicy = MediumSpeed,
      bPaymentTolerance = 0.5
    }

supporterPriceId, legendPriceId, disabledPriceId :: Text
supporterPriceId = "price-supporter"
legendPriceId = "price-legend"
disabledPriceId = "price-retired"

anyPriceOfferId, legendOnlyOfferId, disabledOfferId :: Text
anyPriceOfferId = "offer-3m"
legendOnlyOfferId = "offer-3m-legend"
disabledOfferId = "offer-retired"

seedCheckoutCatalog :: DBStore -> IO ()
seedCheckoutCatalog st = do
  insertPrice st supporterPriceId "supporter" 500 "active"
  insertPrice st legendPriceId "legend" 2000 "active"
  insertPrice st disabledPriceId "supporter" 500 "disabled"
  insertOffer st anyPriceOfferId Nothing 3 10 "active"
  insertOffer st legendOnlyOfferId (Just legendPriceId) 3 10 "active"
  insertOffer st disabledOfferId Nothing 3 10 "disabled"

codeDigest :: Text -> ByteString
codeDigest = C.sha256Hash . encodeUtf8

codeHashText :: Text -> Text
codeHashText = T.filter (/= '=') . safeDecodeUtf8 . B64U.encode . codeDigest

sampleCode :: Text
sampleCode = "YDC8AYGQTMPUYZ92TUXP"

createBody :: Text -> Maybe Text -> Text -> Text -> LB.ByteString
createBody priceId offerId method codeHash =
  J.encode . J.object $
    ["priceId" .= priceId, "method" .= method, "codeHash" .= codeHash]
      <> maybe [] (\o -> ["offerId" .= o]) offerId

postCreateAs :: WebClient -> Int -> LB.ByteString -> IO (Response LB.ByteString)
postCreateAs client i body =
  webRequestBody
    client
    "POST"
    "/api/invoice"
    [(hContentType, "application/json"), ("X-Forwarded-For", BC.pack ("203.0.113." <> show i))]
    body

withCheckout :: (IORef StubState -> WebEnv -> WebClient -> IO a) -> IO a
withCheckout = withCheckoutProvider stubProvider

withCheckoutProvider :: (IORef StubState -> Provider) -> (IORef StubState -> WebEnv -> WebClient -> IO a) -> IO a
withCheckoutProvider mkProvider action =
  withServiceStore $ \st -> do
    createDirectoryIfMissing True "tests/tmp"
    withTempDirectory "tests/tmp" "badge-checkout" $ \root -> do
      staticDir <- prepareStaticDir root
      seedCheckoutCatalog st
      ref <- newIORef (newStubState (Right sampleProviderInvoice))
      let cfg = (testServiceConfig staticDir True) {btcpay = Just testBTCPayConfig}
      withListener [mkProvider ref] True holdMicros st cfg (action ref)

invoiceCount :: DBStore -> IO Int
invoiceCount st = withConnection st $ \db -> do
  rows <- DB.query_ db "SELECT COUNT(*) FROM sx_badge_service_invoices" :: IO [Only Int]
  pure $ case rows of
    (Only n : _) -> n
    [] -> 0

plantCodeHash :: DBStore -> Text -> IO ()
plantCodeHash st code = withConnection st $ \db ->
  DB.execute
    db
    "INSERT INTO sx_badge_service_badge_codes (code_hash, badge_type, months, code_payment_status, created_at) VALUES (?,?,?,?,?)"
    (DB.Binary (codeDigest code), "supporter" :: Text, 1 :: Int, "unpaid" :: Text, "2026-08-31T00:00:00Z" :: Text)

stringField :: HasCallStack => J.Object -> Text -> IO Text
stringField o k = case fieldOf o k of
  Just (J.String t) -> pure t
  other -> failWith (T.unpack k <> " is " <> show other)

errorBody :: Text -> LB.ByteString
errorBody code = "{\"error\":\"" <> LB.pack (T.unpack code) <> "\"}"

testCreateInvoice :: IO ()
testCreateInvoice = bounded "create invoice" $ withCheckout $ \ref env client -> do
  started <- getCurrentTime
  r <- postCreateAs client 1 (createBody supporterPriceId Nothing "btc" (codeHashText sampleCode))
  statusOf r `shouldBe` 200
  headerOf r hContentType `shouldBe` Just "application/json"
  headerOf r hCacheControl `shouldBe` Just "no-store"
  o <- jsonObject r
  sort (map K.toText (KM.keys o))
    `shouldBe` sort ["invoiceId", "badgeType", "months", "amount", "currency", "expiresAt", "address", "cryptoAmount", "cryptoCurrency"]
  fieldOf o "badgeType" `shouldBe` Just (J.String "supporter")
  fieldOf o "months" `shouldBe` Just (J.Number 1)
  fieldOf o "amount" `shouldBe` Just (J.Number 500)
  fieldOf o "currency" `shouldBe` Just (J.String "usd")
  fieldOf o "address" `shouldBe` Just (J.String "bc1qexampleaddress")
  fieldOf o "cryptoAmount" `shouldBe` Just (J.String "0.00050000")
  fieldOf o "cryptoCurrency" `shouldBe` Just (J.String "btc")
  invId <- InvoiceId <$> stringField o "invoiceId"
  Just row <- getInvoice (weStore env) invId
  irStatus row `shouldBe` ISOpen
  irProvider row `shouldBe` PPCrypto
  irProviderRef row `shouldBe` piProviderRef sampleProviderInvoice
  irBadgeType row `shouldBe` BTSupporter
  irMonths row `shouldBe` 1
  irPrice row `shouldBe` CurrencyAmount 500
  irAmount row `shouldBe` CurrencyAmount 500
  irDestination row `shouldBe` piDestination sampleProviderInvoice
  codeRow <- settledCode (weStore env) invId
  bcPaymentStatus codeRow `shouldBe` CPSUnpaid
  fieldOf o "expiresAt" `shouldBe` Just (J.toJSON (irExpiresAt row))
  diffUTCTime (irExpiresAt row) started `shouldSatisfy` \d -> d > 44 * 60 && d <= 46 * 60
  calls <- stubCalls ref
  case calls of
    [StubCreate method draft] -> do
      method `shouldBe` SPMCrypto CCBtc
      odAmount draft `shouldBe` CurrencyAmount 500
      odCurrency draft `shouldBe` "usd"
    other -> expectationFailure ("provider calls: " <> show other)

testXmrReachesTheProviderAsXmr :: IO ()
testXmrReachesTheProviderAsXmr = bounded "xmr is xmr" $ withCheckout $ \ref _ client -> do
  r <- postCreateAs client 1 (createBody supporterPriceId Nothing "xmr" (codeHashText sampleCode))
  statusOf r `shouldBe` 200
  stubCalls ref >>= \case
    [StubCreate method _] -> method `shouldBe` SPMCrypto CCXmr
    other -> expectationFailure ("provider calls: " <> show other)

testCreateCarriesNoCode :: IO ()
testCreateCarriesNoCode = bounded "no code in the response" $ withCheckout $ \_ env client -> do
  let hashText = codeHashText sampleCode
  r <- postCreateAs client 1 (createBody supporterPriceId Nothing "btc" hashText)
  statusOf r `shouldBe` 200
  let raw = LB.unpack (responseBody r)
  raw `shouldNotContain` T.unpack sampleCode
  raw `shouldNotContain` T.unpack hashText
  raw `shouldNotContain` T.unpack supporterPriceId
  raw `shouldContain` "invoiceId"
  invoiceCount (weStore env) `shouldReturn` 1

testCreateDerivesFromCatalog :: IO ()
testCreateDerivesFromCatalog = bounded "derived from the catalog" $ withCheckout $ \_ env client -> do
  let tampered =
        J.encode $
          J.object
            [ "priceId" .= legendPriceId,
              "offerId" .= anyPriceOfferId,
              "method" .= ("btc" :: Text),
              "codeHash" .= codeHashText sampleCode,
              "badgeType" .= ("supporter" :: Text),
              "months" .= (1 :: Int),
              "amount" .= (500 :: Int)
            ]
  r <- postCreateAs client 1 tampered
  statusOf r `shouldBe` 200
  o <- jsonObject r
  fieldOf o "badgeType" `shouldBe` Just (J.String "legend")
  fieldOf o "months" `shouldBe` Just (J.Number 3)
  fieldOf o "amount" `shouldBe` Just (J.Number 5400)
  invId <- InvoiceId <$> stringField o "invoiceId"
  Just row <- getInvoice (weStore env) invId
  irBadgeType row `shouldBe` BTLegend
  irMonths row `shouldBe` 3
  irPrice row `shouldBe` CurrencyAmount 6000
  irAmount row `shouldBe` CurrencyAmount 5400

testCatalogRefusalCostsNothing :: IO ()
testCatalogRefusalCostsNothing = bounded "catalog refusals" $ withCheckout $ \ref env client -> do
  let cases :: [(String, Text, Maybe Text)]
      cases =
        [ ("an unknown price", "price-that-never-existed", Nothing),
          ("a disabled price", disabledPriceId, Nothing),
          ("an unknown offer", supporterPriceId, Just "offer-that-never-existed"),
          ("a disabled offer", supporterPriceId, Just disabledOfferId),
          ("an offer belonging to another price", supporterPriceId, Just legendOnlyOfferId)
        ]
  mapM_
    ( \(i, (label, priceId, offerId)) -> do
        r <- postCreateAs client i (createBody priceId offerId "btc" (codeHashText sampleCode))
        (label, statusOf r) `shouldBe` (label, 400)
        (label, responseBody r) `shouldBe` (label, errorBody "catalog_changed")
    )
    (zip [30 ..] cases)
  stubCalls ref `shouldReturn` []
  invoiceCount (weStore env) `shouldReturn` 0

testCodeConflictCostsNothing :: IO ()
testCodeConflictCostsNothing = bounded "code conflict" $ withCheckout $ \ref env client -> do
  first <- postCreateAs client 1 (createBody supporterPriceId Nothing "btc" (codeHashText sampleCode))
  statusOf first `shouldBe` 200
  sold <- stubCalls ref
  length sold `shouldBe` 1
  again <- postCreateAs client 2 (createBody supporterPriceId Nothing "btc" (codeHashText sampleCode))
  statusOf again `shouldBe` 409
  responseBody again `shouldBe` errorBody "code_conflict"
  stubCalls ref `shouldReturn` sold
  invoiceCount (weStore env) `shouldReturn` 1

testRacingCodeHashIsConflict :: IO ()
testRacingCodeHashIsConflict = bounded "racing code hash" $ do
  planted <- newIORef (Nothing :: Maybe DBStore)
  let racing ref =
        let stub = stubProvider ref
         in stub
              { pCreateInvoice = \method draft -> do
                  readIORef planted >>= mapM_ (`plantCodeHash` sampleCode)
                  pCreateInvoice stub method draft
              }
  withCheckoutProvider racing $ \ref env client -> do
    atomicModifyIORef' planted (const (Just (weStore env), ()))
    r <- postCreateAs client 1 (createBody supporterPriceId Nothing "btc" (codeHashText sampleCode))
    statusOf r `shouldBe` 409
    responseBody r `shouldBe` errorBody "code_conflict"
    length <$> stubCalls ref `shouldReturn` 1
    invoiceCount (weStore env) `shouldReturn` 0

testStoreLostAfterTheProviderCreated :: IO ()
testStoreLostAfterTheProviderCreated = bounded "orphan at the provider" $ do
  broken <- newIORef (Nothing :: Maybe DBStore)
  let losing ref =
        let stub = stubProvider ref
         in stub
              { pCreateInvoice = \method draft -> do
                  readIORef broken >>= mapM_ breakStore
                  pCreateInvoice stub method draft
              }
  withCheckoutProvider losing $ \ref env client -> do
    atomicModifyIORef' broken (const (Just (weStore env), ()))
    r <- postCreateAs client 1 (createBody supporterPriceId Nothing "btc" (codeHashText sampleCode))
    statusOf r `shouldBe` 500
    responseBody r `shouldBe` errorBody "internal"
    headerOf r hCacheControl `shouldBe` Just "no-store"
    length <$> stubCalls ref `shouldReturn` 1
    invoiceCount (weStore env) `shouldReturn` 0

testBadRequestCostsNothing :: IO ()
testBadRequestCostsNothing = bounded "bad requests" $ withCheckout $ \ref env client -> do
  let good = codeHashText sampleCode
      bodies :: [(String, LB.ByteString)]
      bodies =
        [ ("not JSON at all", "priceId=price-supporter"),
          ("a JSON array", "[]"),
          ("no fields", "{}"),
          ("no method", J.encode (J.object ["priceId" .= supporterPriceId, "codeHash" .= good])),
          ("no codeHash", J.encode (J.object ["priceId" .= supporterPriceId, "method" .= ("btc" :: Text)])),
          ("no priceId", J.encode (J.object ["method" .= ("btc" :: Text), "codeHash" .= good])),
          ("an unknown method", createBody supporterPriceId Nothing "paypal" good),
          ("a mistyped method", J.encode (J.object ["priceId" .= supporterPriceId, "method" .= (7 :: Int), "codeHash" .= good])),
          ("a mistyped offerId", J.encode (J.object ["priceId" .= supporterPriceId, "offerId" .= (7 :: Int), "method" .= ("btc" :: Text), "codeHash" .= good])),
          ("an empty codeHash", createBody supporterPriceId Nothing "btc" ""),
          ("a hex codeHash", createBody supporterPriceId Nothing "btc" (T.replicate 64 "a")),
          ("a truncated codeHash", createBody supporterPriceId Nothing "btc" (T.dropEnd 1 good)),
          ("a padded codeHash", createBody supporterPriceId Nothing "btc" (good <> "=")),
          ("a codeHash outside base64url", createBody supporterPriceId Nothing "btc" (T.dropEnd 1 good <> "+")),
          ("a non-canonical codeHash", createBody supporterPriceId Nothing "btc" (T.dropEnd 1 good <> "B"))
        ]
  mapM_
    ( \(i, (label, body)) -> do
        r <- postCreateAs client i body
        (label, statusOf r) `shouldBe` (label, 400)
        (label, responseBody r) `shouldBe` (label, errorBody "bad_request")
    )
    (zip [10 ..] bodies)
  stubCalls ref `shouldReturn` []
  invoiceCount (weStore env) `shouldReturn` 0

testOversizedBodyCostsNothing :: IO ()
testOversizedBodyCostsNothing = bounded "oversized body" $ withCheckout $ \ref env client -> do
  let padded =
        J.encode $
          J.object
            [ "priceId" .= supporterPriceId,
              "method" .= ("btc" :: Text),
              "codeHash" .= codeHashText sampleCode,
              "padding" .= T.replicate (64 * 1024) "x"
            ]
  LB.length padded `shouldSatisfy` (> 64 * 1024)
  r <- postCreateAs client 1 padded
  statusOf r `shouldBe` 400
  responseBody r `shouldBe` errorBody "bad_request"
  stubCalls ref `shouldReturn` []
  invoiceCount (weStore env) `shouldReturn` 0
  small <- postCreateAs client 2 (J.encode (J.object ["priceId" .= supporterPriceId, "method" .= ("btc" :: Text), "codeHash" .= codeHashText sampleCode, "padding" .= T.replicate 16 "x"]))
  statusOf small `shouldBe` 200

testCardIsProviderUnavailable :: IO ()
testCardIsProviderUnavailable = bounded "card is unavailable" $ withCheckout $ \ref env client -> do
  r <- postCreateAs client 1 (createBody supporterPriceId Nothing "card" (codeHashText sampleCode))
  statusOf r `shouldBe` 503
  responseBody r `shouldBe` errorBody "provider_unavailable"
  stubCalls ref `shouldReturn` []
  invoiceCount (weStore env) `shouldReturn` 0

testProviderFailureWritesNothing :: IO ()
testProviderFailureWritesNothing = bounded "provider failure" $ withCheckout $ \ref env client -> do
  atomicModifyIORef' ref $ \s -> (s {ssCreateResult = Left (ProviderError "connection refused")}, ())
  r <- postCreateAs client 1 (createBody supporterPriceId Nothing "btc" (codeHashText sampleCode))
  statusOf r `shouldBe` 503
  responseBody r `shouldBe` errorBody "provider_unavailable"
  length <$> stubCalls ref `shouldReturn` 1
  invoiceCount (weStore env) `shouldReturn` 0
  codeHashExists (weStore env) (codeDigest sampleCode) `shouldReturn` False

-- | provider_ref is unique, so a test creating several invoices has to vary it the way a
-- real provider would.
distinctProviderRefs :: IORef StubState -> Provider
distinctProviderRefs ref =
  let stub = stubProvider ref
   in stub
        { pCreateInvoice = \method draft -> do
            created <- pCreateInvoice stub method draft
            n <- length <$> stubCalls ref
            pure (fmap (\inv -> inv {piProviderRef = piProviderRef inv <> "-" <> T.pack (show n)}) created)
        }

testCreateRateLimit :: IO ()
testCreateRateLimit = bounded "create rate limit" $ withCheckoutProvider distinctProviderRefs $ \ref env client -> do
  let attempt n = postCreateAs client 20 (createBody supporterPriceId Nothing "btc" (codeHashText (sampleCode <> T.pack (show (n :: Int)))))
  allowed <- mapM (fmap statusOf . attempt) [1 .. createsPerMinute]
  allowed `shouldSatisfy` all (== 200)
  refused <- attempt (createsPerMinute + 1)
  statusOf refused `shouldBe` 429
  responseBody refused `shouldBe` errorBody "rate_limited"
  headerOf refused hCacheControl `shouldBe` Just "no-store"
  case headerOf refused "Retry-After" >>= (readMaybe . BC.unpack) of
    Just seconds -> seconds `shouldSatisfy` \s -> s >= (1 :: Int) && s <= 60
    Nothing -> expectationFailure ("Retry-After is " <> show (headerOf refused "Retry-After"))
  length <$> stubCalls ref `shouldReturn` createsPerMinute
  invoiceCount (weStore env) `shouldReturn` createsPerMinute

settleAt :: UTCTime
settleAt = UTCTime (fromGregorian 2026 9 2) (secondsToDiffTime (12 * 3600 + 34 * 60 + 56))

replayAt :: UTCTime
replayAt = addUTCTime 3600 settleAt

-- | We always find out later than the payment happened, so the tests pass this as now and
-- settleAt inside the signal. One value for both would let a build that wrote the wrong
-- one still pass.
detectedAt :: UTCTime
detectedAt = addUTCTime 300 settleAt

rcv :: Word32 -> Maybe Text -> Received
rcv amount crypto = Received {rcvAmount = CurrencyAmount amount, rcvCrypto = crypto, rcvDue = Nothing}

settle :: DBStore -> InvoiceId -> PaymentSignal -> UTCTime -> IO (Either Text InvoiceStatus)
settle st iid signal at = newWaiters >>= \waiters -> settleOrder st waiters iid signal at

seedOpen :: HasCallStack => DBStore -> IO InvoiceId
seedOpen st = do
  seedBadgePrice st "price1"
  createInvoiceRows st sampleInvoice `shouldReturn` Right ()
  pure (niInvoiceId sampleInvoice)

invoiceStatus :: HasCallStack => DBStore -> InvoiceId -> IO InvoiceStatus
invoiceStatus st iid = getInvoice st iid >>= maybe (failWith "no invoice row") (pure . irStatus)

paymentRow :: HasCallStack => DBStore -> InvoiceId -> IO InvoicePayment
paymentRow st iid =
  getInvoice st iid >>= \case
    Just InvoiceRow {irPayment = Just p} -> pure p
    _ -> failWith "no payment row"

-- | The badge_codes row an invoice points at, read directly: the service reaches it only
-- through the code hash at redemption, so there is no production query to borrow.
data CodeRow = CodeRow {bcPaymentStatus :: BadgeCodePaymentStatus, bcExpiresAt :: Maybe UTCTime, bcRevokedAt :: Maybe UTCTime}
  deriving (Eq, Show)

settledCode :: HasCallStack => DBStore -> InvoiceId -> IO CodeRow
settledCode st (InvoiceId iid) = withConnection st $ \db -> do
  rows <-
    DB.query
      db
      "SELECT c.code_payment_status, c.expires_at, c.revoked_at FROM sx_badge_service_badge_codes c JOIN sx_badge_service_badge_code_invoices ci ON ci.code_hash = c.code_hash WHERE ci.invoice_id = ?"
      (Only iid)
  case rows of
    (status, expiresAt, revokedAt) : _ -> pure CodeRow {bcPaymentStatus = fromMaybe CPSUnpaid (textDecode status), bcExpiresAt = expiresAt, bcRevokedAt = revokedAt}
    [] -> failWith "no badge_codes row"

paymentIdentity :: DBStore -> InvoiceId -> IO [(Text, Text, Maybe Text, Maybe Text)]
paymentIdentity st (InvoiceId iid) = withConnection st $ \db ->
  DB.query db "SELECT payment_id, provider, provider_ref, currency FROM sx_badge_service_payments WHERE invoice_id = ?" (Only iid)

touchInvoice :: DBStore -> InvoiceId -> UTCTime -> IO ()
touchInvoice st (InvoiceId iid) at = withConnection st $ \db ->
  DB.execute db "UPDATE sx_badge_service_invoices SET updated_at = ? WHERE invoice_id = ?" (at, iid)

-- | Not through the DBStore: on SQLite it serialises access behind one connection, so a
-- read taken during a transaction would wait for it rather than see what it has not
-- committed, which is exactly what testPublishIsAfterCommit needs to see.
independentStatus :: DBStore -> InvoiceId -> IO (Maybe Text)
#if defined(dbPostgres)
independentStatus st (InvoiceId iid) = withConnection st $ \db -> do
  rows <- DB.query db "SELECT status FROM sx_badge_service_invoices WHERE invoice_id = ?" (Only iid)
  pure $ case rows of
    (Only s : _) -> Just s
    [] -> Nothing
#else
independentStatus DBStore {dbFilePath} (InvoiceId iid) = E.bracket (SQL.open dbFilePath) SQL.close reader `E.catch` locked
  where
    reader c = do
      rows <- SQL.query c "SELECT status FROM sx_badge_service_invoices WHERE invoice_id = ?" (Only iid)
      pure $ case rows of
        (Only s : _) -> Just s
        [] -> Nothing
    locked :: SQL.SQLError -> IO (Maybe Text)
    locked _ = pure Nothing
#endif

-- | Expires the invoice from inside the settling transaction, which is the moment the
-- status guard is about. A trigger rather than a thread, because the SQLite store puts
-- every access behind one connection and so cannot have a concurrent writer.
expireOnNextPayment :: DBStore -> IO ()
#if defined(dbPostgres)
expireOnNextPayment st = withConnection st $ \db -> do
  DB.execute_
    db
    "CREATE FUNCTION sx_badge_service_test_expire() RETURNS trigger AS $$ BEGIN UPDATE sx_badge_service_invoices SET status = 'expired' WHERE invoice_id = NEW.invoice_id; RETURN NEW; END; $$ LANGUAGE plpgsql"
  DB.execute_
    db
    "CREATE TRIGGER sx_badge_service_test_expire_trg AFTER INSERT ON sx_badge_service_payments FOR EACH ROW EXECUTE FUNCTION sx_badge_service_test_expire()"
#else
expireOnNextPayment st = withConnection st $ \db ->
  DB.execute_
    db
    "CREATE TRIGGER sx_badge_service_test_expire AFTER INSERT ON sx_badge_service_payments BEGIN UPDATE sx_badge_service_invoices SET status = 'expired' WHERE invoice_id = NEW.invoice_id; END"
#endif

testSettlesAnOpenInvoice :: IO ()
testSettlesAnOpenInvoice = bounded "settles an open invoice" $ withServiceStore $ \st -> do
  iid <- seedOpen st
  settle st iid (SigSettled (rcv 500 (Just "0.00050000")) settleAt) detectedAt `shouldReturn` Right ISPaid
  invoiceStatus st iid `shouldReturn` ISPaid
  p <- paymentRow st iid
  ipAmount p `shouldBe` Just (CurrencyAmount 500)
  ipCryptoAmount p `shouldBe` Just "0.00050000"
  ipStatus p `shouldBe` "settled"
  ipUpdatedAt p `shouldBe` settleAt
  ipUpdatedAt p `shouldNotBe` detectedAt
  paymentIdentity st iid `shouldReturn` [("inv1", "crypto", Just (niProviderRef sampleInvoice), Just "usd")]
  code <- settledCode st iid
  bcPaymentStatus code `shouldBe` CPSPaid
  bcExpiresAt code `shouldBe` Just (addUTCTime codeLifetime settleAt)
  bcExpiresAt code `shouldNotBe` Just (addUTCTime codeLifetime detectedAt)
  bcRevokedAt code `shouldBe` Nothing

testLateSettlementIsLegal :: IO ()
testLateSettlementIsLegal = bounded "late settlement" $ withServiceStore $ \st -> do
  iid <- seedOpen st
  expireRow st iid
  invoiceStatus st iid `shouldReturn` ISExpired
  settle st iid (SigSettled (rcv 500 Nothing) settleAt) detectedAt `shouldReturn` Right ISPaid
  invoiceStatus st iid `shouldReturn` ISPaid
  ipStatus <$> paymentRow st iid `shouldReturn` "settled"
  bcPaymentStatus <$> settledCode st iid `shouldReturn` CPSPaid

testFundedRecordsWithoutMoving :: IO ()
testFundedRecordsWithoutMoving = bounded "funded records only" $ withServiceStore $ \st -> do
  iid <- seedOpen st
  settle st iid (SigFunded (rcv 200 (Just "0.00020000")) PaidInPart) settleAt `shouldReturn` Right ISOpen
  invoiceStatus st iid `shouldReturn` ISOpen
  p <- paymentRow st iid
  ipAmount p `shouldBe` Just (CurrencyAmount 200)
  ipStatus p `shouldBe` "pending"
  code <- settledCode st iid
  bcPaymentStatus code `shouldBe` CPSUnpaid
  bcExpiresAt code `shouldBe` Nothing
  expireRow st iid
  settle st iid (SigFunded (rcv 300 (Just "0.00030000")) PaidInPart) replayAt `shouldReturn` Right ISExpired
  invoiceStatus st iid `shouldReturn` ISExpired
  ipAmount <$> paymentRow st iid `shouldReturn` Just (CurrencyAmount 300)
  bcPaymentStatus <$> settledCode st iid `shouldReturn` CPSUnpaid

testClosedExpiresAnOpenInvoice :: IO ()
testClosedExpiresAnOpenInvoice = bounded "closed expires" $ withServiceStore $ \st -> do
  iid <- seedOpen st
  settle st iid (SigClosed (rcv 200 (Just "0.00020000"))) settleAt `shouldReturn` Right ISExpired
  invoiceStatus st iid `shouldReturn` ISExpired
  p <- paymentRow st iid
  ipAmount p `shouldBe` Just (CurrencyAmount 200)
  ipStatus p `shouldBe` "pending"
  bcPaymentStatus <$> settledCode st iid `shouldReturn` CPSUnpaid

testClosedWithNothingWritesNoPayment :: IO ()
testClosedWithNothingWritesNoPayment = bounded "closed on nothing" $ withServiceStore $ \st -> do
  iid <- seedOpen st
  settle st iid (SigClosed (rcv 0 Nothing)) settleAt `shouldReturn` Right ISExpired
  invoiceStatus st iid `shouldReturn` ISExpired
  Just row <- getInvoice st iid
  irPayment row `shouldBe` Nothing
  bcPaymentStatus <$> settledCode st iid `shouldReturn` CPSUnpaid

testClosedReplayIsIdempotent :: IO ()
testClosedReplayIsIdempotent = bounded "closed replay" $ withServiceStore $ \st -> do
  iid <- seedOpen st
  settle st iid (SigClosed (rcv 200 (Just "0.00020000"))) settleAt `shouldReturn` Right ISExpired
  settle st iid (SigClosed (rcv 200 (Just "0.00020000"))) replayAt `shouldReturn` Right ISExpired
  invoiceStatus st iid `shouldReturn` ISExpired
  p <- paymentRow st iid
  ipAmount p `shouldBe` Just (CurrencyAmount 200)
  ipCryptoAmount p `shouldBe` Just "0.00020000"
  ipStatus p `shouldBe` "pending"
  length <$> paymentIdentity st iid `shouldReturn` 1

testPaidRefusesEverySignal :: IO ()
testPaidRefusesEverySignal = bounded "paid refuses" $ withServiceStore $ \st -> do
  iid <- seedOpen st
  settle st iid (SigSettled (rcv 500 (Just "0.00050000")) settleAt) detectedAt `shouldReturn` Right ISPaid
  settled <- paymentRow st iid
  deadline <- bcExpiresAt <$> settledCode st iid
  mapM_
    ( \signal -> do
        settle st iid signal replayAt `shouldReturn` Right ISPaid
        (signal, ) <$> paymentRow st iid `shouldReturn` (signal, settled)
    )
    [ SigSettled (rcv 900 (Just "0.00090000")) replayAt,
      SigFunded (rcv 900 (Just "0.00090000")) PaidInPart,
      SigClosed (rcv 900 (Just "0.00090000"))
    ]
  invoiceStatus st iid `shouldReturn` ISPaid
  bcExpiresAt <$> settledCode st iid `shouldReturn` deadline

testAmountIsMonotonic :: IO ()
testAmountIsMonotonic = bounded "monotonic amount" $ withServiceStore $ \st -> do
  iid <- seedOpen st
  settle st iid (SigFunded (rcv 40000 (Just "0.734")) PaidInPart) settleAt `shouldReturn` Right ISOpen
  settle st iid (SigFunded (rcv 10000 (Just "0.180")) PaidInPart) replayAt `shouldReturn` Right ISOpen
  p <- paymentRow st iid
  ipAmount p `shouldBe` Just (CurrencyAmount 40000)
  ipCryptoAmount p `shouldBe` Just "0.734"
  settle st iid (SigFunded (rcv 40000 (Just "0.734")) PaidInPart) replayAt `shouldReturn` Right ISOpen
  settle st iid (SigFunded (rcv 40000 (Just "0.734")) PaidInPart) replayAt `shouldReturn` Right ISOpen
  ipAmount <$> paymentRow st iid `shouldReturn` Just (CurrencyAmount 40000)
  settle st iid (SigFunded (rcv 50000 (Just "0.900")) PaidInPart) replayAt `shouldReturn` Right ISOpen
  p' <- paymentRow st iid
  ipAmount p' `shouldBe` Just (CurrencyAmount 50000)
  ipCryptoAmount p' `shouldBe` Just "0.900"

testDeadlineIsFromTheFirstSettlement :: IO ()
testDeadlineIsFromTheFirstSettlement = bounded "deadline from the first settlement" $ withServiceStore $ \st -> do
  iid <- seedOpen st
  settle st iid (SigSettled (rcv 500 Nothing) settleAt) detectedAt `shouldReturn` Right ISPaid
  let firstDeadline = addUTCTime codeLifetime settleAt
  bcExpiresAt <$> settledCode st iid `shouldReturn` Just firstDeadline
  withTransaction st $ \db -> markCodePaid db (niCodeHash sampleInvoice) (addUTCTime codeLifetime replayAt)
  bcExpiresAt <$> settledCode st iid `shouldReturn` Just firstDeadline
  bcPaymentStatus <$> settledCode st iid `shouldReturn` CPSPaid

testStatusGuardRefusesAStaleObservation :: IO ()
testStatusGuardRefusesAStaleObservation = bounded "stale observation" $ withServiceStore $ \st -> do
  iid <- seedOpen st
  markPaid st iid
  stale <- withTransaction st $ \db -> updateInvoiceStatus db iid ISOpen ISExpired settleAt
  stale `shouldBe` False
  invoiceStatus st iid `shouldReturn` ISPaid
  let second' = sampleInvoice {niInvoiceId = InvoiceId "inv-2", niProviderRef = "p-2", niCodeHash = digestFixture 15}
  createInvoiceRows st second' `shouldReturn` Right ()
  moved <- withTransaction st $ \db -> updateInvoiceStatus db (niInvoiceId second') ISOpen ISExpired settleAt
  moved `shouldBe` True
  invoiceStatus st (niInvoiceId second') `shouldReturn` ISExpired

testUnknownInvoiceSettlesNothing :: IO ()
testUnknownInvoiceSettlesNothing = bounded "unknown invoice" $ withServiceStore $ \st -> do
  _ <- seedOpen st
  r <- settle st (InvoiceId "no-such-invoice") (SigSettled (rcv 500 Nothing) settleAt) settleAt
  r `shouldSatisfy` isLeft
  paymentIdentity st (InvoiceId "no-such-invoice") `shouldReturn` []

testPublishIsAfterCommit :: IO ()
testPublishIsAfterCommit = bounded "publish after commit" $ withServiceStore $ \st -> do
  iid <- seedOpen st
  waiters <- newWaiters
  seen <- newEmptyMVar
  _ <- forkIO $ do
    woken <- awaitStatus waiters iid ((\s -> (s, ("", False))) <$> invoiceStatus st iid) (ISOpen, ("", False)) waitCeiling
    visible <- independentStatus st iid
    putMVar seen (woken, visible)
  threadDelay 100000
  settleOrder st waiters iid (SigSettled (rcv 500 Nothing) settleAt) settleAt `shouldReturn` Right ISPaid
  takeMVar seen `shouldReturn` (ISPaid, Just "paid")

waitCeiling :: Int
waitCeiling = 10 * 1000000

testSettlementWakesAHeldRequest :: IO ()
testSettlementWakesAHeldRequest = bounded "settlement wakes a hold" $ withWebApp $ \env client -> do
  iid <- seedOpenInvoice env
  let settled =
        settleOrder (weStore env) (weWaiters env) iid (SigSettled (rcv 500 (Just "0.00050000")) settleAt) settleAt
          `shouldReturn` Right ISPaid
  o <- wokenBy client iid settled "paid"
  fieldOf o "amountPaid" `shouldBe` Just (J.Number 500)

-- | The upper half of the clamp. A provider sending `receivedDate` in milliseconds gives an
-- instant tens of thousands of years out, which the lower bound cannot catch: it would put the
-- code's deadline past any clock that will read it, so the code never expires.
testFutureSettledInstantIsRefused :: IO ()
testFutureSettledInstantIsRefused = bounded "future settled instant" $ withWebApp $ \env client -> do
  iid <- seedOpenInvoice env
  let inMilliseconds = posixSecondsToUTCTime (1000 * utcTimeToPOSIXSeconds detectedAt)
  settle (weStore env) iid (SigSettled (rcv 500 (Just "0.00050000")) inMilliseconds) detectedAt
    `shouldReturn` Right ISPaid
  code <- settledCode (weStore env) iid
  bcPaymentStatus code `shouldBe` CPSPaid
  -- a year from when we learned of it, not from a clock we cannot believe
  bcExpiresAt code `shouldBe` Just (addUTCTime codeLifetime detectedAt)
  o <- jsonObject =<< webGet client (invoicePath iid)
  fieldOf o "settledAt" `shouldBe` Just (J.toJSON detectedAt)

-- | The lower half. `toMinorUnits` clamps what the provider says about money, and this clamps
-- what it says about time: the instant lands in two rows and in the code's redemption deadline,
-- so a zero would hand the buyer a code that expired decades ago.
testAbsurdSettledInstantIsRefused :: IO ()
testAbsurdSettledInstantIsRefused = bounded "absurd settled instant" $ withWebApp $ \env client -> do
  iid <- seedOpenInvoice env
  let epochZero = posixSecondsToUTCTime 0
  settle (weStore env) iid (SigSettled (rcv 500 (Just "0.00050000")) epochZero) detectedAt
    `shouldReturn` Right ISPaid
  code <- settledCode (weStore env) iid
  bcPaymentStatus code `shouldBe` CPSPaid
  -- the deadline is a year from when we learned of it, not from an instant the provider invented
  bcExpiresAt code `shouldSatisfy` maybe False (> detectedAt)
  -- and the row is still readable, which a timestamp outside the format's range would not be
  o <- jsonObject =<< webGet client (invoicePath iid)
  fieldOf o "status" `shouldBe` Just (J.String "paid")
  fieldOf o "settledAt" `shouldBe` Just (J.toJSON detectedAt)

testSettledAtIsThePaymentRow :: IO ()
testSettledAtIsThePaymentRow = bounded "settledAt" $ withWebApp $ \env client -> do
  iid <- seedOpenInvoice env
  settle (weStore env) iid (SigSettled (rcv 500 Nothing) settleAt) detectedAt `shouldReturn` Right ISPaid
  o <- jsonObject =<< webGet client (invoicePath iid)
  fieldOf o "status" `shouldBe` Just (J.String "paid")
  fieldOf o "settledAt" `shouldBe` Just (J.toJSON settleAt)
  fieldOf o "settledAt" `shouldNotBe` Just (J.toJSON someCreated)
  fieldOf o "settledAt" `shouldNotBe` Just (J.toJSON detectedAt)
  touchInvoice (weStore env) iid replayAt
  o' <- jsonObject =<< webGet client (invoicePath iid)
  fieldOf o' "settledAt" `shouldBe` Just (J.toJSON settleAt)

testLosingTheStatusGuardStillWakesTheHold :: IO ()
testLosingTheStatusGuardStillWakesTheHold = bounded "losing the guard" $ withWebAppHolding raceHold $ \env client -> do
  iid <- seedOpenInvoice env
  expireOnNextPayment (weStore env)
  _ <- wokenBy client iid (settledIntoAnExpiredRow env iid) "expired"
  pure ()

-- | The settlement the hold is waiting for, landing on a row the guard has already expired.
settledIntoAnExpiredRow :: HasCallStack => WebEnv -> InvoiceId -> IO ()
settledIntoAnExpiredRow env iid = do
  settleOrder (weStore env) (weWaiters env) iid (SigSettled (rcv 500 (Just "0.00050000")) settleAt) settleAt
    `shouldReturn` Right ISExpired
  invoiceStatus (weStore env) iid `shouldReturn` ISExpired
  code <- settledCode (weStore env) iid
  bcPaymentStatus code `shouldBe` CPSUnpaid
  bcExpiresAt code `shouldBe` Nothing
  ipStatus <$> paymentRow (weStore env) iid `shouldReturn` "settled"

raceHold :: Int
raceHold = 3 * 1000000

testSettledPaymentIsNotDowngraded :: IO ()
testSettledPaymentIsNotDowngraded = bounded "settled is not downgraded" $ withServiceStore $ \st -> do
  iid <- seedOpen st
  row <- getInvoice st iid >>= maybe (failWith "no invoice row") pure
  withTransaction st $ \db -> upsertPayment db row PSSettled (CurrencyAmount 500) (Just "0.00050000") Nothing False settleAt
  withTransaction st $ \db -> upsertPayment db row PSPending (CurrencyAmount 200) (Just "0.00020000") Nothing False replayAt
  p <- paymentRow st iid
  ipStatus p `shouldBe` "settled"
  ipUpdatedAt p `shouldBe` settleAt
  ipAmount p `shouldBe` Just (CurrencyAmount 500)
  ipCryptoAmount p `shouldBe` Just "0.00050000"
  let second' = sampleInvoice {niInvoiceId = InvoiceId "inv-pending", niProviderRef = "p-pending", niCodeHash = digestFixture 16}
  createInvoiceRows st second' `shouldReturn` Right ()
  row' <- getInvoice st (niInvoiceId second') >>= maybe (failWith "no invoice row") pure
  withTransaction st $ \db -> upsertPayment db row' PSPending (CurrencyAmount 100) Nothing Nothing False settleAt
  withTransaction st $ \db -> upsertPayment db row' PSSettled (CurrencyAmount 500) (Just "0.00050000") Nothing False replayAt
  p' <- paymentRow st (niInvoiceId second')
  ipStatus p' `shouldBe` "settled"
  ipUpdatedAt p' `shouldBe` replayAt

testCryptoAmountFillsInFromNull :: IO ()
testCryptoAmountFillsInFromNull = bounded "crypto fills in" $ withServiceStore $ \st -> do
  iid <- seedOpen st
  settle st iid (SigFunded (rcv 500 Nothing) PaidInPart) settleAt `shouldReturn` Right ISOpen
  ipCryptoAmount <$> paymentRow st iid `shouldReturn` Nothing
  settle st iid (SigSettled (rcv 500 (Just "0.00050000")) settleAt) detectedAt `shouldReturn` Right ISPaid
  ipCryptoAmount <$> paymentRow st iid `shouldReturn` Just "0.00050000"
  ipAmount <$> paymentRow st iid `shouldReturn` Just (CurrencyAmount 500)

pollerFor :: WebEnv -> IO PollerEnv
pollerFor WebEnv {weStore, weConfig, weWaiters, weHints, weProviders} =
  newPollerEnv weStore weWaiters weHints weProviders (poll weConfig)

withStubPoller :: Int -> (IORef StubState -> PollerEnv -> WebEnv -> WebClient -> IO a) -> IO a
withStubPoller hold action =
  withServiceStore $ \st -> do
    createDirectoryIfMissing True "tests/tmp"
    withTempDirectory "tests/tmp" "badge-poller" $ \root -> do
      staticDir <- prepareStaticDir root
      ref <- newIORef (newStubState (Right sampleProviderInvoice))
      let cfg = (testServiceConfig staticDir True) {btcpay = Just testBTCPayConfig}
      withListener [stubProvider ref] True hold st cfg $ \env client -> do
        poller <- pollerFor env
        action ref poller env client

withFakePoller :: (FakeBTCPay -> PollerEnv -> WebEnv -> WebClient -> IO a) -> IO a
withFakePoller = withFakePollerHolding holdMicros

withFakePollerHolding :: Int -> (FakeBTCPay -> PollerEnv -> WebEnv -> WebClient -> IO a) -> IO a
withFakePollerHolding hold action =
  withFakeBTCPay $ \fake ->
    withServiceStore $ \st -> do
      createDirectoryIfMissing True "tests/tmp"
      withTempDirectory "tests/tmp" "badge-poller-fake" $ \root -> do
        staticDir <- prepareStaticDir root
        seedCheckoutCatalog st
        provider <- btcpayProvider (fbConfig fake)
        let cfg = (testServiceConfig staticDir True) {btcpay = Just (fbConfig fake)}
        withListener [provider] True hold st cfg $ \env client -> do
          poller <- pollerFor env
          action fake poller env client

withFakeProvider :: (FakeBTCPay -> Provider -> IO a) -> IO a
withFakeProvider action = withFakeBTCPay $ \fake -> btcpayProvider (fbConfig fake) >>= action fake

setSignals :: IORef StubState -> [(Text, PaymentSignal)] -> IO ()
setSignals ref signals = atomicModifyIORef' ref $ \s -> (s {ssInvoices = Map.fromList signals}, ())

clearCalls :: IORef StubState -> IO ()
clearCalls ref = atomicModifyIORef' ref $ \s -> (s {ssCalls = []}, ())

setSkipped :: IORef StubState -> [(Maybe Text, Text)] -> IO ()
setSkipped ref reasons = atomicModifyIORef' ref $ \s -> (s {ssSkipped = reasons}, ())

failList :: IORef StubState -> Maybe ProviderError -> IO ()
failList ref e = atomicModifyIORef' ref $ \s -> (s {ssListError = e}, ())

-- | Reads are the pass's own lane now, so a provider that is down fails these too.
failRead :: IORef StubState -> Maybe ProviderError -> IO ()
failRead ref e = atomicModifyIORef' ref $ \s -> (s {ssReadError = e}, ())

seedOpenRef :: HasCallStack => DBStore -> Int -> Text -> UTCTime -> IO InvoiceId
seedOpenRef st i providerRef expiresAt = do
  -- created just now, as an invoice a buyer is paying is: the poller reads the ones inside its
  -- settle window and leaves anything older to be swept
  createdAt <- truncateToSecond <$> getCurrentTime
  let n = T.pack (show i)
      iid = InvoiceId ("inv-poll-" <> n)
      ni =
        sampleInvoice
          { niInvoiceId = iid,
            niProviderRef = providerRef,
            niCodeHash = digestFixture (fromIntegral i + 20),
            niExpiresAt = expiresAt,
            niCreatedAt = createdAt
          }
  createInvoiceRows st ni `shouldReturn` Right ()
  pure iid

seedOtherProvider :: HasCallStack => DBStore -> Int -> Text -> IO InvoiceId
seedOtherProvider st i providerRef = do
  -- created now, like `seedOpenRef`: the fixture's own date is fixed and has aged past the
  -- settle window, which would put these rows outside the lane that reads our own rows
  createdAt <- truncateToSecond <$> getCurrentTime
  let n = T.pack (show i)
      iid = InvoiceId ("inv-poll-" <> n)
      ni =
        sampleInvoice
          { niInvoiceId = iid,
            niProviderRef = providerRef,
            niCodeHash = digestFixture (fromIntegral i + 40),
            niProvider = PPStripe,
            niCreatedAt = createdAt
          }
  createInvoiceRows st ni `shouldReturn` Right ()
  pure iid

settledSignal :: PaymentSignal
settledSignal = SigSettled (rcv 500 (Just "0.00050000")) settleAt

testSettlesWithNoWebhookAtAll :: IO ()
testSettlesWithNoWebhookAtAll = bounded "no webhook at all" $ withFakePoller $ \fake poller env client -> do
  r <- postCreateAs client 1 (createBody supporterPriceId Nothing "xmr" (codeHashText sampleCode))
  statusOf r `shouldBe` 200
  o <- jsonObject r
  iid <- InvoiceId <$> stringField o "invoiceId"
  invoiceStatus (weStore env) iid `shouldReturn` ISOpen
  bcPaymentStatus <$> settledCode (weStore env) iid `shouldReturn` CPSUnpaid
  Just row <- getInvoice (weStore env) iid
  setInvoiceState fake (irProviderRef row) ["status" .= ("Settled" :: Text), "paymentMethodPaid" .= ("0.32095000" :: Text)]
  runOnePass poller
  invoiceStatus (weStore env) iid `shouldReturn` ISPaid
  code <- settledCode (weStore env) iid
  bcPaymentStatus code `shouldBe` CPSPaid
  bcExpiresAt code `shouldSatisfy` isJust
  p <- paymentRow (weStore env) iid
  ipStatus p `shouldBe` "settled"
  ipAmount p `shouldBe` Just (CurrencyAmount 5400)
  ipCryptoAmount p `shouldBe` Just "0.32095000"

testPassReadsWhatItAwaits :: IO ()
testPassReadsWhatItAwaits = bounded "reads what it awaits" $ withStubPoller raceHold $ \ref poller env _ -> do
  seedBadgePrice (weStore env) "price1"
  a <- seedOpenRef (weStore env) 1 "p-1" someExpiry
  b <- seedOpenRef (weStore env) 2 "p-2" someExpiry
  c <- seedOpenRef (weStore env) 3 "p-3" someExpiry
  setSignals ref [("p-1", settledSignal), ("p-2", settledSignal), ("p-3", settledSignal)]
  runOnePass poller
  -- three reads, one per invoice we are waiting on, and the stray list, which is due on a first
  -- pass and then only once a minute
  stubCalls ref `shouldReturn` [StubRead "p-1", StubRead "p-2", StubRead "p-3", StubListOpen]
  mapM (invoiceStatus (weStore env)) [a, b, c] `shouldReturn` [ISPaid, ISPaid, ISPaid]

-- The whole point of reading our own rows: a service with nothing outstanding costs the provider
-- nothing at all, where listing a window asked it for every invoice in three days, every pass.
testIdlePassAsksNothing :: IO ()
testIdlePassAsksNothing = bounded "idle pass" $ withStubPoller raceHold $ \ref poller env _ -> do
  seedBadgePrice (weStore env) "price1"
  iid <- seedOpenRef (weStore env) 1 "p-1" someExpiry
  setSignals ref [("p-1", settledSignal)]
  runOnePass poller
  invoiceStatus (weStore env) iid `shouldReturn` ISPaid
  -- the stray list ran on that first pass; from here the row is paid and nothing is awaited
  clearCalls ref
  runOnePass poller
  stubCalls ref `shouldReturn` []

-- Reading one by one is cheaper only while few are open. Past the threshold one list is fewer
-- requests and fewer bytes, so the pass switches back to it.
testManyOpenInvoicesList :: IO ()
testManyOpenInvoicesList = bounded "many open" $ withStubPoller raceHold $ \ref poller env _ -> do
  seedBadgePrice (weStore env) "price1"
  mapM_ (\i -> seedOpenRef (weStore env) i ("p-many-" <> tshow i) someExpiry) [1 .. readsPerPass + 1]
  runOnePass poller
  -- exactly one: the list is the pass's own accounting here, so the stray lane must not send a
  -- second identical request beside it
  stubCalls ref `shouldReturn` [StubListOpen]

-- With no provider configured nothing can be read or listed, so nothing is accounted for. The
-- count of rows decides which lane runs, and it must not decide whether the sweep is safe.
testNoProviderAccountsForNothing :: HasCallStack => IO ()
testNoProviderAccountsForNothing = bounded "no provider" $ withStubPoller raceHold $ \_ poller env _ -> do
  now <- getCurrentTime
  seedBadgePrice (weStore env) "price1"
  let overdue i = seedOpenRef (weStore env) i ("p-none-" <> tshow i) (addUTCTime (negate (expiryGrace + 60)) now)
  few <- mapM overdue [1 .. 3]
  let blind = poller {peProviders = []}
  runOnePass blind
  mapM (invoiceStatus (weStore env)) few `shouldReturn` replicate 3 ISOpen
  many <- mapM overdue [4 .. readsPerPass + 4]
  runOnePass blind
  mapM (invoiceStatus (weStore env)) (few <> many) `shouldReturn` replicate (length few + length many) ISOpen

-- A pass past the threshold lists because that is how it reads, and the stray lane must not send
-- a second one beside it. Whether the forced list also restarts the stray cadence is not pinned:
-- both answers list on a first pass, and telling them apart needs a clock this poller does not take.
testBulkListIsTheOnlyList :: HasCallStack => IO ()
testBulkListIsTheOnlyList = bounded "bulk lists once" $ withStubPoller raceHold $ \ref poller env _ -> do
  seedBadgePrice (weStore env) "price1"
  mapM_ (\i -> seedOpenRef (weStore env) i ("p-cadence-" <> tshow i) someExpiry) [1 .. readsPerPass + 1]
  -- all but one settle on that first pass, so the next is under the threshold and reads
  setSignals ref [("p-cadence-" <> tshow i, settledSignal) | i <- [1 .. readsPerPass]]
  runOnePass poller
  stubCalls ref `shouldReturn` [StubListOpen]
  clearCalls ref
  runOnePass poller
  calls <- stubCalls ref
  filter (== StubListOpen) calls `shouldBe` []

-- The list only answers for the providers it was asked about, so it cannot say that a row naming
-- another one went unread. With enough rows to put the pass on the list, that row is the whole
-- reason the sweep must wait.
testMixedProvidersHoldTheSweep :: HasCallStack => IO ()
testMixedProvidersHoldTheSweep = bounded "mixed providers" $ withStubPoller raceHold $ \_ poller env _ -> do
  now <- getCurrentTime
  seedBadgePrice (weStore env) "price1"
  mapM_ (\i -> seedOpenRef (weStore env) i ("p-mixed-" <> tshow i) someExpiry) [1 .. readsPerPass + 1]
  createdAt <- truncateToSecond <$> getCurrentTime
  let iid = InvoiceId "inv-poll-stripe"
      stripeRow =
        sampleInvoice
          { niInvoiceId = iid,
            niProviderRef = "p-stripe-mixed",
            niCodeHash = digestFixture 99,
            niProvider = PPStripe,
            niExpiresAt = addUTCTime (negate (expiryGrace + 60)) now,
            niCreatedAt = createdAt
          }
  createInvoiceRows (weStore env) stripeRow `shouldReturn` Right ()
  runOnePass poller
  invoiceStatus (weStore env) iid `shouldReturn` ISOpen

testEverySignalSettles :: IO ()
testEverySignalSettles = bounded "every signal settles" $ withStubPoller raceHold $ \ref poller env _ -> do
  seedBadgePrice (weStore env) "price1"
  paid <- seedOpenRef (weStore env) 1 "p-settled" someExpiry
  funded <- seedOpenRef (weStore env) 2 "p-funded" someExpiry
  closed <- seedOpenRef (weStore env) 3 "p-closed" someExpiry
  setSignals
    ref
    [ ("p-settled", settledSignal),
      ("p-funded", SigFunded (rcv 200 (Just "0.00020000")) PaidInPart),
      ("p-closed", SigClosed (rcv 100 (Just "0.00010000")))
    ]
  runOnePass poller
  invoiceStatus (weStore env) paid `shouldReturn` ISPaid
  invoiceStatus (weStore env) funded `shouldReturn` ISOpen
  ipAmount <$> paymentRow (weStore env) funded `shouldReturn` Just (CurrencyAmount 200)
  invoiceStatus (weStore env) closed `shouldReturn` ISExpired
  ipCryptoAmount <$> paymentRow (weStore env) closed `shouldReturn` Just "0.00010000"
  bcPaymentStatus <$> settledCode (weStore env) paid `shouldReturn` CPSPaid
  bcPaymentStatus <$> settledCode (weStore env) funded `shouldReturn` CPSUnpaid
  bcPaymentStatus <$> settledCode (weStore env) closed `shouldReturn` CPSUnpaid

testForeignRefIsPassedOver :: IO ()
testForeignRefIsPassedOver = bounded "foreign ref" $ withStubPoller raceHold $ \ref poller env _ -> do
  seedBadgePrice (weStore env) "price1"
  ours <- seedOpenRef (weStore env) 1 "p-1" someExpiry
  otherLane <- seedOtherProvider (weStore env) 2 "p-stripe"
  setSignals ref [("p-1", settledSignal), ("someone-elses-invoice", settledSignal), ("p-stripe", settledSignal)]
  runOnePass poller
  invoiceStatus (weStore env) ours `shouldReturn` ISPaid
  invoiceStatus (weStore env) otherLane `shouldReturn` ISOpen
  invoiceCount (weStore env) `shouldReturn` 2

testProviderFailureLosesNothing :: IO ()
testProviderFailureLosesNothing = bounded "provider outage" $ withStubPoller raceHold $ \ref poller env _ -> do
  seedBadgePrice (weStore env) "price1"
  iid <- seedOpenRef (weStore env) 1 "p-1" someExpiry
  setSignals ref [("p-1", settledSignal)]
  -- a provider that is down answers neither lane
  failList ref (Just (ProviderError "connection refused"))
  failRead ref (Just (ProviderError "connection refused"))
  runOnePass poller
  invoiceStatus (weStore env) iid `shouldReturn` ISOpen
  Just row <- getInvoice (weStore env) iid
  irPayment row `shouldBe` Nothing
  bcPaymentStatus <$> settledCode (weStore env) iid `shouldReturn` CPSUnpaid
  failList ref Nothing
  failRead ref Nothing
  runOnePass poller
  invoiceStatus (weStore env) iid `shouldReturn` ISPaid
  stubCalls ref `shouldReturn` [StubRead "p-1", StubListOpen, StubRead "p-1"]

testCadenceFollowsTheWaiters :: IO ()
testCadenceFollowsTheWaiters = bounded "cadence" $ withStubPoller raceHold $ \_ poller env client -> do
  iid <- seedOpenInvoice env
  let PollConfig {pWaitingSeconds, pIdleSeconds} = poll (weConfig env)
  pWaitingSeconds `shouldNotBe` pIdleSeconds
  waitingCount (weWaiters env) `shouldReturn` 0
  passDelayNow poller `shouldReturn` (pIdleSeconds * 1000000)
  held <- async $ webGet client (invoicePath iid <> "?wait=open")
  threadDelay 100000
  passDelayNow poller `shouldReturn` (pWaitingSeconds * 1000000)
  waitingCount (weWaiters env) `shouldReturn` 1
  markPaidAndPublish env iid
  _ <- wait held
  passDelayNow poller `shouldReturn` (pIdleSeconds * 1000000)

passDelayNow :: PollerEnv -> IO Int
passDelayNow PollerEnv {peWaiters, pePoll} = passDelay pePoll <$> waitingCount peWaiters

testHintsDoNotPostponeThePass :: IO ()
testHintsDoNotPostponeThePass = bounded "hints do not postpone the pass" $ withStubPoller raceHold $ \ref _ env _ -> do
  seedBadgePrice (weStore env) "price1"
  mapM_ (\i -> seedOpenRef (weStore env) i (burstRef i) someExpiry) burstRefs
  built <- newPollerEnv (weStore env) (weWaiters env) (weHints env) (weProviders env) PollConfig {pWaitingSeconds = 1, pIdleSeconds = 1}
  -- the list is this test's marker for a pass having run, so it runs on every one
  let poller = built {peStrayEvery = 0}
  -- Settle them first, so the pass itself is waiting on nothing: every read after this belongs
  -- to a hint, which is what makes the count below say something.
  setSignals ref [(burstRef i, settledSignal) | i <- burstRefs]
  runOnePass poller
  clearCalls ref
  setReadDelay ref slowHintRead
  mapM_ (\i -> queueReadHint (weHints env) (burstRef i) `shouldReturn` True) burstRefs
  -- Two passes while ten slow hint reads are outstanding: a pass postponed until the backlog
  -- drained would have served all ten before the second list appeared.
  Async.withAsync (runPoller poller) $ \_ -> do
    calls <- awaitCalls ref (\cs -> length (filter (== StubListOpen) cs) >= 2)
    length (filter isRead calls) `shouldSatisfy` (< length burstRefs)
  where
    isRead = \case
      StubRead _ -> True
      _ -> False

burstRefs :: [Int]
burstRefs = [1 .. 10]

burstRef :: Int -> Text
burstRef i = "p-burst-" <> T.pack (show i)

slowHintRead :: Int
slowHintRead = 300000

awaitCalls :: IORef StubState -> ([StubCall] -> Bool) -> IO [StubCall]
awaitCalls ref p = do
  calls <- stubCalls ref
  if p calls then pure calls else threadDelay 10000 >> awaitCalls ref p

testWaiterCutsTheIdleSleepShort :: IO ()
testWaiterCutsTheIdleSleepShort = bounded "waiter cuts the sleep" $ withStubPoller raceHold $ \_ _ env client -> do
  iid <- seedOpenInvoice env
  poller <- newPollerEnv (weStore env) (weWaiters env) (weHints env) (weProviders env) PollConfig {pWaitingSeconds = 1, pIdleSeconds = 10}
  started <- getCurrentTime
  slept <- async (passDue poller >>= atomically)
  threadDelay 200000
  Async.poll slept >>= (`shouldSatisfy` isNothing)
  held <- async $ webGet client (invoicePath iid <> "?wait=open")
  wait slept
  elapsed <- (`diffUTCTime` started) <$> getCurrentTime
  elapsed `shouldSatisfy` (< 5)
  elapsed `shouldSatisfy` (>= 0.9)
  markPaidAndPublish env iid
  _ <- wait held
  pure ()

testCadenceHasAFloor :: IO ()
testCadenceHasAFloor = do
  let zeroed = PollConfig {pWaitingSeconds = 0, pIdleSeconds = 0}
      negated = PollConfig {pWaitingSeconds = -5, pIdleSeconds = -60}
  map (passDelay zeroed) [1, 0] `shouldBe` [1000000, 1000000]
  map (passDelay negated) [1, 0] `shouldBe` [1000000, 1000000]
  map (passDelay PollConfig {pWaitingSeconds = 3, pIdleSeconds = 60}) [1, 0] `shouldBe` [3000000, 60000000]

testPollerWakesAHeldRequest :: IO ()
testPollerWakesAHeldRequest = bounded "poller wakes a hold" $ withStubPoller raceHold $ \ref poller env client -> do
  iid <- seedOpenInvoice env
  setSignals ref [(niProviderRef sampleInvoice, settledSignal)]
  _ <- wokenBy client iid (runOnePass poller) "paid"
  pure ()

testSweepExpiresPastTheGrace :: IO ()
testSweepExpiresPastTheGrace = bounded "expiry sweep" $ withStubPoller raceHold $ \ref poller env _ -> do
  now <- getCurrentTime
  seedBadgePrice (weStore env) "price1"
  overdue <- seedOpenRef (weStore env) 1 "p-overdue" (addUTCTime (negate (expiryGrace + 60)) now)
  inGrace <- seedOpenRef (weStore env) 2 "p-ingrace" (addUTCTime (negate (expiryGrace - 60)) now)
  ahead <- seedOpenRef (weStore env) 3 "p-ahead" (addUTCTime 3600 now)
  runOnePass poller
  invoiceStatus (weStore env) overdue `shouldReturn` ISExpired
  invoiceStatus (weStore env) inGrace `shouldReturn` ISOpen
  invoiceStatus (weStore env) ahead `shouldReturn` ISOpen
  -- the sweep asks the provider nothing: every call here is the read pass accounting for the
  -- three rows first, and the stray list, which is due on a first pass
  stubCalls ref `shouldReturn` [StubRead "p-overdue", StubRead "p-ingrace", StubRead "p-ahead", StubListOpen]

testSweepWritesStatusAlone :: IO ()
testSweepWritesStatusAlone = bounded "sweep writes status alone" $ withStubPoller raceHold $ \ref poller env _ -> do
  now <- getCurrentTime
  seedBadgePrice (weStore env) "price1"
  iid <- seedOpenRef (weStore env) 1 "p-1" (addUTCTime (negate (expiryGrace + 60)) now)
  runOnePass poller
  invoiceStatus (weStore env) iid `shouldReturn` ISExpired
  Just row <- getInvoice (weStore env) iid
  irPayment row `shouldBe` Nothing
  setSignals ref [("p-1", SigClosed (rcv 200 (Just "0.00020000")))]
  runOnePass poller
  invoiceStatus (weStore env) iid `shouldReturn` ISExpired
  p <- paymentRow (weStore env) iid
  ipAmount p `shouldBe` Just (CurrencyAmount 200)
  ipCryptoAmount p `shouldBe` Just "0.00020000"

testSweepWakesAHeldRequest :: IO ()
testSweepWakesAHeldRequest = bounded "sweep wakes a hold" $ withStubPoller raceHold $ \_ poller env client -> do
  now <- getCurrentTime
  seedBadgePrice (weStore env) "price1"
  iid <- seedOpenRef (weStore env) 1 "p-overdue" (addUTCTime (negate (expiryGrace + 60)) now)
  _ <- wokenBy client iid (runOnePass poller) "expired"
  pure ()

testSkipWarningsAreRateLimited :: IO ()
testSkipWarningsAreRateLimited = bounded "skip warnings" $ withStubPoller raceHold $ \ref poller env _ -> do
  let reason = "btcpay invoice SOMEONEELSESINVOICE: unknown status Frobnicated"
      onwards t = addUTCTime t settleAt
  dueToWarn poller settleAt reason `shouldReturn` True
  dueToWarn poller (onwards 3) reason `shouldReturn` False
  dueToWarn poller (onwards (skipWarnInterval - 1)) reason `shouldReturn` False
  dueToWarn poller (onwards skipWarnInterval) reason `shouldReturn` True
  dueToWarn poller (onwards skipWarnInterval) "btcpay invoice OTHER: unknown status Frobnicated" `shouldReturn` True
  seedBadgePrice (weStore env) "price1"
  iid <- seedOpenRef (weStore env) 1 "p-1" someExpiry
  setSkipped ref [(Just "SOMEONEELSESINVOICE", reason)]
  setSignals ref [("p-1", settledSignal)]
  runOnePass poller
  invoiceStatus (weStore env) iid `shouldReturn` ISPaid

testSkipNamingOurInvoiceIsRaised :: IO ()
testSkipNamingOurInvoiceIsRaised = bounded "skip ownership" $ withStubPoller raceHold $ \ref poller env _ -> do
  seedBadgePrice (weStore env) "price1"
  _ <- seedOpenRef (weStore env) 1 "p-ours" someExpiry
  skipOwner poller PPCrypto (Just "p-ours") `shouldReturn` SkipOurs
  skipOwner poller PPCrypto (Just "SOMEONEELSESINVOICE") `shouldReturn` SkipStranger
  -- a skip that names nobody could be any of ours, which is not the same as somebody else's
  skipOwner poller PPCrypto Nothing `shouldReturn` SkipUnaccounted
  _ <- seedOtherProvider (weStore env) 3 "p-stripe-skip"
  skipOwner poller PPCrypto (Just "p-stripe-skip") `shouldReturn` SkipStranger
  iid <- seedOpenRef (weStore env) 2 "p-2" someExpiry
  setSkipped
    ref
    [ (Just "p-ours", "btcpay invoice p-ours: unknown status Frobnicated"),
      (Just "SOMEONEELSESINVOICE", "btcpay invoice SOMEONEELSESINVOICE: unknown status Frobnicated")
    ]
  setSignals ref [("p-2", settledSignal)]
  runOnePass poller
  invoiceStatus (weStore env) iid `shouldReturn` ISPaid

-- | A provider whose store sells through a method this build does not know skips one invoice
-- per pass, each with its own reason, and every one of them is a map key.
testSkipReasonsStayBounded :: IO ()
testSkipReasonsStayBounded = bounded "skip reasons bounded" $ withStubPoller raceHold $ \ref poller _ _ -> do
  let reasons n = [(Just ("p-" <> tshow i), "btcpay invoice p-" <> tshow i <> ": unknown method") | i <- [1 .. n :: Int]]
  setSkipped ref (reasons (maxSkipReasons + 500))
  runOnePass poller
  held <- Map.size <$> readTVarIO (peSkipped poller)
  held `shouldSatisfy` (<= maxSkipReasons)

-- | The failure text carries the whole request, whose window moves with the clock, so keying
-- the limiter on it would print a request dump every three seconds for as long as it lasted.
testOutageWarnsOnceNotEveryPass :: IO ()
testOutageWarnsOnceNotEveryPass = bounded "outage warnings" $ withStubPoller raceHold $ \ref poller _ _ -> do
  let warnKeys PollerEnv {peSkipped} = Map.size <$> readTVarIO peSkipped
  failList ref (Just (ProviderError "list invoices failed: ... startDate=1000000 ..."))
  runOnePass poller
  warnKeys poller `shouldReturn` 1
  failList ref (Just (ProviderError "list invoices failed: ... startDate=1000003 ..."))
  runOnePass poller
  warnKeys poller `shouldReturn` 1

-- | Both loops of a pass read rows, and a row that throws once throws every pass. Neither may
-- take the rest of the pass with it, and neither may let the sweep run over an invoice this pass
-- could not account for.
testOneBadInvoiceDoesNotStopThePass :: IO ()
testOneBadInvoiceDoesNotStopThePass = bounded "one bad invoice" $ withStubPoller raceHold $ \ref poller env _ -> do
  now <- getCurrentTime
  seedBadgePrice (weStore env) "price1"
  InvoiceId badId <- seedOpenRef (weStore env) 1 "p-bad" (addUTCTime 3600 now)
  good <- seedOpenRef (weStore env) 2 "p-good" (addUTCTime 3600 now)
  overdue <- seedOpenRef (weStore env) 3 "p-overdue" (addUTCTime (negate (expiryGrace + 60)) now)
  -- a provider this build does not know: reading the row throws rather than answering
  withConnection (weStore env) $ \db ->
    DB.execute db "UPDATE sx_badge_service_invoices SET provider = ? WHERE invoice_id = ?" ("nonsense" :: Text, badId)

  -- the bad row named in the settle loop
  setSignals ref [("p-bad", settledSignal), ("p-good", settledSignal)]
  runOnePass poller
  -- the good one settles even though the loop met the bad one first; reading the bad row back
  -- would throw here for the same reason the pass could not settle it
  invoiceStatus (weStore env) good `shouldReturn` ISPaid
  -- and the pass could not account for the bad one, so the sweep did not run
  invoiceStatus (weStore env) overdue `shouldReturn` ISOpen

  -- the same row named in the skip loop, which reads rows too
  setSignals ref []
  setSkipped ref [(Just "p-bad", "some reason the provider gave")]
  runOnePass poller
  invoiceStatus (weStore env) overdue `shouldReturn` ISOpen

-- | The guard the sweep rests on, one condition at a time: each returns False from a different
-- place, and any one of them left out expires an invoice whose payment this pass could not see.
testSweepWaitsForAPassThatSawEverything :: IO ()
testSweepWaitsForAPassThatSawEverything = bounded "sweep guards" $ withStubPoller raceHold $ \ref poller env _ -> do
  now <- getCurrentTime
  seedBadgePrice (weStore env) "price1"
  overdue <- seedOpenRef (weStore env) 1 "p-overdue" (addUTCTime (-3600) now)
  let stillOpen :: Text -> IO ()
      stillOpen why = do
        runOnePass poller
        (why, ) <$> invoiceStatus (weStore env) overdue `shouldReturn` (why, ISOpen)
  failRead ref (Just (ProviderError "connection refused"))
  stillOpen "the reads never answered"
  failRead ref Nothing
  -- past the threshold the pass lists instead of reading, and the list's own accounting is what
  -- holds the sweep: an invoice it left unread, or a page it never reached
  mapM_ (\i -> seedOpenRef (weStore env) i ("p-bulk-" <> tshow i) someExpiry) [2 .. readsPerPass + 2]
  setSkipped ref [(Just "p-overdue", "btcpay invoice p-overdue: unknown status Frobnicated")]
  stillOpen "it answered and left one of ours unread"
  setSkipped ref [(Nothing, "btcpay: the list stopped at 50 pages")]
  stillOpen "it stopped early without saying whose invoices it missed"
  -- and with nothing in the way, the very same invoice goes
  setSkipped ref []
  runOnePass poller
  invoiceStatus (weStore env) overdue `shouldReturn` ISExpired

testListPaginates :: IO ()
testListPaginates = bounded "paging" $ withFakeProvider $ \fake p -> do
  refs <- replicateM (listPageSize + 1) (piProviderRef <$> createdAtFake p)
  case reverse refs of
    [] -> failWith "no invoices were created"
    (onSecondPage : _) -> do
      setInvoiceState fake onSecondPage ["status" .= ("Settled" :: Text), "paymentMethodPaid" .= ("0.00050000" :: Text)]
      pListOpen p >>= \case
        Left e -> failWith ("expected a list, got " <> show e)
        Right ListPass {lpMoved} -> map fst lpMoved `shouldSatisfy` elem onSecondPage
      gets <- apiRequests fake "GET" ["invoices"]
      let pageOf FakeRequest {frQuery} = (join (lookup "take" frQuery), join (lookup "skip" frQuery))
          size = BC.pack (show listPageSize)
      map pageOf gets `shouldBe` [(Just size, Just "0"), (Just size, Just size)]

testPagingStopsAtTheCeiling :: IO ()
testPagingStopsAtTheCeiling = bounded "paging ceiling" $ withFakeProvider $ \fake p -> do
  ignoreListPaging fake
  replicateM_ listPageSize (void (createdAtFake p))
  r <- pListOpen p
  gets <- apiRequests fake "GET" ["invoices"]
  length gets `shouldBe` maxListPages
  case r of
    Left e -> failWith ("expected a list, got " <> show e)
    Right ListPass {lpSkipped} -> do
      map snd lpSkipped `shouldSatisfy` any (T.isInfixOf "stopped at")
      map snd lpSkipped `shouldSatisfy` any (T.isInfixOf (T.pack (show (maxListPages * listPageSize))))
      map fst lpSkipped `shouldSatisfy` all isNothing

createdAtFake :: HasCallStack => Provider -> IO ProviderInvoice
createdAtFake p =
  pCreateInvoice p (SPMCrypto CCBtc) sampleDraft >>= \case
    Right inv -> pure inv
    Left e -> failWith ("expected an invoice at the fake, got " <> show e)

webhookPath :: String
webhookPath = "/webhooks/btcpay"

hookRef :: Text
hookRef = "p-hooked"

hookedEvent :: LB.ByteString
hookedEvent = webhookEvent "InvoiceSettled" hookRef

someSig :: [Header]
someSig = webhookSigHeader "not-the-secret" hookedEvent

postWebhook :: WebClient -> [Header] -> LB.ByteString -> IO (Response LB.ByteString)
postWebhook client hdrs = webRequestBody client "POST" webhookPath hdrs

slowProviderRead :: Int
slowProviderRead = 1000000

testWebhookVerifiesARealSignature :: IO ()
testWebhookVerifiesARealSignature = bounded "real signature" $ withFakePoller $ \fake poller env client -> do
  r <- postCreateAs client 1 (createBody supporterPriceId Nothing "xmr" (codeHashText sampleCode))
  statusOf r `shouldBe` 200
  o <- jsonObject r
  iid <- InvoiceId <$> stringField o "invoiceId"
  Just row <- getInvoice (weStore env) iid
  let secret = bWebhookSecret (fbConfig fake)
      body = webhookEvent "InvoiceSettled" (irProviderRef row)
  delivered <- postWebhook client (webhookSigHeader secret body) body
  statusOf delivered `shouldBe` 200
  responseBody delivered `shouldBe` ""
  tampered <- postWebhook client (webhookSigHeader secret body) (body <> " ")
  statusOf tampered `shouldBe` 400
  responseBody tampered `shouldBe` ""
  unsigned <- postWebhook client [] body
  statusOf unsigned `shouldBe` 400
  invoiceStatus (weStore env) iid `shouldReturn` ISOpen
  setInvoiceState fake (irProviderRef row) ["status" .= ("Settled" :: Text), "paymentMethodPaid" .= ("0.32095000" :: Text)]
  drainHints poller
  invoiceStatus (weStore env) iid `shouldReturn` ISPaid
  bcPaymentStatus <$> settledCode (weStore env) iid `shouldReturn` CPSPaid

testWebhookPassesTheRawBytes :: IO ()
testWebhookPassesTheRawBytes = bounded "raw bytes" $ withStubPoller raceHold $ \ref _ env client -> do
  seedBadgePrice (weStore env) "price1"
  _ <- seedOpenRef (weStore env) 1 hookRef someExpiry
  setVerifyResult ref (Right (Just hookRef))
  let hdrs = webhookSigHeader "any-secret" hookedEvent
  r <- postWebhook client hdrs hookedEvent
  statusOf r `shouldBe` 200
  stubWebhooks ref >>= \case
    [(seenHdrs, seenBody)] -> do
      seenBody `shouldBe` LB.toStrict hookedEvent
      lookup "BTCPay-Sig" seenHdrs `shouldBe` lookup "BTCPay-Sig" hdrs
    other -> failWith ("expected one verify call, got " <> show (length other))
  case J.decode hookedEvent :: Maybe J.Value of
    Nothing -> failWith "the event body is not JSON"
    Just parsed -> J.encode parsed `shouldSatisfy` (/= hookedEvent)

testWebhookQueuesARead :: IO ()
testWebhookQueuesARead = bounded "webhook queues" $ withStubPoller raceHold $ \ref _ env client -> do
  seedBadgePrice (weStore env) "price1"
  iid <- seedOpenRef (weStore env) 1 hookRef someExpiry
  setVerifyResult ref (Right (Just hookRef))
  r <- postWebhook client someSig hookedEvent
  statusOf r `shouldBe` 200
  responseBody r `shouldBe` ""
  headerOf r hCacheControl `shouldBe` Just "no-store"
  stubCalls ref `shouldReturn` []
  invoiceStatus (weStore env) iid `shouldReturn` ISOpen
  Just row <- getInvoice (weStore env) iid
  irPayment row `shouldBe` Nothing

testWebhookSettlesByThePollerPath :: IO ()
testWebhookSettlesByThePollerPath = bounded "one settlement lane" $ withStubPoller raceHold $ \ref poller env client -> do
  seedBadgePrice (weStore env) "price1"
  hinted <- seedOpenRef (weStore env) 1 hookRef someExpiry
  listed <- seedOpenRef (weStore env) 2 "p-listed" someExpiry
  setSignals ref [(hookRef, settledSignal), ("p-listed", settledSignal)]
  setVerifyResult ref (Right (Just hookRef))
  r <- postWebhook client someSig hookedEvent
  statusOf r `shouldBe` 200
  invoiceStatus (weStore env) hinted `shouldReturn` ISOpen
  drainHints poller
  stubCalls ref `shouldReturn` [StubRead hookRef]
  invoiceStatus (weStore env) hinted `shouldReturn` ISPaid
  bcPaymentStatus <$> settledCode (weStore env) hinted `shouldReturn` CPSPaid
  ipStatus <$> paymentRow (weStore env) hinted `shouldReturn` "settled"
  runOnePass poller
  invoiceStatus (weStore env) listed `shouldReturn` ISPaid
  bcPaymentStatus <$> settledCode (weStore env) listed `shouldReturn` CPSPaid
  ipStatus <$> paymentRow (weStore env) listed `shouldReturn` "settled"

testWebhookDoesNotWaitOnTheProvider :: IO ()
testWebhookDoesNotWaitOnTheProvider = bounded "webhook does not wait" $ withStubPoller raceHold $ \ref _ env client -> do
  seedBadgePrice (weStore env) "price1"
  _ <- seedOpenRef (weStore env) 1 hookRef someExpiry
  setSignals ref [(hookRef, settledSignal)]
  setVerifyResult ref (Right (Just hookRef))
  setReadDelay ref slowProviderRead
  started <- getCurrentTime
  r <- postWebhook client someSig hookedEvent
  elapsed <- (`diffUTCTime` started) <$> getCurrentTime
  statusOf r `shouldBe` 200
  elapsed `shouldSatisfy` (< 0.5)
  stubCalls ref `shouldReturn` []

testWebhookRefusesASignature :: IO ()
testWebhookRefusesASignature = bounded "webhook refuses" $ withStubPoller raceHold $ \ref poller env client -> do
  seedBadgePrice (weStore env) "price1"
  iid <- seedOpenRef (weStore env) 1 hookRef someExpiry
  setSignals ref [(hookRef, settledSignal)]
  setVerifyResult ref (Left (WebhookError "BTCPay-Sig does not verify"))
  r <- postWebhook client someSig hookedEvent
  statusOf r `shouldBe` 400
  responseBody r `shouldBe` ""
  drainHints poller
  stubCalls ref `shouldReturn` []
  invoiceStatus (weStore env) iid `shouldReturn` ISOpen

testWebhookIgnoresWhatItCannotActOn :: IO ()
testWebhookIgnoresWhatItCannotActOn = bounded "webhook ignores" $ withStubPoller raceHold $ \ref poller env client -> do
  seedBadgePrice (weStore env) "price1"
  ours <- seedOpenRef (weStore env) 1 hookRef someExpiry
  otherLane <- seedOtherProvider (weStore env) 2 "p-stripe"
  setSignals ref [(hookRef, settledSignal), ("p-stripe", settledSignal)]
  setVerifyResult ref (Right Nothing)
  unhandled <- postWebhook client someSig (webhookEvent "InvoiceReceivedPayment" hookRef)
  statusOf unhandled `shouldBe` 200
  responseBody unhandled `shouldBe` ""
  setVerifyResult ref (Right (Just "p-no-such-invoice"))
  unknown <- postWebhook client someSig (webhookEvent "InvoiceSettled" "p-no-such-invoice")
  statusOf unknown `shouldBe` 200
  responseBody unknown `shouldBe` ""
  setVerifyResult ref (Right (Just "p-stripe"))
  crossed <- postWebhook client someSig (webhookEvent "InvoiceSettled" "p-stripe")
  statusOf crossed `shouldBe` 200
  responseBody crossed `shouldBe` ""
  drainHints poller
  stubCalls ref `shouldReturn` []
  invoiceStatus (weStore env) ours `shouldReturn` ISOpen
  invoiceStatus (weStore env) otherLane `shouldReturn` ISOpen

testWebhookRefusesAnOversizedBody :: IO ()
testWebhookRefusesAnOversizedBody = bounded "webhook body cap" $ withStubPoller raceHold $ \ref _ _ client -> do
  setVerifyResult ref (Left (WebhookError "BTCPay-Sig does not verify"))
  atCap <- postWebhook client someSig (LB.replicate (fromIntegral maxWebhookBytes) 'x')
  statusOf atCap `shouldBe` 400
  length <$> stubWebhooks ref `shouldReturn` 1
  over <- postWebhook client someSig (LB.replicate (fromIntegral maxWebhookBytes + 1) 'x')
  statusOf over `shouldBe` 413
  responseBody over `shouldBe` ""
  length <$> stubWebhooks ref `shouldReturn` 1

testWebhookDropsAHintWhenFull :: IO ()
testWebhookDropsAHintWhenFull = bounded "full hint queue" $ withStubPoller raceHold $ \ref poller env client -> do
  seedBadgePrice (weStore env) "price1"
  iid <- seedOpenRef (weStore env) 1 hookRef someExpiry
  setSignals ref [(hookRef, settledSignal)]
  filled <- replicateM (fromIntegral hintQueueSize) (queueReadHint (weHints env) "p-filler")
  filled `shouldBe` replicate (fromIntegral hintQueueSize) True
  queueReadHint (weHints env) "p-filler" `shouldReturn` False
  setVerifyResult ref (Right (Just hookRef))
  r <- postWebhook client someSig hookedEvent
  statusOf r `shouldBe` 200
  responseBody r `shouldBe` ""
  drainHints poller
  invoiceStatus (weStore env) iid `shouldReturn` ISOpen
  runOnePass poller
  invoiceStatus (weStore env) iid `shouldReturn` ISPaid

testWebhookNeverAnswers5xx :: IO ()
testWebhookNeverAnswers5xx = bounded "webhook never 5xx" $ withStubPoller raceHold $ \ref _ env client -> do
  seedBadgePrice (weStore env) "price1"
  _ <- seedOpenRef (weStore env) 1 hookRef someExpiry
  setVerifyThrows ref True
  threw <- postWebhook client someSig hookedEvent
  statusOf threw `shouldBe` 200
  responseBody threw `shouldBe` ""
  setVerifyThrows ref False
  setVerifyResult ref (Right (Just hookRef))
  breakStore (weStore env)
  r <- postWebhook client someSig hookedEvent
  statusOf r `shouldBe` 200
  responseBody r `shouldBe` ""
  stubCalls ref `shouldReturn` []

paidInFull, paidInPart :: Text
paidInFull = "0.00050000"
paidInPart = "0.00020000"

fullyPaid, partlyPaid :: J.Value
fullyPaid = J.Number 5400
partlyPaid = J.Number 2160

buyBadge :: HasCallStack => WebEnv -> WebClient -> Int -> Text -> IO (InvoiceId, Text)
buyBadge env client i code = do
  r <- postCreateAs client i (createBody supporterPriceId Nothing "btc" (codeHashText code))
  statusOf r `shouldBe` 200
  o <- jsonObject r
  iid <- InvoiceId <$> stringField o "invoiceId"
  getInvoice (weStore env) iid >>= \case
    Just InvoiceRow {irProviderRef} -> pure (iid, irProviderRef)
    Nothing -> failWith "the create answered 200 and wrote no invoice row"

atProvider :: FakeBTCPay -> Text -> Text -> Text -> Text -> IO ()
atProvider fake ref status additional paid =
  atProviderDue fake ref status additional paid (if paid == paidInFull then "0.00000000" else dueAfterPart)

-- | The provider recomputes what is owed on every read, so a fake that moves the paid figure
-- without the due figure cannot exercise the field the payment screen prints.
atProviderDue :: FakeBTCPay -> Text -> Text -> Text -> Text -> Text -> IO ()
atProviderDue fake ref status additional paid due =
  setInvoiceState fake ref ["status" .= status, "additionalStatus" .= additional, "paymentMethodPaid" .= paid, "due" .= due]

dueAfterPart :: Text
dueAfterPart = "0.00030500"

viewInvoice :: HasCallStack => WebClient -> InvoiceId -> IO J.Object
viewInvoice client iid = do
  r <- webGet client (invoicePath iid)
  statusOf r `shouldBe` 200
  headerOf r hCacheControl `shouldBe` Just "no-store"
  jsonObject r

scenarioPaidPurchase :: IO ()
scenarioPaidPurchase = bounded "scenario: paid purchase" $ withFakePoller $ \fake poller env client -> do
  (iid, ref) <- buyBadge env client 1 sampleCode
  unpaid <- viewInvoice client iid
  fieldOf unpaid "status" `shouldBe` Just (J.String "open")
  fieldOf unpaid "amountPaid" `shouldBe` Nothing
  fieldOf unpaid "cryptoAmountPaid" `shouldBe` Nothing
  atProvider fake ref "Settled" "None" paidInFull
  runOnePass poller
  paid <- viewInvoice client iid
  fieldOf paid "status" `shouldBe` Just (J.String "paid")
  fieldOf paid "amountPaid" `shouldBe` Just fullyPaid
  fieldOf paid "cryptoAmountPaid" `shouldBe` Just (J.String paidInFull)
  fieldOf paid "settledAt" `shouldSatisfy` isJust
  p <- paymentRow (weStore env) iid
  ipStatus p `shouldBe` "settled"
  code <- settledCode (weStore env) iid
  bcPaymentStatus code `shouldBe` CPSPaid
  bcExpiresAt code `shouldBe` Just (addUTCTime codeLifetime (ipUpdatedAt p))

scenarioHeldWaitWakes :: IO ()
scenarioHeldWaitWakes = bounded "scenario: held wait" $ withFakePollerHolding raceHold $ \fake poller env client -> do
  (iid, ref) <- buyBadge env client 1 sampleCode
  held <- async $ webGet client (invoicePath iid <> "?wait=open")
  threadDelay 100000
  Async.poll held >>= \parked -> parked `shouldSatisfy` isNothing
  atProvider fake ref "Settled" "None" paidInFull
  settling <- getCurrentTime
  runOnePass poller
  r <- wait held
  elapsed <- (`diffUTCTime` settling) <$> getCurrentTime
  statusOf r `shouldBe` 200
  o <- jsonObject r
  fieldOf o "status" `shouldBe` Just (J.String "paid")
  fieldOf o "amountPaid" `shouldBe` Just fullyPaid
  elapsed `shouldSatisfy` (< 1)
  invoiceStatus (weStore env) iid `shouldReturn` ISPaid

scenarioPartPaymentIsReported :: IO ()
scenarioPartPaymentIsReported = bounded "scenario: part paid" $ withFakePoller $ \fake poller env client -> do
  (iid, ref) <- buyBadge env client 1 sampleCode
  atProvider fake ref "New" "PaidPartial" paidInPart
  runOnePass poller
  o <- viewInvoice client iid
  fieldOf o "status" `shouldBe` Just (J.String "open")
  fieldOf o "amountPaid" `shouldBe` Just partlyPaid
  fieldOf o "cryptoAmountPaid" `shouldBe` Just (J.String paidInPart)
  -- the provider's own figure, not the difference: it carries the network fee a partial
  -- payment adds, so 0.00030500 rather than the 0.00030000 a subtraction would give
  fieldOf o "cryptoAmountDue" `shouldBe` Just (J.String dueAfterPart)
  fieldOf o "settledAt" `shouldBe` Nothing
  invoiceStatus (weStore env) iid `shouldReturn` ISOpen
  ipStatus <$> paymentRow (weStore env) iid `shouldReturn` "pending"
  code <- settledCode (weStore env) iid
  bcPaymentStatus code `shouldBe` CPSUnpaid
  bcExpiresAt code `shouldBe` Nothing

scenarioExpiryReportsWhatArrived :: IO ()
scenarioExpiryReportsWhatArrived = bounded "scenario: expired part paid" $ withFakePoller $ \fake poller env client -> do
  (iid, ref) <- buyBadge env client 1 sampleCode
  atProvider fake ref "Expired" "PaidPartial" paidInPart
  runOnePass poller
  o <- viewInvoice client iid
  fieldOf o "status" `shouldBe` Just (J.String "expired")
  fieldOf o "amountPaid" `shouldBe` Just partlyPaid
  fieldOf o "cryptoAmountPaid" `shouldBe` Just (J.String paidInPart)
  fieldOf o "settledAt" `shouldBe` Nothing
  invoiceStatus (weStore env) iid `shouldReturn` ISExpired
  ipStatus <$> paymentRow (weStore env) iid `shouldReturn` "pending"
  code <- settledCode (weStore env) iid
  bcPaymentStatus code `shouldBe` CPSUnpaid
  bcExpiresAt code `shouldBe` Nothing

scenarioLateSettlement :: IO ()
scenarioLateSettlement = bounded "scenario: late settlement" $ withFakePoller $ \fake poller env client -> do
  (iid, ref) <- buyBadge env client 1 sampleCode
  atProvider fake ref "Expired" "PaidPartial" paidInPart
  runOnePass poller
  expired <- viewInvoice client iid
  fieldOf expired "status" `shouldBe` Just (J.String "expired")
  fieldOf expired "amountPaid" `shouldBe` Just partlyPaid
  invoiceStatus (weStore env) iid `shouldReturn` ISExpired
  bcPaymentStatus <$> settledCode (weStore env) iid `shouldReturn` CPSUnpaid
  atProvider fake ref "Settled" "PaidLate" paidInFull
  runOnePass poller
  paid <- viewInvoice client iid
  fieldOf paid "status" `shouldBe` Just (J.String "paid")
  fieldOf paid "amountPaid" `shouldBe` Just fullyPaid
  fieldOf paid "cryptoAmountPaid" `shouldBe` Just (J.String paidInFull)
  fieldOf paid "settledAt" `shouldSatisfy` isJust
  invoiceStatus (weStore env) iid `shouldReturn` ISPaid
  p <- paymentRow (weStore env) iid
  ipStatus p `shouldBe` "settled"
  code <- settledCode (weStore env) iid
  bcPaymentStatus code `shouldBe` CPSPaid
  bcExpiresAt code `shouldBe` Just (addUTCTime codeLifetime (ipUpdatedAt p))

scenarioReplayChangesNothing :: IO ()
scenarioReplayChangesNothing = bounded "scenario: replay" $ withFakePoller $ \fake poller env client -> do
  (iid, ref) <- buyBadge env client 1 sampleCode
  atProvider fake ref "Settled" "None" paidInFull
  runOnePass poller
  settled <- viewInvoice client iid
  fieldOf settled "status" `shouldBe` Just (J.String "paid")
  firstPayment <- paymentRow (weStore env) iid
  firstCode <- settledCode (weStore env) iid
  length <$> paymentIdentity (weStore env) iid `shouldReturn` 1
  runOnePass poller
  -- one list, on the first pass; the second read the invoice by its own id
  length <$> apiRequests fake "GET" ["invoices"] `shouldReturn` 1
  length <$> paymentIdentity (weStore env) iid `shouldReturn` 1
  paymentRow (weStore env) iid `shouldReturn` firstPayment
  settledCode (weStore env) iid `shouldReturn` firstCode
  viewInvoice client iid `shouldReturn` settled

scenarioInvalidClosesAsExpired :: IO ()
scenarioInvalidClosesAsExpired = bounded "scenario: invoice invalid" $ withFakePoller $ \fake poller env client -> do
  (iid, ref) <- buyBadge env client 1 sampleCode
  atProvider fake ref "Invalid" "None" paidInPart
  let body = webhookEvent "InvoiceInvalid" ref
  delivered <- postWebhook client (webhookSigHeader (bWebhookSecret (fbConfig fake)) body) body
  statusOf delivered `shouldBe` 200
  responseBody delivered `shouldBe` ""
  invoiceStatus (weStore env) iid `shouldReturn` ISOpen
  drainHints poller
  o <- viewInvoice client iid
  fieldOf o "status" `shouldBe` Just (J.String "expired")
  fieldOf o "amountPaid" `shouldBe` Just partlyPaid
  fieldOf o "cryptoAmountPaid" `shouldBe` Just (J.String paidInPart)
  invoiceStatus (weStore env) iid `shouldReturn` ISExpired
  bcPaymentStatus <$> settledCode (weStore env) iid `shouldReturn` CPSUnpaid
  detailReads <- apiRequests fake "GET" ["invoices", ref]
  length detailReads `shouldBe` 1

scenarioCodeConflictCreatesNothing :: IO ()
scenarioCodeConflictCreatesNothing = bounded "scenario: code conflict" $ withFakePoller $ \fake poller env client -> do
  (iid, ref) <- buyBadge env client 1 sampleCode
  fakeInvoiceIds fake `shouldReturn` [ref]
  again <- postCreateAs client 2 (createBody supporterPriceId Nothing "btc" (codeHashText sampleCode))
  statusOf again `shouldBe` 409
  responseBody again `shouldBe` errorBody "code_conflict"
  fakeInvoiceIds fake `shouldReturn` [ref]
  invoiceCount (weStore env) `shouldReturn` 1
  (_, otherRef) <- buyBadge env client 3 (sampleCode <> "2")
  otherRef `shouldSatisfy` (/= ref)
  fakeInvoiceIds fake `shouldReturn` sort [ref, otherRef]
  invoiceCount (weStore env) `shouldReturn` 2
  atProvider fake ref "Settled" "None" paidInFull
  runOnePass poller
  paid <- viewInvoice client iid
  fieldOf paid "status" `shouldBe` Just (J.String "paid")
  bcPaymentStatus <$> settledCode (weStore env) iid `shouldReturn` CPSPaid

scenarioNoWebhookAnywhere :: IO ()
scenarioNoWebhookAnywhere = bounded "scenario: no webhook" $ withFakePoller $ \fake poller env client -> do
  (iid, ref) <- buyBadge env client 1 sampleCode
  atProvider fake ref "Settled" "None" paidInFull
  drainHints poller
  invoiceStatus (weStore env) iid `shouldReturn` ISOpen
  runOnePass poller
  paid <- viewInvoice client iid
  fieldOf paid "status" `shouldBe` Just (J.String "paid")
  bcPaymentStatus <$> settledCode (weStore env) iid `shouldReturn` CPSPaid
  -- no webhook was delivered: the pass found it by reading the invoice it holds a row for
  byId <- apiRequests fake "GET" ["invoices", ref]
  byId `shouldSatisfy` (not . null)

cancelPath :: InvoiceId -> String
cancelPath iid = invoicePath iid <> "/cancel"

webPost :: WebClient -> String -> IO (Response LB.ByteString)
webPost client target = webRequest client "POST" target []

withCancel :: (IORef StubState -> WebEnv -> WebClient -> IO a) -> IO a
withCancel action = withCheckout $ \ref env client -> do
  _ <- seedOpenInvoice env
  action ref env client

testCancelClosesTheInvoiceAtBothEnds :: IO ()
testCancelClosesTheInvoiceAtBothEnds = bounded "cancel" $ withCancel $ \ref _ client -> do
  let iid = niInvoiceId sampleInvoice
  r <- webPost client (cancelPath iid)
  statusOf r `shouldBe` 200
  o <- jsonObject r
  fieldOf o "status" `shouldBe` Just (J.String "expired")
  -- the provider is told, or its invoice would keep taking payment at an address the
  -- buyer still has
  stubCalls ref `shouldReturn` [StubCancel (niProviderRef sampleInvoice)]
  reread <- webGet client (invoicePath iid)
  ao <- jsonObject reread
  fieldOf ao "status" `shouldBe` Just (J.String "expired")

-- | Every other way an invoice closes wakes the browsers waiting on it; a cancel is one more.
-- The buyer's other tab, or their phone, is holding `?wait=open` and would otherwise sit there
-- until the hold times out, showing an address the provider has already invalidated.
testCancelWakesAHeldRequest :: IO ()
testCancelWakesAHeldRequest = bounded "cancel wakes a hold" $ withCancel $ \_ _ client -> do
  let iid = niInvoiceId sampleInvoice
  _ <- wokenBy client iid (void (webPost client (cancelPath iid))) "expired"
  pure ()

testCancelIsRefusedOnceItIsNotOpen :: IO ()
testCancelIsRefusedOnceItIsNotOpen = bounded "cancel not open" $ withCancel $ \ref env client -> do
  let iid = niInvoiceId sampleInvoice
  markPaid (weStore env) iid
  r <- webPost client (cancelPath iid)
  statusOf r `shouldBe` 409
  responseBody r `shouldBe` errorBody "not_open"
  -- a paid invoice must not be cancelled at the provider either: the money is already in
  stubCalls ref `shouldReturn` []
  reread <- webGet client (invoicePath iid)
  ao <- jsonObject reread
  fieldOf ao "status" `shouldBe` Just (J.String "paid")

testCancelLeavesTheInvoiceOpenWhenTheProviderFails :: IO ()
testCancelLeavesTheInvoiceOpenWhenTheProviderFails = bounded "cancel provider down" $ withCancel $ \ref _ client -> do
  let iid = niInvoiceId sampleInvoice
  atomicModifyIORef' ref $ \s -> (s {ssCancelError = Just (ProviderError "boom")}, ())
  r <- webPost client (cancelPath iid)
  statusOf r `shouldBe` 503
  -- open at both ends is recoverable; closed here and open there is not
  reread <- webGet client (invoicePath iid)
  ao <- jsonObject reread
  fieldOf ao "status" `shouldBe` Just (J.String "open")

testCancelIsOpaqueForAnUnknownInvoice :: IO ()
testCancelIsOpaqueForAnUnknownInvoice = bounded "cancel unknown" $ withCancel $ \ref _ client -> do
  r <- webPost client "/api/invoice/no-such-invoice/cancel"
  statusOf r `shouldBe` 404
  responseBody r `shouldBe` notFoundBody
  stubCalls ref `shouldReturn` []

testCancelRefusesOtherMethods :: IO ()
testCancelRefusesOtherMethods = bounded "cancel verb" $ withCancel $ \_ _ client -> do
  r <- webGet client (cancelPath (niInvoiceId sampleInvoice))
  statusOf r `shouldBe` 405
  headerOf r "Allow" `shouldBe` Just "POST"

testExpireOverdueSparesAFundedInvoice :: IO ()
testExpireOverdueSparesAFundedInvoice = withServiceStore $ \st -> do
  seedBadgePrice st "price1"
  now <- getCurrentTime
  let pastExpiry = addUTCTime (-3600) now
      funded = sampleInvoice {niExpiresAt = pastExpiry}
      empty' = sampleInvoice {niInvoiceId = InvoiceId "inv-empty", niProviderRef = "p-empty", niCodeHash = digestFixture 15, niExpiresAt = pastExpiry}
  createInvoiceRows st funded `shouldReturn` Right ()
  createInvoiceRows st empty' `shouldReturn` Right ()
  -- seen in the mempool, not yet confirmed: the window bounds the rate hold, and that
  -- stopped mattering the moment the money arrived
  settle st (niInvoiceId funded) (SigFunded (rcv 500 (Just "0.00050000")) PaidInPart) now `shouldReturn` Right ISOpen
  moved <- expireOverdue st now
  moved `shouldBe` [niInvoiceId empty']
  Just fundedRow <- getInvoice st (niInvoiceId funded)
  irStatus fundedRow `shouldBe` ISOpen
  -- and a confirmation an hour later still pays it
  settle st (niInvoiceId funded) (SigSettled (rcv 500 (Just "0.00050000")) now) now `shouldReturn` Right ISPaid

-- | The sweep spares a funded invoice, and cancel cannot: BTCPay has already been told to
-- invalidate it by the time the row is written, so leaving it open would keep drawing an address
-- nothing can be sent to.
testCancelExpiresAFundedInvoice :: IO ()
testCancelExpiresAFundedInvoice = withServiceStore $ \st -> do
  seedBadgePrice st "price1"
  now <- getCurrentTime
  let iid = niInvoiceId sampleInvoice
  createInvoiceRows st sampleInvoice `shouldReturn` Right ()
  settle st iid (SigFunded (rcv 500 (Just "0.00050000")) PaidInPart) now `shouldReturn` Right ISOpen
  cancelOpenInvoice st iid now `shouldReturn` True
  Just row <- getInvoice st iid
  irStatus row `shouldBe` ISExpired
  -- the money is not expired with the invoice: settlement still has the payment row to work from
  (ipCryptoAmount <$> irPayment row) `shouldBe` Just (Just "0.00050000")

testCancelIsRefusedOnceItIsFunded :: IO ()
testCancelIsRefusedOnceItIsFunded = bounded "cancel funded" $ withCancel $ \ref env client -> do
  let iid = niInvoiceId sampleInvoice
  settle (weStore env) iid (SigFunded (rcv 500 (Just "0.00050000")) PaidInPart) someCreated `shouldReturn` Right ISOpen
  r <- webPost client (cancelPath iid)
  statusOf r `shouldBe` 409
  -- the two refusals read differently to the buyer, so which one this is has to be pinned
  responseBody r `shouldBe` errorBody "funded"
  -- invalidating it at BTCPay would strand what the buyer already sent
  stubCalls ref `shouldReturn` []
  reread <- webGet client (invoicePath iid)
  ao <- jsonObject reread
  fieldOf ao "status" `shouldBe` Just (J.String "open")

testViewNamesTheConfirmationsSettlementNeeds :: IO ()
testViewNamesTheConfirmationsSettlementNeeds = bounded "required confirmations" $ withCancel $ \_ _ client -> do
  r <- webGet client (invoicePath (niInvoiceId sampleInvoice))
  statusOf r `shouldBe` 200
  o <- jsonObject r
  -- MediumSpeed is BTCPay's one-confirmation policy; Greenfield reports no running count,
  -- so this is what settlement needs, not how far along it is
  fieldOf o "requiredConfirmations" `shouldBe` Just (J.Number 1)

testViewOmitsConfirmationsWithoutBTCPay :: IO ()
testViewOmitsConfirmationsWithoutBTCPay = bounded "no confirmations" $ withWebApp $ \env client -> do
  iid <- seedOpenInvoice env
  r <- webGet client (invoicePath iid)
  o <- jsonObject r
  fieldOf o "requiredConfirmations" `shouldBe` Nothing

testHeldWaitWakesOnAPaymentThatDoesNotSettle :: IO ()
testHeldWaitWakesOnAPaymentThatDoesNotSettle = bounded "funded wakes a hold" $ withCheckout $ \_ env client -> do
  iid <- seedOpenInvoice env
  -- the browser is holding on ?wait=open, and the payment BTCPay reports as Processing
  -- leaves the invoice open: without a wake this answers only when the hold times out
  held <- async $ webGet client (invoicePath iid <> "?wait=open")
  -- the hold has to be registered before the settle, or this proves nothing
  threadDelay 100000
  waitingCount (weWaiters env) `shouldReturn` 1
  settleOrder (weStore env) (weWaiters env) iid (SigFunded (rcv 500 (Just "0.00050000")) PaidInPart) someCreated
    `shouldReturn` Right ISOpen
  r <- wait held
  statusOf r `shouldBe` 200
  o <- jsonObject r
  fieldOf o "status" `shouldBe` Just (J.String "open")
  fieldOf o "cryptoAmountPaid" `shouldBe` Just (J.String "0.00050000")

-- | The provider re-sends what it already sent on every pass. Rewriting the row would publish a
-- payment to every browser holding `?wait=`, and each would re-request at once: the long poll
-- becomes a poll loop with a write per pass. The guard is in `newPayment`, and only an open
-- invoice reaches it, since a settled one is decided before that.
testARepeatedSignalDoesNotWakeAHold :: IO ()
testARepeatedSignalDoesNotWakeAHold = bounded "repeat signal" $ withWebApp $ \env client -> do
  iid <- seedOpenInvoice env
  let funded = SigFunded (rcv 500 (Just "0.00050000")) PaidInPart
  settleOrder (weStore env) (weWaiters env) iid funded settleAt `shouldReturn` Right ISOpen
  held <- async $ webGet client (invoicePath iid <> "?wait=open&seenPaid=0.00050000&seenFull=0")
  threadDelay holdParkDelay
  Async.poll held >>= (`shouldSatisfy` isNothing)

  -- the same figures again, which is what the next pass brings
  settleOrder (weStore env) (weWaiters env) iid funded settleAt `shouldReturn` Right ISOpen
  threadDelay holdParkDelay
  stillParked <- Async.poll held
  stillParked `shouldSatisfy` isNothing

  -- and something that did move still releases it, so this is not a hold that never wakes
  settleOrder (weStore env) (weWaiters env) iid (SigSettled (rcv 500 (Just "0.00050000")) settleAt) settleAt
    `shouldReturn` Right ISPaid
  r <- wait held
  statusOf r `shouldBe` 200
  o <- jsonObject r
  fieldOf o "status" `shouldBe` Just (J.String "paid")

testHeldWaitIsNotWokenByAPassThatWroteNothing :: IO ()
testHeldWaitIsNotWokenByAPassThatWroteNothing = bounded "no churn" $ withCheckout $ \_ env _ -> do
  iid <- seedOpenInvoice env
  -- a settled invoice ignores every later signal, so this writes nothing and must not
  -- release a hold: every poller pass would otherwise churn the browsers waiting
  markPaid (weStore env) iid
  waitingBefore <- waitingCount (weWaiters env)
  settleOrder (weStore env) (weWaiters env) iid (SigFunded (rcv 500 (Just "0.00050000")) PaidInPart) someCreated
    `shouldReturn` Right ISPaid
  waitingAfter <- waitingCount (weWaiters env)
  waitingAfter `shouldBe` waitingBefore

testViewCarriesTheProvidersPaidVerdict :: IO ()
testViewCarriesTheProvidersPaidVerdict = bounded "paid in full" $ withCheckout $ \_ env client -> do
  iid <- seedOpenInvoice env
  -- BTCPay applies its own payment tolerance, so a tolerated underpayment is Processing:
  -- the browser cannot tell that from the amounts and must be told
  settleOrder (weStore env) (weWaiters env) iid (SigFunded (rcv 400 (Just "0.00040000")) PaidInFull) someCreated
    `shouldReturn` Right ISOpen
  o <- jsonObject =<< webGet client (invoicePath iid)
  fieldOf o "status" `shouldBe` Just (J.String "open")
  fieldOf o "paidInFull" `shouldBe` Just (J.Bool True)
  fieldOf o "amountPaid" `shouldBe` Just (J.Number 400)

testPaidVerdictIsNotWithdrawnByALaterRead :: IO ()
testPaidVerdictIsNotWithdrawnByALaterRead = bounded "verdict sticks" $ withCheckout $ \_ env client -> do
  iid <- seedOpenInvoice env
  settleOrder (weStore env) (weWaiters env) iid (SigFunded (rcv 500 (Just "0.00050000")) PaidInFull) someCreated
    `shouldReturn` Right ISOpen
  -- a later pass that reports the same payment as not-yet-accepted must not send the
  -- buyer back to "send the rest": the address is gone from that screen
  settleOrder (weStore env) (weWaiters env) iid (SigFunded (rcv 500 (Just "0.00050000")) PaidInPart) someCreated
    `shouldReturn` Right ISOpen
  o <- jsonObject =<< webGet client (invoicePath iid)
  fieldOf o "paidInFull" `shouldBe` Just (J.Bool True)

-- | The Monero case: BTCPay reports an invoice as paid in full while its figures are still
-- zero. Reading the fiat amount alone made both the sweep and the cancel call it unfunded.
testExpireOverdueSparesAZeroAmount :: IO ()
testExpireOverdueSparesAZeroAmount = withServiceStore $ \st -> do
  seedBadgePrice st "price1"
  now <- getCurrentTime
  let pastExpiry = addUTCTime (-3600) now
      dust = sampleInvoice {niExpiresAt = pastExpiry}
      verdict = sampleInvoice {niInvoiceId = InvoiceId "inv-verdict", niProviderRef = "p-verdict", niCodeHash = digestFixture 16, niExpiresAt = pastExpiry}
  createInvoiceRows st dust `shouldReturn` Right ()
  createInvoiceRows st verdict `shouldReturn` Right ()
  -- a crypto amount too small to round up to a cent, and a verdict with no figures at all
  settle st (niInvoiceId dust) (SigFunded (rcv 0 (Just "0.00000001")) PaidInPart) now `shouldReturn` Right ISOpen
  settle st (niInvoiceId verdict) (SigFunded (rcv 0 Nothing) PaidInFull) now `shouldReturn` Right ISOpen
  expireOverdue st now `shouldReturn` []
  mapM_ (\iid ->
    getInvoice st iid >>= \case
      Just InvoiceRow {irStatus, irPayment} -> do
        irStatus `shouldBe` ISOpen
        maybe False paymentHolds irPayment `shouldBe` True
      Nothing -> failWith "the invoice went missing") [niInvoiceId dust, niInvoiceId verdict]

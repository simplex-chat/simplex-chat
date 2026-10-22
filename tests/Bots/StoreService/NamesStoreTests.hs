{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Bots.StoreService.NamesStoreTests (namesStoreTests, at, btc, insertInvoiceRow, invoiceStatus, issue, newReg, order, ownerA, ownerB, paidOrder, placeCode, placePaid, price, reg, reveal, signature, t0, usable, write) where

import Bots.StoreService.WebTests (columnsOf, digestFixture, runMigrations, withServiceStore)
import Control.Arrow ((&&&))
import Control.Monad (forM_, void)
import Data.Int (Int64)
import Data.List (sort)
import Data.Maybe (fromJust, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime (..), addUTCTime)
import Data.Word (Word8)
import Simplex.Chat.Badges.Types (BadgeCodePaymentStatus (..))
import Simplex.Chat.PaymentService.Types (CardProvider (..), CryptoCurrency (..), InvoiceId (..), ServicePaymentMethod (..))
import Simplex.Messaging.Agent.Store.Common (DBStore, withTransaction)
import qualified Simplex.Messaging.Agent.Store.DB as DB
import Simplex.Messaging.Agent.Store.Shared (Migration (..), MigrationsToRun (..), toDownMigration)
import Simplex.Messaging.Crypto.Secp256k1 (RecoverableSignature)
import Simplex.Messaging.Eth.Address (Address, unAddress)
import Simplex.Messaging.SimplexName (LabelHash (..), labelHash)
import StoreService.Names.Chain (TxHash (..))
import StoreService.Names.Phases (RegistrationId (..))
import StoreService.Names.Signatures (OrderFields (..), parseAnyCaseAddress, parseSignature)
import StoreService.Names.Store
import StoreService.Store (RevokeResult (..))
import Test.Hspec

#if defined(dbPostgres)
import Database.PostgreSQL.Simple (Only (..), (:.) (..))
import Database.PostgreSQL.Simple.Errors (ConstraintViolation (..), constraintViolation)
import StoreService.Store.Postgres.Migrations (storeServiceSchemaMigrations)
#else
import Database.SQLite.Simple (Error (ErrorConstraint), Only (..), SQLError (..), (:.) (..))
import StoreService.Store.SQLite.Migrations (storeServiceSchemaMigrations)
#endif

namesStoreTests :: Spec
namesStoreTests = describe "StoreService names: store" $ do
  describe "migration" $ do
    it "creates the names tables with the planned columns" testNamesColumns
    it "rolls back and re-applies only the names migrations, with rows in them, leaving the badge tables and paid invoices as they were" testNamesDownUp
    it "turns a code_issued order taken at price 0 and not live at its next version, dropping issued_code_id with its index and foreign key, which the down migration restores" testDropTakenCodes
    it "makes taken orders not live and gives a taken paid order a balance of its price, none to a code order or a price of 0, and rolls back the balance tables and columns" testNameBalancesMigration
  describe "order hash" $ do
    it "is equal for the same order fields, and differs when any one field differs" testOrderHashFields
    it "keeps field boundaries, so moving a byte between two fields changes it" testOrderHashBoundaries
  describe "registrations" $ do
    it "creates a paid order unpaid and live at version 1, linked to its invoice" testCreatePaid
    it "creates a code order queued with its code reserved" testCreateCode
    it "refuses a second live order for one owner and label hash, or for one commitment" testLiveUnique
    it "leaves no registration when the code cannot be reserved" testCodeNotReservable
    it "finds an exact resend by its order hash, live or not" testOrderHashLookup
    it "tells an overdue live order apart, whatever the poller marked, unless paid" testLiveOrdersOverdue
    it "closes an overdue order so a new one from the same wallet fits in the same transaction" testCloseOverdue
    it "closes nothing from a stale version" testCloseOverdueStale
    it "bumps the version on every write and changes nothing for a stale one" testVersionBumps
    it "writes and reads back every changeable field, and clears the reveal" testRoundTrip
    it "keeps live in step with the phase" testLiveFollowsPhase
    it "is live while an order can still progress to a registration for its name" testOrderLive
    it "lets a wallet order a name again once its registration is done" testReorderAfterRegistered
    it "refuses to queue a closed order, also when a newer live order holds its name" testLatePayment
    it "refuses a write that would meet another live order, writing nothing" testWriteMeetsLiveOrder
    it "retargets to the new fields, clearing the reveal, transactions, reveal times and refusal" testRetarget
    it "finds an owner's orders newest first, at most 50" testFind
    it "orders registrations made in the same second by their id" testSameSecondOrder
    it "tells a label being registered here" testLabelRegistering
    it "lists the registrations a worker moves on" testActive
    it "lists unpaid orders whose invoice expired before the cutoff without a payment, and deletes one with its invoice" testUnpaidCleanup
    it "lists code orders failed before the cutoff that still hold their code" testFailedCodeOrders
  describe "prepaid codes" $ do
    it "issues one code per hash" testIssueCode
    it "reserves only an unused code, releases and spends it only for its holder" testReserveReleaseSpend
    it "refuses an unpaid, revoked or expired code" testReserveRefused
    it "lets a registration hold one code" testOneCodePerRegistration
    it "revokes an unspent code, reserved or not, never a spent one" testRevoke
  describe "transactions" $ do
    it "keeps a signed transaction pending until its outcome, and lists them by owner" testTransactions
    it "moves a transaction only from the status its caller read" testTransactionFromStatus
  describe "record edits" $ do
    it "holds one pending or mined edit per owner and nonce, freed by a revert" testEditNonceUnique
    it "settles a pending edit once, with its mined hash" testSettleEdit
    it "round-trips a clear edit" testClearEdit
    it "gives the times of a name's relayed edits in the window" testRelayedEditTimes

t0 :: UTCTime
t0 = UTCTime (fromGregorian 2026 10 21) 0

at :: Int -> UTCTime
at minutes = addUTCTime (fromIntegral $ minutes * 60) t0

ownerA :: Address
ownerA = address "0x014f9c09CcEeDEF1B713110d04D3a07d5B5D9b73"

ownerB :: Address
ownerB = address "0x8A791620dd6260079BF849Dc5567aDC3F2FdC318"

address :: Text -> Address
address = either (error . show) id . parseAnyCaseAddress

-- r all 0x11, s all 0x22 (a low s), v 27
signature :: RecoverableSignature
signature = either (error . show) id $ parseSignature $ "0x" <> T.replicate 32 "11" <> T.replicate 32 "22" <> "1b"

twoYears :: Int
twoYears = 2

order :: Text -> Word8 -> OrderFields
order label n =
  OrderFields
    { ofLabelHash = labelHash label,
      ofLength = T.length label,
      ofDuration = 69292800,
      ofCommitment = digestFixture n,
      ofSealed = "sealed-" <> label <> "-" <> T.pack (show n)
    }

newReg :: Text -> Address -> OrderFields -> NewRegistration
newReg rid nrgOwner nrgOrder = NewRegistration {nrgId = RegistrationId rid, nrgOwner, nrgOrder, nrgYears = twoYears}

btc :: ServicePaymentMethod
btc = SPMCrypto CCBtc

price :: Int
price = 20000

paidOrder :: Text -> PaidOrder
paidOrder invId = PaidOrder {poMethod = btc, poPrice = price, poInvoiceId = InvoiceId invId, poProviderRef = "ref-" <> invId}

insertInvoiceRow :: DB.Connection -> Text -> UTCTime -> Text -> IO ()
insertInvoiceRow db invId expiresAt status =
  DB.execute
    db
    "INSERT INTO sx_badge_service_invoices (invoice_id, provider, price, amount, currency, payment_address, payment_crypto_currency, payment_crypto_amount, expires_at, status, created_at, updated_at) VALUES (?,?,?,?,?,?,?,?,?,?,?,?)"
    ((invId, "crypto" :: Text, price, price, "usd" :: Text, "bc1qexampleaddress" :: Text, "btc" :: Text, "0.00312" :: Text) :. (expiresAt, status, t0, t0))

-- | A paid order whose invoice, named after the registration, expires at the given time.
placePaid :: DBStore -> Text -> Address -> OrderFields -> UTCTime -> IO (Either RegistrationRefused ())
placePaid st rid owner o expiresAt = withTransaction st $ \db -> do
  insertInvoiceRow db ("inv-" <> rid) expiresAt "open"
  createPaidRegistration db (newReg rid owner o) (OTInvoice btc price) (paidOrder $ "inv-" <> rid) t0

issue :: DBStore -> Word8 -> NewPrepaidCode -> IO PrepaidCode
issue st n c = withTransaction st $ \db -> do
  Right codeId <- insertPrepaidCode db c {npcCodeHash = digestFixture n} t0
  fromJust <$> getPrepaidCodeById db codeId

usable :: NewPrepaidCode
usable = NewPrepaidCode {npcCodeHash = "", npcMinLength = 8, npcYears = twoYears, npcPaymentStatus = CPSPaid, npcExpiresAt = Nothing}

placeCode :: DBStore -> Text -> Address -> OrderFields -> PrepaidCode -> IO (Either RegistrationRefused ())
placeCode st rid owner o code = withTransaction st $ \db -> createCodeRegistration db (newReg rid owner o) code t0

reg :: DBStore -> Text -> IO NameRegistration
reg st rid = withTransaction st $ \db -> fromJust <$> getRegistration db (RegistrationId rid)

write :: DBStore -> NameRegistration -> IO VersionWrite
write st r = withTransaction st $ \db -> updateRegistration db r (at 1)

namesTables :: [Text]
namesTables = map ("sx_badge_service_name_" <>) ["prepaid_codes", "registrations", "invoices", "record_edits", "transactions", "balances", "late_credits"]

-- | Added to the badge tables' payments by the names migrations.
balancePaymentColumns :: [Text]
balancePaymentColumns = ["confirmed_amount", "received_units", "confirmed_units", "invoice_rate"]

badgeTables :: [Text]
badgeTables = map ("sx_badge_service_" <>) ["invoices", "payments", "badge_prices", "badge_offers", "badge_purchases", "badge_ledger", "badge_issuances", "badge_codes", "badge_code_invoices", "group"]

testNamesColumns :: IO ()
testNamesColumns = withServiceStore $ \st -> do
  let has table cols = columnsOf st table >>= \cs -> filter (`notElem` cs) cols `shouldBe` []
  has "sx_badge_service_name_prepaid_codes" ["name_prepaid_code_id", "code_hash", "min_length", "years", "code_payment_status", "reserved_by", "reserved_at", "spent_at", "revoked_at", "expires_at", "created_at"]
  has
    "sx_badge_service_name_registrations"
    [ "registration_id", "label_hash", "length", "label", "secret", "contact_link", "channel_link", "intent_nonce", "intent_deadline", "intent_signature",
      "sealed", "owner", "commitment", "phase", "commit_tx", "commit_block", "register_tx", "register_block", "reveal_after", "reveal_before",
      "years", "duration", "expires_at", "price", "name_prepaid_code_id", "registered_at", "failed_at", "error", "created_at",
      "updated_at", "version", "live", "order_hash", "method", "moves", "links_refused_at", "balance_source", "balance_used"
    ]
  has "sx_badge_service_name_invoices" ["invoice_id", "registration_id", "provider_ref", "late_alerted_at", "late_alerted_units", "late_support"]
  has "sx_badge_service_name_balances" ["registration_id", "amount", "credited", "refunded", "reserved_by", "refunded_at", "created_at", "updated_at"]
  has "sx_badge_service_name_late_credits" ["invoice_id", "credit_no", "registration_id", "units", "invoice_rate", "current_rate", "rate_used", "cents", "created_at"]
  has "sx_badge_service_payments" balancePaymentColumns
  has "sx_badge_service_name_record_edits" ["label_hash", "owner", "nonce", "record_key", "record_value", "deadline", "signature", "body_hash", "state", "revert", "mined_tx", "created_at"]
  has "sx_badge_service_name_transactions" ["registration_id", "name_record_edit_id", "kind", "raw", "tx_hash", "registrar_nonce", "sent_at", "mined_block", "status"]

testNamesDownUp :: IO ()
testNamesDownUp = withServiceStore $ \st -> do
  let names = dropWhile ((/= "20261010_names") . name) storeServiceSchemaMigrations
      badgeColumns = mapM (fmap sort . columnsOf st) badgeTables
  map name names `shouldBe` ["20261010_names", "20261022_drop_taken_codes", "20261022_name_balances", "20261022_name_late_wait"]
  badgeBefore <- badgeColumns
  code <- issue st 1 usable
  placeCode st "r1" ownerA (order "alicebakery" 1) code `shouldReturn` Right ()
  placePaid st "r2" ownerB (order "bobsbakery" 2) (at 60) `shouldReturn` Right ()
  placePaid st "r3" ownerB (order "carolsbakery" 3) (at 60) `shouldReturn` Right ()
  withTransaction st $ \db -> insertPayment db "inv-r3"
  runMigrations st $ MTRDown (mapMaybe toDownMigration names)
  mapM (columnsOf st) namesTables `shouldReturn` map (const []) namesTables
  mapM (invoiceCount st) ["inv-r2", "inv-r3"] `shouldReturn` [0, 1]
  badgeColumns `shouldReturn` map (filter (`notElem` balancePaymentColumns)) badgeBefore
  runMigrations st $ MTRUp names
  mapM (fmap null . columnsOf st) namesTables `shouldReturn` map (const False) namesTables
  badgeColumns `shouldReturn` badgeBefore

testDropTakenCodes :: IO ()
testDropTakenCodes = withServiceStore $ \st -> do
  let dropCodes = [m | m@Migration {name = "20261022_drop_taken_codes"} <- storeServiceSchemaMigrations]
      issuedCode = (,) <$> (elem "issued_code_id" <$> columnsOf st "sx_badge_service_name_registrations") <*> indexExists st "sx_badge_service_idx_name_registrations_issued_code"
      worth r = (nrPhase r, nrPrice r, nrLive r, nrVersion r)
  length dropCodes `shouldBe` 1
  runMigrations st $ MTRDown (mapMaybe toDownMigration dropCodes)
  issuedCode `shouldReturn` (True, True)
  code <- issue st 1 usable
  placePaid st "r1" ownerA (order "alicebakery" 1) (at 60) `shouldReturn` Right ()
  placePaid st "r2" ownerB (order "bobsbakery" 2) (at 60) `shouldReturn` Right ()
  withTransaction st $ \db -> DB.execute db "UPDATE sx_badge_service_name_registrations SET phase = 'code_issued', issued_code_id = ? WHERE registration_id = 'r1'" (Only $ pcId code)
  r2 <- reg st "r2"
  write st r2 {nrPhase = PTaken} `shouldReturn` Written 2
  taken <- reg st "r2"
  runMigrations st $ MTRUp dropCodes
  issuedCode `shouldReturn` (False, False)
  worth <$> reg st "r1" `shouldReturn` (PTaken, 0, False, 2)
  reg st "r2" `shouldReturn` taken
  withTransaction st (\db -> getPrepaidCodeById db (pcId code)) `shouldReturn` Just code
  runMigrations st $ MTRDown (mapMaybe toDownMigration dropCodes)
  issuedCode `shouldReturn` (True, True)
  worth <$> reg st "r1" `shouldReturn` (PTaken, 0, False, 2)
  withTransaction st (\db -> DB.execute db "UPDATE sx_badge_service_name_registrations SET issued_code_id = ? WHERE registration_id = 'r1'" (Only $ pcId code + 1)) `shouldThrow` foreignKeyViolation

testNameBalancesMigration :: IO ()
testNameBalancesMigration = withServiceStore $ \st -> do
  let balances = [m | m@Migration {name = "20261022_name_balances"} <- storeServiceSchemaMigrations]
      applied :: IO (Bool, Bool, Bool)
      applied = (,,) <$> (not . null <$> columnsOf st "sx_badge_service_name_balances") <*> (elem "balance_source" <$> columnsOf st "sx_badge_service_name_registrations") <*> (elem "confirmed_units" <$> columnsOf st "sx_badge_service_payments")
      live :: Text -> IO Bool
      live rid = withTransaction st $ \db -> DB.query db "SELECT live FROM sx_badge_service_name_registrations WHERE registration_id = ?" (Only rid) >>= \case
        [Only (DB.BI l)] -> pure l
        rows -> fail $ "registration " <> show rid <> ": " <> show (length rows) <> " rows"
      balanceOf :: Text -> IO (Maybe (Int, Int, Int, Maybe BalanceHold))
      balanceOf rid = withTransaction st $ \db -> fmap (\Balance {blAmount, blCredited, blRefunded, blHold} -> (blAmount, blCredited, blRefunded, blHold)) <$> getBalance db (RegistrationId rid) t0
  length balances `shouldBe` 1
  runMigrations st $ MTRDown (mapMaybe toDownMigration balances)
  applied `shouldReturn` (False, False, False)
  code <- issue st 1 usable
  withTransaction st $ \db -> do
    insertInvoiceRow db "inv-paid" (at 60) "paid"
    insertOldRegistration db "paid" 1 "taken" price Nothing (Just "inv-paid")
    insertOldRegistration db "code" 2 "taken" 0 (Just $ pcId code) Nothing
    insertInvoiceRow db "inv-converted" (at 60) "paid"
    insertOldRegistration db "converted" 3 "taken" 0 Nothing (Just "inv-converted")
    insertInvoiceRow db "inv-queued" (at 60) "paid"
    insertOldRegistration db "queued" 4 "queued" price Nothing (Just "inv-queued")
  runMigrations st $ MTRUp balances
  applied `shouldReturn` (True, True, True)
  mapM live ["paid", "code", "converted", "queued"] `shouldReturn` [False, False, False, True]
  mapM balanceOf ["paid", "code", "converted", "queued"] `shouldReturn` [Just (price, price, 0, Nothing), Nothing, Nothing, Nothing]
  runMigrations st $ MTRDown (mapMaybe toDownMigration balances)
  applied `shouldReturn` (False, False, False)
  mapM live ["paid", "queued"] `shouldReturn` [False, True]
  runMigrations st $ MTRUp balances
  balanceOf "paid" `shouldReturn` Just (price, price, 0, Nothing)

-- | A registration row as the code before the balances wrote it, paid by the invoice or the code given.
insertOldRegistration :: DB.Connection -> Text -> Word8 -> Text -> Int -> Maybe Int64 -> Maybe Text -> IO ()
insertOldRegistration db rid n phase cents codeId invId = do
  let OrderFields {ofLabelHash = LabelHash lh, ofLength, ofDuration, ofCommitment, ofSealed} = order ("alicebakery" <> rid) n
  DB.execute
    db
    ( "INSERT INTO sx_badge_service_name_registrations (registration_id, version, label_hash, length, duration, owner, commitment, sealed, phase, years,"
        <> " price, method, name_prepaid_code_id, live, order_hash, created_at, updated_at) VALUES (?,?,?,?,?,?,?,?,?,?, ?,?,?,?,?,?,?)"
    )
    ( (rid, 1 :: Int, DB.Binary lh, ofLength, fromIntegral ofDuration :: Int64, DB.Binary $ unAddress ownerA, DB.Binary ofCommitment, ofSealed, phase, twoYears)
        :. (cents, ("btc" :: Text) <$ invId, codeId, DB.BI True, DB.Binary $ digestFixture n, t0, t0)
    )
  mapM_ (\i -> DB.execute db "INSERT INTO sx_badge_service_name_invoices (invoice_id, registration_id, provider_ref, created_at) VALUES (?,?,?,?)" (i, rid, "ref-" <> i, t0)) invId

testOrderHashFields :: IO ()
testOrderHashFields = do
  let o = order "alicebakery" 1
      paid = OTInvoice btc price
      base = orderHash ownerA o paid
      variants =
        [ ("labelHash" :: Text, orderHash ownerA o {ofLabelHash = labelHash "alicebakers"} paid),
          ("length", orderHash ownerA o {ofLength = 12} paid),
          ("duration", orderHash ownerA o {ofDuration = 69292801} paid),
          ("owner", orderHash ownerB o paid),
          ("commitment", orderHash ownerA o {ofCommitment = digestFixture 2} paid),
          ("sealed", orderHash ownerA o {ofSealed = ofSealed o <> "x"} paid),
          ("method", orderHash ownerA o (OTInvoice (SPMCard CPStripe) price)),
          ("price", orderHash ownerA o (OTInvoice btc (price + 1))),
          ("code instead of invoice", orderHash ownerA o (OTCode $ digestFixture 3)),
          ("another code", orderHash ownerA o (OTCode $ digestFixture 4))
        ]
  orderHash ownerA (order "alicebakery" 1) paid `shouldBe` base
  forM_ variants $ \(field, h) -> (field, h == base) `shouldBe` (field, False)
  orderHash ownerA o (OTCode $ digestFixture 3) `shouldNotBe` orderHash ownerA o (OTCode $ digestFixture 4)

testOrderHashBoundaries :: IO ()
testOrderHashBoundaries = do
  let o = order "alicebakery" 1
  orderHash ownerA o {ofSealed = "ab"} (OTCode "c") `shouldNotBe` orderHash ownerA o {ofSealed = "a"} (OTCode "bc")

testCreatePaid :: IO ()
testCreatePaid = withServiceStore $ \st -> do
  let o = order "alicebakery" 1
  placePaid st "r1" ownerA o (at 60) `shouldReturn` Right ()
  r <- reg st "r1"
  r
    `shouldBe` NameRegistration
      { nrId = RegistrationId "r1",
        nrVersion = 1,
        nrLabelHash = ofLabelHash o,
        nrLength = 11,
        nrDuration = 69292800,
        nrOwner = ownerA,
        nrCommitment = ofCommitment o,
        nrSealed = ofSealed o,
        nrPhase = PUnpaid,
        nrYears = twoYears,
        nrPrice = price,
        nrPayment = RPInvoice btc (InvoiceId "inv-r1"),
        nrBalanceSource = Nothing,
        nrBalanceUsed = 0,
        nrReveal = Nothing,
        nrCommitTx = Nothing,
        nrCommitBlock = Nothing,
        nrRegisterTx = Nothing,
        nrRegisterBlock = Nothing,
        nrRevealAfter = Nothing,
        nrRevealBefore = Nothing,
        nrExpiresAt = Nothing,
        nrRegisteredAt = Nothing,
        nrFailedAt = Nothing,
        nrError = Nothing,
        nrLive = True,
        nrOrderHash = orderHash ownerA o (OTInvoice btc price),
        nrMoves = 0,
        nrLinksRefusedAt = Nothing,
        nrCreatedAt = t0,
        nrUpdatedAt = t0
      }

testCreateCode :: IO ()
testCreateCode = withServiceStore $ \st -> do
  let o = order "alicebakery" 1
  code@PrepaidCode {pcId, pcCodeHash} <- issue st 1 usable
  placeCode st "r1" ownerA o code `shouldReturn` Right ()
  r <- reg st "r1"
  (nrPhase r, nrLive r, nrPrice r, nrPayment r, nrVersion r) `shouldBe` (PQueued, True, 0, RPCode pcId, 1)
  nrOrderHash r `shouldBe` orderHash ownerA o (OTCode pcCodeHash)
  held <- withTransaction st $ \db -> getPrepaidCode db pcCodeHash
  (pcReservedBy =<< held, pcReservedAt =<< held) `shouldBe` (Just (RegistrationId "r1"), Just t0)

testLiveUnique :: IO ()
testLiveUnique = withServiceStore $ \st -> do
  let o = order "alicebakery" 1
  placePaid st "r1" ownerA o (at 60) `shouldReturn` Right ()
  placePaid st "r2" ownerA o {ofCommitment = digestFixture 2} (at 60) `shouldReturn` Left RRLiveOrder
  placePaid st "r3" ownerB o (at 60) `shouldReturn` Left RRLiveOrder
  code <- issue st 9 usable
  placeCode st "r4" ownerA (order "alicebakery" 3) code `shouldReturn` Left RRLiveOrder
  placePaid st "r5" ownerB (order "alicebakery" 5) (at 60) `shouldReturn` Right ()
  existing <- withTransaction st $ \db -> mapM (getRegistration db . RegistrationId) ["r2", "r3", "r4"]
  existing `shouldBe` [Nothing, Nothing, Nothing]
  free <- withTransaction st $ \db -> getPrepaidCodeById db (pcId code)
  (pcReservedBy =<< free) `shouldBe` Nothing

testCodeNotReservable :: IO ()
testCodeNotReservable = withServiceStore $ \st -> do
  code <- issue st 1 usable
  placeCode st "r1" ownerA (order "alicebakery" 1) code `shouldReturn` Right ()
  placeCode st "r2" ownerB (order "bobsbakery" 2) code `shouldReturn` Left RRCodeNotReservable
  withTransaction st (\db -> getRegistration db (RegistrationId "r2")) `shouldReturn` Nothing
  held <- withTransaction st $ \db -> getPrepaidCodeById db (pcId code)
  (pcReservedBy =<< held) `shouldBe` Just (RegistrationId "r1")

testOrderHashLookup :: IO ()
testOrderHashLookup = withServiceStore $ \st -> do
  let o = order "alicebakery" 1
  placePaid st "r1" ownerA o (at 1) `shouldReturn` Right ()
  r <- reg st "r1"
  closed <- withTransaction st $ \db -> closeOverdue db r (at 2)
  closed `shouldBe` Just 2
  found <- withTransaction st $ \db -> getRegistrationByOrderHash db (orderHash ownerA o (OTInvoice btc price))
  (nrId <$> found, nrLive <$> found) `shouldBe` (Just (RegistrationId "r1"), Just False)
  withTransaction st (\db -> getRegistrationByOrderHash db (orderHash ownerA o (OTInvoice btc (price + 1)))) `shouldReturn` Nothing

testLiveOrdersOverdue :: IO ()
testLiveOrdersOverdue = withServiceStore $ \st -> do
  let o = order "alicebakery" 1
      live now = withTransaction st $ \db -> map (\LiveOrder {loRegistration, loOverdue} -> (nrId loRegistration, loOverdue)) <$> getLiveOrders db (ofCommitment o) ownerA (ofLabelHash o) now
      setStatus status = withTransaction st $ \db -> DB.execute db "UPDATE sx_badge_service_invoices SET status = ? WHERE invoice_id = ?" (status :: Text, "inv-r1" :: Text)
  placePaid st "r1" ownerA o (at 60) `shouldReturn` Right ()
  placePaid st "r2" ownerB (order "alicebakery" 2) (at 60) `shouldReturn` Right ()
  live (at 30) `shouldReturn` [(RegistrationId "r1", False)]
  live (at 61) `shouldReturn` [(RegistrationId "r1", True)]
  withTransaction st (\db -> map (nrId . loRegistration) <$> getLiveOrders db (ofCommitment o) ownerB (labelHash "zzzzzzzz") (at 30))
    `shouldReturn` [RegistrationId "r1"]
  setStatus "expired"
  live (at 61) `shouldReturn` [(RegistrationId "r1", True)]
  setStatus "paid"
  live (at 61) `shouldReturn` [(RegistrationId "r1", False)]

testCloseOverdue :: IO ()
testCloseOverdue = withServiceStore $ \st -> do
  let o = order "alicebakery" 1
      o' = o {ofCommitment = digestFixture 2, ofSealed = "new"}
  placePaid st "r1" ownerA o (at 60) `shouldReturn` Right ()
  placed <- withTransaction st $ \db -> do
    [LiveOrder {loRegistration, loOverdue = True}] <- getLiveOrders db (ofCommitment o') ownerA (ofLabelHash o') (at 61)
    void $ closeOverdue db loRegistration (at 61)
    insertInvoiceRow db "inv-r2" (at 120) "open"
    createPaidRegistration db (newReg "r2" ownerA o') (OTInvoice btc price) (paidOrder "inv-r2") (at 61)
  placed `shouldBe` Right ()
  old <- reg st "r1"
  (nrLive old, nrPhase old, nrVersion old) `shouldBe` (False, PUnpaid, 2)
  invoiceStatus st "inv-r1" `shouldReturn` "expired"
  invoiceStatus st "inv-r2" `shouldReturn` "open"

testCloseOverdueStale :: IO ()
testCloseOverdueStale = withServiceStore $ \st -> do
  placePaid st "r1" ownerA (order "alicebakery" 1) (at 60) `shouldReturn` Right ()
  r <- reg st "r1"
  write st r {nrPhase = PQueued} `shouldReturn` Written 2
  withTransaction st (\db -> closeOverdue db r (at 61)) `shouldReturn` Nothing
  invoiceStatus st "inv-r1" `shouldReturn` "open"
  (nrLive &&& nrPhase) <$> reg st "r1" `shouldReturn` (True, PQueued)

invoiceStatus :: DBStore -> Text -> IO Text
invoiceStatus st invId = withTransaction st $ \db ->
  DB.query db "SELECT status FROM sx_badge_service_invoices WHERE invoice_id = ?" (Only invId) >>= \case
    [Only status] -> pure status
    rows -> fail $ "invoice " <> show invId <> ": " <> show (length rows) <> " rows"

reveal :: NameReveal
reveal =
  NameReveal
    { rvLabel = "alicebakery",
      rvSecret = digestFixture 7,
      rvContactLink = Just "https://smp8.simplex.im/a#abc",
      rvChannelLink = Nothing,
      rvNonce = 3,
      rvDeadline = 1792915140,
      rvSignature = signature
    }

testVersionBumps :: IO ()
testVersionBumps = withServiceStore $ \st -> do
  code <- issue st 1 usable
  placeCode st "r1" ownerA (order "alicebakery" 1) code `shouldReturn` Right ()
  r1 <- reg st "r1"
  write st r1 {nrPhase = PCommitting, nrCommitTx = Just (TxHash $ digestFixture 8)} `shouldReturn` Written 2
  r2 <- reg st "r1"
  write st r2 {nrPhase = PCommitted, nrRevealAfter = Just 1792828800, nrRevealBefore = Just 1792915140} `shouldReturn` Written 3
  r3 <- reg st "r1"
  write st r3 {nrReveal = Just reveal} `shouldReturn` Written 4
  write st r3 {nrLinksRefusedAt = Just (at 5)} `shouldReturn` VersionChanged
  write st r1 {nrPhase = PFailed} `shouldReturn` VersionChanged
  r4 <- reg st "r1"
  (nrVersion r4, nrPhase r4, nrReveal r4, nrLinksRefusedAt r4) `shouldBe` (4, PCommitted, Just reveal, Nothing)

testRoundTrip :: IO ()
testRoundTrip = withServiceStore $ \st -> do
  code <- issue st 1 usable
  placeCode st "r1" ownerA (order "alicebakery" 1) code `shouldReturn` Right ()
  r <- reg st "r1"
  let changed =
        r
          { nrPhase = PRegistered,
            nrReveal = Just reveal {rvChannelLink = Just "simplex:/c#xyz"},
            nrCommitTx = Just (TxHash $ digestFixture 8),
            nrCommitBlock = Just 100,
            nrRegisterTx = Just (TxHash $ digestFixture 9),
            nrRegisterBlock = Just 120,
            nrRevealAfter = Just 1792828800,
            nrRevealBefore = Just 1792915140,
            nrExpiresAt = Just 1861833600,
            nrRegisteredAt = Just (at 3),
            nrFailedAt = Just (at 2),
            nrError = Just "The name was not signed in time",
            nrLinksRefusedAt = Just (at 4),
            nrMoves = 2
          }
  write st changed `shouldReturn` Written 2
  reg st "r1" `shouldReturn` changed {nrVersion = 2, nrLive = False, nrUpdatedAt = at 1}
  write st changed {nrVersion = 2, nrReveal = Nothing} `shouldReturn` Written 3
  nrReveal <$> reg st "r1" `shouldReturn` Nothing

testLiveFollowsPhase :: IO ()
testLiveFollowsPhase = withServiceStore $ \st -> do
  placePaid st "r1" ownerA (order "alicebakery" 1) (at 60) `shouldReturn` Right ()
  code <- issue st 1 usable
  placeCode st "r2" ownerB (order "bobsbakery" 2) code `shouldReturn` Right ()
  let step rid phase expected = do
        r <- reg st rid
        write st r {nrPhase = phase} `shouldReturn` Written (nrVersion r + 1)
        (nrPhase &&& nrLive) <$> reg st rid `shouldReturn` (phase, expected)
  step "r1" PQueued True
  step "r1" PTaken True
  step "r1" PFailed True
  step "r1" PTaken True
  step "r2" PRegistering True
  step "r2" PTaken False
  placePaid st "r3" ownerA (order "carolsbakery" 3) (at 60) `shouldReturn` Right ()
  r3 <- reg st "r3"
  write st r3 {nrLive = False} `shouldReturn` Written 2
  nrLive <$> reg st "r3" `shouldReturn` False

testOrderLive :: IO ()
testOrderLive = withServiceStore $ \st -> do
  placePaid st "r1" ownerA (order "alicebakery" 1) (at 60) `shouldReturn` Right ()
  paid <- reg st "r1"
  let code = paid {nrPayment = RPCode 1}
      liveIn r phase = (phase, orderLive r {nrPhase = phase})
  map (liveIn paid) [minBound .. maxBound]
    `shouldBe` [(PUnpaid, True), (PQueued, True), (PCommitting, True), (PCommitted, True), (PRegistering, True), (PRegistered, False), (PTaken, True), (PFailed, True)]
  map (liveIn code) [PTaken, PFailed, PRegistered] `shouldBe` [(PTaken, False), (PFailed, True), (PRegistered, False)]
  orderLive paid {nrLive = False} `shouldBe` False
  orderLive paid {nrLive = False, nrPhase = PQueued} `shouldBe` True

testReorderAfterRegistered :: IO ()
testReorderAfterRegistered = withServiceStore $ \st -> do
  let o = order "alicebakery" 1
  code <- issue st 1 usable
  placeCode st "r1" ownerA o code `shouldReturn` Right ()
  placePaid st "r2" ownerA o {ofCommitment = digestFixture 2} (at 60) `shouldReturn` Left RRLiveOrder
  r <- reg st "r1"
  write st r {nrPhase = PRegistered, nrRegisteredAt = Just (at 1)} `shouldReturn` Written 2
  nrLive <$> reg st "r1" `shouldReturn` False
  placePaid st "r3" ownerA o {ofCommitment = digestFixture 2} (at 60) `shouldReturn` Right ()

testLatePayment :: IO ()
testLatePayment = withServiceStore $ \st -> do
  let o = order "alicebakery" 1
      o' = o {ofCommitment = digestFixture 2, ofSealed = "new"}
  placePaid st "r1" ownerA o (at 60) `shouldReturn` Right ()
  closed <- reg st "r1"
  withTransaction st (\db -> closeOverdue db closed (at 61)) `shouldReturn` Just 2
  r1 <- reg st "r1"
  write st r1 {nrPhase = PQueued} `shouldReturn` NotLive
  reg st "r1" `shouldReturn` r1
  placePaid st "r2" ownerA o' (at 120) `shouldReturn` Right ()
  write st r1 {nrPhase = PQueued} `shouldReturn` NotLive
  reg st "r1" `shouldReturn` r1
  (nrPhase &&& nrLive) <$> reg st "r2" `shouldReturn` (PUnpaid, True)
  write st r1 {nrVersion = 1, nrPhase = PQueued} `shouldReturn` VersionChanged

testWriteMeetsLiveOrder :: IO ()
testWriteMeetsLiveOrder = withServiceStore $ \st -> do
  let held = order "bobsbakery" 2
  placePaid st "r1" ownerA (order "alicebakery" 1) (at 60) `shouldReturn` Right ()
  placePaid st "r2" ownerA held (at 60) `shouldReturn` Right ()
  placePaid st "r3" ownerB (order "carolsbakery" 3) (at 60) `shouldReturn` Right ()
  r1 <- reg st "r1"
  write st r1 {nrPhase = PTaken} `shouldReturn` Written 2
  taken <- reg st "r1"
  let retarget o = withTransaction st $ \db -> retargetRegistration db taken o 10000 (at 6)
  retarget held {ofCommitment = digestFixture 4} `shouldReturn` LiveOrderExists
  r3 <- reg st "r3"
  retarget (order "carolsbakery" 5) {ofCommitment = nrCommitment r3} `shouldReturn` LiveOrderExists
  reg st "r1" `shouldReturn` taken
  retarget (order "davesbakery" 6) `shouldReturn` Written 3

testRetarget :: IO ()
testRetarget = withServiceStore $ \st -> do
  let o = order "alicebakery" 1
      o' = order "alicebakers" 2
  placePaid st "r1" ownerA o (at 60) `shouldReturn` Right ()
  r <- reg st "r1"
  let taken =
        r
          { nrPhase = PTaken,
            nrReveal = Just reveal,
            nrCommitTx = Just (TxHash $ digestFixture 8),
            nrCommitBlock = Just 100,
            nrRegisterTx = Just (TxHash $ digestFixture 9),
            nrRegisterBlock = Just 120,
            nrRevealAfter = Just 1,
            nrRevealBefore = Just 2,
            nrLinksRefusedAt = Just (at 4),
            nrFailedAt = Just (at 5),
            nrError = Just "failed"
          }
  write st taken `shouldReturn` Written 2
  withTransaction st (\db -> retargetRegistration db taken {nrVersion = 2} o' 10000 (at 6)) `shouldReturn` Written 3
  r' <- reg st "r1"
  r'
    `shouldBe` r
      { nrVersion = 3,
        nrLabelHash = ofLabelHash o',
        nrCommitment = ofCommitment o',
        nrSealed = ofSealed o',
        nrPrice = 10000,
        nrPhase = PQueued,
        nrMoves = 1,
        nrUpdatedAt = at 6
      }
  withTransaction st (\db -> retargetRegistration db taken {nrVersion = 2} o 10000 (at 7)) `shouldReturn` VersionChanged

testFind :: IO ()
testFind = withServiceStore $ \st -> do
  forM_ [1 .. 52 :: Int] $ \i -> withTransaction st $ \db -> do
    let rid = "r" <> T.pack (show i)
        o = order ("alicebakery" <> T.pack (show i)) (fromIntegral i)
    insertInvoiceRow db ("inv-" <> rid) (at 60) "open"
    createPaidRegistration db (newReg rid ownerA o) (OTInvoice btc price) (paidOrder $ "inv-" <> rid) (at i) `shouldReturn` Right ()
  placePaid st "other" ownerB (order "bobsbakery" 99) (at 60) `shouldReturn` Right ()
  found <- withTransaction st $ \db -> findRegistrations db ownerA
  map nrId found `shouldBe` [RegistrationId $ "r" <> T.pack (show i) | i <- [52, 51 .. 3 :: Int]]
  withTransaction st (\db -> findRegistrations db (address "0x0000000000000000000000000000000000000001")) `shouldReturn` []

testSameSecondOrder :: IO ()
testSameSecondOrder = withServiceStore $ \st -> do
  let o = order "alicebakery" 1
      ids = ["r2", "r3", "r1"]
  forM_ (zip ids [10 ..]) $ \(rid, n) -> do
    placePaid st rid ownerA (order ("alicebakery" <> rid) n) (at 60) `shouldReturn` Right ()
    r <- reg st rid
    write st r {nrPhase = PQueued} `shouldReturn` Written 2
  map nrId <$> withTransaction st (`findRegistrations` ownerA) `shouldReturn` map RegistrationId ["r3", "r2", "r1"]
  map nrId <$> withTransaction st activeRegistrations `shouldReturn` map RegistrationId ["r1", "r2", "r3"]
  forM_ ["r5", "r4"] $ \rid -> do
    placePaid st rid ownerB o (at 60) `shouldReturn` Right ()
    r <- reg st rid
    withTransaction st (\db -> closeOverdue db r (at 61)) `shouldReturn` Just 2
  found <- withTransaction st $ \db -> getRegistrationByOrderHash db (orderHash ownerB o (OTInvoice btc price))
  nrId <$> found `shouldBe` Just (RegistrationId "r5")

testLabelRegistering :: IO ()
testLabelRegistering = withServiceStore $ \st -> do
  let o = order "alicebakery" 1
      registering = withTransaction st $ \db -> isLabelRegistering db (ofLabelHash o)
  code <- issue st 1 usable
  placeCode st "r1" ownerA o code `shouldReturn` Right ()
  registering `shouldReturn` False
  r <- reg st "r1"
  void $ write st r {nrPhase = PRegistering}
  registering `shouldReturn` True
  r' <- reg st "r1"
  void $ write st r' {nrPhase = PRegistered}
  registering `shouldReturn` False

testActive :: IO ()
testActive = withServiceStore $ \st -> do
  let phases = [PUnpaid ..]
  forM_ (zip [1 ..] phases) $ \(i, phase) -> do
    let rid = "r" <> T.pack (show i)
    placePaid st rid ownerA (order ("alicebakery" <> T.pack (show i)) i) (at 60) `shouldReturn` Right ()
    r <- reg st rid
    void $ write st r {nrPhase = phase}
  active <- withTransaction st activeRegistrations
  sort (map (fromEnum . nrPhase) active) `shouldBe` map fromEnum [PQueued, PCommitting, PCommitted, PRegistering]

testUnpaidCleanup :: IO ()
testUnpaidCleanup = withServiceStore $ \st -> do
  placePaid st "old" ownerA (order "alicebakery" 1) (at 60) `shouldReturn` Right ()
  placePaid st "funded" ownerA (order "bobsbakery" 2) (at 60) `shouldReturn` Right ()
  placePaid st "queued" ownerA (order "carolsbakery" 3) (at 60) `shouldReturn` Right ()
  placePaid st "open" ownerA (order "davesbakery" 4) (at 120) `shouldReturn` Right ()
  withTransaction st $ \db -> insertPayment db "inv-funded"
  r <- reg st "queued"
  void $ write st r {nrPhase = PQueued}
  candidates <- withTransaction st $ \db -> unpaidCleanupCandidates db (at 61)
  map (\(RegistrationId rid, InvoiceId invId) -> (rid, invId)) candidates `shouldBe` [("old", "inv-old")]
  withTransaction st (\db -> mapM (deleteUnpaidRegistration db . RegistrationId) ["old", "funded", "queued"]) `shouldReturn` [Right (), Left CKPaid, Left CKPaid]
  withTransaction st (\db -> getRegistration db (RegistrationId "old")) `shouldReturn` Nothing
  invoiceCount st "inv-old" `shouldReturn` 0
  invoiceCount st "inv-funded" `shouldReturn` 1
  nrPhase <$> reg st "funded" `shouldReturn` PUnpaid

insertPayment :: DB.Connection -> Text -> IO ()
insertPayment db invId =
  DB.execute
    db
    "INSERT INTO sx_badge_service_payments (payment_id, invoice_id, provider, status, created_at, updated_at) VALUES (?,?,?,?,?,?)"
    (invId, invId, "crypto" :: Text, "pending" :: Text, t0, t0)

foreignKeyViolation :: Selector DB.SQLError
#if defined(dbPostgres)
foreignKeyViolation e = case constraintViolation e of
  Just ForeignKeyViolation {} -> True
  _ -> False
#else
foreignKeyViolation e = sqlError e == ErrorConstraint && "FOREIGN KEY" `T.isInfixOf` sqlErrorDetails e
#endif

indexExists :: DBStore -> Text -> IO Bool
indexExists st index = withTransaction st $ \db -> do
  rows <- DB.query db indexQuery (Only index)
  pure $ not $ null (rows :: [Only Text])
  where
#if defined(dbPostgres)
    indexQuery = "SELECT indexname FROM pg_indexes WHERE schemaname = current_schema() AND indexname = ?"
#else
    indexQuery = "SELECT name FROM sqlite_master WHERE type = 'index' AND name = ?"
#endif

invoiceCount :: DBStore -> Text -> IO Int
invoiceCount st invId = withTransaction st $ \db ->
  DB.query db "SELECT COUNT(*) FROM sx_badge_service_invoices WHERE invoice_id = ?" (Only invId) >>= \case
    [Only n] -> pure n
    rows -> fail $ "count: " <> show (length rows) <> " rows"

testFailedCodeOrders :: IO ()
testFailedCodeOrders = withServiceStore $ \st -> do
  codes@[oldCode, _, releasedCode, _] <- mapM (\n -> issue st n usable) [1, 2, 3, 4]
  forM_ (zip3 ["old", "recent", "released", "queued"] [1 ..] codes) $ \(rid, n, code) ->
    placeCode st rid ownerA (order ("alicebakery" <> rid) n) code `shouldReturn` Right ()
  let failAt rid t = reg st rid >>= \r -> void (write st r {nrPhase = PFailed, nrFailedAt = Just t})
  failAt "old" (at 1)
  failAt "recent" (at 30)
  failAt "released" (at 1)
  withTransaction st (\db -> releasePrepaidCode db (pcId releasedCode) (RegistrationId "released")) `shouldReturn` True
  withTransaction st (\db -> failedCodeOrdersBefore db (at 10)) `shouldReturn` [(RegistrationId "old", pcId oldCode)]

testIssueCode :: IO ()
testIssueCode = withServiceStore $ \st -> do
  code <- issue st 1 usable {npcMinLength = 6, npcYears = 5, npcPaymentStatus = CPSFree, npcExpiresAt = Just (at 100)}
  (pcMinLength code, pcYears code, pcPaymentStatus code, pcExpiresAt code, pcCreatedAt code) `shouldBe` (6, 5, CPSFree, Just (at 100), t0)
  withTransaction st (\db -> insertPrepaidCode db usable {npcCodeHash = digestFixture 1} t0) `shouldReturn` Left CodeHashTaken
  withTransaction st (\db -> getPrepaidCode db (digestFixture 1)) `shouldReturn` Just code

testReserveReleaseSpend :: IO ()
testReserveReleaseSpend = withServiceStore $ \st -> do
  PrepaidCode {pcId} <- issue st 1 usable
  placePaid st "r1" ownerA (order "alicebakery" 1) (at 60) `shouldReturn` Right ()
  placePaid st "r2" ownerB (order "bobsbakery" 2) (at 60) `shouldReturn` Right ()
  let r1 = RegistrationId "r1"
      r2 = RegistrationId "r2"
      run :: (DB.Connection -> IO a) -> IO a
      run = withTransaction st
  run (\db -> releasePrepaidCode db pcId r1) `shouldReturn` False
  run (\db -> spendPrepaidCode db pcId r1 t0) `shouldReturn` False
  run (\db -> reservePrepaidCode db pcId r1 (at 1)) `shouldReturn` True
  run (\db -> reservePrepaidCode db pcId r2 (at 1)) `shouldReturn` False
  run (\db -> releasePrepaidCode db pcId r2) `shouldReturn` False
  run (\db -> spendPrepaidCode db pcId r2 (at 2)) `shouldReturn` False
  run (\db -> releasePrepaidCode db pcId r1) `shouldReturn` True
  run (\db -> reservePrepaidCode db pcId r2 (at 2)) `shouldReturn` True
  run (\db -> spendPrepaidCode db pcId r2 (at 3)) `shouldReturn` True
  run (\db -> releasePrepaidCode db pcId r2) `shouldReturn` False
  run (\db -> reservePrepaidCode db pcId r1 (at 4)) `shouldReturn` False
  run (\db -> spendPrepaidCode db pcId r2 (at 4)) `shouldReturn` False
  spent <- run $ \db -> getPrepaidCodeById db pcId
  (pcReservedBy =<< spent, pcSpentAt =<< spent) `shouldBe` (Just r2, Just (at 3))

testReserveRefused :: IO ()
testReserveRefused = withServiceStore $ \st -> do
  placePaid st "r1" ownerA (order "alicebakery" 1) (at 60) `shouldReturn` Right ()
  unpaid <- issue st 1 usable {npcPaymentStatus = CPSUnpaid}
  expired <- issue st 2 usable {npcExpiresAt = Just (at 10)}
  revoked <- issue st 3 usable
  free <- issue st 4 usable {npcPaymentStatus = CPSFree, npcExpiresAt = Just (at 30)}
  withTransaction st (\db -> revokePrepaidCode db (pcCodeHash revoked) t0) `shouldReturn` Revoked (pcId revoked)
  let reserve code = withTransaction st $ \db -> reservePrepaidCode db (pcId code) (RegistrationId "r1") (at 20)
  mapM reserve [unpaid, expired, revoked] `shouldReturn` [False, False, False]
  reserve free `shouldReturn` True

testOneCodePerRegistration :: IO ()
testOneCodePerRegistration = withServiceStore $ \st -> do
  first <- issue st 1 usable
  second <- issue st 2 usable
  placeCode st "r1" ownerA (order "alicebakery" 1) first `shouldReturn` Right ()
  withTransaction st (\db -> reservePrepaidCode db (pcId second) (RegistrationId "r1") t0) `shouldThrow` anyException
  free <- withTransaction st $ \db -> getPrepaidCodeById db (pcId second)
  (pcReservedBy =<< free) `shouldBe` Nothing

testRevoke :: IO ()
testRevoke = withServiceStore $ \st -> do
  reserved <- issue st 1 usable
  spent <- issue st 2 usable
  unused <- issue st 3 usable
  placeCode st "r1" ownerA (order "alicebakery" 1) reserved `shouldReturn` Right ()
  placeCode st "r2" ownerA (order "bobsbakery" 2) spent `shouldReturn` Right ()
  withTransaction st (\db -> spendPrepaidCode db (pcId spent) (RegistrationId "r2") t0) `shouldReturn` True
  let revoke code = withTransaction st $ \db -> revokePrepaidCode db (pcCodeHash code) (at 1)
  revoke reserved `shouldReturn` Revoked (pcId reserved)
  revoke unused `shouldReturn` Revoked (pcId unused)
  revoke unused `shouldReturn` AlreadyRevoked (pcId unused)
  revoke spent `shouldReturn` AlreadyRedeemed
  withTransaction st (\db -> revokePrepaidCode db (digestFixture 9) t0) `shouldReturn` NoSuchCode
  held <- withTransaction st $ \db -> getPrepaidCodeById db (pcId reserved)
  (pcReservedBy =<< held, pcRevokedAt =<< held) `shouldBe` (Just (RegistrationId "r1"), Just (at 1))

newEdit :: Address -> Word8 -> RecordChange -> NewRecordEdit
newEdit owner n change =
  NewRecordEdit
    { nreLabelHash = labelHash "alicebakery",
      nreOwner = owner,
      nreNonce = 4,
      nreChange = change,
      nreDeadline = 1792832400,
      nreSignature = signature,
      nreBodyHash = digestFixture n
    }

setChannel :: RecordChange
setChannel = RCSetText "simplex.channel" "https://smp9.simplex.im/c#abc"

testTransactions :: IO ()
testTransactions = withServiceStore $ \st -> do
  code <- issue st 1 usable
  placeCode st "r1" ownerA (order "alicebakery" 1) code `shouldReturn` Right ()
  Just editId <- withTransaction st $ \db -> insertRecordEdit db (newEdit ownerA 20 setChannel) t0
  let r1 = RegistrationId "r1"
      tx :: Word8 -> NewTransaction
      tx n = NewTransaction {ntxRaw = digestFixture (n + 100), ntxHash = TxHash $ digestFixture n, ntxNonce = fromIntegral n}
  [commitId, _, registerId] <- withTransaction st $ \db ->
    sequence [insertTransaction db (TxCommit r1) (tx 2) (at 1), insertTransaction db (TxEdit editId) (tx 1) (at 2), insertTransaction db (TxRegister r1) (tx 3) (at 3)]
  unsettled <- withTransaction st pendingTransactions
  map (\NameTransaction {ntFor, ntNonce, ntStatus} -> (ntFor, ntNonce, ntStatus)) unsettled
    `shouldBe` [(TxEdit editId, 1, TSPending), (TxCommit r1, 2, TSPending), (TxRegister r1, 3, TSPending)]
  map ntRaw unsettled `shouldBe` map (\n -> digestFixture (n + 100)) [1, 2, 3]
  let outcome n from status block = withTransaction st $ \db -> recordTransactionOutcome db (TxHash $ digestFixture n) from status block
  outcome 2 TSPending TSMined (Just 77) `shouldReturn` True
  outcome 1 TSPending TSReverted (Just 78) `shouldReturn` True
  outcome 50 TSPending TSMined (Just 79) `shouldReturn` False
  outcome 3 TSMined TSPending Nothing `shouldReturn` False
  map ntFor <$> withTransaction st pendingTransactions `shouldReturn` [TxRegister r1]
  ofRegistration <- withTransaction st $ \db -> registrationTransactions db r1
  map (\NameTransaction {ntId, ntFor, ntMinedBlock, ntStatus, ntSentAt} -> (ntId, ntFor, ntMinedBlock, ntStatus, ntSentAt)) ofRegistration
    `shouldBe` [(commitId, TxCommit r1, Just 77, TSMined, at 1), (registerId, TxRegister r1, Nothing, TSPending, at 3)]
  map ntStatus <$> withTransaction st (\db -> editTransactions db editId) `shouldReturn` [TSReverted]

testTransactionFromStatus :: IO ()
testTransactionFromStatus = withServiceStore $ \st -> do
  code <- issue st 1 usable
  placeCode st "r1" ownerA (order "alicebakery" 1) code `shouldReturn` Right ()
  let outcome from status block = withTransaction st $ \db -> recordTransactionOutcome db (TxHash $ digestFixture 2) from status block
      current = withTransaction st $ \db -> map (\NameTransaction {ntStatus, ntMinedBlock} -> (ntStatus, ntMinedBlock)) <$> registrationTransactions db (RegistrationId "r1")
  void $ withTransaction st $ \db -> insertTransaction db (TxCommit $ RegistrationId "r1") NewTransaction {ntxRaw = "raw", ntxHash = TxHash $ digestFixture 2, ntxNonce = 0} t0
  outcome TSPending TSMined (Just 77) `shouldReturn` True
  outcome TSPending TSDropped Nothing `shouldReturn` False
  current `shouldReturn` [(TSMined, Just 77)]
  outcome TSMined TSPending Nothing `shouldReturn` True
  current `shouldReturn` [(TSPending, Nothing)]

testEditNonceUnique :: IO ()
testEditNonceUnique = withServiceStore $ \st -> do
  let run action = withTransaction st action
  Just first <- run $ \db -> insertRecordEdit db (newEdit ownerA 1 setChannel) t0
  run (\db -> insertRecordEdit db (newEdit ownerA 2 RCClear) t0) `shouldReturn` Nothing
  Just other <- run $ \db -> insertRecordEdit db (newEdit ownerB 3 setChannel) t0
  other `shouldNotBe` first
  run (\db -> settleRecordEdit db first (ESReverted (Just "SignatureExpired"))) `shouldReturn` True
  Just second <- run $ \db -> insertRecordEdit db (newEdit ownerA 2 RCClear) (at 1)
  fmap reId <$> run (\db -> getRecordEditByNonce db ownerA 4) `shouldReturn` Just second
  fmap reState <$> run (\db -> getRecordEditByBody db (digestFixture 1)) `shouldReturn` Just (ESReverted (Just "SignatureExpired"))
  run (\db -> settleRecordEdit db second (ESMined Nothing)) `shouldReturn` True
  run (\db -> insertRecordEdit db (newEdit ownerA 5 setChannel) (at 2)) `shouldReturn` Nothing

testSettleEdit :: IO ()
testSettleEdit = withServiceStore $ \st -> do
  Just editId <- withTransaction st $ \db -> insertRecordEdit db (newEdit ownerA 1 setChannel) t0
  withTransaction st (\db -> settleRecordEdit db editId (ESMined (Just $ TxHash $ digestFixture 9))) `shouldReturn` True
  withTransaction st (\db -> settleRecordEdit db editId (ESReverted Nothing)) `shouldReturn` False
  edit <- withTransaction st $ \db -> getRecordEdit db editId
  edit
    `shouldBe` Just
      RecordEdit
        { reId = editId,
          reLabelHash = labelHash "alicebakery",
          reOwner = ownerA,
          reNonce = 4,
          reChange = setChannel,
          reDeadline = 1792832400,
          reSignature = signature,
          reBodyHash = digestFixture 1,
          reState = ESMined (Just $ TxHash $ digestFixture 9),
          reCreatedAt = t0
        }

testClearEdit :: IO ()
testClearEdit = withServiceStore $ \st -> do
  Just editId <- withTransaction st $ \db -> insertRecordEdit db (newEdit ownerA 1 RCClear) t0
  fmap (\RecordEdit {reChange, reState} -> (reChange, reState)) <$> withTransaction st (\db -> getRecordEdit db editId)
    `shouldReturn` Just (RCClear, ESPending)

testRelayedEditTimes :: IO ()
testRelayedEditTimes = withServiceStore $ \st -> do
  forM_ (zip [1 ..] [0, 10, 20]) $ \(n, m) ->
    withTransaction st $ \db -> void $ insertRecordEdit db (newEdit ownerA n setChannel) {nreNonce = fromIntegral n} (at m)
  withTransaction st (\db -> void $ insertRecordEdit db (newEdit ownerA 9 setChannel) {nreLabelHash = labelHash "bobsbakery", nreNonce = 9} (at 15))
  withTransaction st (\db -> relayedEditTimes db (labelHash "alicebakery") (at 5)) `shouldReturn` [at 10, at 20]

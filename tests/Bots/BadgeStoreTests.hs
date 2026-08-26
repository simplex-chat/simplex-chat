{-# LANGUAGE CPP #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Tests for the CLIENT badge store ("Simplex.Chat.Store.Badges"): the purchase and payment
-- rows a code redemption writes, the issuances the worker collects, and the replica of the
-- service's ledger.
--
-- Registered under the "Supporter badges" hspec path so CI runs it (plan §4 rule 8), but inside
-- 'testBracket': unlike the other two client-side badge modules these tests need a real chat
-- database with a @users@ row, which the plain 'Spec' in "BadgeTests" has no way to provide.
-- No chat controller is started and no service is involved -- the store is exercised directly
-- over a migrated chat database.
--
-- Field selectors are used through record PATTERNS throughout rather than as bare functions:
-- 'StatementEntry' (the wire entry) and 'BadgeLedgerEntry' (the stored one) share almost every
-- field name, and both must be imported with @(..)@ for record construction to resolve, which
-- makes every bare selector ambiguous.
module Bots.BadgeStoreTests (badgeStoreTests) where

import ChatClient
import ChatTests.DBUtils
import ChatTests.Utils
import Control.Concurrent.STM (atomically)
import Control.Exception (finally)
import Control.Monad.Except (ExceptT, runExceptT)
import qualified Data.Aeson as J
import qualified Data.Aeson.KeyMap as JM
import Data.ByteString (ByteString)
import Data.Int (Int64)
import Data.Maybe (isNothing)
import Data.Text (Text)
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime (..), addUTCTime, getCurrentTime, nominalDay, secondsToDiffTime)
import Simplex.Chat.Badges (BadgeCredential, BadgeInfo (..), BadgePurchase (..), BadgeRequest (..), BadgeType (..), generateMasterKey, issueBadge, verifyPayment)
import Simplex.Chat.Badges.Service
  ( BadgeStatement (..),
    StatementCreditType (..),
    StatementDebitType (..),
    StatementEntry (..),
    StatementEntryType (..),
  )
import Simplex.Chat.Badges.Types
  ( BadgeIssuance (..),
    BadgeLedgerEntry (..),
    BadgePurchaseStatus (..),
    LedgerCreditType (..),
    LedgerDebitType (..),
    LedgerEntryType (..),
  )
import Simplex.Chat.Controller (ChatDatabase (..))
import Simplex.Chat.Store.Badges
import Simplex.Chat.Store.Profiles (createUserRecordAt)
import Simplex.Chat.Store.Shared (StoreError)
import Simplex.Chat.Types (AgentUserId (..), Profile (..), User (..))
import Simplex.Messaging.Agent.Store.Common (DBStore, withTransaction)
import Simplex.Messaging.Agent.Store.DB (Binary (..))
import qualified Simplex.Messaging.Agent.Store.DB as DB
import Simplex.Messaging.Agent.Store.Interface (closeDBStore)
import qualified Simplex.Messaging.Crypto as C
import Simplex.Messaging.Crypto.BBS (bbsKeyGen)
import Test.Hspec hiding (it)
#if defined(dbPostgres)
import Database.PostgreSQL.Simple (Only (..))
#else
import Database.SQLite.Simple (Only (..))
#endif

badgeStoreTests :: SpecWith TestParams
badgeStoreTests = do
  describe "client badge ledger" $ do
    it "reads back the last entry of a statement, and an opening entry restates the balance absolutely" testOpeningRestatesBalance
    it "preserves the statement's array order when every entry shares one createdAt" testEntryOrderFromArrayPosition
    it "re-delivering the same complete statement writes nothing and changes nothing" testStatementRedeliveryIsIdempotent
    it "a statement with no previousEntryId replaces the ledger, dropping entries the service no longer holds" testStatementWithoutCursorReplaces
    it "a replacement that reorders the entries it re-sends drops the held rows rather than leaving them out of order" testReorderedReplacementDropsHeldRows
    it "appends after previousEntryId without touching the entries already held" testStatementWithCursorAppends
    it "stores an unrecognised entry tag verbatim and hands it back unchanged" testUnknownEntryTypeRoundTrip
    it "refuses a charge credit rather than coercing its id into the charge_id TEXT column" testChargeCreditRefused
  describe "client badge purchases" $ do
    it "createCodePayment and createPurchase leave one issued purchase paid for by a settled code payment" testCodePaymentAndPurchase
    it "supersedePurchases clears the paid slot only, and setShownPurchase points at the new purchase" testSupersedeAndShownBadge
    it "getShownPurchase returns the purchase with its private key intact" testGetShownPurchaseKeepsPrivateKey
    it "neither shown-badge function will cross profiles, so one user cannot be handed another's private key" testShownPurchaseIsScopedToItsUser
  describe "client badge issuances" $
    it "createIssuance writes one row per period" testIssuancePerPeriod

-- Fixtures --------------------------------------------------------------------

-- | A migrated chat database with one user, and nothing else: no controller, no agent, no SMP.
withBadgeStore :: TestParams -> (DBStore -> User -> IO a) -> IO a
withBadgeStore ps action = do
  Right ChatDatabase {chatStore, agentStore} <- createDatabase ps testCoreOpts "badge_store"
  insertUser agentStore
  ts <- getCurrentTime
  Right user <-
    withTransaction chatStore $ \db ->
      runExceptT $ createUserRecordAt db (AgentUserId 1) False False aliceProfile {preferences = Nothing} True ts
  action chatStore user `finally` (closeDBStore chatStore >> closeDBStore agentStore)

storeIO :: DBStore -> (DB.Connection -> IO a) -> IO a
storeIO = withTransaction

storeE :: DBStore -> (DB.Connection -> ExceptT StoreError IO a) -> IO a
storeE st action = withTransaction st (runExceptT . action) >>= either (fail . show) pure

storeErr :: DBStore -> (DB.Connection -> ExceptT StoreError IO a) -> IO StoreError
storeErr st action =
  withTransaction st (runExceptT . action) >>= \case
    Left e -> pure e
    Right _ -> fail "expected the store to refuse this write"

epoch :: UTCTime
epoch = UTCTime (fromGregorian 2026 1 4) (secondsToDiffTime 0)

-- | Stands in for the client's own clock where a test asserts @created_at@ back. A whole-second
-- time, not 'getCurrentTime': Postgres @TIMESTAMPTZ@ keeps microseconds and 'getCurrentTime' has
-- nanosecond resolution on Linux, so a wall-clock instant does not survive the round trip on
-- one of the two backends.
replicatedAt :: UTCTime
replicatedAt = UTCTime (fromGregorian 2026 1 5) (secondsToDiffTime 3600)

-- | One statement entry. @createdAt@ is the SERVICE's clock and is deliberately the same for
-- every entry of a statement, which is what the service writes (plan §9): order lives in the
-- array, not in the timestamps.
entry :: Text -> Int -> Int -> StatementEntryType -> StatementEntry
entry entryId changeMonths balanceMonths entryType =
  StatementEntry
    { entryId,
      changeMonths,
      balanceMonths,
      balanceStartTs = epoch,
      balanceBadgeType = BTSupporter,
      wasPausedSince = Nothing,
      createdAt = epoch,
      entryType
    }

fullStatement :: [StatementEntry] -> BadgeStatement
fullStatement entries = BadgeStatement {entries, previousEntryId = Nothing}

testCredential :: BadgeType -> UTCTime -> IO BadgeCredential
testCredential badgeType expiry = do
  Right (_pk, sk) <- bbsKeyGen
  drg <- C.newRandom
  mk <- generateMasterKey drg
  let req = BadgeRequest {masterKey = mk, badgeInfo = BadgeInfo {badgeType, badgeExpiry = Just expiry, badgeExtra = ""}}
  Just vreq <- verifyPayment (BPRedeemCode "TEST") req
  Right cred <- issueBadge 1 sk vreq
  pure cred

-- | A purchase row with its payment, as a code redemption writes them.
newCodePurchase :: DBStore -> User -> BadgeType -> IO UserBadgePurchase
newCodePurchase st User {userId} badgeType = do
  drg <- C.newRandom
  (pubKey, privKey) <- atomically $ C.generateKeyPair drg
  mk <- generateMasterKey drg
  now <- getCurrentTime
  storeIO st $ \db -> do
    paymentId <- createCodePayment db now
    createPurchase db userId pubKey privKey mk badgeType paymentId now

purchaseId :: UserBadgePurchase -> Int64
purchaseId UserBadgePurchase {badgePurchaseId} = badgePurchaseId

-- | @(badge_purchase_id, initial_badge_type, current_badge_type, status)@, read from the table
-- rather than from the record 'createPurchase' returns: both badge-type columns must be asserted
-- against the DATABASE, or swapping the two arguments that write them would go unnoticed.
purchaseStatuses :: DBStore -> IO [(Int64, Text, Text, Text)]
purchaseStatuses st =
  storeIO st $ \db ->
    DB.query_ db "SELECT badge_purchase_id, initial_badge_type, current_badge_type, status FROM badge_purchases ORDER BY badge_purchase_id ASC"

ledgerUuids :: DBStore -> Int64 -> IO [Text]
ledgerUuids st pId =
  map fromOnly
    <$> storeIO
      st
      (\db -> DB.query db "SELECT entry_uuid FROM badge_ledger WHERE badge_purchase_id = ? ORDER BY entry_id ASC" (Only pId))

ledgerRows :: DBStore -> Int64 -> IO [(Int64, Text, Int, Int)]
ledgerRows st pId =
  storeIO
    st
    ( \db ->
        DB.query
          db
          "SELECT entry_id, entry_uuid, change_months, balance_months FROM badge_ledger WHERE badge_purchase_id = ? ORDER BY entry_id ASC"
          (Only pId)
    )

-- Ledger ----------------------------------------------------------------------

-- | The statement's balances are the SERVICE's and are stored as stated. The @opening@ entry
-- here restates the balance as 12 after a history that ran it down to 2 -- and its own
-- @changeMonths@ is 0, so a store that derived the balance from the entries instead of copying
-- what each one states would land on 2, not 12.
testOpeningRestatesBalance :: HasCallStack => TestParams -> IO ()
testOpeningRestatesBalance ps = withBadgeStore ps $ \st user -> do
  pId <- purchaseId <$> newCodePurchase st user BTSupporter
  let now = replicatedAt
      statement =
        fullStatement
          [ entry "uuid-1" 3 3 (SECredit SCPayment {invoiceId = Nothing}),
            entry "uuid-2" (-1) 2 (SEDebit SDBadge),
            entry "uuid-3" 0 12 (SECredit SCOpening)
          ]
  storeE st $ \db -> insertLedgerEntries db pId statement now
  Just BadgeLedgerEntry {entryUuid, balanceMonths, changeMonths, entryType, serviceCreatedAt, createdAt} <-
    storeE st (`getLastBadgeLedgerEntry` pId)
  entryUuid `shouldBe` "uuid-3"
  balanceMonths `shouldBe` 12
  changeMonths `shouldBe` 0
  entryType `shouldBe` LECredit CTOpening
  -- the service's clock is kept apart from the client's: service_created_at is what the wire
  -- reported, created_at is when this client replicated the row
  serviceCreatedAt `shouldBe` epoch
  createdAt `shouldBe` now
  -- ... and a later opening restates it again, downwards, from an unrelated balance
  let reopened = BadgeStatement {entries = [entry "uuid-4" 0 1 (SECredit SCOpening)], previousEntryId = Just "uuid-3"}
  storeE st $ \db -> insertLedgerEntries db pId reopened now
  Just BadgeLedgerEntry {entryUuid = uuid2, balanceMonths = balance2} <- storeE st (`getLastBadgeLedgerEntry` pId)
  uuid2 `shouldBe` "uuid-4"
  balance2 `shouldBe` 1

-- | Every entry of one statement carries an identical @createdAt@ and the wire has no
-- @serviceCreatedAt@ at all, so array position is the only thing that orders them.
testEntryOrderFromArrayPosition :: HasCallStack => TestParams -> IO ()
testEntryOrderFromArrayPosition ps = withBadgeStore ps $ \st user -> do
  pId <- purchaseId <$> newCodePurchase st user BTSupporter
  now <- getCurrentTime
  let statement =
        fullStatement
          [ entry "ord-1" 12 12 (SECredit SCPayment {invoiceId = Nothing}),
            entry "ord-2" (-1) 11 (SEDebit SDBadge),
            entry "ord-3" (-1) 10 (SEDebit SDBadge),
            entry "ord-4" (-1) 9 (SEDebit SDLapse)
          ]
  storeE st $ \db -> insertLedgerEntries db pId statement now
  ledgerUuids st pId `shouldReturn` ["ord-1", "ord-2", "ord-3", "ord-4"]
  Just BadgeLedgerEntry {entryUuid, balanceMonths} <- storeE st (`getLastBadgeLedgerEntry` pId)
  entryUuid `shouldBe` "ord-4"
  balanceMonths `shouldBe` 9

-- | @purchaseBadge@ and @getBadgeCatalog@ return the complete history every time, so a retry
-- re-delivers rows the client already holds. The second insert must be a no-op, not merely
-- non-crashing: same uuids, same local ids, same count.
testStatementRedeliveryIsIdempotent :: HasCallStack => TestParams -> IO ()
testStatementRedeliveryIsIdempotent ps = withBadgeStore ps $ \st user -> do
  pId <- purchaseId <$> newCodePurchase st user BTSupporter
  now <- getCurrentTime
  let statement =
        fullStatement
          [ entry "dup-1" 3 3 (SECredit SCPayment {invoiceId = Nothing}),
            entry "dup-2" (-1) 2 (SEDebit SDBadge)
          ]
  storeE st $ \db -> insertLedgerEntries db pId statement now
  rowsBefore <- ledgerRows st pId
  storeE st $ \db -> insertLedgerEntries db pId statement (addUTCTime nominalDay now)
  rowsAfter <- ledgerRows st pId
  length rowsBefore `shouldBe` 2
  rowsAfter `shouldBe` rowsBefore

-- | An absent @previousEntryId@ with entries present marks entries that attach to nothing
-- (docs/protocol/badges-rpc.md): the statement IS the ledger, so a row the client holds that the
-- statement does not carry is stale and goes. Appending instead would leave the client holding
-- an entry the service has dropped.
testStatementWithoutCursorReplaces :: HasCallStack => TestParams -> IO ()
testStatementWithoutCursorReplaces ps = withBadgeStore ps $ \st user -> do
  pId <- purchaseId <$> newCodePurchase st user BTSupporter
  now <- getCurrentTime
  let paymentEntry = entry "rep-1" 3 3 (SECredit SCPayment {invoiceId = Nothing})
      first = fullStatement [paymentEntry, entry "rep-stale" (-1) 2 (SEDebit SDLapse)]
      -- the service healed its own ledger: rep-stale never happened, and rep-2 took its place
      replacement = fullStatement [paymentEntry, entry "rep-2" (-1) 2 (SEDebit SDBadge)]
  storeE st $ \db -> insertLedgerEntries db pId first now
  ledgerUuids st pId `shouldReturn` ["rep-1", "rep-stale"]
  storeE st $ \db -> insertLedgerEntries db pId replacement now
  ledgerUuids st pId `shouldReturn` ["rep-1", "rep-2"]
  Just BadgeLedgerEntry {entryType} <- storeE st (`getLastBadgeLedgerEntry` pId)
  entryType `shouldBe` LEDebit DTBadge
  -- an empty statement attaches nothing and is not a claim that the ledger is empty
  storeE st $ \db -> insertLedgerEntries db pId (fullStatement []) now
  ledgerUuids st pId `shouldReturn` ["rep-1", "rep-2"]

-- | The REPLACE path keeps rows the statement re-sends, so they hold their local @entry_id@ and
-- an issuance can reference them -- but that is only safe while the rows it keeps sit in the same
-- order the statement puts them in. A service that REWROTE its ledger rather than extending it
-- can re-send the same uuids in a different order, and then keeping them would leave the client
-- reading a last entry that is not the statement's last entry. The held rows are dropped and
-- re-inserted instead.
--
-- This is the branch behind everything else the step does, and no other example reaches it:
-- every other REPLACE test starts from an empty ledger, or keeps a genuine prefix, or re-delivers
-- an identical statement.
testReorderedReplacementDropsHeldRows :: HasCallStack => TestParams -> IO ()
testReorderedReplacementDropsHeldRows ps = withBadgeStore ps $ \st user -> do
  pId <- purchaseId <$> newCodePurchase st user BTSupporter
  now <- getCurrentTime
  let entryA = entry "reo-a" 3 3 (SECredit SCPayment {invoiceId = Nothing})
      entryB = entry "reo-b" (-1) 2 (SEDebit SDBadge)
      -- same two uuids, opposite order, and the balances swapped with them
      entryA' = entry "reo-a" (-1) 2 (SEDebit SDBadge)
      entryB' = entry "reo-b" 3 3 (SECredit SCPayment {invoiceId = Nothing})
  storeE st $ \db -> insertLedgerEntries db pId (fullStatement [entryA, entryB]) now
  ledgerUuids st pId `shouldReturn` ["reo-a", "reo-b"]
  storeE st $ \db -> insertLedgerEntries db pId (fullStatement [entryB', entryA']) now
  ledgerUuids st pId `shouldReturn` ["reo-b", "reo-a"]
  -- and the last entry is the statement's last entry, not whichever row happened to survive
  Just BadgeLedgerEntry {entryUuid, balanceMonths, entryType} <- storeE st (`getLastBadgeLedgerEntry` pId)
  entryUuid `shouldBe` "reo-a"
  balanceMonths `shouldBe` 2
  entryType `shouldBe` LEDebit DTBadge

-- | A present @previousEntryId@ names an entry the client already holds: the entries attach
-- after it and nothing is removed.
testStatementWithCursorAppends :: HasCallStack => TestParams -> IO ()
testStatementWithCursorAppends ps = withBadgeStore ps $ \st user -> do
  pId <- purchaseId <$> newCodePurchase st user BTSupporter
  now <- getCurrentTime
  storeE st $ \db -> insertLedgerEntries db pId (fullStatement [entry "app-1" 3 3 (SECredit SCPayment {invoiceId = Nothing})]) now
  storeE st $ \db ->
    insertLedgerEntries db pId BadgeStatement {entries = [entry "app-2" (-1) 2 (SEDebit SDBadge)], previousEntryId = Just "app-1"} now
  ledgerUuids st pId `shouldReturn` ["app-1", "app-2"]

-- | A service ahead of this build can name an entry type this build has no constructor for.
-- It is stored as received, tag and object both, and handed back unchanged.
testUnknownEntryTypeRoundTrip :: HasCallStack => TestParams -> IO ()
testUnknownEntryTypeRoundTrip ps = withBadgeStore ps $ \st user -> do
  pId <- purchaseId <$> newCodePurchase st user BTSupporter
  now <- getCurrentTime
  let creditObj = JM.fromList [("type", J.String "grant"), ("grantId", J.String "g-1")]
      debitObj = JM.fromList [("type", J.String "clawback"), ("reason", J.String "chargeback")]
      statement =
        fullStatement
          [ entry "unk-1" 6 6 (SECredit SCUnknown {tag = "grant", json = creditObj}),
            entry "unk-2" (-6) 0 (SEDebit SDUnknown {tag = "clawback", json = debitObj})
          ]
  storeE st $ \db -> insertLedgerEntries db pId statement now
  Just BadgeLedgerEntry {entryType} <- storeE st (`getLastBadgeLedgerEntry` pId)
  entryType `shouldBe` LEDebit DTUnknown {tag = "clawback", json = debitObj}
  -- the tag is also readable as SQL, so the fallback columns are inspectable without decoding
  tags <-
    storeIO st $ \db ->
      DB.query
        db
        "SELECT entry_type, entry_credit_type, entry_debit_type, entry_type_unknown FROM badge_ledger WHERE badge_purchase_id = ? ORDER BY entry_id ASC"
        (Only pId)
  (tags :: [(Text, Maybe Text, Maybe Text, Int)])
    `shouldBe` [("credit", Just "grant", Nothing, 1), ("debit", Nothing, Just "clawback", 1)]

-- | @charge@'s id is 'Int64' in Haskell and the column it would go in is @subscription_charges@'
-- TEXT primary key. The service's codec refuses it rather than inventing a coercion, and so does
-- this one; subscriptions are out of scope, so nothing produces one.
testChargeCreditRefused :: HasCallStack => TestParams -> IO ()
testChargeCreditRefused ps = withBadgeStore ps $ \st user -> do
  pId <- purchaseId <$> newCodePurchase st user BTSupporter
  now <- getCurrentTime
  let statement = fullStatement [entry "chg-1" 1 1 (SECredit SCCharge {chargeId = "charge-1"})]
  err <- storeErr st $ \db -> insertLedgerEntries db pId statement now
  show err `shouldContain` "credit(charge)"
  -- the refusal is total: no partial row was written
  ledgerUuids st pId `shouldReturn` []

-- Purchases -------------------------------------------------------------------

testCodePaymentAndPurchase :: HasCallStack => TestParams -> IO ()
testCodePaymentAndPurchase ps = withBadgeStore ps $ \st user -> do
  UserBadgePurchase {badgePurchaseId = pId, paymentId, status, initialBadgeType, currentBadgeType} <-
    newCodePurchase st user BTLegend
  status `shouldBe` PSIssued
  initialBadgeType `shouldBe` BTLegend
  currentBadgeType `shouldBe` BTLegend
  purchaseStatuses st `shouldReturn` [(pId, "legend", "legend", "issued")]
  Just pmtId <- pure paymentId
  payments <- storeIO st $ \db -> DB.query db "SELECT provider, status, invoice_id FROM payments WHERE payment_id = ?" (Only pmtId)
  (payments :: [(Text, Text, Maybe Text)]) `shouldBe` [("code", "settled", Nothing)]
  linked <- storeIO st $ \db -> DB.query db "SELECT payment_id FROM badge_purchases WHERE badge_purchase_id = ?" (Only pId)
  (linked :: [Only (Maybe Text)]) `shouldBe` [Only (Just pmtId)]
  -- the ledger's payment_id stays NULL: the client has no payments row for the SERVICE's
  -- payment, and the wire carries no id for it (plan §9)
  now <- getCurrentTime
  storeE st $ \db -> insertLedgerEntries db pId (fullStatement [entry "pay-1" 12 12 (SECredit SCPayment {invoiceId = Nothing})]) now
  ledgerPaymentIds <- storeIO st $ \db -> DB.query db "SELECT payment_id FROM badge_ledger WHERE badge_purchase_id = ?" (Only pId)
  (ledgerPaymentIds :: [Only (Maybe Text)]) `shouldBe` [Only Nothing]

-- | A second purchase takes the paid slot: the first moves to @superseded@ and
-- @users.shown_badge_id@ follows the new one. An investor purchase is a different slot and must
-- be left alone.
testSupersedeAndShownBadge :: HasCallStack => TestParams -> IO ()
testSupersedeAndShownBadge ps = withBadgeStore ps $ \st user@User {userId} -> do
  supporterId <- purchaseId <$> newCodePurchase st user BTSupporter
  investorId <- purchaseId <$> newCodePurchase st user BTInvestor
  storeE st $ \db -> setShownPurchase db userId supporterId
  legendId <- purchaseId <$> newCodePurchase st user BTLegend
  now <- getCurrentTime
  storeE st $ \db -> do
    supersedePurchases db userId legendId now
    setShownPurchase db userId legendId
  purchaseStatuses st
    `shouldReturn` [ (supporterId, "supporter", "supporter", "superseded"),
                     (investorId, "investor", "investor", "issued"),
                     (legendId, "legend", "legend", "issued")
                   ]
  shown <- storeIO st $ \db -> DB.query db "SELECT shown_badge_id FROM users WHERE user_id = ?" (Only userId)
  (shown :: [Only (Maybe Int64)]) `shouldBe` [Only (Just legendId)]
  -- exactly one issued purchase in the paid slot
  issuedPaid <- filter (\(_, _, bt, s) -> s == "issued" && badgeSlot (readBadgeType bt) == BSPaid) <$> purchaseStatuses st
  length issuedPaid `shouldBe` 1
  where
    readBadgeType = \case
      "supporter" -> BTSupporter
      "legend" -> BTLegend
      "investor" -> BTInvestor
      t -> BTUnknown t

testGetShownPurchaseKeepsPrivateKey :: HasCallStack => TestParams -> IO ()
testGetShownPurchaseKeepsPrivateKey ps = withBadgeStore ps $ \st user@User {userId} -> do
  noBadge <- storeE st (`getShownPurchase` userId)
  noBadge `shouldSatisfy` isNothing
  UserBadgePurchase {badgePurchaseId = pId, purchaseKey, purchasePrivKey, masterKey} <- newCodePurchase st user BTSupporter
  storeE st $ \db -> setShownPurchase db userId pId
  Just UserBadgePurchase {badgePurchaseId = shownId, userId = shownUserId, purchaseKey = shownPub, purchasePrivKey = shownPriv, masterKey = shownMk} <-
    storeE st (`getShownPurchase` userId)
  shownId `shouldBe` pId
  shownUserId `shouldBe` userId
  shownPub `shouldBe` purchaseKey
  shownPriv `shouldBe` purchasePrivKey
  shownMk `shouldBe` masterKey

-- | @users.shown_badge_id@ is a bare foreign key to @badge_purchases@: nothing in the schema
-- stops it pointing at a purchase that belongs to a different profile, and the row it reaches
-- carries a PRIVATE KEY the worker signs @issueBadge@ with. Both ends refuse to cross profiles --
-- the write outright, and the read even when the pointer is already wrong, which is simulated
-- here with raw SQL because the writer will no longer produce it.
testShownPurchaseIsScopedToItsUser :: HasCallStack => TestParams -> IO ()
testShownPurchaseIsScopedToItsUser ps = withBadgeStore ps $ \st alice -> do
  bob <- addSecondUser st
  let User {userId = aliceId} = alice
      User {userId = bobId} = bob
  alicePurchase <- purchaseId <$> newCodePurchase st alice BTSupporter
  bobPurchase <- purchaseId <$> newCodePurchase st bob BTLegend
  -- bob cannot point his profile at alice's purchase
  err <- storeErr st $ \db -> setShownPurchase db bobId alicePurchase
  show err `shouldContain` "does not belong to user"
  -- ... and his own purchase is fine
  storeE st $ \db -> setShownPurchase db bobId bobPurchase
  Just UserBadgePurchase {badgePurchaseId = shownForBob} <- storeE st (`getShownPurchase` bobId)
  shownForBob `shouldBe` bobPurchase
  -- a pointer that crossed profiles anyway resolves to nothing, rather than to alice's key
  storeIO st $ \db -> DB.execute db "UPDATE users SET shown_badge_id = ? WHERE user_id = ?" (alicePurchase, bobId)
  crossed <- storeE st (`getShownPurchase` bobId)
  crossed `shouldSatisfy` isNothing
  -- alice still reads her own
  storeE st $ \db -> setShownPurchase db aliceId alicePurchase
  Just UserBadgePurchase {badgePurchaseId = shownForAlice} <- storeE st (`getShownPurchase` aliceId)
  shownForAlice `shouldBe` alicePurchase

addSecondUser :: DBStore -> IO User
addSecondUser st = do
  ts <- getCurrentTime
  Right user <-
    withTransaction st $ \db ->
      runExceptT $ createUserRecordAt db (AgentUserId 2) False False bobProfile {preferences = Nothing} False ts
  pure user

-- Issuances -------------------------------------------------------------------

testIssuancePerPeriod :: HasCallStack => TestParams -> IO ()
testIssuancePerPeriod ps = withBadgeStore ps $ \st user -> do
  pId <- purchaseId <$> newCodePurchase st user BTSupporter
  now <- getCurrentTime
  let periodStart1 = epoch
      periodEnd1 = addUTCTime (30 * nominalDay) epoch
      expiry1 = addUTCTime (37 * nominalDay) epoch
  cred1 <- testCredential BTSupporter expiry1
  BadgeIssuance {credential, periodStart, periodEnd, entryId} <-
    storeIO st $ \db -> createIssuance db (newIssuance pId periodStart1 periodEnd1 expiry1 cred1) now
  credential `shouldBe` cred1
  periodStart `shouldBe` Just periodStart1
  periodEnd `shouldBe` Just periodEnd1
  entryId `shouldBe` Nothing
  stored1 <- storedIssuances st pId
  length stored1 `shouldBe` 1
  -- the stored credential is its own JSON encoding, so the row round-trips
  [(_, Binary credBytes)] <- pure stored1
  J.eitherDecodeStrict credBytes `shouldBe` Right cred1
  -- the next period writes a second row rather than replacing the first
  let periodStart2 = periodEnd1
      periodEnd2 = addUTCTime (60 * nominalDay) epoch
      expiry2 = addUTCTime (67 * nominalDay) epoch
  cred2 <- testCredential BTSupporter expiry2
  _ <- storeIO st $ \db -> createIssuance db (newIssuance pId periodStart2 periodEnd2 expiry2 cred2) now
  stored2 <- storedIssuances st pId
  length stored2 `shouldBe` 2
  map fst stored2 `shouldBe` [periodStart1, periodStart2]
  where
    newIssuance pId periodStart periodEnd expiry credential =
      NewBadgeIssuance {badgePurchaseId = pId, badgeType = BTSupporter, periodStart, periodEnd, expiry, ledgerEntryId = Nothing, credential}

storedIssuances :: DBStore -> Int64 -> IO [(UTCTime, Binary ByteString)]
storedIssuances st pId =
  storeIO
    st
    ( \db ->
        DB.query
          db
          "SELECT period_start, credential FROM badge_issuances WHERE badge_purchase_id = ? ORDER BY period_start ASC"
          (Only pId)
    )

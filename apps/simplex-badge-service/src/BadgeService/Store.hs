{-# LANGUAGE CPP #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeOperators #-}

-- | Queries over the badge service's own tables: purchases, payments, the ledger, issuances,
-- redemption codes and the price catalog. Structured after
-- "Directory.Store"\/"Directory.Store.Migrate": every function here takes a 'DB.Connection'
-- and opens no transaction of its own; 'withServiceTransaction' is the only place a
-- transaction is opened, which is what lets a later step compose a purchase row, a payment
-- row, several ledger entries, an issuance and a code redemption into one atomic transaction.
--
-- The order, invoice and provider-event functions belong to D0. This module owns everything
-- the RPC path needs on top of that: purchases and payments, the ledger, issuances, codes
-- and the catalog. There is no store function that resolves an order to a code or a purchase
-- -- that join does not exist in the schema (docs/protocol/badges-web.md §3 Linkage); a
-- caller that needs it derives the code from the order id and looks it up by hash with
-- 'getCodeByHash'.
module BadgeService.Store
  ( ServiceError (..),
    withServiceTransaction,

    -- * Purchases and payments
    BadgePurchaseRow (..),
    getPurchaseByKey,
    createPurchase,
    createCodePayment,

    -- * Ledger
    getLastLedgerEntry,
    appendLedgerEntry,
    getLedgerSince,

    -- * Issuances
    NewIssuance (..),
    getIssuanceForPeriod,
    getIssuanceForRedeemedCode,
    createIssuance,

    -- * Codes
    BadgeCode (..),
    NewBadgeCode (..),
    getCodeByHash,
    markCodeRedeemed,
    unredeemCode,
    insertCodes,
    revokeCode,
    revokeBatch,

    -- * Catalog
    getActiveCatalog,
    getPriceById,
    getOfferById,
    setPriceStatus,
    setOfferStatus,
  )
where

import qualified Control.Exception as E
import Control.Monad (when)
import Control.Monad.Except (ExceptT, liftEither, runExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as J
import qualified Data.ByteString.Lazy as BL
import Data.ByteString (ByteString)
import Data.Int (Int64)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (UTCTime)
import Data.Word (Word32, Word8)
import Simplex.Chat.Badges (BadgeCredential (..), BadgeMasterKey (..), BadgeType (..))
import Simplex.Chat.Badges.Service (BadgeCatalog (..), BadgeOffer (..), BadgePrice (..))
import Simplex.Chat.Badges.Types
  ( BadgeIssuance (..),
    BadgeItemStatus (..),
    BadgeLedgerEntry (..),
    BadgeOfferId (..),
    BadgePriceId (..),
    BadgePurchaseStatus (..),
    LedgerCreditType (..),
    LedgerDebitType (..),
    LedgerEntryType (..),
    OfferDiscount (..),
  )
import Simplex.Chat.PaymentService.Types (CurrencyAmount (..), PaymentStatus (..))
import Simplex.Messaging.Agent.Store.Common (DBStore, withTransaction)
import Simplex.Messaging.Agent.Store.DB (Binary (..))
import qualified Simplex.Messaging.Agent.Store.DB as DB
import qualified Simplex.Messaging.Crypto as C
import Simplex.Messaging.Util (tshow)
#if defined(dbPostgres)
import Database.PostgreSQL.Simple (Only (..), Query, (:.) (..))
import Database.PostgreSQL.Simple.SqlQQ (sql)
#else
import Database.SQLite.Simple (Only (..), Query, (:.) (..))
import Database.SQLite.Simple.QQ (sql)
#endif

-- | The error type of every store function: not-found (a lookup targeted by a mutation
-- doesn't exist), conflict (a write would clobber state a caller didn't expect to overwrite)
-- and decode failures (a row can't be reconstructed into its Haskell shape).
data ServiceError
  = SEPurchaseNotFound
  | SECodeNotFound
  | SEPriceNotFound
  | SEOfferNotFound
  | -- | 'createCodePayment' only: the purchase already has a payment attached.
    SEPaymentConflict
  | SEDecodeError Text
  deriving (Eq, Show)

-- | Thrown internally to force a real rollback out of 'withTransaction': postgresql-simple\/
-- sqlite-simple commit on a normal return and only roll back on an exception, so a plain
-- @'Left' e@ returned from the wrapped action would otherwise be committed along with
-- whatever it already wrote. Never leaves this module.
newtype ServiceRollback = ServiceRollback ServiceError

instance Show ServiceRollback where
  show (ServiceRollback e) = "ServiceRollback " <> show e

instance E.Exception ServiceRollback

-- | The only place a transaction is opened. Every store function above takes a
-- 'DB.Connection' and opens none of its own, so a command handler can sequence several of
-- them here and get one transaction: a 'Left' rolls back everything the action already wrote.
withServiceTransaction :: DBStore -> (DB.Connection -> ExceptT ServiceError IO a) -> IO (Either ServiceError a)
withServiceTransaction st action =
  (Right <$> withTransaction st runInTransaction) `E.catch` \(ServiceRollback e) -> pure (Left e)
  where
    runInTransaction db =
      runExceptT (action db) >>= \case
        Left e -> E.throwIO (ServiceRollback e)
        Right a -> pure a

-- Purchases and payments -----------------------------------------------------

-- | The service's own projection of a @badge_purchases@ row. The shared 'Types.BadgePurchase'
-- carries client-only columns (@user_id@, @purchase_priv_key@, alert bookkeeping) that only
-- exist on the client's own table (added by the client-only @20260731_user_badges@
-- migration); the service's table has just the columns 'badgeSchema' creates.
data BadgePurchaseRow = BadgePurchaseRow
  { badgePurchaseId :: Int64,
    purchaseKey :: C.PublicKeyEd25519,
    masterKey :: BadgeMasterKey,
    initialBadgeType :: BadgeType,
    currentBadgeType :: BadgeType,
    paymentId :: Maybe Text,
    status :: BadgePurchaseStatus,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving (Show)

type PurchaseRow = (Int64, C.PublicKeyEd25519, Binary ByteString, BadgeType, BadgeType, Maybe Text, BadgePurchaseStatus, UTCTime, UTCTime)

rowToPurchase :: PurchaseRow -> BadgePurchaseRow
rowToPurchase (badgePurchaseId, purchaseKey, Binary mk, initialBadgeType, currentBadgeType, paymentId, status, createdAt, updatedAt) =
  BadgePurchaseRow {badgePurchaseId, purchaseKey, masterKey = BadgeMasterKey mk, initialBadgeType, currentBadgeType, paymentId, status, createdAt, updatedAt}

purchaseSelectColumns :: Query
purchaseSelectColumns =
  "badge_purchase_id, purchase_key, master_key, initial_badge_type, current_badge_type, payment_id, status, created_at, updated_at"

getPurchaseByKey :: DB.Connection -> C.PublicKeyEd25519 -> ExceptT ServiceError IO (Maybe BadgePurchaseRow)
getPurchaseByKey db purchaseKey = do
  rows <-
    liftIO $
      DB.query
        db
        ("SELECT " <> purchaseSelectColumns <> " FROM sx_badge_service_badge_purchases WHERE purchase_key = ?")
        (Only purchaseKey)
  pure $ rowToPurchase <$> listToMaybe rows

-- | Writes the row with status 'PSIssued' and both badge-type columns set to the same value:
-- the service has no pre-response row, so this is the first and only write that establishes
-- a purchase for a given key. @payment_id@ starts NULL; 'createCodePayment' attaches one
-- later.
createPurchase :: DB.Connection -> C.PublicKeyEd25519 -> BadgeMasterKey -> BadgeType -> UTCTime -> ExceptT ServiceError IO BadgePurchaseRow
createPurchase db purchaseKey masterKey@(BadgeMasterKey mk) badgeType now = do
  [Only badgePurchaseId] <-
    liftIO $
      DB.query
        db
        [sql|
          INSERT INTO sx_badge_service_badge_purchases
            (purchase_key, master_key, initial_badge_type, current_badge_type, status, created_at, updated_at)
          VALUES (?,?,?,?,?,?,?)
          RETURNING badge_purchase_id
        |]
        (purchaseKey, Binary mk, badgeType, badgeType, PSIssued, now, now)
  pure
    BadgePurchaseRow
      { badgePurchaseId,
        purchaseKey,
        masterKey,
        initialBadgeType = badgeType,
        currentBadgeType = badgeType,
        paymentId = Nothing,
        status = PSIssued,
        createdAt = now,
        updatedAt = now
      }

-- | The service's DB has no 'Simplex.Chat.PaymentService.Types.PaymentProvider' column codec
-- yet (no step has needed to persist more than this one literal); D0\/E2\/F1 should add a
-- proper 'TextEncoding' instance once a second provider needs writing from the service side.
codePaymentProviderText :: Text
codePaymentProviderText = "code"

-- | Writes the @payments@ row (caller-minted UUID as @payment_id@, @provider = 'code'@,
-- @invoice_id@ NULL, @status = 'settled'@ via 'PSSettled'\'s 'ToField'), then points
-- the purchase's @payment_id@ at it. The second write is guarded by @payment_id IS NULL@ so a
-- purchase that already has a payment is never silently repointed; on no rows affected, a
-- follow-up existence check distinguishes an unknown purchase ('SEPurchaseNotFound') from one
-- that already has a payment ('SEPaymentConflict').
createCodePayment :: DB.Connection -> Int64 -> Text -> UTCTime -> ExceptT ServiceError IO ()
createCodePayment db badgePurchaseId paymentId now = do
  liftIO $
    DB.execute
      db
      [sql|
        INSERT INTO sx_badge_service_payments (payment_id, invoice_id, provider, status, created_at, updated_at)
        VALUES (?,?,?,?,?,?)
      |]
      (paymentId, Nothing :: Maybe Text, codePaymentProviderText, PSSettled, now, now)
  attached <-
    liftIO $
      DB.query
        db
        [sql|
          UPDATE sx_badge_service_badge_purchases
          SET payment_id = ?, updated_at = ?
          WHERE badge_purchase_id = ? AND payment_id IS NULL
          RETURNING badge_purchase_id
        |]
        (paymentId, now, badgePurchaseId)
  when (null (attached :: [Only Int64])) $ do
    exists <-
      liftIO $
        DB.query
          db
          "SELECT 1 FROM sx_badge_service_badge_purchases WHERE badge_purchase_id = ?"
          (Only badgePurchaseId)
    throwError $ if null (exists :: [Only Int]) then SEPurchaseNotFound else SEPaymentConflict

-- Ledger ----------------------------------------------------------------------

-- The service's own @badge_ledger@ table has just the columns 'badgeSchema' creates: the
-- @entry_type_unknown@\/@entry_type_value@ fallback columns that would let 'CTUnknown'\/
-- 'DTUnknown' round-trip only exist on the client's table (again added by the client-only
-- @20260731_user_badges@ ALTERs). Those variants exist for a client decoding an entry type
-- from a service ahead of it; the service, originating every entry it writes, never needs to
-- persist one, so 'encodeLedgerEntryType' rejects them.

type LedgerCoreRow = (Int64, Text, Int64, Int, Int, UTCTime, BadgeType, Maybe UTCTime, UTCTime, UTCTime)

type LedgerTypeRow = (Text, Maybe Text, Maybe Text, Maybe Text, Maybe Text, Maybe Int64, Maybe Int64)

ledgerSelectColumns :: Query
ledgerSelectColumns =
  "entry_id, entry_uuid, badge_purchase_id, change_months, balance_months, balance_start_ts, balance_badge_type, was_paused_since, service_created_at, created_at, "
    <> "entry_type, entry_credit_type, entry_debit_type, payment_id, charge_id, from_purchase_id, to_purchase_id"

-- | @'CTPayment' {invoiceId}@ and @'CTCharge' {chargeId}@ are typed 'Int64' in
-- "Simplex.Chat.Badges.Types", but the columns they would persist through (@payment_id@,
-- @charge_id@) are the referenced tables' TEXT primary keys. This is not this step's
-- decision to paper over: it is already recorded as an open finding awaiting a human ruling
-- (SDD progress log, Phase A: "LedgerCreditType CTPayment.invoiceId/CTCharge.chargeId left
-- alone -- wrong against TEXT columns but marked confirmed... needs a human decision"). Both
-- directions reject the two constructors explicitly rather than inventing a silent, possibly
-- wrong, numeric<->text coercion.
encodeLedgerEntryType :: LedgerEntryType -> ExceptT ServiceError IO LedgerTypeRow
encodeLedgerEntryType = \case
  LECredit creditType -> case creditType of
    CTPayment {} -> throwError $ SEDecodeError "CTPayment.invoiceId (Int64) does not fit the payment_id TEXT column; unresolved type mismatch, see SDD progress log"
    CTCharge {} -> throwError $ SEDecodeError "CTCharge.chargeId (Int64) does not fit the charge_id TEXT column; unresolved type mismatch, see SDD progress log"
    CTSupport -> pure ("credit", Just "support", Nothing, Nothing, Nothing, Nothing, Nothing)
    CTTransferIn {fromPurchaseId} -> pure ("credit", Just "transfer_in", Nothing, Nothing, Nothing, fromPurchaseId, Nothing)
    CTOpening -> pure ("credit", Just "opening", Nothing, Nothing, Nothing, Nothing, Nothing)
    CTUnknown {tag} -> throwError $ SEDecodeError ("cannot persist unknown ledger credit type: " <> tag)
  LEDebit debitType -> case debitType of
    DTRefund -> pure ("debit", Nothing, Just "refund", Nothing, Nothing, Nothing, Nothing)
    DTUpgrade {toPurchaseId} -> pure ("debit", Nothing, Just "upgrade", Nothing, Nothing, Nothing, Just toPurchaseId)
    DTTransferOut {toPurchaseId} -> pure ("debit", Nothing, Just "transfer_out", Nothing, Nothing, Nothing, Just toPurchaseId)
    DTSupport -> pure ("debit", Nothing, Just "support", Nothing, Nothing, Nothing, Nothing)
    DTBadge -> pure ("debit", Nothing, Just "badge", Nothing, Nothing, Nothing, Nothing)
    DTLapse -> pure ("debit", Nothing, Just "lapse", Nothing, Nothing, Nothing, Nothing)
    DTUnknown {tag} -> throwError $ SEDecodeError ("cannot persist unknown ledger debit type: " <> tag)

decodeLedgerEntryType :: LedgerTypeRow -> Either ServiceError LedgerEntryType
decodeLedgerEntryType row = case row of
  ("credit", Just "support", _, _, _, _, _) -> Right $ LECredit CTSupport
  ("credit", Just "transfer_in", _, _, _, fromPurchaseId, _) -> Right $ LECredit (CTTransferIn fromPurchaseId)
  ("credit", Just "opening", _, _, _, _, _) -> Right $ LECredit CTOpening
  ("debit", _, Just "refund", _, _, _, _) -> Right $ LEDebit DTRefund
  ("debit", _, Just "upgrade", _, _, _, Just toPurchaseId) -> Right $ LEDebit (DTUpgrade toPurchaseId)
  ("debit", _, Just "transfer_out", _, _, _, Just toPurchaseId) -> Right $ LEDebit (DTTransferOut toPurchaseId)
  ("debit", _, Just "support", _, _, _, _) -> Right $ LEDebit DTSupport
  ("debit", _, Just "badge", _, _, _, _) -> Right $ LEDebit DTBadge
  ("debit", _, Just "lapse", _, _, _, _) -> Right $ LEDebit DTLapse
  _ -> Left $ SEDecodeError ("malformed or unsupported ledger entry type row: " <> tshow row)

rowToLedgerEntry :: (LedgerCoreRow :. LedgerTypeRow) -> Either ServiceError BadgeLedgerEntry
rowToLedgerEntry ((entryId, entryUuid, badgePurchaseId, changeMonths, balanceMonths, balanceStartTs, balanceBadgeType, wasPausedSince, serviceCreatedAt, createdAt) :. typeRow) = do
  entryType <- decodeLedgerEntryType typeRow
  Right BadgeLedgerEntry {entryId, entryUuid, badgePurchaseId, changeMonths, balanceMonths, balanceStartTs, balanceBadgeType, wasPausedSince, serviceCreatedAt, createdAt, entryType}

-- | Ignores the input's 'entryId' (DB-assigned via IDENTITY) and returns the entry with the
-- assigned id filled in.
appendLedgerEntry :: DB.Connection -> BadgeLedgerEntry -> ExceptT ServiceError IO BadgeLedgerEntry
appendLedgerEntry db entry = do
  let BadgeLedgerEntry {entryUuid, badgePurchaseId, changeMonths, balanceMonths, balanceStartTs, balanceBadgeType, wasPausedSince, serviceCreatedAt, createdAt, entryType} = entry
  typeRow <- encodeLedgerEntryType entryType
  [Only newEntryId] <-
    liftIO $
      DB.query
        db
        [sql|
          INSERT INTO sx_badge_service_badge_ledger
            (entry_uuid, badge_purchase_id, change_months, balance_months, balance_start_ts, balance_badge_type, was_paused_since, service_created_at, created_at,
             entry_type, entry_credit_type, entry_debit_type, payment_id, charge_id, from_purchase_id, to_purchase_id)
          VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)
          RETURNING entry_id
        |]
        ((entryUuid, badgePurchaseId, changeMonths, balanceMonths, balanceStartTs, balanceBadgeType, wasPausedSince, serviceCreatedAt, createdAt) :. typeRow)
  pure BadgeLedgerEntry {entryId = newEntryId, entryUuid, badgePurchaseId, changeMonths, balanceMonths, balanceStartTs, balanceBadgeType, wasPausedSince, serviceCreatedAt, createdAt, entryType}

getLastLedgerEntry :: DB.Connection -> Int64 -> ExceptT ServiceError IO (Maybe BadgeLedgerEntry)
getLastLedgerEntry db badgePurchaseId = do
  rows <-
    liftIO $
      DB.query
        db
        ("SELECT " <> ledgerSelectColumns <> " FROM sx_badge_service_badge_ledger WHERE badge_purchase_id = ? ORDER BY entry_id DESC LIMIT 1")
        (Only badgePurchaseId)
  case rows of
    [] -> pure Nothing
    (row : _) -> Just <$> liftEither (rowToLedgerEntry row)

-- | Entries for a purchase, oldest first. @Nothing@ returns the full ledger; @Just entryId@
-- returns only entries strictly after it, matching 'BadgeStatement.previousEntryId'\'s
-- semantics (absent for the full ledger).
getLedgerSince :: DB.Connection -> Int64 -> Maybe Int64 -> ExceptT ServiceError IO [BadgeLedgerEntry]
getLedgerSince db badgePurchaseId sinceEntryId = do
  rows <- case sinceEntryId of
    Nothing ->
      liftIO $
        DB.query
          db
          ("SELECT " <> ledgerSelectColumns <> " FROM sx_badge_service_badge_ledger WHERE badge_purchase_id = ? ORDER BY entry_id ASC")
          (Only badgePurchaseId)
    Just sinceId ->
      liftIO $
        DB.query
          db
          ("SELECT " <> ledgerSelectColumns <> " FROM sx_badge_service_badge_ledger WHERE badge_purchase_id = ? AND entry_id > ? ORDER BY entry_id ASC")
          (badgePurchaseId, sinceId)
  liftEither $ mapM rowToLedgerEntry rows

-- Issuances ---------------------------------------------------------------------

-- | Fields needed to create one @badge_issuances@ row. Unlike the shared 'BadgeIssuance',
-- @period_start@\/@period_end@\/@expiry@ are definite here, matching the NOT NULL columns;
-- 'BadgeIssuance' declares them 'Maybe' only because that record is also the wire shape,
-- which has no reason to require them from a client.
data NewIssuance = NewIssuance
  { issuanceId :: Text,
    badgePurchaseId :: Int64,
    badgeType :: BadgeType,
    periodStart :: UTCTime,
    periodEnd :: UTCTime,
    expiry :: UTCTime,
    -- | Named apart from 'BadgeIssuance'\'s 'entryId' (the same @entry_id@ column) so the two
    -- don't collide as a bare selector under 'DuplicateRecordFields' at call sites that have
    -- both types in scope.
    ledgerEntryId :: Maybe Int64,
    credential :: BadgeCredential
  }

type IssuanceRow = (Text, Int64, Maybe Int64, BadgeType, UTCTime, UTCTime, UTCTime, Binary ByteString, UTCTime)

issuanceSelectColumns :: Query
issuanceSelectColumns = "issuance_id, badge_purchase_id, entry_id, badge_type, period_start, period_end, expiry, credential, created_at"

-- | 'BadgeCredential' already has 'J.ToJSON'\/'J.FromJSON' (it crosses the wire in
-- 'BSPBadgeCredential'); the @credential@ BYTEA\/BLOB column stores that same JSON encoding,
-- so a malformed row is a genuine 'SEDecodeError' rather than a second, DB-only codec.
rowToIssuance :: IssuanceRow -> Either ServiceError BadgeIssuance
rowToIssuance (issuanceId, badgePurchaseId, entryId, badgeType, periodStart, periodEnd, expiry, Binary credBytes, createdAt) =
  case J.eitherDecodeStrict credBytes of
    Left err -> Left $ SEDecodeError ("issuance " <> issuanceId <> " credential: " <> T.pack err)
    Right credential ->
      Right
        BadgeIssuance
          { issuanceId,
            badgePurchaseId,
            badgeType,
            periodStart = Just periodStart,
            periodEnd = Just periodEnd,
            expiry = Just expiry,
            entryId,
            credential,
            createdAt
          }

createIssuance :: DB.Connection -> NewIssuance -> UTCTime -> ExceptT ServiceError IO BadgeIssuance
createIssuance db NewIssuance {issuanceId, badgePurchaseId, badgeType, periodStart, periodEnd, expiry, ledgerEntryId, credential} now = do
  liftIO $
    DB.execute
      db
      [sql|
        INSERT INTO sx_badge_service_badge_issuances
          (issuance_id, badge_purchase_id, entry_id, badge_type, period_start, period_end, expiry, credential, created_at)
        VALUES (?,?,?,?,?,?,?,?,?)
      |]
      (issuanceId, badgePurchaseId, ledgerEntryId, badgeType, periodStart, periodEnd, expiry, Binary (BL.toStrict (J.encode credential)), now)
  pure
    BadgeIssuance
      { issuanceId,
        badgePurchaseId,
        badgeType,
        periodStart = Just periodStart,
        periodEnd = Just periodEnd,
        expiry = Just expiry,
        entryId = ledgerEntryId,
        credential,
        createdAt = now
      }

-- | The issuance for a purchase whose period covers a given instant (half-open:
-- @period_start <= t < period_end@).
getIssuanceForPeriod :: DB.Connection -> Int64 -> UTCTime -> ExceptT ServiceError IO (Maybe BadgeIssuance)
getIssuanceForPeriod db badgePurchaseId asOf = do
  rows <-
    liftIO $
      DB.query
        db
        ("SELECT " <> issuanceSelectColumns <> " FROM sx_badge_service_badge_issuances WHERE badge_purchase_id = ? AND period_start <= ? AND period_end > ?")
        (badgePurchaseId, asOf, asOf)
  case rows of
    [] -> pure Nothing
    (row : _) -> Just <$> liftEither (rowToIssuance row)

-- | code hash -> @redeemed_purchase_id@ -> the issuance whose period contains @redeemed_at@.
-- A purchase may have several issuances by the time B7's replay path runs this, so the
-- redemption time (not just the purchase) picks out exactly one.
getIssuanceForRedeemedCode :: DB.Connection -> ByteString -> ExceptT ServiceError IO (Maybe BadgeIssuance)
getIssuanceForRedeemedCode db codeHash = do
  rows <-
    liftIO $
      DB.query
        db
        [sql|
          SELECT i.issuance_id, i.badge_purchase_id, i.entry_id, i.badge_type, i.period_start, i.period_end, i.expiry, i.credential, i.created_at
          FROM sx_badge_service_codes c
          JOIN sx_badge_service_badge_issuances i ON i.badge_purchase_id = c.redeemed_purchase_id
          WHERE c.code_hash = ? AND c.redeemed_at IS NOT NULL AND i.period_start <= c.redeemed_at AND i.period_end > c.redeemed_at
        |]
        (Only (Binary codeHash))
  case rows of
    [] -> pure Nothing
    (row : _) -> Just <$> liftEither (rowToIssuance row)

-- Codes -----------------------------------------------------------------------

data BadgeCode = BadgeCode
  { codeHash :: ByteString,
    badgeType :: BadgeType,
    months :: Word8,
    batch :: Text,
    expiresAt :: UTCTime,
    redeemedPurchaseId :: Maybe Int64,
    redeemedAt :: Maybe UTCTime,
    unredeemedAt :: Maybe UTCTime,
    revokedAt :: Maybe UTCTime,
    createdAt :: UTCTime
  }
  deriving (Show)

-- | Fields needed to create a new, unredeemed code; 'insertCodes' is the only writer of a
-- fresh code, so there is no representable state where a new row starts out redeemed,
-- unredeemed-again or revoked.
data NewBadgeCode = NewBadgeCode
  { codeHash :: ByteString,
    badgeType :: BadgeType,
    months :: Word8,
    batch :: Text,
    expiresAt :: UTCTime
  }

type CodeRow = (Binary ByteString, BadgeType, Int, Text, UTCTime, Maybe Int64, Maybe UTCTime, Maybe UTCTime, Maybe UTCTime, UTCTime)

-- | @months@ is read as a signed 'Int' and converted: see 'word8FromInt'.
rowToCode :: CodeRow -> Either ServiceError BadgeCode
rowToCode (Binary codeHash, badgeType, monthsInt, batch, expiresAt, redeemedPurchaseId, redeemedAt, unredeemedAt, revokedAt, createdAt) = do
  months <- word8FromInt "code months" monthsInt
  Right BadgeCode {codeHash, badgeType, months, batch, expiresAt, redeemedPurchaseId, redeemedAt, unredeemedAt, revokedAt, createdAt}

-- | Joined to @badge_purchases@ so the caller sees the @purchase_key@ behind
-- @redeemed_purchase_id@: a redeeming request whose own key matches is a replay of its own
-- redemption, one whose key differs is another key trying to use an already-used code -- the
-- two get different responses later. @Nothing@ purchase key means the code has never been
-- redeemed.
getCodeByHash :: DB.Connection -> ByteString -> ExceptT ServiceError IO (Maybe (BadgeCode, Maybe C.PublicKeyEd25519))
getCodeByHash db codeHash = do
  rows <-
    liftIO $
      DB.query
        db
        [sql|
          SELECT c.code_hash, c.badge_type, c.months, c.batch, c.expires_at, c.redeemed_purchase_id, c.redeemed_at, c.unredeemed_at, c.revoked_at, c.created_at,
                 p.purchase_key
          FROM sx_badge_service_codes c
          LEFT JOIN sx_badge_service_badge_purchases p ON p.badge_purchase_id = c.redeemed_purchase_id
          WHERE c.code_hash = ?
        |]
        (Only (Binary codeHash))
  case rows of
    [] -> pure Nothing
    ((codeRow :. Only redeemerKey) : _) -> do
      code <- liftEither $ rowToCode codeRow
      pure $ Just (code, redeemerKey)

markCodeRedeemed :: DB.Connection -> ByteString -> Int64 -> UTCTime -> ExceptT ServiceError IO ()
markCodeRedeemed db codeHash badgePurchaseId now = do
  rows <-
    liftIO $
      DB.query
        db
        [sql|
          UPDATE sx_badge_service_codes
          SET redeemed_purchase_id = ?, redeemed_at = ?
          WHERE code_hash = ?
          RETURNING code_hash
        |]
        (badgePurchaseId, now, Binary codeHash)
  when (null (rows :: [Only (Binary ByteString)])) $ throwError SECodeNotFound

-- | Clears both redemption columns and sets @unredeemed_at@, which both re-enables
-- redemption and reopens E4's disclosure window.
unredeemCode :: DB.Connection -> ByteString -> UTCTime -> ExceptT ServiceError IO ()
unredeemCode db codeHash now = do
  rows <-
    liftIO $
      DB.query
        db
        [sql|
          UPDATE sx_badge_service_codes
          SET redeemed_purchase_id = NULL, redeemed_at = NULL, unredeemed_at = ?
          WHERE code_hash = ?
          RETURNING code_hash
        |]
        (now, Binary codeHash)
  when (null (rows :: [Only (Binary ByteString)])) $ throwError SECodeNotFound

insertCodes :: DB.Connection -> [NewBadgeCode] -> UTCTime -> ExceptT ServiceError IO ()
insertCodes db codes now =
  liftIO $
    DB.executeMany
      db
      [sql|
        INSERT INTO sx_badge_service_codes (code_hash, badge_type, months, batch, expires_at, created_at)
        VALUES (?,?,?,?,?,?)
      |]
      (map toRow codes)
  where
    toRow NewBadgeCode {codeHash, badgeType, months, batch, expiresAt} = (Binary codeHash, badgeType, months, batch, expiresAt, now)

revokeCode :: DB.Connection -> ByteString -> UTCTime -> ExceptT ServiceError IO ()
revokeCode db codeHash now = do
  rows <-
    liftIO $
      DB.query
        db
        [sql|
          UPDATE sx_badge_service_codes
          SET revoked_at = ?
          WHERE code_hash = ? AND revoked_at IS NULL
          RETURNING code_hash
        |]
        (now, Binary codeHash)
  when (null (rows :: [Only (Binary ByteString)])) $ throwError SECodeNotFound

-- | Sets @revoked_at@ on every unrevoked code of a batch through @idx_codes_batch@, which is
-- what B8's @codes revoke --batch@ calls. Returns the number of codes revoked; a batch name
-- that matches nothing (typo, already fully revoked) is not an error, just zero.
revokeBatch :: DB.Connection -> Text -> UTCTime -> ExceptT ServiceError IO Int
revokeBatch db batch now = do
  rows <-
    liftIO $
      DB.query
        db
        [sql|
          UPDATE sx_badge_service_codes
          SET revoked_at = ?
          WHERE batch = ? AND revoked_at IS NULL
          RETURNING code_hash
        |]
        (now, batch)
  pure $ length (rows :: [Only (Binary ByteString)])

-- Catalog -----------------------------------------------------------------------

-- | postgresql-simple has no 'DB.FromField' instance for any @Word*@ type (Postgres has no
-- unsigned integer type); sqlite-simple does. Every @Word8@\/@Word32@ column here is read as
-- a signed integer and converted, so both backends decode the same way; the range check turns
-- a corrupt or out-of-range value into a named 'SEDecodeError' rather than a silent
-- 'fromIntegral' wrap (the same hazard 'BadgeService.Catalog.chargeableMonths' guards against).
word8FromInt :: Text -> Int -> Either ServiceError Word8
word8FromInt label n
  | n >= 0 && n <= fromIntegral (maxBound :: Word8) = Right (fromIntegral n)
  | otherwise = Left $ SEDecodeError (label <> ": " <> tshow n <> " does not fit a Word8")

word32FromInt64 :: Text -> Int64 -> Either ServiceError Word32
word32FromInt64 label n
  | n >= 0 && n <= fromIntegral (maxBound :: Word32) = Right (fromIntegral n)
  | otherwise = Left $ SEDecodeError (label <> ": " <> tshow n <> " does not fit a Word32")

type PriceRow = (Text, BadgeType, Int64, Text, BadgeItemStatus, UTCTime)

rowToPrice :: PriceRow -> Either ServiceError BadgePrice
rowToPrice (priceId, badgeType, monthPriceMinor, currency, status, createdAt) = do
  monthPriceAmount <- word32FromInt64 ("price " <> priceId <> " month_price") monthPriceMinor
  Right BadgePrice {priceId = BadgePriceId priceId, badgeType, monthPrice = CurrencyAmount monthPriceAmount, currency, status, createdAt}

type OfferRow = (Text, Maybe Text, Int, Maybe Int, Maybe Int, BadgeItemStatus, UTCTime)

decodeDiscount :: Text -> Maybe Word8 -> Maybe Word8 -> Either ServiceError OfferDiscount
decodeDiscount _ (Just freeMonths) Nothing = Right (ODFreeMonths freeMonths)
decodeDiscount _ Nothing (Just discount) = Right (ODDiscount discount)
decodeDiscount offerId freeMonths discount =
  Left $ SEDecodeError ("offer " <> offerId <> ": invalid free_months/discount combination: " <> tshow (freeMonths, discount))

rowToOffer :: OfferRow -> Either ServiceError BadgeOffer
rowToOffer (offerId, priceId, monthsInt, freeMonthsInt, discountPercentInt, status, createdAt) = do
  months <- word8FromInt ("offer " <> offerId <> " months") monthsInt
  freeMonths <- mapM (word8FromInt ("offer " <> offerId <> " free_months")) freeMonthsInt
  discountPercent <- mapM (word8FromInt ("offer " <> offerId <> " discount")) discountPercentInt
  discount <- decodeDiscount offerId freeMonths discountPercent
  Right BadgeOffer {offerId = BadgeOfferId offerId, priceId = BadgePriceId <$> priceId, months, discount, status, createdAt, total = Nothing}

priceSelectColumns :: Query
priceSelectColumns = "price_id, badge_type, month_price, currency, status, created_at"

offerSelectColumns :: Query
offerSelectColumns = "offer_id, price_id, months, free_months, discount, status, created_at"

-- | Prices and offers with status 'BISActive' or 'BISDeprecated'. The offers query joins to
-- prices under the same status filter, which is what makes both guarantees hold at once: an
-- offer with a NULL @price_id@ has nothing to join to and is dropped, and an offer pinned to
-- a price that isn't itself active\/deprecated fails the join too. Every offer this returns
-- therefore has a resolvable price in the same result, which is the invariant
-- 'BadgeService.Catalog.catalogTotals' relies on to stay a total function.
getActiveCatalog :: DB.Connection -> ExceptT ServiceError IO BadgeCatalog
getActiveCatalog db = do
  priceRows <-
    liftIO $
      DB.query
        db
        ("SELECT " <> priceSelectColumns <> " FROM sx_badge_service_badge_prices WHERE status IN (?,?)")
        (BISActive, BISDeprecated)
  offerRows <-
    liftIO $
      DB.query
        db
        [sql|
          SELECT o.offer_id, o.price_id, o.months, o.free_months, o.discount, o.status, o.created_at
          FROM sx_badge_service_badge_offers o
          JOIN sx_badge_service_badge_prices p ON o.price_id = p.price_id
          WHERE o.status IN (?,?) AND p.status IN (?,?)
        |]
        (BISActive, BISDeprecated, BISActive, BISDeprecated)
  offers <- liftEither $ mapM rowToOffer offerRows
  prices <- liftEither $ mapM rowToPrice priceRows
  pure BadgeCatalog {prices, offers}

-- | Reachable regardless of status (including 'BISDisabled'), unlike 'getActiveCatalog'.
getPriceById :: DB.Connection -> BadgePriceId -> ExceptT ServiceError IO (Maybe BadgePrice)
getPriceById db (BadgePriceId priceId) = do
  rows <-
    liftIO $
      DB.query
        db
        ("SELECT " <> priceSelectColumns <> " FROM sx_badge_service_badge_prices WHERE price_id = ?")
        (Only priceId)
  case rows of
    [] -> pure Nothing
    (row : _) -> Just <$> liftEither (rowToPrice row)

-- | Reachable regardless of status, same as 'getPriceById'.
getOfferById :: DB.Connection -> BadgeOfferId -> ExceptT ServiceError IO (Maybe BadgeOffer)
getOfferById db (BadgeOfferId offerId) = do
  rows <-
    liftIO $
      DB.query
        db
        ("SELECT " <> offerSelectColumns <> " FROM sx_badge_service_badge_offers WHERE offer_id = ?")
        (Only offerId)
  case rows of
    [] -> pure Nothing
    (row : _) -> Just <$> liftEither (rowToOffer row)

-- | One of the only two production writers of a catalog status (with 'setOfferStatus'):
-- operators use it to deprecate a price on repricing.
setPriceStatus :: DB.Connection -> BadgePriceId -> BadgeItemStatus -> ExceptT ServiceError IO ()
setPriceStatus db (BadgePriceId priceId) status = do
  rows <-
    liftIO $
      DB.query
        db
        "UPDATE sx_badge_service_badge_prices SET status = ? WHERE price_id = ? RETURNING price_id"
        (status, priceId)
  when (null (rows :: [Only Text])) $ throwError SEPriceNotFound

setOfferStatus :: DB.Connection -> BadgeOfferId -> BadgeItemStatus -> ExceptT ServiceError IO ()
setOfferStatus db (BadgeOfferId offerId) status = do
  rows <-
    liftIO $
      DB.query
        db
        "UPDATE sx_badge_service_badge_offers SET status = ? WHERE offer_id = ? RETURNING offer_id"
        (status, offerId)
  when (null (rows :: [Only Text])) $ throwError SEOfferNotFound

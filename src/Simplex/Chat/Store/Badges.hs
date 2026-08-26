{-# LANGUAGE CPP #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

-- | The CLIENT's badge store: the user's purchases, the payment row a code redemption writes,
-- the issuances the worker collects and the ledger replica the service sends.
--
-- The tables are the ones @badgeSchema@ creates
-- ("Simplex.Chat.Store.SQLite.Migrations.M20260731_user_badges"), unprefixed, plus the
-- client-only columns that migration ALTERs in: @badge_purchases.user_id@\/@purchase_priv_key@,
-- @badge_ledger.entry_type_unknown@\/@entry_type_value@ and @users.shown_badge_id@. The service
-- has the same tables under its own prefix, so "BadgeService.Store" is the mirror of this module
-- and the column spellings of every status and entry type are deliberately identical to its
-- 'BadgeService.Store.encodeLedgerEntryType'\/'BadgeService.Store.decodeLedgerEntryType'; a second
-- spelling of @credit@\/@support@\/@opening@\/@lapse@ would be a defect, not a style choice.
--
-- Two things this module is NOT:
--
-- * It is not an author of ledger entries. The service writes the ledger (docs\/protocol
--   \/badges-rpc.md, "The ledger is written by the service alone"); the client keeps a verbatim
--   replica, so 'insertLedgerEntries' copies what it is given and mints nothing. @entry_uuid@ in
--   particular is the service's, never generated here.
--
-- * It opens no transaction. Every function takes a 'DB.Connection', as the rest of
--   "Simplex.Chat.Store" does, so a caller composes a payment row, a purchase row, the ledger
--   rows, an issuance and the shown-badge pointer into one @withStore@ transaction.
module Simplex.Chat.Store.Badges
  ( -- * Purchases and payments
    UserBadgePurchase (..),
    BadgeSlot (..),
    badgeSlot,
    createCodePayment,
    createPurchase,
    supersedePurchases,
    setShownPurchase,
    getShownPurchase,

    -- * Issuances
    NewBadgeIssuance (..),
    createIssuance,
    hasIssuanceForPeriod,

    -- * Ledger
    getLastBadgeLedgerEntry,
    checkStatementEntries,
    insertLedgerEntries,
  )
where

import Control.Monad (forM_, unless, when)
import Control.Monad.Except (ExceptT, liftEither, throwError)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as J
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LB
import Data.Int (Int64)
import Data.List (isPrefixOf)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Time.Clock (UTCTime)
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import Simplex.Chat.Badges (BadgeCredential, BadgeMasterKey (..), BadgeType (..))
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
import Simplex.Chat.PaymentService.Types (PaymentStatus (..))
import Simplex.Chat.Store.Shared (StoreError (..))
import Simplex.Messaging.Agent.Protocol (UserId)
import Simplex.Messaging.Agent.Store.DB (Binary (..), BoolInt (..))
import qualified Simplex.Messaging.Agent.Store.DB as DB
import qualified Simplex.Messaging.Crypto as C
#if defined(dbPostgres)
import Database.PostgreSQL.Simple (Only (..), Query, (:.) (..))
import Database.PostgreSQL.Simple.SqlQQ (sql)
#else
import Database.SQLite.Simple (Only (..), Query, (:.) (..))
import Database.SQLite.Simple.QQ (sql)
#endif

-- Purchases and payments ------------------------------------------------------

-- | The client's own projection of a @badge_purchases@ row: exactly its columns, with the
-- client-only @user_id@ and @purchase_priv_key@ that the service's table does not have.
--
-- This is deliberately NOT the single shared purchase record core §3 drafted as
-- @Badges.Types.BadgePurchase@. That draft declared @priceId@, @offerId@ and @credential@, none
-- of which is a column of this table: the price and offer of a purchase live on its
-- @badge_invoices@ rows (one per invoice, so not a function of the purchase), and the credential
-- lives on its @badge_issuances@ rows (one per period). A row type that had to invent three
-- fields to be constructed would report a purchase the database does not hold.
-- 'BadgeService.Store.BadgePurchaseRow' is the same decision on the service side. C2 deleted the
-- draft once both row types existed and split its API half off as
-- 'Simplex.Chat.Badges.Types.UserBadge', which carries no secrets and so may cross the FFI.
data UserBadgePurchase = UserBadgePurchase
  { badgePurchaseId :: Int64,
    userId :: UserId,
    purchaseKey :: C.PublicKeyEd25519,
    purchasePrivKey :: C.PrivateKeyEd25519,
    masterKey :: BadgeMasterKey,
    initialBadgeType :: BadgeType,
    currentBadgeType :: BadgeType,
    paymentId :: Maybe Text,
    status :: BadgePurchaseStatus,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving (Show)

-- | The badge a purchase occupies on a profile (plan §3): @paid@ for @supporter@ and @legend@,
-- @investor@ for @investor@. It is derived from @current_badge_type@; no column stores it. At
-- most one purchase per slot is 'PSIssued' at a time, which is what 'supersedePurchases'
-- maintains. A badge type this build does not know keeps its own slot rather than joining
-- @paid@: superseding a purchase because of a tag we cannot interpret would remove a badge the
-- user paid for.
data BadgeSlot = BSPaid | BSInvestor | BSOther Text
  deriving (Eq, Show)

badgeSlot :: BadgeType -> BadgeSlot
badgeSlot = \case
  BTSupporter -> BSPaid
  BTLegend -> BSPaid
  BTInvestor -> BSInvestor
  BTUnknown tag -> BSOther tag

-- | @payments.provider@ has no column codec yet: 'Simplex.Chat.PaymentService.Types.PaymentProvider'
-- has no 'TextEncoding' instance, and adding one is D0\/E2\/F1's, once a second provider needs
-- writing. Until then this literal is the client twin of
-- 'BadgeService.Store.codePaymentProviderText' and must keep the same spelling.
codePaymentProviderText :: Text
codePaymentProviderText = "code"

-- | The @payments@ row of a code redemption: a caller-minted UUID (the column is
-- @TEXT NOT NULL PRIMARY KEY@ with no default), @provider = 'code'@, no invoice — a code
-- payment never has one — and @settled@, because a redeemed code is paid for by definition.
-- Returns the id, which 'createPurchase' takes.
--
-- This row is the CLIENT's and is unrelated to the service's own @payments@ row for the same
-- redemption: the two databases share only the ledger entries, and even there the client's
-- @payment_id@ is NULL (see 'insertLedgerEntries').
createCodePayment :: DB.Connection -> UTCTime -> IO Text
createCodePayment db now = do
  paymentId <- UUID.toText <$> UUID.nextRandom
  DB.execute
    db
    [sql|
      INSERT INTO payments (payment_id, invoice_id, provider, status, created_at, updated_at)
      VALUES (?,?,?,?,?,?)
    |]
    (paymentId, Nothing :: Maybe Text, codePaymentProviderText, PSSettled, now, now)
  pure paymentId

-- | The purchase row of a code redemption, written on success only: a code redemption does not
-- learn its badge type until the response arrives, and @initial_badge_type@\/@current_badge_type@
-- are both NOT NULL with no default. Nothing is persisted before the send; a response lost in
-- flight is recovered with @codes unredeem@, not by reusing a stored key.
--
-- The status is 'PSIssued' directly, not 'PSAcquiring': the credential is in hand by the time
-- this runs.
createPurchase :: DB.Connection -> UserId -> C.PublicKeyEd25519 -> C.PrivateKeyEd25519 -> BadgeMasterKey -> BadgeType -> Text -> UTCTime -> IO UserBadgePurchase
createPurchase db userId purchaseKey purchasePrivKey masterKey@(BadgeMasterKey mk) badgeType paymentId now = do
  [Only badgePurchaseId] <-
    DB.query
      db
      [sql|
        INSERT INTO badge_purchases
          (user_id, purchase_key, purchase_priv_key, master_key, initial_badge_type, current_badge_type, payment_id, status, created_at, updated_at)
        VALUES (?,?,?,?,?,?,?,?,?,?)
        RETURNING badge_purchase_id
      |]
      ((userId, purchaseKey, purchasePrivKey, Binary mk, badgeType, badgeType, paymentId) :. (PSIssued, now, now))
  pure
    UserBadgePurchase
      { badgePurchaseId,
        userId,
        purchaseKey,
        purchasePrivKey,
        masterKey,
        initialBadgeType = badgeType,
        currentBadgeType = badgeType,
        paymentId = Just paymentId,
        status = PSIssued,
        createdAt = now,
        updatedAt = now
      }

-- | Moves every OTHER 'PSIssued' purchase of the same slot for this user to 'PSSuperseded', so
-- the slot has exactly one issued purchase again. The superseded row keeps its unconsumed
-- months: purchases are unlinkable, so the service cannot move a balance between them.
--
-- The slot comes from the KEPT ROW's own @current_badge_type@, read here, not from a badge type
-- the caller states: a caller that paired a purchase id with the wrong badge type would
-- otherwise clear the wrong slot and leave two issued purchases in the right one. That read also
-- scopes the purchase to the user, so a purchase id belonging to another profile cannot decide
-- which of this profile's badges is superseded.
--
-- The slot filter is applied in Haskell rather than as a SQL @IN@ over the slot's badge types:
-- @IN ?@ with a list is postgresql-simple's 'Database.PostgreSQL.Simple.In', which
-- sqlite-simple has no counterpart for, and a user holds at most a handful of purchases.
supersedePurchases :: DB.Connection -> UserId -> Int64 -> UTCTime -> ExceptT StoreError IO ()
supersedePurchases db userId keepPurchaseId now = do
  kept <-
    liftIO $
      DB.query
        db
        "SELECT current_badge_type FROM badge_purchases WHERE badge_purchase_id = ? AND user_id = ?"
        (keepPurchaseId, userId)
  slot <- case kept of
    (Only badgeType : _) -> pure $ badgeSlot badgeType
    [] -> throwError $ notThisUsersPurchase keepPurchaseId userId
  rows <-
    liftIO $
      DB.query
        db
        "SELECT badge_purchase_id, current_badge_type FROM badge_purchases WHERE user_id = ? AND status = ? AND badge_purchase_id <> ?"
        (userId, PSIssued, keepPurchaseId)
  liftIO $ forM_ [pId | (pId :: Int64, bt) <- rows, badgeSlot bt == slot] $ \pId ->
    DB.execute
      db
      "UPDATE badge_purchases SET status = ?, updated_at = ? WHERE badge_purchase_id = ?"
      (PSSuperseded, now, pId)

notThisUsersPurchase :: Int64 -> UserId -> StoreError
notThisUsersPurchase badgePurchaseId userId =
  SEInternalError $ "badge purchase " <> show badgePurchaseId <> " does not belong to user " <> show userId

-- | Points @users.shown_badge_id@ at a purchase. Separate from
-- 'Simplex.Chat.Store.Profiles.setUserBadge', which writes the profile's badge columns: this
-- one records WHICH purchase the profile's badge came from, which is what
-- 'getShownPurchase' reads back to sign the next @issueBadge@.
--
-- The purchase must be this user's. The column is a bare foreign key to @badge_purchases@ with
-- nothing scoping it to the row's owner, so without this guard one profile could be pointed at
-- another profile's purchase — and 'getShownPurchase' would then hand back the OTHER profile's
-- private key for the worker to sign @issueBadge@ with. The write is the cheapest place to
-- enforce it; 'getShownPurchase' enforces it again on read.
setShownPurchase :: DB.Connection -> UserId -> Int64 -> ExceptT StoreError IO ()
setShownPurchase db userId badgePurchaseId = do
  updated <-
    liftIO $
      DB.query
        db
        [sql|
          UPDATE users SET shown_badge_id = ?
          WHERE user_id = ?
            AND EXISTS (SELECT 1 FROM badge_purchases p WHERE p.badge_purchase_id = ? AND p.user_id = ?)
          RETURNING user_id
        |]
        (badgePurchaseId, userId, badgePurchaseId, userId)
  when (null (updated :: [Only UserId])) $ throwError $ notThisUsersPurchase badgePurchaseId userId

purchaseSelectColumns :: Query
purchaseSelectColumns =
  "p.badge_purchase_id, p.user_id, p.purchase_key, p.purchase_priv_key, p.master_key, "
    <> "p.initial_badge_type, p.current_badge_type, p.payment_id, p.status, p.created_at, p.updated_at"

type PurchaseRow =
  (Int64, Maybe UserId, C.PublicKeyEd25519, Maybe C.PrivateKeyEd25519, Binary ByteString)
    :. (BadgeType, BadgeType, Maybe Text, BadgePurchaseStatus, UTCTime, UTCTime)

-- | @user_id@ and @purchase_priv_key@ are nullable only because @20260731_user_badges@ adds them
-- with @ALTER TABLE@, which cannot add a NOT NULL column without a default. 'createPurchase' is
-- the only writer and always sets both, so a NULL is a corrupt row and is named as one rather
-- than being turned into a purchase whose key is missing — the private key is the whole point of
-- reading this row back.
rowToPurchase :: PurchaseRow -> Either StoreError UserBadgePurchase
rowToPurchase ((badgePurchaseId, userId_, purchaseKey, purchasePrivKey_, Binary mk) :. (initialBadgeType, currentBadgeType, paymentId, status, createdAt, updatedAt)) =
  case (userId_, purchasePrivKey_) of
    (Just userId, Just purchasePrivKey) ->
      Right
        UserBadgePurchase
          { badgePurchaseId,
            userId,
            purchaseKey,
            purchasePrivKey,
            masterKey = BadgeMasterKey mk,
            initialBadgeType,
            currentBadgeType,
            paymentId,
            status,
            createdAt,
            updatedAt
          }
    _ ->
      Left . SEInternalError $
        "badge purchase " <> show badgePurchaseId <> " has no user_id or purchase_priv_key"

-- | The purchase @users.shown_badge_id@ points at, with its keypair and badge master key —
-- what the badge screen renders and what the worker signs @issueBadge@ with. 'Nothing' when the
-- user has no badge, which is the ordinary case.
--
-- The join carries @AND p.user_id = u.user_id@: this row is read to obtain a PRIVATE KEY, and
-- @shown_badge_id@ is a bare foreign key to @badge_purchases@ that the schema does not scope to
-- the pointing user. A pointer that ever crossed profiles — through a restored database, a
-- future writer, or a bug in one — would otherwise hand this profile another profile's key to
-- sign with. It also makes 'rowToPurchase'\'s NULL @user_id@ check reachable rather than
-- vestigial, since the join now depends on that column.
getShownPurchase :: DB.Connection -> UserId -> ExceptT StoreError IO (Maybe UserBadgePurchase)
getShownPurchase db userId = do
  rows <-
    liftIO $
      DB.query
        db
        ( "SELECT "
            <> purchaseSelectColumns
            <> " FROM badge_purchases p JOIN users u ON u.shown_badge_id = p.badge_purchase_id AND p.user_id = u.user_id WHERE u.user_id = ?"
        )
        (Only userId)
  liftEither $ mapM rowToPurchase (listToMaybe rows)

-- Issuances -------------------------------------------------------------------

-- | Fields for one @badge_issuances@ row. As on the service side, the period and expiry are
-- definite here, matching the NOT NULL columns; 'BadgeIssuance' declares them 'Maybe' only
-- because it is also a wire shape.
data NewBadgeIssuance = NewBadgeIssuance
  { badgePurchaseId :: Int64,
    badgeType :: BadgeType,
    periodStart :: UTCTime,
    periodEnd :: UTCTime,
    expiry :: UTCTime,
    -- | The @badge_ledger.entry_id@ this issuance was debited by, when the caller has it.
    -- Named apart from 'BadgeIssuance'\'s @entryId@ (the same column) so the two do not collide
    -- as bare selectors under @DuplicateRecordFields@.
    ledgerEntryId :: Maybe Int64,
    credential :: BadgeCredential
  }

-- | Writes one issuance, minting its @issuance_id@ (the column is @TEXT NOT NULL PRIMARY KEY@
-- with no default). One row per period: the worker calls this again for each new period, so a
-- purchase accumulates issuances rather than replacing one.
--
-- The credential is stored as its own JSON encoding, the same one it crosses the wire in, so
-- there is no second, database-only codec for it to drift from.
createIssuance :: DB.Connection -> NewBadgeIssuance -> UTCTime -> IO BadgeIssuance
createIssuance db NewBadgeIssuance {badgePurchaseId, badgeType, periodStart, periodEnd, expiry, ledgerEntryId, credential} now = do
  issuanceId <- UUID.toText <$> UUID.nextRandom
  DB.execute
    db
    [sql|
      INSERT INTO badge_issuances
        (issuance_id, badge_purchase_id, entry_id, badge_type, period_start, period_end, expiry, credential, created_at)
      VALUES (?,?,?,?,?,?,?,?,?)
    |]
    (issuanceId, badgePurchaseId, ledgerEntryId, badgeType, periodStart, periodEnd, expiry, Binary (LB.toStrict (J.encode credential)), now)
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

-- | Whether an issuance already exists for this purchase and period start.
--
-- Nothing in the schema dedupes @badge_issuances@ on @(badge_purchase_id, period_start)@ the way
-- 'insertLedgerEntries' dedupes the ledger on @entry_uuid@: a fresh @issuance_id@ is minted on
-- every 'createIssuance' call. A healed ledger's complete history can re-present the same
-- @debit(badge)@ the client already holds an issuance for, so the caller must check this first.
hasIssuanceForPeriod :: DB.Connection -> Int64 -> UTCTime -> IO Bool
hasIssuanceForPeriod db badgePurchaseId periodStart =
  fromOnly . head
    <$> DB.query db "SELECT EXISTS (SELECT 1 FROM badge_issuances WHERE badge_purchase_id = ? AND period_start = ?)" (badgePurchaseId, periodStart)

-- Ledger ----------------------------------------------------------------------

-- | @entry_type, entry_credit_type, entry_debit_type, payment_id, charge_id, from_purchase_id,
-- to_purchase_id, entry_type_unknown, entry_type_value@.
--
-- The first seven are 'BadgeService.Store.LedgerTypeRow' exactly, spelling for spelling. The
-- last two are the client-only fallback columns @20260731_user_badges@ adds: a service ahead of
-- this build can send an entry type this build has no constructor for, and the client stores it
-- as received so a later version can decode it (docs\/protocol\/badges-rpc.md: "An unknown type
-- is stored as received and decoded after an app upgrade"). The service, authoring every entry
-- it writes, never needs them and its table does not have them.
type LedgerTypeRow = (Text, Maybe Text, Maybe Text, Maybe Text, Maybe Text, Maybe Int64, Maybe Int64, BoolInt, Maybe Text)

type LedgerCoreRow = (Int64, Text, Int64, Int, Int, UTCTime, BadgeType, Maybe UTCTime, UTCTime, UTCTime)

ledgerSelectColumns :: Query
ledgerSelectColumns =
  "entry_id, entry_uuid, badge_purchase_id, change_months, balance_months, balance_start_ts, balance_badge_type, was_paused_since, service_created_at, created_at, "
    <> "entry_type, entry_credit_type, entry_debit_type, payment_id, charge_id, from_purchase_id, to_purchase_id, entry_type_unknown, entry_type_value"

-- | The wire entry type as the client stores it: the inverse of
-- 'BadgeService.Service.statementEntryType'.
--
-- @payment@ becomes @'CTPayment' 'Nothing'@ and the wire's @invoiceId@ is DROPPED, which is not
-- a loss of information the client can hold: @badge_ledger.payment_id@ references @payments@,
-- and a code redemption writes no client @payments@ row for the SERVICE's payment — that row
-- exists only in the service database, and the wire carries no id for it. Inventing one would
-- make the column a dangling reference (plan §9, and §10's cross-database invariant, which
-- excludes @payment_id@ for exactly this reason).
--
-- @charge@, @transferIn@, @upgrade@ and @transferOut@ are refused, matching
-- 'BadgeService.Service.statementEntryType', which refuses to put any of them on the wire in the
-- first place, so nothing can send one. @charge@'s @chargeId@ is 'Int64' against a TEXT column
-- (an unresolved type mismatch, plan §9) and the other three carry a purchase KEY where the
-- column holds a purchase id; both would need a coercion this plan has not decided, and
-- subscriptions, upgrades and transfers are out of scope (§6).
storedEntryType :: StatementEntryType -> Either StoreError LedgerEntryType
storedEntryType = \case
  SECredit creditType -> LECredit <$> case creditType of
    SCPayment {} -> Right CTPayment {paymentId = Nothing}
    SCSupport -> Right CTSupport
    SCOpening -> Right CTOpening
    SCUnknown {tag, json} -> Right CTUnknown {tag, json}
    SCCharge {} -> unsupported "credit(charge)" "the stored LedgerCreditType.CTCharge types chargeId as Int64 while the wire and the charge_id column are both TEXT; subscriptions are out of scope"
    SCTransferIn {} -> unsupported "credit(transferIn)" "carries a purchase key where from_purchase_id holds a purchase id; transfers are out of scope"
  SEDebit debitType -> LEDebit <$> case debitType of
    SDRefund -> Right DTRefund
    SDSupport -> Right DTSupport
    SDBadge -> Right DTBadge
    SDLapse -> Right DTLapse
    SDUnknown {tag, json} -> Right DTUnknown {tag, json}
    SDUpgrade {} -> unsupported "debit(upgrade)" "carries a purchase key where to_purchase_id holds a purchase id; upgrades are out of scope"
    SDTransferOut {} -> unsupported "debit(transferOut)" "carries a purchase key where to_purchase_id holds a purchase id; transfers are out of scope"
  where
    unsupported what why = Left . SEInternalError $ "cannot store badge ledger " <> what <> ": " <> why

-- | Known tags are spelled exactly as 'BadgeService.Store.encodeLedgerEntryType' spells them —
-- the two stores write the same columns of the same schema, and the service reads back what it
-- wrote. An unknown tag keeps its own spelling in @entry_credit_type@\/@entry_debit_type@ and
-- its whole object in @entry_type_value@, flagged by @entry_type_unknown@, which is what makes
-- 'decodeLedgerEntryType' able to hand it back unchanged after an upgrade.
encodeLedgerEntryType :: LedgerEntryType -> Either StoreError LedgerTypeRow
encodeLedgerEntryType = \case
  LECredit creditType -> case creditType of
    CTPayment {paymentId} -> Right ("credit", Just "payment", Nothing, paymentId, Nothing, Nothing, Nothing, BI False, Nothing)
    CTSupport -> Right ("credit", Just "support", Nothing, Nothing, Nothing, Nothing, Nothing, BI False, Nothing)
    CTTransferIn {fromPurchaseId} -> Right ("credit", Just "transfer_in", Nothing, Nothing, Nothing, fromPurchaseId, Nothing, BI False, Nothing)
    CTOpening -> Right ("credit", Just "opening", Nothing, Nothing, Nothing, Nothing, Nothing, BI False, Nothing)
    CTUnknown {tag, json} -> Right ("credit", Just tag, Nothing, Nothing, Nothing, Nothing, Nothing, BI True, Just (encodeUnknown json))
    CTCharge {} -> Left $ SEInternalError "cannot store badge ledger credit(charge): chargeId is Int64 against the charge_id TEXT column"
  LEDebit debitType -> case debitType of
    DTRefund -> Right ("debit", Nothing, Just "refund", Nothing, Nothing, Nothing, Nothing, BI False, Nothing)
    DTUpgrade {toPurchaseId} -> Right ("debit", Nothing, Just "upgrade", Nothing, Nothing, Nothing, Just toPurchaseId, BI False, Nothing)
    DTTransferOut {toPurchaseId} -> Right ("debit", Nothing, Just "transfer_out", Nothing, Nothing, Nothing, Just toPurchaseId, BI False, Nothing)
    DTSupport -> Right ("debit", Nothing, Just "support", Nothing, Nothing, Nothing, Nothing, BI False, Nothing)
    DTBadge -> Right ("debit", Nothing, Just "badge", Nothing, Nothing, Nothing, Nothing, BI False, Nothing)
    DTLapse -> Right ("debit", Nothing, Just "lapse", Nothing, Nothing, Nothing, Nothing, BI False, Nothing)
    DTUnknown {tag, json} -> Right ("debit", Nothing, Just tag, Nothing, Nothing, Nothing, Nothing, BI True, Just (encodeUnknown json))

-- | @entry_type_value@ holds the entry type's whole JSON object, including its own @type@ key,
-- because that object is what 'Simplex.Chat.Badges.Service.SCUnknown' re-encodes verbatim. The
-- column is TEXT and aeson emits UTF-8, so it round-trips through 'decodeUnknown'.
encodeUnknown :: J.Object -> Text
encodeUnknown = decodeUtf8 . LB.toStrict . J.encode . J.Object

decodeUnknown :: Text -> Maybe J.Object
decodeUnknown = J.decodeStrict . encodeUtf8

-- | Reads back what 'encodeLedgerEntryType' wrote. The unknown flag is checked FIRST: a service
-- ahead of this build could use a tag this build later learns, and the flag records what this
-- row actually was when it was stored.
decodeLedgerEntryType :: LedgerTypeRow -> Either StoreError LedgerEntryType
decodeLedgerEntryType row = case row of
  ("credit", Just tag, _, _, _, _, _, BI True, Just v) -> unknownCredit tag v
  ("debit", _, Just tag, _, _, _, _, BI True, Just v) -> unknownDebit tag v
  -- payment_id is read whether or not it is there: a client-written entry always holds NULL.
  ("credit", Just "payment", _, paymentId, _, _, _, _, _) -> Right $ LECredit CTPayment {paymentId}
  ("credit", Just "support", _, _, _, _, _, _, _) -> Right $ LECredit CTSupport
  ("credit", Just "transfer_in", _, _, _, fromPurchaseId, _, _, _) -> Right $ LECredit CTTransferIn {fromPurchaseId}
  ("credit", Just "opening", _, _, _, _, _, _, _) -> Right $ LECredit CTOpening
  ("debit", _, Just "refund", _, _, _, _, _, _) -> Right $ LEDebit DTRefund
  ("debit", _, Just "upgrade", _, _, _, Just toPurchaseId, _, _) -> Right $ LEDebit DTUpgrade {toPurchaseId}
  ("debit", _, Just "transfer_out", _, _, _, Just toPurchaseId, _, _) -> Right $ LEDebit DTTransferOut {toPurchaseId}
  ("debit", _, Just "support", _, _, _, _, _, _) -> Right $ LEDebit DTSupport
  ("debit", _, Just "badge", _, _, _, _, _, _) -> Right $ LEDebit DTBadge
  ("debit", _, Just "lapse", _, _, _, _, _, _) -> Right $ LEDebit DTLapse
  (entryType, creditType, debitType, _, _, _, _, BI unknown, _) ->
    Left . SEInternalError $
      "malformed badge ledger entry type row: " <> show (entryType, creditType, debitType, unknown)
  where
    unknownCredit tag v = maybe (badValue tag) (\json -> Right $ LECredit CTUnknown {tag, json}) (decodeUnknown v)
    unknownDebit tag v = maybe (badValue tag) (\json -> Right $ LEDebit DTUnknown {tag, json}) (decodeUnknown v)
    badValue tag = Left . SEInternalError $ "badge ledger entry_type_value is not a JSON object, tag: " <> T.unpack tag

rowToLedgerEntry :: (LedgerCoreRow :. LedgerTypeRow) -> Either StoreError BadgeLedgerEntry
rowToLedgerEntry ((entryId, entryUuid, badgePurchaseId, changeMonths, balanceMonths, balanceStartTs, balanceBadgeType, wasPausedSince, serviceCreatedAt, createdAt) :. typeRow) = do
  entryType <- decodeLedgerEntryType typeRow
  Right BadgeLedgerEntry {entryId, entryUuid, badgePurchaseId, changeMonths, balanceMonths, balanceStartTs, balanceBadgeType, wasPausedSince, serviceCreatedAt, createdAt, entryType}

-- | The purchase's last entry: its @balanceMonths@ and @balanceStartTs@ are the balance the
-- client believes it holds, which is what the worker asserts to the service and what the badge
-- screen's paid-through date is computed from. Ordered by @entry_id@, which is the order
-- 'insertLedgerEntries' wrote the statement in.
getLastBadgeLedgerEntry :: DB.Connection -> Int64 -> ExceptT StoreError IO (Maybe BadgeLedgerEntry)
getLastBadgeLedgerEntry db badgePurchaseId = do
  rows <-
    liftIO $
      DB.query
        db
        ("SELECT " <> ledgerSelectColumns <> " FROM badge_ledger WHERE badge_purchase_id = ? ORDER BY entry_id DESC LIMIT 1")
        (Only badgePurchaseId)
  liftEither $ mapM rowToLedgerEntry (listToMaybe rows)

-- | Copies a statement's entries into the client's replica of the service's ledger.
--
-- __Order is carried only by array position.__ Every entry the service writes for one command
-- gets an identical @createdAt@, and @serviceCreatedAt@ is not on the wire at all, so
-- @statement.entries@ is inserted in the order given and @entry_id@ (the local autoincrement) is
-- the only thing that records it. Sorting by any timestamp would silently scramble the ledger.
--
-- __@previousEntryId@ decides REPLACE against APPEND.__ Present, it names an entry the client
-- already holds and the entries attach after it; absent with entries present, the entries
-- "attach to nothing" (docs\/protocol\/badges-rpc.md) — they are the COMPLETE ledger for this
-- purchase, and anything the client holds that the statement does not carry is stale and is
-- deleted. Appending on an absent @previousEntryId@ would leave a client that had healed or
-- reset holding rows the service no longer has. A statement with no entries at all changes
-- nothing either way: it attaches nothing and is not a claim that the ledger is empty.
--
-- REPLACE is expressed as "delete what the statement does not carry, then insert what it does",
-- not as a wipe-and-rewrite, so rows the service is merely re-sending keep their local
-- @entry_id@ — @badge_issuances.entry_id@ references them. When the surviving rows are not a
-- prefix of the delivered entries (a ledger the service rewrote rather than extended) they are
-- all dropped instead, so the re-inserted entries cannot end up out of order.
--
-- __Re-delivery is normal, not exceptional.__ @purchaseBadge@ and @getBadgeCatalog@ return the
-- complete history every time; only @issueBadge@ honours a cursor. Insertion is therefore
-- @ON CONFLICT (entry_uuid) DO NOTHING@ against @idx_badge_ledger_uuid@ — a second delivery of
-- the same statement writes nothing and changes nothing, rather than merely not crashing.
-- | Whether every entry of a statement can be stored, without opening a transaction or touching
-- the database.
--
-- 'insertLedgerEntries' is the only fallible call its callers make with rows already written in
-- the same transaction, and a @Left@ from it does NOT roll them back: @withStore@ runs its
-- 'ExceptT' inside the transaction, so a @Left@ returns normally and the transaction COMMITS
-- what preceded it. A caller therefore asks this first, outside the transaction, and the only
-- failure that reaches 'insertLedgerEntries' proper is one that would also have failed here.
--
-- It is exactly 'insertLedgerEntries'' own row conversion with the row discarded, so the two
-- cannot disagree about what is storable.
checkStatementEntries :: BadgeStatement -> Either StoreError ()
checkStatementEntries BadgeStatement {entries} =
  mapM_ (\StatementEntry {entryType} -> encodeLedgerEntryType =<< storedEntryType entryType) entries

insertLedgerEntries :: DB.Connection -> Int64 -> BadgeStatement -> UTCTime -> ExceptT StoreError IO ()
insertLedgerEntries db badgePurchaseId BadgeStatement {entries, previousEntryId} now = do
  rows <- liftEither $ mapM entryRow entries
  liftIO $ do
    case previousEntryId of
      Just _ -> pure ()
      Nothing -> unless (null entries) $ dropStaleEntries (map statementEntryId entries)
    forM_ rows $ \row ->
      DB.execute
        db
        [sql|
          INSERT INTO badge_ledger
            (entry_uuid, badge_purchase_id, change_months, balance_months, balance_start_ts, balance_badge_type, was_paused_since, service_created_at, created_at,
             entry_type, entry_credit_type, entry_debit_type, payment_id, charge_id, from_purchase_id, to_purchase_id, entry_type_unknown, entry_type_value)
          VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)
          ON CONFLICT (entry_uuid) DO NOTHING
        |]
        row
  where
    statementEntryId StatementEntry {entryId} = entryId
    -- service_created_at is the service's own clock, which the wire reports as the entry's
    -- createdAt; created_at is this client's, i.e. when the row was replicated. Keeping them
    -- apart is what lets the two databases be compared on the service's timestamp.
    entryRow StatementEntry {entryId, changeMonths, balanceMonths, balanceStartTs, balanceBadgeType, wasPausedSince, createdAt, entryType} = do
      typeRow <- encodeLedgerEntryType =<< storedEntryType entryType
      Right $
        (entryId, badgePurchaseId, changeMonths, balanceMonths, balanceStartTs, balanceBadgeType, wasPausedSince, createdAt, now)
          :. typeRow
    dropStaleEntries uuids = do
      held <-
        map fromOnly
          <$> DB.query
            db
            "SELECT entry_uuid FROM badge_ledger WHERE badge_purchase_id = ? ORDER BY entry_id ASC"
            (Only badgePurchaseId)
      let kept = filter (`elem` uuids) held
          stale
            | kept `isPrefixOf` uuids = filter (`notElem` uuids) held
            | otherwise = held
      forM_ stale $ \entryUuid ->
        DB.execute db "DELETE FROM badge_ledger WHERE badge_purchase_id = ? AND entry_uuid = ?" (badgePurchaseId, entryUuid)

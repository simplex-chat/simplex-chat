{-# LANGUAGE CPP #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.Badges
  ( BadgeCodeRedemption (..),
    UserBadgePurchase (..),
    getUserBadgePurchases,
    setBadgeAlertAcked,
    clearShownBadge,
    getBadgeCodeRedemption,
    createBadgeCodeRedemption,
    deleteBadgeCodeRedemption,
    createCodeBadgePurchase,
    storeBadgeIssuance,
    getLatestIssuedCredential,
    storeBadgeStatement,
    getBadgeLedgerBalance,
    getBadgeLedgerLastEntry,
    getBadgeLedgerEntryId,
  )
where

import Control.Concurrent.STM (TVar, atomically)
import Control.Monad (forM_)
import Crypto.Random (ChaChaDRG)
import qualified Data.Aeson as J
import qualified Data.ByteString.Lazy.Char8 as LB
import Data.Int (Int64)
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Simplex.Chat.Badges
import Simplex.Chat.Badges.Ledger
import Simplex.Chat.Badges.Service (StatementCreditType (..), StatementDebitType (..), StatementEntry (..), StatementEntryType (..))
import Simplex.Chat.Badges.Types (BadgeAlertKind, BadgePurchaseStatus (..))
import Simplex.Chat.Store.Shared (insertedRowId)
import Simplex.Chat.Types
import Simplex.Messaging.Agent.Store.DB (Binary (..), BoolInt (..))
import qualified Simplex.Messaging.Agent.Store.DB as DB
import qualified Simplex.Messaging.Crypto as C
import Simplex.Messaging.Encoding.String (strEncode)
import Simplex.Messaging.Util (maybeFirstRow, safeDecodeUtf8)

#if defined(dbPostgres)
import Database.PostgreSQL.Simple (Only (..), (:.) (..))
import Database.PostgreSQL.Simple.SqlQQ (sql)
#else
import Database.SQLite.Simple (Only (..), (:.) (..))
import Database.SQLite.Simple.QQ (sql)
#endif

-- | The keys one redemption attempt is signed with, stashed before the request is sent so that a
-- retry reaches the service as the same signer and is answered with the credential already issued.
data BadgeCodeRedemption = BadgeCodeRedemption
  { redemptionId :: Int64,
    purchaseKey :: C.PublicKeyEd25519,
    purchasePrivKey :: C.PrivateKeyEd25519,
    masterKey :: BadgeMasterKey
  }

getBadgeCodeRedemption :: DB.Connection -> User -> Text -> IO (Maybe BadgeCodeRedemption)
getBadgeCodeRedemption db User {userId} code =
  maybeFirstRow toRedemption $
    DB.query
      db
      [sql|
        SELECT badge_code_redemption_id, purchase_key, purchase_priv_key, master_key
        FROM badge_code_redemptions
        WHERE user_id = ? AND code = ?
      |]
      (userId, code)
  where
    toRedemption (redemptionId, purchaseKey, purchasePrivKey, Binary mk) =
      BadgeCodeRedemption {redemptionId, purchaseKey, purchasePrivKey, masterKey = BadgeMasterKey mk}

createBadgeCodeRedemption :: DB.Connection -> TVar ChaChaDRG -> User -> Text -> UTCTime -> IO BadgeCodeRedemption
createBadgeCodeRedemption db g User {userId} code now = do
  (purchaseKey, purchasePrivKey) <- atomically $ C.generateKeyPair g
  masterKey@(BadgeMasterKey mk) <- generateMasterKey g
  DB.execute
    db
    [sql|
      INSERT INTO badge_code_redemptions (user_id, code, purchase_key, purchase_priv_key, master_key, created_at)
      VALUES (?,?,?,?,?,?)
    |]
    (userId, code, purchaseKey, purchasePrivKey, Binary mk, now)
  redemptionId <- insertedRowId db
  pure BadgeCodeRedemption {redemptionId, purchaseKey, purchasePrivKey, masterKey}

-- | Drop a stashed attempt whose code the service refused for good, unless a purchase already
-- came from it - badge_purchases references this row.
deleteBadgeCodeRedemption :: DB.Connection -> Int64 -> IO ()
deleteBadgeCodeRedemption db redemptionId =
  DB.execute
    db
    [sql|
      DELETE FROM badge_code_redemptions
      WHERE badge_code_redemption_id = ?
        AND NOT EXISTS (SELECT 1 FROM badge_purchases WHERE badge_code_redemption_id = ?)
    |]
    (redemptionId, redemptionId)

-- | The purchase a redeemed code created and the profile's pointer to it. 'False' when the code
-- was already redeemed here: the service replays the credential it issued, and that must add no
-- purchase and leave the shown badge alone - though its statement is still stored, so a client
-- that lost the first response ends holding the rows the service holds.
createCodeBadgePurchase :: DB.Connection -> User -> BadgeCodeRedemption -> BadgeCredential -> UTCTime -> IO (Int64, Bool)
createCodeBadgePurchase db User {userId} redemption credential now =
  getCodeBadgePurchase db redemption >>= \case
    Just purchaseId -> pure (purchaseId, False)
    Nothing -> do
      DB.execute
        db
        [sql|
          INSERT INTO badge_purchases
            (user_id, purchase_key, purchase_priv_key, master_key, initial_badge_type, current_badge_type, status, badge_code_redemption_id, created_at, updated_at)
          VALUES (?,?,?,?,?,?,?,?,?,?)
        |]
        (userId, purchaseKey, purchasePrivKey, Binary mk, badgeType, badgeType, PSIssued, redemptionId, now, now)
      purchaseId <- insertedRowId db
      DB.execute db "UPDATE users SET shown_badge_id = ? WHERE user_id = ?" (purchaseId, userId)
      pure (purchaseId, True)
  where
    BadgeCodeRedemption {redemptionId, purchaseKey, purchasePrivKey, masterKey = BadgeMasterKey mk} = redemption
    BadgeCredential {badgeInfo = BadgeInfo {badgeType}} = credential

-- | The credential the service issued, stored against the @debit(badge)@ row it was issued for.
-- The period comes from the ledger rather than from the credential, whose expiry is the end of the
-- following Sunday and so up to a week later than the month actually ends.
-- 'False' when the ledger has no row before the one issued, so no period can be named for it.
-- It should be unreachable - a debit(badge) row always has a predecessor, and statements are
-- stored in order - but silence here would leave the ledger saying a month was issued while no
-- issuance holds its credential, and the profile showing last month's badge with no diagnostic.
storeBadgeIssuance :: DB.Connection -> TVar ChaChaDRG -> Int64 -> Int64 -> BadgeCredential -> UTCTime -> IO Bool
storeBadgeIssuance db g badgePurchaseId entryId credential now = do
  period_ <- getIssuedPeriod db badgePurchaseId entryId
  fmap (const $ isJust period_) $ forM_ period_ $ \(periodStart, periodEnd) -> do
    issuanceId <- safeDecodeUtf8 . strEncode <$> atomically (C.randomBytes 16 g)
    DB.execute
      db
      [sql|
        INSERT INTO badge_issuances (issuance_id, badge_purchase_id, entry_id, badge_type, period_start, period_end, expiry, credential, created_at)
        VALUES (?,?,?,?,?,?,?,?,?)
      |]
      ((issuanceId, badgePurchaseId, entryId, badgeType) :. (periodStart, periodEnd, badgeExpiry, Binary (LB.toStrict $ J.encode credential), now))
  where
    BadgeCredential {badgeInfo = BadgeInfo {badgeType, badgeExpiry}} = credential

-- | The credential of the latest month issued for this badge, which is the one the profile should
-- be showing.
getLatestIssuedCredential :: DB.Connection -> Int64 -> IO (Maybe BadgeCredential)
getLatestIssuedCredential db badgePurchaseId = do
  rows <-
    DB.query
      db
      [sql|
        SELECT credential FROM badge_issuances
        WHERE badge_purchase_id = ?
        ORDER BY period_end DESC
        LIMIT 1
      |]
      (Only badgePurchaseId)
  pure $ case rows of
    [Only (Binary bs)] -> J.decodeStrict' bs
    _ -> Nothing

-- | The month a @debit(badge)@ row issued: from the balance start of the row before it to its own.
-- Month arithmetic clips, so the start cannot be recovered by subtracting a month from the end.
getIssuedPeriod :: DB.Connection -> Int64 -> Int64 -> IO (Maybe (UTCTime, UTCTime))
getIssuedPeriod db badgePurchaseId entryId = do
  rows <-
    DB.query
      db
      [sql|
        SELECT
          (SELECT p.balance_start_ts FROM badge_ledger p
           WHERE p.badge_purchase_id = e.badge_purchase_id AND p.entry_id < e.entry_id
           ORDER BY p.entry_id DESC LIMIT 1),
          e.balance_start_ts
        FROM badge_ledger e
        WHERE e.badge_purchase_id = ? AND e.entry_id = ?
      |]
      (badgePurchaseId, entryId)
  -- no preceding row means no credit was ever stored, so the period this row issued is unknown
  pure $ case rows of
    [(Just periodStart, periodEnd)] -> Just (periodStart, periodEnd)
    _ -> Nothing

getCodeBadgePurchase :: DB.Connection -> BadgeCodeRedemption -> IO (Maybe Int64)
getCodeBadgePurchase db BadgeCodeRedemption {redemptionId} =
  maybeFirstRow fromOnly $
    DB.query db "SELECT badge_purchase_id FROM badge_purchases WHERE badge_code_redemption_id = ?" (Only redemptionId)

-- | A badge the worker may have work for, with the keys its requests are signed with.
data UserBadgePurchase = UserBadgePurchase
  { badgePurchaseId :: Int64,
    purchaseKey :: C.PublicKeyEd25519,
    purchasePrivKey :: C.PrivateKeyEd25519,
    masterKey :: BadgeMasterKey,
    badgeType :: BadgeType,
    shown :: Bool,
    alertAcked :: Maybe (BadgeAlertKind, Text),
    alertSnoozeUntil :: Maybe UTCTime
  }

getUserBadgePurchases :: DB.Connection -> User -> IO [UserBadgePurchase]
getUserBadgePurchases db User {userId} =
  map toPurchase
    <$> DB.query
      db
      [sql|
        SELECT p.badge_purchase_id, p.purchase_key, p.purchase_priv_key, p.master_key, p.current_badge_type,
               (u.shown_badge_id = p.badge_purchase_id), p.alert_acked_kind, p.alert_acked_episode, p.alert_snooze_until
        FROM badge_purchases p
        JOIN users u ON u.user_id = p.user_id
        WHERE p.user_id = ? AND p.purchase_priv_key IS NOT NULL
        ORDER BY p.badge_purchase_id
      |]
      (Only userId)
  where
    toPurchase (badgePurchaseId, purchaseKey, purchasePrivKey, Binary mk, badgeType, shown_, ackedKind_, ackedEpisode_, alertSnoozeUntil) =
      UserBadgePurchase
        { badgePurchaseId,
          purchaseKey,
          purchasePrivKey,
          masterKey = BadgeMasterKey mk,
          badgeType,
          shown = maybe False unBI shown_,
          alertAcked = (,) <$> ackedKind_ <*> ackedEpisode_,
          alertSnoozeUntil
        }

-- | Acknowledging an alert records the occurrence it answered, so the same one is not shown again;
-- snoozing holds it until the given time instead.
setBadgeAlertAcked :: DB.Connection -> Int64 -> BadgeAlertKind -> Text -> Maybe UTCTime -> IO ()
setBadgeAlertAcked db badgePurchaseId kind episode snoozeUntil = case snoozeUntil of
  Nothing ->
    DB.execute
      db
      "UPDATE badge_purchases SET alert_acked_kind = ?, alert_acked_episode = ?, alert_snooze_until = NULL WHERE badge_purchase_id = ?"
      (kind, episode, badgePurchaseId)
  Just t ->
    DB.execute
      db
      "UPDATE badge_purchases SET alert_snooze_until = ? WHERE badge_purchase_id = ?"
      (t, badgePurchaseId)

-- | Stop showing a badge that has expired unrenewed; the profile update is broadcast by the caller.
clearShownBadge :: DB.Connection -> User -> Int64 -> IO ()
clearShownBadge db User {userId} badgePurchaseId =
  DB.execute db "UPDATE users SET shown_badge_id = NULL WHERE user_id = ? AND shown_badge_id = ?" (userId, badgePurchaseId)

-- | Store the statement's entries exactly as the service sent them, entry_uuid and entry type
-- included. The client authors no row of its own, or the two sides would stop holding the same
-- ledger. Re-applying a statement adds nothing: the unique index on entry_uuid would otherwise
-- throw on a retry rather than no-op.
storeBadgeStatement :: DB.Connection -> Int64 -> [StatementEntry] -> UTCTime -> IO ()
storeBadgeStatement db badgePurchaseId entries now = forM_ entries storeEntry
  where
    storeEntry StatementEntry {entryId, changeMonths, balanceMonths, balanceStartTs, balanceBadgeType, wasPausedSince, createdAt, entryType} =
      DB.execute
        db
        [sql|
          INSERT INTO badge_ledger
            (entry_uuid, badge_purchase_id, change_months, balance_months, balance_start_ts, balance_badge_type,
             was_paused_since, service_created_at, created_at, entry_type, entry_credit_type, entry_debit_type,
             entry_type_unknown, entry_type_value)
          VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?,?)
          ON CONFLICT (entry_uuid) DO NOTHING
        |]
        ( (entryId, badgePurchaseId, changeMonths, balanceMonths, balanceStartTs, balanceBadgeType, wasPausedSince)
            :. (createdAt, now, entryTypeT, creditType, debitType, BI (isJust unknown_), unknownJSON)
        )
      where
        (entryTypeT, creditType, debitType) = entryTypeColumns entryType
        -- a type this version cannot decode is kept verbatim, to be read after an app upgrade
        unknown_ = case entryType of
          SECredit SCUnknown {json} -> Just json
          SEDebit SDUnknown {json} -> Just json
          _ -> Nothing
        unknownJSON = safeDecodeUtf8 . LB.toStrict . J.encode <$> unknown_

-- | The balance is the last row, on both sides; nothing derives it by summing the history.
getBadgeLedgerBalance :: DB.Connection -> Int64 -> IO (Maybe LedgerBalance)
getBadgeLedgerBalance db badgePurchaseId =
  maybeFirstRow toBalance $
    DB.query
      db
      [sql|
        SELECT balance_months, balance_start_ts, balance_badge_type
        FROM badge_ledger
        WHERE badge_purchase_id = ?
        ORDER BY entry_id DESC
        LIMIT 1
      |]
      (Only badgePurchaseId)
  where
    toBalance (balanceMonths, balanceStartTs, balanceBadgeType) = LedgerBalance {balanceMonths, balanceStartTs, balanceBadgeType}

-- | The entry the client asserts to the service: its last row, or none before the first statement.
getBadgeLedgerLastEntry :: DB.Connection -> Int64 -> IO (Maybe StatementEntry)
getBadgeLedgerLastEntry db badgePurchaseId =
  (>>= toEntry) <$> maybeFirstRow id (DB.query db q (Only badgePurchaseId))
  where
    q =
      [sql|
        SELECT entry_uuid, change_months, balance_months, balance_start_ts, balance_badge_type,
               was_paused_since, service_created_at, entry_type, entry_credit_type, entry_debit_type
        FROM badge_ledger
        WHERE badge_purchase_id = ?
        ORDER BY entry_id DESC
        LIMIT 1
      |]
    toEntry (entryId, changeMonths, balanceMonths, balanceStartTs, balanceBadgeType, wasPausedSince, createdAt, entryType_, credit_, debit_) =
      (\entryType -> StatementEntry {entryId, changeMonths, balanceMonths, balanceStartTs, balanceBadgeType, wasPausedSince, createdAt, entryType})
        <$> entryTypeFromColumns entryType_ credit_ debit_

-- | The local id of a replicated row, which the issuance written beside it references.
getBadgeLedgerEntryId :: DB.Connection -> Int64 -> Text -> IO (Maybe Int64)
getBadgeLedgerEntryId db badgePurchaseId entryUuid =
  maybeFirstRow fromOnly $
    DB.query db "SELECT entry_id FROM badge_ledger WHERE badge_purchase_id = ? AND entry_uuid = ?" (badgePurchaseId, entryUuid)

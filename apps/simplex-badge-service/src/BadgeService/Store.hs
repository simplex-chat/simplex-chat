{-# LANGUAGE CPP #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module BadgeService.Store
  ( IssuedCode (..),
    KeyRedemption (..),
    KeyPurchase (..),
    NewCodePurchase (..),
    ServicePurchase (..),
    ManagedGroup (..),
    getManagedGroup,
    insertManagedGroup,
    clearCodeGroupItems,
    markOwnerBootstrapped,
    getBadgeCode,
    getCodePurchaseForKey,
    purchaseKeyExists,
    getPurchaseByKey,
    getLedgerTip,
    getLedgerEntryId,
    getLedgerEntries,
    getCurrentIssuance,
    appendLedgerPlan,
    createCodePurchase,
    insertBadgeCode,
    setCodeGroupItem,
    CodeTracker (..),
    getCodeTracker,
    getEditableTrackers,
    RevokeResult (..),
    revokeCode,
  )
where

import BadgeService.Store.Invoices (executeChanging)
import Control.Monad (forM)
import qualified Data.Aeson as J
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as LB
import Data.Int (Int64)
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Simplex.Chat.Badges (BadgeCredential, BadgeMasterKey (..), BadgeType)
import Simplex.Chat.Badges.Ledger
import Simplex.Chat.Badges.Service (StatementEntry (..))
import Simplex.Chat.Badges.Types (BadgeCodePaymentStatus, BadgePurchaseStatus (..))
import Simplex.Chat.Store.Shared (insertedRowId)
import Simplex.Messaging.Agent.Store.DB (Binary (..), BoolInt (..))
import qualified Simplex.Messaging.Agent.Store.DB as DB
import qualified Simplex.Messaging.Crypto as C
import Simplex.Messaging.Util (maybeFirstRow, maybeFirstRow')

#if defined(dbPostgres)
import Database.PostgreSQL.Simple (Only (..), (:.) (..))
import Database.PostgreSQL.Simple.SqlQQ (sql)
#else
import Database.SQLite.Simple (Only (..), (:.) (..))
import Database.SQLite.Simple.QQ (sql)
#endif

data IssuedCode = IssuedCode
  { badgeCodeId :: Int64,
    badgeType :: BadgeType,
    months :: Int,
    paymentStatus :: BadgeCodePaymentStatus,
    revokedAt :: Maybe UTCTime,
    expiresAt :: Maybe UTCTime,
    redeemLimit :: Int,
    redeemCount :: Int
  }

data KeyRedemption
  = KeyUnredeemed
  | KeyRedeemed KeyPurchase
  | KeyRedeemedUnreadable

data KeyPurchase = KeyPurchase
  { badgePurchaseId :: Int64,
    credential :: BadgeCredential
  }

data NewCodePurchase = NewCodePurchase
  { badgeCodeId :: Int64,
    purchaseKey :: C.PublicKeyEd25519,
    masterKey :: BadgeMasterKey,
    badgeType :: BadgeType
  }

data ServicePurchase = ServicePurchase
  { badgePurchaseId :: Int64,
    masterKey :: BadgeMasterKey,
    badgeType :: BadgeType
  }

data ManagedGroup = ManagedGroup
  { mgGroupId :: Int64,
    mgGroupLink :: Text,
    mgOwnerBootstrapped :: Bool
  }
  deriving (Eq)

-- The join link is a bearer secret, so it is left out.
instance Show ManagedGroup where
  show ManagedGroup {mgGroupId, mgOwnerBootstrapped} = "managed group " <> show mgGroupId <> ", owner set up: " <> show mgOwnerBootstrapped

getManagedGroup :: DB.Connection -> IO (Maybe ManagedGroup)
getManagedGroup db =
  maybeFirstRow toGroup $
    DB.query_ db "SELECT group_id, group_link, owner_bootstrapped FROM sx_badge_service_group LIMIT 1"
  where
    toGroup (mgGroupId, mgGroupLink, BI mgOwnerBootstrapped) = ManagedGroup {mgGroupId, mgGroupLink, mgOwnerBootstrapped}

-- getManagedGroup reads with no ordering, so a second row would change which group is used.
insertManagedGroup :: DB.Connection -> Int64 -> Text -> UTCTime -> IO ()
insertManagedGroup db gid link now =
  DB.execute
    db
    [sql|
      INSERT INTO sx_badge_service_group (group_id, group_link, owner_bootstrapped, created_at)
      SELECT ?,?,0,? WHERE NOT EXISTS (SELECT 1 FROM sx_badge_service_group)
    |]
    (gid, link, now)

-- A tracker's item id is only found in the group it was posted to, so a new group starts with none.
clearCodeGroupItems :: DB.Connection -> IO ()
clearCodeGroupItems db =
  DB.execute_ db "UPDATE sx_badge_service_badge_codes SET group_item_id = NULL, group_item_sent_at = NULL WHERE group_item_id IS NOT NULL"

markOwnerBootstrapped :: DB.Connection -> Int64 -> IO Bool
markOwnerBootstrapped db gid =
  (> 0)
    <$> executeChanging
      db
      "UPDATE sx_badge_service_group SET owner_bootstrapped = 1 WHERE group_id = ? AND owner_bootstrapped = 0"
      (Only gid)

getBadgeCode :: DB.Connection -> ByteString -> IO (Maybe IssuedCode)
getBadgeCode db codeHash =
  maybeFirstRow toCode $
    DB.query
      db
      [sql|
        SELECT badge_code_id, badge_type, months, code_payment_status, revoked_at, expires_at, redeem_limit, redeem_count
        FROM sx_badge_service_badge_codes
        WHERE code_hash = ?
      |]
      (Only (Binary codeHash))
  where
    toCode (badgeCodeId, badgeType, months, paymentStatus, revokedAt, expiresAt, redeemLimit, redeemCount) =
      IssuedCode {badgeCodeId, badgeType, months, paymentStatus, revokedAt, expiresAt, redeemLimit, redeemCount}

getCodePurchaseForKey :: DB.Connection -> Int64 -> C.PublicKeyEd25519 -> IO KeyRedemption
getCodePurchaseForKey db badgeCodeId key =
  maybeFirstRow' KeyUnredeemed toRedemption $
    DB.query
      db
      [sql|
        SELECT p.badge_purchase_id, i.credential
        FROM sx_badge_service_badge_purchases p
        LEFT JOIN sx_badge_service_badge_issuances i ON i.badge_purchase_id = p.badge_purchase_id
        WHERE p.badge_code_id = ? AND p.purchase_key = ?
        ORDER BY i.period_end DESC
        LIMIT 1
      |]
      (badgeCodeId, key)
  where
    toRedemption (badgePurchaseId, credential_) = case decodeCredential =<< credential_ of
      Just credential -> KeyRedeemed KeyPurchase {badgePurchaseId, credential}
      Nothing -> KeyRedeemedUnreadable
    decodeCredential (Binary bs) = J.decodeStrict' bs

purchaseKeyExists :: DB.Connection -> C.PublicKeyEd25519 -> IO Bool
purchaseKeyExists db key =
  maybeFirstRow' False (\(Only (_ :: Int64)) -> True) $
    DB.query db "SELECT badge_purchase_id FROM sx_badge_service_badge_purchases WHERE purchase_key = ?" (Only key)

getPurchaseByKey :: DB.Connection -> C.PublicKeyEd25519 -> IO (Maybe ServicePurchase)
getPurchaseByKey db key =
  maybeFirstRow toPurchase $
    DB.query
      db
      [sql|
        SELECT badge_purchase_id, master_key, current_badge_type
        FROM sx_badge_service_badge_purchases
        WHERE purchase_key = ?
      |]
      (Only key)
  where
    toPurchase (badgePurchaseId, Binary mk, badgeType) =
      ServicePurchase {badgePurchaseId, masterKey = BadgeMasterKey mk, badgeType}

getLedgerTip :: DB.Connection -> Int64 -> IO (Maybe StatementEntry)
getLedgerTip db purchaseId =
  maybeFirstRow' Nothing toEntry $
    DB.query
      db
      [sql|
        SELECT entry_uuid, change_months, balance_months, balance_start_ts, balance_anchor_ts, balance_badge_type,
               entry_type, entry_credit_type, entry_debit_type, service_created_at
        FROM sx_badge_service_badge_ledger
        WHERE badge_purchase_id = ?
        ORDER BY entry_id DESC
        LIMIT 1
      |]
      (Only purchaseId)

getLedgerEntryId :: DB.Connection -> Int64 -> Text -> IO (Maybe Int64)
getLedgerEntryId db purchaseId entryUuid =
  maybeFirstRow fromOnly $
    DB.query
      db
      "SELECT entry_id FROM sx_badge_service_badge_ledger WHERE badge_purchase_id = ? AND entry_uuid = ?"
      (purchaseId, entryUuid)

-- | 0 returns the whole ledger, as entry_id starts at 1.
getLedgerEntries :: DB.Connection -> Int64 -> Int64 -> IO (Maybe [StatementEntry])
getLedgerEntries db purchaseId afterEntryId =
  mapM toEntry
    <$> DB.query
      db
      [sql|
        SELECT entry_uuid, change_months, balance_months, balance_start_ts, balance_anchor_ts, balance_badge_type,
               entry_type, entry_credit_type, entry_debit_type, service_created_at
        FROM sx_badge_service_badge_ledger
        WHERE badge_purchase_id = ? AND entry_id > ?
        ORDER BY entry_id
      |]
      (purchaseId, afterEntryId)

toEntry :: (Text, Int, Int, UTCTime, UTCTime, BadgeType, Text, Maybe Text, Maybe Text, UTCTime) -> Maybe StatementEntry
toEntry (entryId, changeMonths, balanceMonths, balanceStartTs, balanceAnchorTs, balanceBadgeType, entryType_, credit_, debit_, createdAt) =
  (\entryType -> StatementEntry {entryId, changeMonths, balanceMonths, balanceStartTs, balanceAnchorTs, balanceBadgeType, wasPausedSince = Nothing, createdAt, entryType})
    <$> entryTypeFromColumns entryType_ credit_ debit_

getCurrentIssuance :: DB.Connection -> Int64 -> UTCTime -> IO (Maybe BadgeCredential)
getCurrentIssuance db purchaseId now = do
  rs <-
    DB.query
      db
      [sql|
        SELECT credential FROM sx_badge_service_badge_issuances
        WHERE badge_purchase_id = ? AND period_end > ?
        ORDER BY period_end DESC
        LIMIT 1
      |]
      (purchaseId, now)
  pure $ case rs of
    [Only (Binary bs)] -> J.decodeStrict' bs
    _ -> Nothing

-- TODO write the reference columns (payment_id, charge_id, from_purchase_id, to_purchase_id) for entry types that carry one; only the tag is written today.
appendLedgerPlan :: DB.Connection -> Int64 -> [StatementEntry] -> Maybe (StatementEntry, StatementEntry, BadgeCredential) -> IO ()
appendLedgerPlan db purchaseId rows issuance_ = do
  mapM_ appendRow rows
  case issuance_ of
    Nothing -> pure ()
    Just (previous, issued@StatementEntry {entryId, balanceStartTs = periodEnd, balanceBadgeType, createdAt}, credential) -> do
      rowId <- appendRow issued
      DB.execute
        db
        [sql|
          INSERT INTO sx_badge_service_badge_issuances
            (issuance_id, badge_purchase_id, entry_id, badge_type, period_start, period_end, expiry, credential, created_at)
          VALUES (?,?,?,?,?,?,?,?,?)
        |]
        ( (entryId, purchaseId, rowId, balanceBadgeType)
            :. (balanceStartTs previous, periodEnd, endOfMondayAfter periodEnd, Binary (LB.toStrict $ J.encode credential), createdAt)
        )
  where
    appendRow StatementEntry {entryId, changeMonths, balanceMonths, balanceStartTs, balanceAnchorTs, balanceBadgeType, createdAt, entryType} = do
      let (entryTypeT, creditType, debitType) = entryTypeColumns entryType
      DB.execute
        db
        [sql|
          INSERT INTO sx_badge_service_badge_ledger
            (entry_uuid, badge_purchase_id, change_months, balance_months, balance_start_ts, balance_anchor_ts,
             balance_badge_type, service_created_at, created_at, entry_type, entry_credit_type, entry_debit_type)
          VALUES (?,?,?,?,?,?,?,?,?,?,?,?)
        |]
        ((entryId, purchaseId, changeMonths, balanceMonths, balanceStartTs, balanceAnchorTs) :. (balanceBadgeType, createdAt, createdAt, entryTypeT, creditType, debitType))
      insertedRowId db

-- The claim takes one use before adding the purchase, so a concurrent revoke or redemption waits on this row and sees the new count.
-- Run it in the credential's transaction. It returns the use count this claim reached.
createCodePurchase :: DB.Connection -> NewCodePurchase -> UTCTime -> IO (Maybe (Int64, Int))
createCodePurchase db NewCodePurchase {badgeCodeId, purchaseKey, masterKey = BadgeMasterKey mk, badgeType} now = do
  claimed_ <-
    maybeFirstRow fromOnly $
      DB.query
        db
        "UPDATE sx_badge_service_badge_codes SET redeem_count = redeem_count + 1, redeemed_at = ? WHERE badge_code_id = ? AND redeem_count < redeem_limit AND revoked_at IS NULL RETURNING redeem_count"
        (now, badgeCodeId)
  forM claimed_ $ \claimedCount -> do
    DB.execute
      db
      [sql|
        INSERT INTO sx_badge_service_badge_purchases
          (purchase_key, master_key, initial_badge_type, current_badge_type, status, badge_code_id, created_at, updated_at)
        VALUES (?,?,?,?,?,?,?,?)
      |]
      (purchaseKey, Binary mk, badgeType, badgeType, PSIssued, badgeCodeId, now, now)
    (,claimedCount) <$> insertedRowId db

-- | Revoked and AlreadyRevoked carry the code id, so the caller can retire the code's group tracker,
-- or repair one an earlier revoke left live.
data RevokeResult = Revoked Int64 | AlreadyRevoked Int64 | AlreadyRedeemed | NoSuchCode
  deriving (Eq, Show)

-- | A code with no uses left can't be revoked, because every badge it grants was already given out.
revokeCode :: DB.Connection -> ByteString -> UTCTime -> IO RevokeResult
revokeCode db codeHash now = do
  revoked <-
    executeChanging
      db
      "UPDATE sx_badge_service_badge_codes SET revoked_at = ? WHERE code_hash = ? AND revoked_at IS NULL AND redeem_count < redeem_limit"
      (now, Binary codeHash)
  -- The result is read in the same transaction as the UPDATE, so the row it answers about is the row that changed.
  maybeFirstRow' NoSuchCode (result revoked) $
    DB.query db "SELECT badge_code_id, revoked_at FROM sx_badge_service_badge_codes WHERE code_hash = ?" (Only (Binary codeHash))
  where
    result :: Int -> (Int64, Maybe UTCTime) -> RevokeResult
    result revoked (badgeCodeId, revokedAt)
      | revoked > 0 = Revoked badgeCodeId
      | isJust revokedAt = AlreadyRevoked badgeCodeId
      | otherwise = AlreadyRedeemed

insertBadgeCode :: DB.Connection -> ByteString -> BadgeType -> Int -> BadgeCodePaymentStatus -> Int -> UTCTime -> IO Int64
insertBadgeCode db codeHash badgeType months paymentStatus redeemLimit now = do
  DB.execute
    db
    [sql|
      INSERT INTO sx_badge_service_badge_codes (code_hash, badge_type, months, code_payment_status, redeem_limit, created_at)
      VALUES (?,?,?,?,?,?)
    |]
    (Binary codeHash, badgeType, months, paymentStatus, redeemLimit, now)
  insertedRowId db

setCodeGroupItem :: DB.Connection -> Int64 -> Int64 -> UTCTime -> IO ()
setCodeGroupItem db badgeCodeId itemId sentAt =
  DB.execute
    db
    "UPDATE sx_badge_service_badge_codes SET group_item_id = ?, group_item_sent_at = ? WHERE badge_code_id = ?"
    (itemId, sentAt, badgeCodeId)

data CodeTracker = CodeTracker
  { trackerItemId :: Int64,
    trackerSentAt :: UTCTime,
    redeemLimit :: Int,
    redeemCount :: Int,
    revokedAt :: Maybe UTCTime,
    redeemedAt :: Maybe UTCTime
  }

getCodeTracker :: DB.Connection -> Int64 -> IO (Maybe CodeTracker)
getCodeTracker db badgeCodeId =
  maybeFirstRow toTracker $
    DB.query
      db
      [sql|
        SELECT group_item_id, group_item_sent_at, redeem_limit, redeem_count, revoked_at, redeemed_at
        FROM sx_badge_service_badge_codes
        WHERE badge_code_id = ? AND group_item_id IS NOT NULL AND group_item_sent_at IS NOT NULL
      |]
      (Only badgeCodeId)
  where
    toTracker (trackerItemId, trackerSentAt, redeemLimit, redeemCount, revokedAt, redeemedAt) =
      CodeTracker {trackerItemId, trackerSentAt, redeemLimit, redeemCount, revokedAt, redeemedAt}

getEditableTrackers :: DB.Connection -> UTCTime -> IO [(Int64, Int64)]
getEditableTrackers db sentAfter =
  DB.query
    db
    [sql|
      SELECT badge_code_id, group_item_id
      FROM sx_badge_service_badge_codes
      WHERE group_item_id IS NOT NULL AND group_item_sent_at > ?
      ORDER BY badge_code_id
    |]
    (Only sentAfter)

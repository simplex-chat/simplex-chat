{-# LANGUAGE CPP #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module BadgeService.Store
  ( IssuedCode (..),
    CodeRedemption (..),
    RedeemedCode (..),
    NewCodePurchase (..),
    ServicePurchase (..),
    LedgerTip (..),
    ServiceLedgerEntry (..),
    SignedPlan (..),
    getBadgeCode,
    purchaseKeyExists,
    getPurchaseByKey,
    getLedgerTip,
    getLedgerEntryId,
    getLedgerEntries,
    getCurrentIssuance,
    appendLedgerPlan,
    createCodePurchase,
    insertBadgeCode,
  )
where

import Control.Concurrent.STM (TVar, atomically)
import Crypto.Random (ChaChaDRG)
import qualified Data.Aeson as J
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as LB
import Data.Int (Int64)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Simplex.Chat.Badges (BadgeCredential, BadgeMasterKey (..), BadgeType)
import Simplex.Chat.Badges.Ledger
import Simplex.Chat.Badges.Service (StatementEntryType)
import Simplex.Chat.Badges.Types (BadgeCodePaymentStatus, BadgePurchaseStatus (..))
import Simplex.Chat.Store.Shared (insertedRowId)
import Simplex.Messaging.Agent.Store.DB (Binary (..))
import qualified Simplex.Messaging.Agent.Store.DB as DB
import qualified Simplex.Messaging.Crypto as C
import Simplex.Messaging.Encoding.String (strEncode)
import Simplex.Messaging.Util (maybeFirstRow, maybeFirstRow', safeDecodeUtf8)

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
    redemption :: CodeRedemption
  }

-- A code that has a purchase is spent, even if its credential cannot be read. Treating that as
-- an unredeemed code would issue a second credential for it.
data CodeRedemption
  = CodeUnredeemed
  | CodeRedeemed RedeemedCode
  | CodeRedeemedUnreadable

data RedeemedCode = RedeemedCode
  { badgePurchaseId :: Int64,
    purchaseKey :: C.PublicKeyEd25519,
    credential :: BadgeCredential
  }

-- Its rows and issuance are appended by 'appendLedgerPlan' in the same transaction: a code marked
-- redeemed while another write failed would be spent with no credential, and nothing reissues it.
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

data LedgerTip = LedgerTip
  { tipEntryId :: Int64,
    tipEntryUuid :: Text,
    tipBalance :: LedgerBalance
  }

data ServiceLedgerEntry = ServiceLedgerEntry
  { entryUuid :: Text,
    changeMonths :: Int,
    balance :: LedgerBalance,
    entryType :: StatementEntryType,
    createdAt :: UTCTime
  }

-- | The plan once signing succeeded, holding the credential against the row that spends the
-- month, so that row cannot be written without it.
data SignedPlan = SignedPlan
  { spRows :: [LedgerRow],
    spIssuance :: Maybe (LedgerRow, BadgePeriod, BadgeCredential)
  }

getBadgeCode :: DB.Connection -> ByteString -> IO (Maybe IssuedCode)
getBadgeCode db codeHash =
  maybeFirstRow toCode $
    DB.query
      db
      [sql|
        SELECT c.badge_code_id, c.badge_type, c.months, p.badge_purchase_id, p.purchase_key, i.credential
        FROM sx_badge_service_badge_codes c
        LEFT JOIN sx_badge_service_badge_purchases p ON p.badge_code_id = c.badge_code_id
        LEFT JOIN sx_badge_service_badge_issuances i ON i.badge_purchase_id = p.badge_purchase_id
        WHERE c.code_hash = ?
        ORDER BY i.period_end DESC
        LIMIT 1
      |]
      (Only (Binary codeHash))
  where
    toCode (badgeCodeId, badgeType, months, purchaseId_, purchaseKey_, credential_) =
      IssuedCode {badgeCodeId, badgeType, months, redemption = codeRedemption purchaseId_ purchaseKey_ credential_}
    codeRedemption purchaseId_ purchaseKey_ credential_ = case (purchaseId_, purchaseKey_) of
      (Just badgePurchaseId, Just purchaseKey) -> case decodeCredential =<< credential_ of
        Just credential -> CodeRedeemed RedeemedCode {badgePurchaseId, purchaseKey, credential}
        Nothing -> CodeRedeemedUnreadable
      _ -> CodeUnredeemed
    decodeCredential (Binary bs) = J.decodeStrict' bs

purchaseKeyExists :: DB.Connection -> C.PublicKeyEd25519 -> IO Bool
purchaseKeyExists db key =
  maybeFirstRow' False (\(Only (_ :: Int64)) -> True) $
    DB.query db "SELECT badge_purchase_id FROM sx_badge_service_badge_purchases WHERE purchase_key = ?" (Only key)

-- | The only route from a command to a purchase, so a client cannot name one it cannot sign for.
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

getLedgerTip :: DB.Connection -> Int64 -> IO (Maybe LedgerTip)
getLedgerTip db purchaseId =
  maybeFirstRow toTip $
    DB.query
      db
      [sql|
        SELECT entry_id, entry_uuid, balance_months, balance_start_ts, balance_anchor_ts, balance_badge_type
        FROM sx_badge_service_badge_ledger
        WHERE badge_purchase_id = ?
        ORDER BY entry_id DESC
        LIMIT 1
      |]
      (Only purchaseId)
  where
    toTip (tipEntryId, tipEntryUuid, balanceMonths, balanceStartTs, balanceAnchorTs, balanceBadgeType) =
      LedgerTip {tipEntryId, tipEntryUuid, tipBalance = LedgerBalance {balanceMonths, balanceStartTs, balanceAnchorTs, balanceBadgeType}}

-- | Scoped to the client's own purchase, so asserting another's entry tells it nothing.
getLedgerEntryId :: DB.Connection -> Int64 -> Text -> IO (Maybe Int64)
getLedgerEntryId db purchaseId entryUuid =
  maybeFirstRow fromOnly $
    DB.query
      db
      "SELECT entry_id FROM sx_badge_service_badge_ledger WHERE badge_purchase_id = ? AND entry_uuid = ?"
      (purchaseId, entryUuid)

-- | 0 for the whole ledger, as entry_id starts at 1. 'Nothing' when a stored row has a type this
-- version cannot represent, rather than sending it changed into another.
getLedgerEntries :: DB.Connection -> Int64 -> Int64 -> IO (Maybe [ServiceLedgerEntry])
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
  where
    toEntry (entryUuid, changeMonths, balanceMonths, balanceStartTs, balanceAnchorTs, balanceBadgeType, entryType_, credit_, debit_, createdAt) =
      (\entryType -> ServiceLedgerEntry {entryUuid, changeMonths, balance = LedgerBalance {balanceMonths, balanceStartTs, balanceAnchorTs, balanceBadgeType}, entryType, createdAt})
        <$> entryTypeFromColumns entryType_ credit_ debit_

-- | Answers a repeat inside an issued month, rather than signing the same content twice.
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

-- | entry_uuid is assigned here, so no caller can author one.
-- TODO [badges] also write the reference columns - payment_id, charge_id, from_purchase_id,
-- to_purchase_id - for the entry types that carry one. Only the tag is written today, so a
-- payment, charge, transferIn, upgrade or transferOut row would be stored without its reference.
appendLedgerPlan :: DB.Connection -> TVar ChaChaDRG -> Int64 -> SignedPlan -> UTCTime -> IO ()
appendLedgerPlan db g purchaseId SignedPlan {spRows, spIssuance} now = do
  mapM_ appendRow spRows
  case spIssuance of
    Nothing -> pure ()
    Just (issueRow, BadgePeriod {periodStart, periodEnd, badgeExpiry}, credential) -> do
      entryId <- appendRow issueRow
      issuanceId <- randomId g
      DB.execute
        db
        [sql|
          INSERT INTO sx_badge_service_badge_issuances
            (issuance_id, badge_purchase_id, entry_id, badge_type, period_start, period_end, expiry, credential, created_at)
          VALUES (?,?,?,?,?,?,?,?,?)
        |]
        ((issuanceId, purchaseId, entryId, balanceBadgeType (rowBalance issueRow)) :. (periodStart, periodEnd, badgeExpiry, Binary (LB.toStrict $ J.encode credential), now))
  where
    appendRow LedgerRow {rowChange, rowBalance, rowType} = do
      entryUuid <- randomId g
      let LedgerBalance {balanceMonths, balanceStartTs, balanceAnchorTs, balanceBadgeType} = rowBalance
          (entryType, creditType, debitType) = entryTypeColumns rowType
      DB.execute
        db
        [sql|
          INSERT INTO sx_badge_service_badge_ledger
            (entry_uuid, badge_purchase_id, change_months, balance_months, balance_start_ts, balance_anchor_ts,
             balance_badge_type, service_created_at, created_at, entry_type, entry_credit_type, entry_debit_type)
          VALUES (?,?,?,?,?,?,?,?,?,?,?,?)
        |]
        ((entryUuid, purchaseId, rowChange, balanceMonths, balanceStartTs, balanceAnchorTs) :. (balanceBadgeType, now, now, entryType, creditType, debitType))
      insertedRowId db

randomId :: TVar ChaChaDRG -> IO Text
randomId g = safeDecodeUtf8 . strEncode <$> atomically (C.randomBytes 16 g)

-- the caller has already signed and appends the plan in this same transaction, so no code is
-- left spent without a credential
createCodePurchase :: DB.Connection -> NewCodePurchase -> UTCTime -> IO Int64
createCodePurchase db NewCodePurchase {badgeCodeId, purchaseKey, masterKey = BadgeMasterKey mk, badgeType} now = do
  DB.execute
    db
    [sql|
      INSERT INTO sx_badge_service_badge_purchases
        (purchase_key, master_key, initial_badge_type, current_badge_type, status, badge_code_id, created_at, updated_at)
      VALUES (?,?,?,?,?,?,?,?)
    |]
    (purchaseKey, Binary mk, badgeType, badgeType, PSIssued, badgeCodeId, now, now)
  purchaseId <- insertedRowId db
  DB.execute db "UPDATE sx_badge_service_badge_codes SET redeemed_at = ? WHERE badge_code_id = ?" (now, badgeCodeId)
  pure purchaseId

insertBadgeCode :: DB.Connection -> ByteString -> BadgeType -> Int -> BadgeCodePaymentStatus -> UTCTime -> IO ()
insertBadgeCode db codeHash badgeType months paymentStatus now =
  DB.execute
    db
    [sql|
      INSERT INTO sx_badge_service_badge_codes (code_hash, badge_type, months, code_payment_status, created_at)
      VALUES (?,?,?,?,?)
    |]
    (Binary codeHash, badgeType, months, paymentStatus, now)

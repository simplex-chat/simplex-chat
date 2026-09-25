{-# LANGUAGE CPP #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module BadgeService.Store
  ( IssuedCode (..),
    FundingClaim (..),
    ClaimedPurchase (..),
    NewCodePurchase (..),
    NewStorePurchase (..),
    ServicePurchase (..),
    getBadgeCode,
    getStorePaymentClaim,
    purchaseKeyExists,
    getPurchaseByKey,
    getLedgerTip,
    getLedgerEntryId,
    getLedgerEntries,
    getCurrentIssuance,
    appendLedgerPlan,
    createCodePurchase,
    createStorePurchase,
    insertBadgeCode,
    RevokeResult (..),
    revokeCode,
  )
where

import BadgeService.Store.Invoices (executeChanging, paymentStatusText, providerText)
import qualified Data.Aeson as J
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as LB
import Data.Int (Int64)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Simplex.Chat.Badges (BadgeCredential, BadgeMasterKey (..), BadgeType)
import Simplex.Chat.Badges.Ledger
import Simplex.Chat.Badges.Service (StatementCreditType (..), StatementEntry (..), StatementEntryType (..))
import Simplex.Chat.Badges.Types (BadgeCodePaymentStatus, BadgePurchaseStatus (..))
import Simplex.Chat.PaymentService.Types (CurrencyAmount (..), InvoiceId (..), PaymentProvider, PaymentStatus (..))
import Simplex.Chat.Store.Shared (insertedRowId)
import Simplex.Messaging.Agent.Store.DB (Binary (..))
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
    redemption :: FundingClaim
  }

-- | Whether a code or a store payment has already funded a purchase, and which.
data FundingClaim
  = Unclaimed
  | Claimed ClaimedPurchase
  | ClaimedUnreadable

data ClaimedPurchase = ClaimedPurchase
  { badgePurchaseId :: Int64,
    purchaseKey :: C.PublicKeyEd25519,
    credential :: BadgeCredential
  }

data NewCodePurchase = NewCodePurchase
  { badgeCodeId :: Int64,
    purchaseKey :: C.PublicKeyEd25519,
    masterKey :: BadgeMasterKey,
    badgeType :: BadgeType
  }

data NewStorePurchase = NewStorePurchase
  { paymentId :: Text,
    provider :: PaymentProvider,
    providerRef :: Text,
    paid :: Maybe (CurrencyAmount, Text),
    purchaseKey :: C.PublicKeyEd25519,
    masterKey :: BadgeMasterKey,
    badgeType :: BadgeType
  }

data ServicePurchase = ServicePurchase
  { badgePurchaseId :: Int64,
    masterKey :: BadgeMasterKey,
    badgeType :: BadgeType
  }

getBadgeCode :: DB.Connection -> ByteString -> IO (Maybe IssuedCode)
getBadgeCode db codeHash =
  maybeFirstRow toCode $
    DB.query
      db
      [sql|
        SELECT c.badge_code_id, c.badge_type, c.months, c.code_payment_status, c.revoked_at,
               c.expires_at, p.badge_purchase_id, p.purchase_key, i.credential
        FROM sx_badge_service_badge_codes c
        LEFT JOIN sx_badge_service_badge_purchases p ON p.badge_code_id = c.badge_code_id
        LEFT JOIN sx_badge_service_badge_issuances i ON i.badge_purchase_id = p.badge_purchase_id
        WHERE c.code_hash = ?
        ORDER BY i.period_end DESC
        LIMIT 1
      |]
      (Only (Binary codeHash))
  where
    toCode (badgeCodeId, badgeType, months, paymentStatus, revokedAt, expiresAt, purchaseId_, purchaseKey_, credential_) =
      IssuedCode {badgeCodeId, badgeType, months, paymentStatus, revokedAt, expiresAt, redemption = fundingClaim purchaseId_ purchaseKey_ credential_}

-- | The purchase a store transaction's payment funds, read the same way as a code's.
getStorePaymentClaim :: DB.Connection -> PaymentProvider -> Text -> IO FundingClaim
getStorePaymentClaim db provider providerRef =
  maybeFirstRow' Unclaimed (\(purchaseId, purchaseKey, credential_) -> fundingClaim (Just purchaseId) (Just purchaseKey) credential_) $
    DB.query
      db
      [sql|
        SELECT p.badge_purchase_id, p.purchase_key, i.credential
        FROM sx_badge_service_payments pay
        JOIN sx_badge_service_badge_purchases p ON p.payment_id = pay.payment_id
        LEFT JOIN sx_badge_service_badge_issuances i ON i.badge_purchase_id = p.badge_purchase_id
        WHERE pay.provider = ? AND pay.provider_ref = ?
        ORDER BY i.period_end DESC
        LIMIT 1
      |]
      (providerText provider, providerRef)

fundingClaim :: Maybe Int64 -> Maybe C.PublicKeyEd25519 -> Maybe (Binary ByteString) -> FundingClaim
fundingClaim purchaseId_ purchaseKey_ credential_ = case (purchaseId_, purchaseKey_) of
  (Just badgePurchaseId, Just purchaseKey) -> case decodeCredential =<< credential_ of
    Just credential -> Claimed ClaimedPurchase {badgePurchaseId, purchaseKey, credential}
    Nothing -> ClaimedUnreadable
  _ -> Unclaimed
  where
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
        SELECT l.entry_uuid, l.change_months, l.balance_months, l.balance_start_ts, l.balance_anchor_ts, l.balance_badge_type,
               l.entry_type, l.entry_credit_type, l.entry_debit_type, l.service_created_at, l.payment_id, pay.invoice_id
        FROM sx_badge_service_badge_ledger l
        LEFT JOIN sx_badge_service_payments pay ON pay.payment_id = l.payment_id
        WHERE l.badge_purchase_id = ?
        ORDER BY l.entry_id DESC
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
        SELECT l.entry_uuid, l.change_months, l.balance_months, l.balance_start_ts, l.balance_anchor_ts, l.balance_badge_type,
               l.entry_type, l.entry_credit_type, l.entry_debit_type, l.service_created_at, l.payment_id, pay.invoice_id
        FROM sx_badge_service_badge_ledger l
        LEFT JOIN sx_badge_service_payments pay ON pay.payment_id = l.payment_id
        WHERE l.badge_purchase_id = ? AND l.entry_id > ?
        ORDER BY l.entry_id
      |]
      (purchaseId, afterEntryId)

toEntry :: (Text, Int, Int, UTCTime, UTCTime, BadgeType, Text, Maybe Text, Maybe Text, UTCTime) :. (Maybe Text, Maybe Text) -> Maybe StatementEntry
toEntry ((entryId, changeMonths, balanceMonths, balanceStartTs, balanceAnchorTs, balanceBadgeType, entryType_, credit_, debit_, createdAt) :. (paymentId_, invoiceId_)) =
  (\entryType -> StatementEntry {entryId, changeMonths, balanceMonths, balanceStartTs, balanceAnchorTs, balanceBadgeType, wasPausedSince = Nothing, createdAt, entryType})
    <$> entryTypeFromColumns ((InvoiceId <$> invoiceId_) <$ paymentId_) entryType_ credit_ debit_

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

-- | creditPaymentId_ names the payment behind a payment credit among the rows.
-- TODO write the other reference columns (charge_id, from_purchase_id, to_purchase_id) for entry types that carry one; only the tag is written today.
appendLedgerPlan :: DB.Connection -> Int64 -> Maybe Text -> [StatementEntry] -> Maybe (StatementEntry, StatementEntry, BadgeCredential) -> IO ()
appendLedgerPlan db purchaseId creditPaymentId_ rows issuance_ = do
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
          paymentId_ = case entryType of
            SECredit SCPayment {} -> creditPaymentId_
            _ -> Nothing
      DB.execute
        db
        [sql|
          INSERT INTO sx_badge_service_badge_ledger
            (entry_uuid, badge_purchase_id, change_months, balance_months, balance_start_ts, balance_anchor_ts,
             balance_badge_type, service_created_at, created_at, entry_type, entry_credit_type, entry_debit_type, payment_id)
          VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?)
        |]
        ((entryId, purchaseId, changeMonths, balanceMonths, balanceStartTs, balanceAnchorTs) :. (balanceBadgeType, createdAt, createdAt, entryTypeT, creditType, debitType, paymentId_))
      insertedRowId db

-- redeemed_at is stamped here, so this must run in the same transaction as the credential rows.
-- Mark the code as redeemed before adding the purchase. On Postgres, a revoke or redemption running
-- at the same time then waits, sees the code is taken, and fails.
createCodePurchase :: DB.Connection -> NewCodePurchase -> UTCTime -> IO (Maybe Int64)
createCodePurchase db NewCodePurchase {badgeCodeId, purchaseKey, masterKey = BadgeMasterKey mk, badgeType} now = do
  claimed <-
    executeChanging
      db
      "UPDATE sx_badge_service_badge_codes SET redeemed_at = ? WHERE badge_code_id = ? AND redeemed_at IS NULL AND revoked_at IS NULL"
      (now, badgeCodeId)
  if claimed == 0
    then pure Nothing
    else do
      DB.execute
        db
        [sql|
          INSERT INTO sx_badge_service_badge_purchases
            (purchase_key, master_key, initial_badge_type, current_badge_type, status, badge_code_id, created_at, updated_at)
          VALUES (?,?,?,?,?,?,?,?)
        |]
        (purchaseKey, Binary mk, badgeType, badgeType, PSIssued, badgeCodeId, now, now)
      Just <$> insertedRowId db

-- | Must run in the same transaction as the credential rows. The payment is inserted before the
-- purchase, and its (provider, provider_ref) is unique: a transaction presented twice at once, or
-- under another key, inserts nothing the second time and fails.
createStorePurchase :: DB.Connection -> NewStorePurchase -> UTCTime -> IO (Maybe Int64)
createStorePurchase db NewStorePurchase {paymentId, provider, providerRef, paid, purchaseKey, masterKey = BadgeMasterKey mk, badgeType} now = do
  claimed <-
    executeChanging
      db
      [sql|
        INSERT INTO sx_badge_service_payments (payment_id, provider, provider_ref, amount, currency, status, created_at, updated_at)
        VALUES (?,?,?,?,?,?,?,?)
        ON CONFLICT (provider, provider_ref) DO NOTHING
      |]
      (paymentId, providerText provider, providerRef, (\(CurrencyAmount a) -> a) . fst <$> paid, snd <$> paid, paymentStatusText PSSettled, now, now)
  if claimed == 0
    then pure Nothing
    else do
      DB.execute
        db
        [sql|
          INSERT INTO sx_badge_service_badge_purchases
            (purchase_key, master_key, initial_badge_type, current_badge_type, status, payment_id, created_at, updated_at)
          VALUES (?,?,?,?,?,?,?,?)
        |]
        (purchaseKey, Binary mk, badgeType, badgeType, PSIssued, paymentId, now, now)
      Just <$> insertedRowId db

data RevokeResult = Revoked | AlreadyRevoked | AlreadyRedeemed | NoSuchCode
  deriving (Eq, Show)

-- | A code that was already redeemed can't be revoked, because its badge was already given out.
revokeCode :: DB.Connection -> ByteString -> UTCTime -> IO RevokeResult
revokeCode db codeHash now = do
  revoked <-
    executeChanging
      db
      "UPDATE sx_badge_service_badge_codes SET revoked_at = ? WHERE code_hash = ? AND revoked_at IS NULL AND redeemed_at IS NULL"
      (now, Binary codeHash)
  if revoked > 0
    then pure Revoked
    else
      maybeFirstRow' NoSuchCode refusal $
        DB.query db "SELECT revoked_at FROM sx_badge_service_badge_codes WHERE code_hash = ?" (Only (Binary codeHash))
  where
    refusal :: Only (Maybe UTCTime) -> RevokeResult
    refusal (Only revokedAt) = maybe AlreadyRedeemed (const AlreadyRevoked) revokedAt

insertBadgeCode :: DB.Connection -> ByteString -> BadgeType -> Int -> BadgeCodePaymentStatus -> UTCTime -> IO ()
insertBadgeCode db codeHash badgeType months paymentStatus now =
  DB.execute
    db
    [sql|
      INSERT INTO sx_badge_service_badge_codes (code_hash, badge_type, months, code_payment_status, created_at)
      VALUES (?,?,?,?,?)
    |]
    (Binary codeHash, badgeType, months, paymentStatus, now)

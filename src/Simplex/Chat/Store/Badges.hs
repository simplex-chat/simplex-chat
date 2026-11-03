{-# LANGUAGE CPP #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeOperators #-}

module Simplex.Chat.Store.Badges
  ( BadgeStash (..),
    BadgeStashRef (..),
    UserBadgePurchase (..),
    UnrenewedRun (..),
    PurchaseKind (..),
    getUserBadgePurchase,
    getBadgePurchase,
    setBadgeSubscriptionStatus,
    userHasBadge,
    setBadgeAlertAcked,
    setBadgeIssueError,
    setBadgeNextWake,
    setBadgeUnrenewedRun,
    clearShownBadge,
    getBadgeCodeRedemption,
    createBadgeCodeRedemption,
    BadgeReceiptRecord (..),
    BadgeReceiptStatus (..),
    StoreHoldKind (..),
    holdStoreReceipt,
    holdStoreTransfer,
    purchaseUnreached,
    purchaseMovedOut,
    paymentMovedOut,
    getReceiptRecord,
    createBadgeStoreReceipt,
    getOpenStorePurchases,
    getPendingStoreReceipts,
    getNextHeldStoreReceipt,
    getStoreReceiptFailures,
    storeReceiptHeldFor,
    recordStoreReceiptFailure,
    refuseStoreReceipt,
    clearStoreReceiptRefusal,
    settleStoreReceipt,
    reholdStoreReceipt,
    showChargedPurchase,
    deleteUnfundedStoreReceipts,
    deleteBadgeCodeRedemption,
    createStashBadgePurchase,
    getStashBadgePurchase,
    storeBadgeIssuance,
    getLatestIssuedCredential,
    storeBadgeStatement,
    setBadgePurchaseType,
    getBadgeLedgerLastEntry,
    getLastChargeCreditAt,
    getBadgeLedger,
    getBadgeLedgerEntryId,
  )
where

import Control.Concurrent.STM (TVar, atomically)
import Control.Monad (forM, forM_, join, when)
import Crypto.Random (ChaChaDRG)
import qualified Data.Aeson as J
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as LB
import Data.Int (Int64)
import Data.List (find)
import Data.Maybe (isJust, mapMaybe)
import Data.Text (Text)
import Data.Time.Clock (UTCTime, addUTCTime, nominalDay)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Simplex.Chat.Badges
import Simplex.Chat.Badges.Ledger
import Simplex.Chat.Badges.Service (StatementCreditType (..), StatementDebitType (..), StatementEntry (..), StatementEntryType (..))
import Simplex.Chat.Badges.Types (BadgeAlertKind, BadgeIssueError (..), BadgeIssueFailure (..), BadgePurchaseStatus (..), BadgeServiceErrorCode (..), BadgeSubscription (..), OpenStorePurchase (..), StoreReceiptOrigin (..), SubscriptionState (..))
import Simplex.Chat.PaymentService (AppleTransactionFacts (..), ServicePayment (..), appleTransactionFacts)
import Simplex.Chat.PaymentService.Types (StoreTransactionRef (..))
import Simplex.Chat.Store.Shared (StoreError, insertedRowId)
import Simplex.Chat.Types
import Simplex.Messaging.Agent.Protocol (UserId)
import Simplex.Messaging.Agent.Store.DB (Binary (..), BoolInt (..), fromTextField_)
import qualified Simplex.Messaging.Agent.Store.DB as DB
import qualified Simplex.Messaging.Crypto as C
import Simplex.Messaging.Encoding.String (TextEncoding (..), strEncode)
import Simplex.Messaging.Util (decodeJSON, maybeFirstRow, maybeFirstRow', safeDecodeUtf8)

#if defined(dbPostgres)
import Database.PostgreSQL.Simple (Only (..), (:.) (..))
import Database.PostgreSQL.Simple.FromField (FromField (..))
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Simple.ToField (ToField (..))
#else
import Database.SQLite.Simple (Only (..), (:.) (..))
import Database.SQLite.Simple.FromField (FromField (..))
import Database.SQLite.Simple.QQ (sql)
import Database.SQLite.Simple.ToField (ToField (..))
#endif

-- | The keys one attempt to fund a badge is signed with, stashed before the request is sent so that
-- a retry reaches the service as the same signer and is answered with the credential already issued.
data BadgeStash = BadgeStash
  { stashRef :: BadgeStashRef,
    purchaseKey :: C.PublicKeyEd25519,
    purchasePrivKey :: C.PrivateKeyEd25519,
    masterKey :: BadgeMasterKey
  }

data BadgeStashRef = BSRCodeRedemption Int64 | BSRStoreReceipt Int64

getBadgeCodeRedemption :: DB.Connection -> User -> Text -> IO (Maybe BadgeStash)
getBadgeCodeRedemption db User {userId} code =
  maybeFirstRow (toBadgeStash BSRCodeRedemption) $
    DB.query
      db
      [sql|
        SELECT badge_code_redemption_id, purchase_key, purchase_priv_key, master_key
        FROM badge_code_redemptions
        WHERE user_id = ? AND code = ?
      |]
      (userId, code)

createBadgeCodeRedemption :: DB.Connection -> TVar ChaChaDRG -> User -> Text -> UTCTime -> IO BadgeStash
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
  pure BadgeStash {stashRef = BSRCodeRedemption redemptionId, purchaseKey, purchasePrivKey, masterKey}

-- | Our record of a store receipt, as against the receipt itself: held until the service credits or refuses it.
data BadgeReceiptRecord = BadgeReceiptRecord
  { receiptId :: Int64,
    ownerId :: UserId,
    status :: BadgeReceiptStatus
  }

data BadgeReceiptStatus
  = RSHeld {stash :: BadgeStash, invoiceId :: Maybe Text, payment :: Text, nextAttemptAt :: UTCTime, retryDelay :: Maybe Int64, holdKind :: StoreHoldKind}
  | RSCredited {badgePurchaseId :: Int64, refusalRecorded :: Bool} -- a resume or a tier change of it was refused
  | RSRefused {refusal :: Maybe BadgeIssueFailure}

-- | What a held record waits for: its own purchase, or the resume of one, a move of its payment to new keys,
-- or a change of the tier of the purchase it funds.
data StoreHoldKind = SHKPurchase | SHKTransfer | SHKTierChange
  deriving (Eq, Show)

instance TextEncoding StoreHoldKind where
  textEncode = \case
    SHKPurchase -> "purchase"
    SHKTransfer -> "transfer"
    SHKTierChange -> "tier_change"
  textDecode = \case
    "purchase" -> Just SHKPurchase
    "transfer" -> Just SHKTransfer
    "tier_change" -> Just SHKTierChange
    _ -> Nothing

instance FromField StoreHoldKind where fromField = fromTextField_ textDecode

instance ToField StoreHoldKind where toField = toField . textEncode

-- | Resolves a store transaction to the record that owns it, looking up by its reference again after each step
-- rather than trusting the write: finding none still leaves the echoed invoice's record to attach it to, and a
-- concurrent hand-over of the same transaction may win either write. It stays with the record that already has
-- it, whose keys the service may have credited; failing that it joins the record Buy created; and when no record
-- can take it - the store echoed no invoice, or the one it echoed is already held by another transaction - the
-- presenting profile gets a new one, nothing else saying who paid, unless the app found the transaction in the
-- store's list: then it may be another device's, and nothing is recorded.
holdStoreReceipt :: DB.Connection -> TVar ChaChaDRG -> User -> StoreReceiptOrigin -> Maybe Text -> StoreTransactionRef -> ServicePayment -> UTCTime -> IO (Maybe BadgeReceiptRecord)
holdStoreReceipt db g User {userId} origin invoiceId_ txRef@StoreTransactionRef {provider, transactionRef} payment now =
  getReceiptRecord db txRef >>= \case
    Just r@BadgeReceiptRecord {receiptId, status} -> do
      -- a resolved record stays resolved here, so a hand-over landing just after its credit cannot hold it again;
      -- only a resume re-holds a credited one, which the hand-over decides
      case status of
        RSHeld {payment = held}
          | laterAppleTransaction payment held ->
              DB.execute
                db
                "UPDATE badge_store_receipts SET payment = ?, next_attempt_at = ?, retry_delay = NULL, credit_error = NULL WHERE badge_store_receipt_id = ? AND payment = ?"
                (paymentJSON, now, receiptId, held)
        _ -> pure ()
      -- the Buy record the store echoed for this charge is never attached, as the charge continues this record's subscription;
      -- this record takes its invoice id, so the credit names the charge the app keeps unfinished
      forM_ invoiceId_ $ \invoiceId -> do
        deleted <-
          maybeFirstRow' False ((== (1 :: Int)) . fromOnly) $
            DB.query
              db
              [sql|
                DELETE FROM badge_store_receipts
                WHERE invoice_id = ? AND transaction_ref IS NULL AND payment IS NULL
                  AND NOT EXISTS (SELECT 1 FROM badge_purchases p WHERE p.badge_store_receipt_id = badge_store_receipts.badge_store_receipt_id)
                RETURNING 1
              |]
              (Only invoiceId)
        when deleted $ DB.execute db "UPDATE badge_store_receipts SET invoice_id = ? WHERE badge_store_receipt_id = ?" (invoiceId, receiptId)
      pure $ Just r
    Nothing -> do
      forM_ invoiceId_ $ \invoiceId ->
        DB.execute
          db
          "UPDATE badge_store_receipts SET provider = ?, transaction_ref = ?, payment = ?, next_attempt_at = ? WHERE invoice_id = ? AND transaction_ref IS NULL"
          (provider, transactionRef, paymentJSON, now, invoiceId)
      getReceiptRecord db txRef >>= \case
        Just r -> pure $ Just r
        Nothing -> case origin of
          SROPurchased -> do
            insertReceipt SHKPurchase
            getReceiptRecord db txRef
          SRORestored -> do
            insertReceipt SHKTransfer
            getReceiptRecord db txRef
          SROListed -> pure Nothing
  where
    paymentJSON = safeDecodeUtf8 . LB.toStrict $ J.encode payment
    insertReceipt holdKind = do
      (purchaseKey, purchasePrivKey) <- atomically (C.generateKeyPair g) :: IO C.KeyPairEd25519
      BadgeMasterKey mk <- generateMasterKey g
      -- an invoice id another record holds is not repeated: this record is then keyed by its transaction alone
      invoiceId' <- fmap join . forM invoiceId_ $ \invoiceId -> do
        taken <- DB.query db "SELECT badge_store_receipt_id FROM badge_store_receipts WHERE invoice_id = ?" (Only invoiceId) :: IO [Only Int64]
        pure $ if null taken then Just invoiceId else Nothing
      DB.execute
        db
        [sql|
          INSERT INTO badge_store_receipts
            (user_id, invoice_id, provider, transaction_ref, payment, next_attempt_at, purchase_key, purchase_priv_key, master_key, hold_kind, created_at)
          VALUES (?,?,?,?,?,?,?,?,?,?,?)
          ON CONFLICT DO NOTHING
        |]
        ((userId, invoiceId', provider, transactionRef, paymentJSON, now) :. (purchaseKey, purchasePrivKey, Binary mk, holdKind, now))

-- | A hand-over of a transaction already recorded, by Restore or by a resubscription no resume reaches: a subscription
-- record that funds no purchase of this profile, only one its payment moved away from, or one neither shown nor its
-- owner's newest, is taken by this profile and held as a move of its payment to it; a one-time record never moves.
-- True when its last refusal was transfer_limited, which the hand-over answers.
holdStoreTransfer :: DB.Connection -> TVar ChaChaDRG -> User -> StoreTransactionRef -> ServicePayment -> UTCTime -> IO Bool
holdStoreTransfer db g user@User {userId} txRef payment now =
  getReceiptRecord db txRef >>= \case
    Just BadgeReceiptRecord {receiptId, status = RSHeld {stash, holdKind = SHKPurchase}} ->
      getStashBadgePurchase db stash >>= \case
        Nothing ->
          False
            <$ DB.execute
              db
              "UPDATE badge_store_receipts SET user_id = ?, hold_kind = ?, next_attempt_at = ? WHERE badge_store_receipt_id = ? AND payment IS NOT NULL AND hold_kind = ?"
              (userId, SHKTransfer, now, receiptId, SHKPurchase)
        Just _ -> pure False
    Just BadgeReceiptRecord {receiptId, status = RSRefused {refusal = Just BIFServiceError {code}}}
      | code == BSEReceiptUsed || code == BSETransferLimited -> (code == BSETransferLimited) <$ holdTransfer receiptId
    Just BadgeReceiptRecord {receiptId, ownerId, status = RSCredited {badgePurchaseId}} ->
      movesHere ownerId badgePurchaseId >>= \case
        False -> pure False
        True -> do
          lastRefusal <- maybeFirstRow fromOnly $ DB.query db "SELECT credit_error FROM badge_store_receipts WHERE badge_store_receipt_id = ?" (Only receiptId)
          (purchaseKey, purchasePrivKey) <- atomically (C.generateKeyPair g) :: IO C.KeyPairEd25519
          BadgeMasterKey mk <- generateMasterKey g
          DB.execute db "UPDATE badge_store_receipts SET purchase_key = ?, purchase_priv_key = ?, master_key = ? WHERE badge_store_receipt_id = ? AND payment IS NULL" (purchaseKey, purchasePrivKey, Binary mk, receiptId)
          holdTransfer receiptId
          pure $ case join lastRefusal of
            Just BIFServiceError {code = BSETransferLimited} -> True
            _ -> False
    _ -> pure False
  where
    movesHere ownerId badgePurchaseId =
      getBadgePurchase db badgePurchaseId >>= \case
        -- a one-time purchase is never moved, as its months were issued when it was bought
        Just UserBadgePurchase {purchaseKind = PKSubscription _}
          | ownerId /= userId -> pure True
          | otherwise -> (||) <$> purchaseMovedOut db badgePurchaseId <*> purchaseUnreached db user badgePurchaseId
        _ -> pure False
    holdTransfer receiptId =
      DB.execute
        db
        "UPDATE badge_store_receipts SET user_id = ?, payment = ?, next_attempt_at = ?, retry_delay = NULL, retry_count = 0, credit_error = NULL, hold_kind = ? WHERE badge_store_receipt_id = ? AND payment IS NULL"
        (userId, safeDecodeUtf8 . LB.toStrict $ J.encode payment, now, SHKTransfer, receiptId)

-- | A subscription neither shown nor its owner's newest, which no resume reaches.
purchaseUnreached :: DB.Connection -> User -> Int64 -> IO Bool
purchaseUnreached db owner badgePurchaseId = do
  p_ <- getBadgePurchase db badgePurchaseId
  newestId <- fmap (\UserBadgePurchase {badgePurchaseId = i} -> i) <$> getUserBadgePurchase db owner
  pure $ case p_ of
    Just UserBadgePurchase {shown = False, purchaseKind = PKSubscription _} -> newestId /= Just badgePurchaseId
    _ -> False

-- | The service answers a purchase whose payment moved away ended, so a resume of it never succeeds.
purchaseMovedOut :: DB.Connection -> Int64 -> IO Bool
purchaseMovedOut db badgePurchaseId = maybe False paymentMovedOut <$> getBadgeLedgerLastEntry db badgePurchaseId

-- | A ledger tip that moved the purchase's payment to another key.
paymentMovedOut :: StatementEntry -> Bool
paymentMovedOut StatementEntry {entryType} = case entryType of
  SEDebit SDTransferOut {} -> True
  _ -> False

-- | Whether a held Apple record presents this payment next: another transaction of its subscription, bought
-- later and not revoked. The core decides which is newest, since the app's store listener races its sweep, and a
-- re-delivered or re-signed copy of the held transaction changes nothing, so it cannot cost another store check.
laterAppleTransaction :: ServicePayment -> Text -> Bool
laterAppleTransaction payment held = case (payment, decodeJSON held) of
  (SPApple {jws}, Just SPApple {jws = heldJws}) -> case (appleTransactionFacts jws, appleTransactionFacts heldJws) of
    ( Just AppleTransactionFacts {transactionId, purchaseDate = Just purchasedAt, revoked = False},
      Just AppleTransactionFacts {transactionId = heldId, purchaseDate = Just heldPurchasedAt}
      ) -> transactionId /= heldId && purchasedAt > heldPurchasedAt
    _ -> False
  _ -> False

getReceiptRecord :: DB.Connection -> StoreTransactionRef -> IO (Maybe BadgeReceiptRecord)
getReceiptRecord db StoreTransactionRef {provider, transactionRef} =
  maybeFirstRow toReceiptRecord $
    DB.query
      db
      [sql|
        SELECT r.badge_store_receipt_id, r.user_id, r.purchase_key, r.purchase_priv_key, r.master_key,
          r.invoice_id, r.payment, r.next_attempt_at, r.retry_delay, p.badge_purchase_id, r.credit_error, r.hold_kind
        FROM badge_store_receipts r
        LEFT JOIN badge_purchases p ON p.badge_store_receipt_id = r.badge_store_receipt_id
        WHERE r.provider = ? AND r.transaction_ref = ?
      |]
      (provider, transactionRef)
  where
    toReceiptRecord ((receiptId, ownerId, purchaseKey, purchasePrivKey, mk) :. (invoiceId, payment_, nextAttemptAt_, retryDelay, purchaseId_, refusal, holdKind)) =
      BadgeReceiptRecord {receiptId, ownerId, status}
      where
        status = case (payment_, nextAttemptAt_) of
          (Just payment, Just nextAttemptAt) -> RSHeld {stash = toBadgeStash BSRStoreReceipt (receiptId, purchaseKey, purchasePrivKey, mk), invoiceId, payment, nextAttemptAt, retryDelay, holdKind}
          _ -> maybe (RSRefused refusal) (\badgePurchaseId -> RSCredited {badgePurchaseId, refusalRecorded = maybe False ownRefusal refusal}) purchaseId_
        -- only a move is refused transfer_limited, and a refused move must not hold off the purchase's own resume or tier change
        ownRefusal = \case
          BIFServiceError {code = BSETransferLimited} -> False
          _ -> True

-- | The record made when Buy is tapped, before any receipt: the store echoes its invoice id.
createBadgeStoreReceipt :: DB.Connection -> TVar ChaChaDRG -> User -> Text -> UTCTime -> IO ()
createBadgeStoreReceipt db g User {userId} invoiceId now = do
  (purchaseKey, purchasePrivKey) <- atomically (C.generateKeyPair g) :: IO C.KeyPairEd25519
  BadgeMasterKey mk <- generateMasterKey g
  DB.execute
    db
    [sql|
      INSERT INTO badge_store_receipts (user_id, invoice_id, purchase_key, purchase_priv_key, master_key, created_at)
      VALUES (?,?,?,?,?,?)
    |]
    (userId, invoiceId, purchaseKey, purchasePrivKey, Binary mk, now)

-- | No receipt yet, or one held and not lapsed: a credited or refused record is resolved and not listed.
getOpenStorePurchases :: DB.Connection -> User -> UTCTime -> IO [OpenStorePurchase]
getOpenStorePurchases db User {userId} now =
  mapMaybe toOpenStorePurchase
    <$> DB.query
      db
      [sql|
        SELECT payment, invoice_id, transaction_ref, credit_error
        FROM badge_store_receipts
        WHERE user_id = ? AND (transaction_ref IS NULL OR payment IS NOT NULL)
        ORDER BY badge_store_receipt_id
      |]
      (Only userId)
  where
    toOpenStorePurchase (payment_, invoiceId, transactionRef, creditError)
      | maybe False (appleChargeLapsed now) payment_ = Nothing
      | otherwise = Just OpenStorePurchase {invoiceId, transactionRef, creditError}

-- | Each held receipt's payment and its last attempt's failure, if any; a lapsed one is not pending.
getPendingStoreReceipts :: DB.Connection -> User -> UTCTime -> IO [(Text, Maybe BadgeIssueFailure)]
getPendingStoreReceipts db User {userId} now =
  filter (not . appleChargeLapsed now . fst)
    <$> DB.query db "SELECT payment, credit_error FROM badge_store_receipts WHERE user_id = ? AND payment IS NOT NULL" (Only userId)

-- | A profile showing a badge waits with its held records, but for a change of the tier of the badge it shows.
-- A lapsed one is not attempted; a later charge handed over re-arms it.
getNextHeldStoreReceipt :: DB.Connection -> UserId -> UTCTime -> IO (Either StoreError (Maybe BadgeReceiptRecord))
getNextHeldStoreReceipt db userId now =
  fmap (Right . fmap toHeld . find (not . lapsed)) $
    DB.query
      db
      [sql|
        SELECT r.badge_store_receipt_id, r.purchase_key, r.purchase_priv_key, r.master_key, r.invoice_id, r.payment, r.next_attempt_at, r.retry_delay, r.hold_kind
        FROM badge_store_receipts r
        JOIN users u ON u.user_id = r.user_id
        LEFT JOIN badge_purchases p ON p.badge_store_receipt_id = r.badge_store_receipt_id
        WHERE r.user_id = ? AND r.payment IS NOT NULL
          AND (u.shown_badge_id IS NULL OR (r.hold_kind = ? AND u.shown_badge_id = p.badge_purchase_id))
        ORDER BY r.next_attempt_at, r.retry_count, r.badge_store_receipt_id
      |]
      (userId, SHKTierChange)
  where
    toHeld (stashRow@(receiptId, _, _, _) :. (invoiceId, payment, nextAttemptAt, retryDelay, holdKind)) =
      BadgeReceiptRecord {receiptId, ownerId = userId, status = RSHeld {stash = toBadgeStash BSRStoreReceipt stashRow, invoiceId, payment, nextAttemptAt, retryDelay, holdKind}}
    lapsed (_ :. (_, payment, _, _, _)) = appleChargeLapsed now payment

-- | A held Apple subscription charge whose paid period ended more than a day ago, as one refunded before it was credited:
-- it neither blocks a new badge nor shows as open nor is attempted, and its record stays, as a later charge under its original id continues it.
appleChargeLapsed :: UTCTime -> Text -> Bool
appleChargeLapsed now payment = case decodeJSON payment of
  Just SPApple {jws}
    | Just AppleTransactionFacts {expiresDate = Just expires, autoRenewable = True} <- appleTransactionFacts jws ->
        nominalDay `addUTCTime` posixSecondsToUTCTime (fromIntegral expires / 1000) <= now
  _ -> False

storeReceiptHeldFor :: DB.Connection -> Int64 -> UserId -> StoreHoldKind -> IO Bool
storeReceiptHeldFor db receiptId userId holdKind =
  maybeFirstRow' False ((== (1 :: Int)) . fromOnly) $
    DB.query db "SELECT 1 FROM badge_store_receipts WHERE badge_store_receipt_id = ? AND user_id = ? AND hold_kind = ? AND payment IS NOT NULL" (receiptId, userId, holdKind)

-- | The attempts that failed since the record was last held: each hold resets the count.
getStoreReceiptFailures :: DB.Connection -> Int64 -> IO Int64
getStoreReceiptFailures db receiptId =
  maybeFirstRow' 0 fromOnly $ DB.query db "SELECT retry_count FROM badge_store_receipts WHERE badge_store_receipt_id = ?" (Only receiptId)

-- | Only while the record still holds the payment that failed: one adopted meanwhile is due at once.
recordStoreReceiptFailure :: DB.Connection -> Int64 -> Text -> Int64 -> UTCTime -> BadgeIssueFailure -> IO ()
recordStoreReceiptFailure db receiptId payment retryDelay nextAttemptAt failure =
  DB.execute
    db
    [sql|
      UPDATE badge_store_receipts
      SET retry_delay = ?, next_attempt_at = ?, retry_count = retry_count + 1, credit_error = ?
      WHERE badge_store_receipt_id = ? AND payment = ?
    |]
    (retryDelay, nextAttemptAt, failure, receiptId, payment)

-- | Kept rather than deleted, so that a later hand-over of the same transaction is answered from the record.
-- A refused move returns the record to the profile whose purchase it still funds.
-- Only while the record still holds the payment refused, which is when it answers True: one adopted meanwhile is due at once.
refuseStoreReceipt :: DB.Connection -> Int64 -> Text -> BadgeIssueFailure -> IO Bool
refuseStoreReceipt db receiptId payment refusal =
  maybeFirstRow' False ((== (1 :: Int)) . fromOnly) $
    DB.query
      db
      [sql|
        UPDATE badge_store_receipts
        SET payment = NULL, next_attempt_at = NULL, retry_delay = NULL, retry_count = 0, credit_error = ?, hold_kind = ?,
          user_id = COALESCE((SELECT p.user_id FROM badge_purchases p WHERE p.badge_store_receipt_id = ?), user_id)
        WHERE badge_store_receipt_id = ? AND payment = ?
        RETURNING 1
      |]
      (refusal, SHKPurchase, receiptId, receiptId, payment)

-- | Only while the record is resolved: one held meanwhile keeps its hold.
clearStoreReceiptRefusal :: DB.Connection -> Int64 -> IO ()
clearStoreReceiptRefusal db receiptId =
  DB.execute db "UPDATE badge_store_receipts SET credit_error = NULL WHERE badge_store_receipt_id = ? AND payment IS NULL" (Only receiptId)

-- | Every settle clears the failure, so one recorded on a credited record is a refused resume or tier change.
-- It returns the record to the profile whose purchase it funds, which a Restore may have taken it from meanwhile.
settleStoreReceipt :: DB.Connection -> Int64 -> IO ()
settleStoreReceipt db receiptId =
  DB.execute
    db
    [sql|
      UPDATE badge_store_receipts
      SET payment = NULL, next_attempt_at = NULL, retry_delay = NULL, retry_count = 0, credit_error = NULL, hold_kind = ?,
        user_id = COALESCE((SELECT p.user_id FROM badge_purchases p WHERE p.badge_store_receipt_id = ?), user_id)
      WHERE badge_store_receipt_id = ?
    |]
    (SHKPurchase, receiptId, receiptId)

-- | Whether a later charge of a credited record's subscription is answered as held: its purchase must be its owner's newest
-- and a subscription. A tier change, of a shown purchase, is held whatever else is held, as the worker serves it while the
-- badge is shown. A resume, of a purchase not shown, is held only while no other receipt of the owner is held, which the
-- worker would credit first; until then the app keeps the charge unfinished and hands it over again. No other kind is held.
reholdStoreReceipt :: DB.Connection -> User -> Int64 -> Int64 -> StoreHoldKind -> ServicePayment -> UTCTime -> IO Bool
reholdStoreReceipt db owner receiptId badgePurchaseId holdKind payment now = do
  newest_ <- getUserBadgePurchase db owner
  case newest_ of
    Just UserBadgePurchase {badgePurchaseId = newestId, shown, purchaseKind = PKSubscription _}
      | newestId == badgePurchaseId -> case holdKind of
          SHKTierChange | shown -> True <$ hold
          SHKPurchase | not shown -> do
            pending <- getPendingStoreReceipts db owner now
            True <$ when (null pending) hold
          _ -> pure False
    _ -> pure False
  where
    hold =
      DB.execute
        db
        "UPDATE badge_store_receipts SET payment = ?, next_attempt_at = ?, retry_delay = NULL, retry_count = 0, credit_error = NULL, hold_kind = ? WHERE badge_store_receipt_id = ? AND payment IS NULL"
        (safeDecodeUtf8 . LB.toStrict $ J.encode payment, now, holdKind, receiptId)

-- | A resume's or a tier change's credit: shows the purchase, again if a retire cleared it meanwhile, and keeps only the
-- state the answer stored, since the charge disproves an ended or a billing retry stored before.
showChargedPurchase :: DB.Connection -> User -> Int64 -> Int64 -> Maybe SubscriptionState -> IO ()
showChargedPurchase db User {userId} receiptId badgePurchaseId state = do
  settleStoreReceipt db receiptId
  DB.execute db "UPDATE badge_purchases SET subscription_status = ? WHERE badge_purchase_id = ?" (state, badgePurchaseId)
  DB.execute db "UPDATE users SET shown_badge_id = ? WHERE user_id = ?" (badgePurchaseId, userId)

-- | A held record is paid for and a credited one is referenced by its purchase, so neither is deleted.
deleteUnfundedStoreReceipts :: DB.Connection -> UTCTime -> IO ()
deleteUnfundedStoreReceipts db createdBefore =
  DB.execute
    db
    [sql|
      DELETE FROM badge_store_receipts
      WHERE payment IS NULL AND created_at < ?
        AND NOT EXISTS (SELECT 1 FROM badge_purchases p WHERE p.badge_store_receipt_id = badge_store_receipts.badge_store_receipt_id)
    |]
    (Only createdBefore)

toBadgeStash :: (Int64 -> BadgeStashRef) -> (Int64, C.PublicKeyEd25519, C.PrivateKeyEd25519, Binary ByteString) -> BadgeStash
toBadgeStash ref (stashId, purchaseKey, purchasePrivKey, Binary mk) =
  BadgeStash {stashRef = ref stashId, purchaseKey, purchasePrivKey, masterKey = BadgeMasterKey mk}

-- | Drop the keys stashed for a code the service refused for good, unless a purchase already came
-- from them - badge_purchases references their row.
deleteBadgeCodeRedemption :: DB.Connection -> User -> Text -> IO ()
deleteBadgeCodeRedemption db User {userId} code =
  DB.execute
    db
    [sql|
      DELETE FROM badge_code_redemptions
      WHERE user_id = ? AND code = ?
        AND NOT EXISTS (SELECT 1 FROM badge_purchases p WHERE p.badge_code_redemption_id = badge_code_redemptions.badge_code_redemption_id)
    |]
    (userId, code)

-- | 'False' when the stash already funded a purchase here: the service replays the credential it
-- issued, and that must add no purchase and leave the shown badge alone.
createStashBadgePurchase :: DB.Connection -> User -> BadgeStash -> BadgeCredential -> UTCTime -> IO (Int64, Bool)
createStashBadgePurchase db User {userId} stash credential now = do
  -- a moved payment's record still links the purchase it moved from, which would be taken for a replay
  forM_ storeReceiptId_ $ \receiptId ->
    DB.execute
      db
      "UPDATE badge_purchases SET badge_store_receipt_id = NULL WHERE badge_store_receipt_id = ? AND EXISTS (SELECT 1 FROM badge_store_receipts WHERE badge_store_receipt_id = ? AND hold_kind = ?)"
      (receiptId, receiptId, SHKTransfer)
  purchase <- getStashBadgePurchase db stash >>= \case
    Just purchaseId -> pure (purchaseId, False)
    Nothing -> do
      DB.execute
        db
        [sql|
          INSERT INTO badge_purchases
            (user_id, purchase_key, purchase_priv_key, master_key, initial_badge_type, current_badge_type, status,
             badge_code_redemption_id, badge_store_receipt_id, created_at, updated_at)
          VALUES (?,?,?,?,?,?,?,?,?,?,?)
        |]
        ((userId, purchaseKey, purchasePrivKey, Binary mk, badgeType, badgeType) :. (PSIssued, redemptionId_, storeReceiptId_, now, now))
      purchaseId <- insertedRowId db
      DB.execute db "UPDATE users SET shown_badge_id = ? WHERE user_id = ?" (purchaseId, userId)
      pure (purchaseId, True)
  -- the credit settles a held receipt in the transaction that stores its purchase, a replay included,
  -- once the purchase links it, so the record goes to the profile that purchase is of
  forM_ storeReceiptId_ $ settleStoreReceipt db
  pure purchase
  where
    BadgeStash {stashRef, purchaseKey, purchasePrivKey, masterKey = BadgeMasterKey mk} = stash
    BadgeCredential {badgeInfo = BadgeInfo {badgeType}} = credential
    (redemptionId_, storeReceiptId_) = case stashRef of
      BSRCodeRedemption redemptionId -> (Just redemptionId, Nothing)
      BSRStoreReceipt storeReceiptId -> (Nothing, Just storeReceiptId)

-- | The period comes from the ledger, the expiry from the credential, which runs a week longer.
-- 'False' means no issuance row was written, which the caller reports rather than drop in silence.
-- A replayed statement names a month already issued, and one month has one issuance.
storeBadgeIssuance :: DB.Connection -> TVar ChaChaDRG -> Int64 -> Int64 -> BadgeCredential -> UTCTime -> IO Bool
storeBadgeIssuance db g badgePurchaseId entryId credential now =
  getIssuedPeriod db badgePurchaseId entryId >>= \case
    Nothing -> pure False
    Just (periodStart, periodEnd) -> do
      issuanceId <- safeDecodeUtf8 . strEncode <$> atomically (C.randomBytes 16 g)
      DB.execute
        db
        [sql|
          INSERT INTO badge_issuances (issuance_id, badge_purchase_id, entry_id, badge_type, period_start, period_end, expiry, credential, created_at)
          VALUES (?,?,?,?,?,?,?,?,?)
          ON CONFLICT (badge_purchase_id, entry_id) DO NOTHING
        |]
        ((issuanceId, badgePurchaseId, entryId, badgeType) :. (periodStart, periodEnd, badgeExpiry, Binary (LB.toStrict $ J.encode credential), now))
      -- a month stored ends the run of failures
      DB.execute
        db
        "UPDATE badge_purchases SET issue_failed_since = NULL, issue_error_at = NULL, issue_error = NULL WHERE badge_purchase_id = ?"
        (Only badgePurchaseId)
      pure True
  where
    BadgeCredential {badgeInfo = BadgeInfo {badgeType, badgeExpiry}} = credential

getLatestIssuedCredential :: DB.Connection -> Int64 -> IO (Maybe BadgeCredential)
getLatestIssuedCredential db badgePurchaseId = do
  rows <-
    DB.query
      db
      [sql|
        SELECT credential FROM badge_issuances
        WHERE badge_purchase_id = ?
        ORDER BY entry_id DESC
        LIMIT 1
      |]
      (Only badgePurchaseId)
  pure $ case rows of
    [Only (Binary bs)] -> J.decodeStrict' bs
    _ -> Nothing

-- the start is read from the row before rather than by subtracting a month, which clips
getIssuedPeriod :: DB.Connection -> Int64 -> Int64 -> IO (Maybe (UTCTime, UTCTime))
getIssuedPeriod db badgePurchaseId entryId = do
  rows <-
    DB.query
      db
      [sql|
        SELECT
          (SELECT prev.balance_start_ts FROM badge_ledger prev
           WHERE prev.badge_purchase_id = issued.badge_purchase_id AND prev.entry_id < issued.entry_id
           ORDER BY prev.entry_id DESC LIMIT 1),
          issued.balance_start_ts
        FROM badge_ledger issued
        WHERE issued.badge_purchase_id = ? AND issued.entry_id = ?
      |]
      (badgePurchaseId, entryId)
  -- no preceding row means no credit was ever stored, so the period this row issued is unknown
  pure $ case rows of
    [(Just periodStart, periodEnd)] -> Just (periodStart, periodEnd)
    _ -> Nothing

getStashBadgePurchase :: DB.Connection -> BadgeStash -> IO (Maybe Int64)
getStashBadgePurchase db BadgeStash {stashRef} =
  maybeFirstRow fromOnly $ case stashRef of
    BSRCodeRedemption redemptionId ->
      DB.query db "SELECT badge_purchase_id FROM badge_purchases WHERE badge_code_redemption_id = ?" (Only redemptionId)
    BSRStoreReceipt storeReceiptId ->
      DB.query db "SELECT badge_purchase_id FROM badge_purchases WHERE badge_store_receipt_id = ?" (Only storeReceiptId)

data UserBadgePurchase = UserBadgePurchase
  { badgePurchaseId :: Int64,
    purchaseKey :: C.PublicKeyEd25519,
    purchasePrivKey :: C.PrivateKeyEd25519,
    masterKey :: BadgeMasterKey,
    badgeType :: BadgeType,
    shown :: Bool,
    alertAcked :: Maybe (BadgeAlertKind, Text),
    alertSnoozeUntil :: Maybe UTCTime,
    issueError :: Maybe BadgeIssueError,
    nextWakeAt :: Maybe UTCTime,
    unrenewedRun :: Maybe UnrenewedRun,
    purchaseKind :: PurchaseKind
  }

data UnrenewedRun = UnrenewedRun {unrenewedSince :: UTCTime, lastUnrenewedAt :: UTCTime}
  deriving (Eq)

-- | A subscription's months come from store charges, so reaching paidThrough does not end it: the
-- service says when it has. Derived from the replica, which holds a charge credit exactly for a subscription.
data PurchaseKind = PKPrepaid | PKSubscription BadgeSubscription
  deriving (Eq)

-- | Newest, not the one shown_badge_id points at - retirement clears that, and the support ended
-- alert is recomputed from this purchase after the badge stops being shown.
getUserBadgePurchase :: DB.Connection -> User -> IO (Maybe UserBadgePurchase)
getUserBadgePurchase db User {userId} =
  maybeFirstRow fromOnly newestId >>= maybe (pure Nothing) (getBadgePurchase db)
  where
    newestId =
      DB.query
        db
        [sql|
          SELECT badge_purchase_id FROM badge_purchases
          WHERE user_id = ? AND purchase_priv_key IS NOT NULL
          ORDER BY badge_purchase_id DESC
          LIMIT 1
        |]
        (Only userId)

-- shown is a CASE because in Postgres a comparison is boolean, which BoolInt rejects.
getBadgePurchase :: DB.Connection -> Int64 -> IO (Maybe UserBadgePurchase)
getBadgePurchase db purchaseId =
  maybeFirstRow toPurchase $
    DB.query
      db
      [sql|
        SELECT p.badge_purchase_id, p.purchase_key, p.purchase_priv_key, p.master_key, p.current_badge_type,
               (CASE WHEN u.shown_badge_id = p.badge_purchase_id THEN 1 ELSE 0 END),
               p.alert_acked_kind, p.alert_acked_episode, p.alert_snooze_until,
               p.issue_failed_since, p.issue_error_at, p.issue_error, p.next_wake_at,
               (SELECT l.change_months FROM badge_ledger l
                WHERE l.badge_purchase_id = p.badge_purchase_id AND l.entry_type = 'credit' AND l.entry_credit_type = 'charge'
                ORDER BY l.entry_id DESC
                LIMIT 1),
               p.subscription_status, p.unrenewed_since, p.last_unrenewed_at, r.transaction_ref
        FROM badge_purchases p
        JOIN users u ON u.user_id = p.user_id
        LEFT JOIN badge_store_receipts r ON r.badge_store_receipt_id = p.badge_store_receipt_id
        WHERE p.badge_purchase_id = ? AND p.purchase_priv_key IS NOT NULL
      |]
      (Only purchaseId)
  where
    toPurchase ((badgePurchaseId, purchaseKey, purchasePrivKey, Binary mk, badgeType, shown_, ackedKind_, ackedEpisode_, alertSnoozeUntil) :. (failedSince_, lastAttemptAt_, reason_, nextWakeAt, planMonths_, status, since_, lastAt_, transactionRef)) =
      UserBadgePurchase
        { badgePurchaseId,
          purchaseKey,
          purchasePrivKey,
          masterKey = BadgeMasterKey mk,
          badgeType,
          shown = unBI shown_,
          alertAcked = (,) <$> ackedKind_ <*> ackedEpisode_,
          alertSnoozeUntil,
          issueError = BadgeIssueError <$> failedSince_ <*> lastAttemptAt_ <*> reason_,
          nextWakeAt,
          unrenewedRun = UnrenewedRun <$> since_ <*> lastAt_,
          purchaseKind = maybe PKPrepaid (\planMonths -> PKSubscription BadgeSubscription {planMonths, status, transactionRef}) planMonths_
        }

setBadgeSubscriptionStatus :: DB.Connection -> Int64 -> SubscriptionState -> IO ()
setBadgeSubscriptionStatus db badgePurchaseId state =
  DB.execute db "UPDATE badge_purchases SET subscription_status = ? WHERE badge_purchase_id = ?" (state, badgePurchaseId)

-- | Whether a badge is on the profile now: set when a redemption stores one, cleared when it is
-- retired. Read as the id rather than as a comparison, which in Postgres would be a boolean.
userHasBadge :: DB.Connection -> User -> IO Bool
userHasBadge db User {userId} =
  maybeFirstRow' False shownBadge $
    DB.query db "SELECT shown_badge_id FROM users WHERE user_id = ?" (Only userId)
  where
    shownBadge :: Only (Maybe Int64) -> Bool
    shownBadge = isJust . fromOnly

-- | An ack and a snooze both record the occurrence answered; a snooze also records how long it
-- holds, so that it silences that occurrence and not whichever one is derived next.
setBadgeAlertAcked :: DB.Connection -> User -> Int64 -> BadgeAlertKind -> Text -> Maybe UTCTime -> IO ()
setBadgeAlertAcked db User {userId} badgePurchaseId kind episode snoozeUntil =
  DB.execute
    db
    "UPDATE badge_purchases SET alert_acked_kind = ?, alert_acked_episode = ?, alert_snooze_until = ? WHERE badge_purchase_id = ? AND user_id = ?"
    (kind, episode, snoozeUntil, badgePurchaseId, userId)

-- | Record a renewal that ended without a credential. The COALESCE keeps the start of the current
-- run of failures, which is the alert's episode and must survive a restart.
setBadgeIssueError :: DB.Connection -> Int64 -> UTCTime -> BadgeIssueFailure -> IO ()
setBadgeIssueError db badgePurchaseId now failure =
  DB.execute
    db
    [sql|
      UPDATE badge_purchases
      SET issue_failed_since = COALESCE(issue_failed_since, ?), issue_error_at = ?, issue_error = ?
      WHERE badge_purchase_id = ?
    |]
    (now, now, failure, badgePurchaseId)

-- | The wake the worker is about to wait for, so the state the apps hold says when it will try again.
setBadgeNextWake :: DB.Connection -> Int64 -> Maybe UTCTime -> IO ()
setBadgeNextWake db badgePurchaseId at_ =
  DB.execute db "UPDATE badge_purchases SET next_wake_at = ? WHERE badge_purchase_id = ?" (at_, badgePurchaseId)

setBadgeUnrenewedRun :: DB.Connection -> Int64 -> Maybe UnrenewedRun -> IO ()
setBadgeUnrenewedRun db badgePurchaseId run_ =
  DB.execute
    db
    "UPDATE badge_purchases SET unrenewed_since = ?, last_unrenewed_at = ? WHERE badge_purchase_id = ?"
    (unrenewedSince <$> run_, lastUnrenewedAt <$> run_, badgePurchaseId)

-- | Stop showing a badge that has expired unrenewed; the profile update is broadcast by the caller.
clearShownBadge :: DB.Connection -> User -> Int64 -> IO ()
clearShownBadge db User {userId} badgePurchaseId =
  DB.execute db "UPDATE users SET shown_badge_id = NULL WHERE user_id = ? AND shown_badge_id = ?" (userId, badgePurchaseId)

-- | Verbatim, entry_uuid and type included: the client authors no row, or the two sides stop
-- holding the same ledger. DO NOTHING makes a re-applied statement a no-op rather than a throw.
-- An entry whose balance does not follow from the one before it is stored and marked, not refused.
storeBadgeStatement :: DB.Connection -> Int64 -> BadgeType -> Maybe StatementEntry -> [StatementEntry] -> UTCTime -> IO ()
storeBadgeStatement db badgePurchaseId badgeType tip entries now =
  mapM_ storeEntry $ balanceChecked now badgeType tip entries
  where
    storeEntry (StatementEntry {entryId, changeMonths, balanceMonths, balanceStartTs, balanceAnchorTs, balanceBadgeType, wasPausedSince, createdAt, entryType}, checked) =
      DB.execute
        db
        [sql|
          INSERT INTO badge_ledger
            (entry_uuid, badge_purchase_id, change_months, balance_months, balance_start_ts, balance_anchor_ts, balance_badge_type,
             was_paused_since, service_created_at, created_at, entry_type, entry_credit_type, entry_debit_type,
             entry_type_unknown, entry_type_value, balance_checked)
          VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)
          ON CONFLICT (entry_uuid) DO NOTHING
        |]
        ( (entryId, badgePurchaseId, changeMonths, balanceMonths, balanceStartTs, balanceAnchorTs, balanceBadgeType, wasPausedSince)
            :. (createdAt, now, entryTypeT, creditType, debitType, BI typeUnknown, entryTypeValue, BI <$> checked)
        )
      where
        (entryTypeT, creditType, debitType) = entryTypeColumns entryType
        -- kept for every entry, not only for a type this version cannot decode: a tag alone does
        -- not rebuild the types that name an invoice, a charge or another purchase
        entryTypeValue = safeDecodeUtf8 . LB.toStrict $ case entryType of
          SECredit c -> J.encode c
          SEDebit d -> J.encode d
        typeUnknown = case entryType of
          SECredit SCUnknown {} -> True
          SEDebit SDUnknown {} -> True
          _ -> False

setBadgePurchaseType :: DB.Connection -> Int64 -> BadgeType -> UTCTime -> IO ()
setBadgePurchaseType db badgePurchaseId badgeType now =
  DB.execute db "UPDATE badge_purchases SET current_badge_type = ?, updated_at = ? WHERE badge_purchase_id = ?" (badgeType, now, badgePurchaseId)

-- | The balance is the last row; nothing derives it by summing the history.
getBadgeLedgerLastEntry :: DB.Connection -> Int64 -> IO (Maybe StatementEntry)
getBadgeLedgerLastEntry db badgePurchaseId =
  maybeFirstRow' Nothing toStatementEntry $
    DB.query
      db
      [sql|
        SELECT entry_uuid, change_months, balance_months, balance_start_ts, balance_anchor_ts, balance_badge_type,
               was_paused_since, service_created_at, entry_type, entry_credit_type, entry_debit_type, entry_type_value
        FROM badge_ledger
        WHERE badge_purchase_id = ?
        ORDER BY entry_id DESC
        LIMIT 1
      |]
      (Only badgePurchaseId)

-- | When the service wrote the purchase's last charge credit, which it dates no earlier than the charge's period start.
getLastChargeCreditAt :: DB.Connection -> Int64 -> IO (Maybe UTCTime)
getLastChargeCreditAt db badgePurchaseId =
  maybeFirstRow fromOnly $
    DB.query
      db
      [sql|
        SELECT service_created_at FROM badge_ledger
        WHERE badge_purchase_id = ? AND entry_type = 'credit' AND entry_credit_type = 'charge'
        ORDER BY entry_id DESC
        LIMIT 1
      |]
      (Only badgePurchaseId)

-- | Oldest first. A row whose type this version cannot rebuild is left out, as it is from the tip.
getBadgeLedger :: DB.Connection -> User -> Int64 -> IO [StatementEntry]
getBadgeLedger db User {userId} badgePurchaseId =
  mapMaybe toStatementEntry
    <$> DB.query
      db
      [sql|
        SELECT l.entry_uuid, l.change_months, l.balance_months, l.balance_start_ts, l.balance_anchor_ts, l.balance_badge_type,
               l.was_paused_since, l.service_created_at, l.entry_type, l.entry_credit_type, l.entry_debit_type, l.entry_type_value
        FROM badge_ledger l
        JOIN badge_purchases p ON p.badge_purchase_id = l.badge_purchase_id
        WHERE l.badge_purchase_id = ? AND p.user_id = ?
        ORDER BY l.entry_id
      |]
      (badgePurchaseId, userId)

toStatementEntry :: (Text, Int, Int, UTCTime, UTCTime, BadgeType) :. (Maybe UTCTime, UTCTime, Text, Maybe Text, Maybe Text, Maybe Text) -> Maybe StatementEntry
toStatementEntry ((entryId, changeMonths, balanceMonths, balanceStartTs, balanceAnchorTs, balanceBadgeType) :. (wasPausedSince, createdAt, entryType_, credit_, debit_, value_)) =
  (\entryType -> StatementEntry {entryId, changeMonths, balanceMonths, balanceStartTs, balanceAnchorTs, balanceBadgeType, wasPausedSince, createdAt, entryType})
    <$> maybe (entryTypeFromColumns noEntryReferences entryType_ credit_ debit_) (entryTypeFromValue entryType_) value_

-- | Decodes the stored JSON rather than rebuilding from the tag, so a version that has since
-- learnt the type reads it with its fields, and one that has not still gets it back verbatim.
entryTypeFromValue :: Text -> Text -> Maybe StatementEntryType
entryTypeFromValue entryTypeT value_ = case entryTypeT of
  "credit" -> SECredit <$> decodeJSON value_
  "debit" -> SEDebit <$> decodeJSON value_
  _ -> Nothing

getBadgeLedgerEntryId :: DB.Connection -> Int64 -> Text -> IO (Maybe Int64)
getBadgeLedgerEntryId db badgePurchaseId entryUuid =
  maybeFirstRow fromOnly $
    DB.query db "SELECT entry_id FROM badge_ledger WHERE badge_purchase_id = ? AND entry_uuid = ?" (badgePurchaseId, entryUuid)

{-# LANGUAGE CPP #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module BadgeService.Store
  ( MintedCode (..),
    CodeRedemption (..),
    RedeemedCode (..),
    CodeIssuance (..),
    getBadgeCode,
    purchaseKeyExists,
    writeCodeRedemption,
    insertBadgeCode,
  )
where

import qualified Data.Aeson as J
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as LB
import Data.Int (Int64)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Simplex.Chat.Badges (BadgeCredential, BadgeMasterKey (..), BadgeType)
import Simplex.Chat.Badges.Types (BadgeCodePaymentStatus, BadgePurchaseStatus (..))
import Simplex.Chat.Store.Shared (insertedRowId)
import Simplex.Messaging.Agent.Store.DB (Binary (..))
import qualified Simplex.Messaging.Agent.Store.DB as DB
import qualified Simplex.Messaging.Crypto as C
import Simplex.Messaging.Util (maybeFirstRow, maybeFirstRow')

#if defined(dbPostgres)
import Database.PostgreSQL.Simple (Only (..))
import Database.PostgreSQL.Simple.SqlQQ (sql)
#else
import Database.SQLite.Simple (Only (..))
import Database.SQLite.Simple.QQ (sql)
#endif

data MintedCode = MintedCode
  { badgeCodeId :: Int64,
    badgeType :: BadgeType,
    redemption :: CodeRedemption
  }

-- spent-but-unreadable is not unclaimed - collapsing them would issue the code twice
data CodeRedemption
  = CodeUnredeemed
  | CodeRedeemed RedeemedCode
  | CodeRedeemedUnreadable

data RedeemedCode = RedeemedCode
  { purchaseKey :: C.PublicKeyEd25519,
    credential :: BadgeCredential
  }

-- prepared before the transaction opens, because the credential is signed first
data CodeIssuance = CodeIssuance
  { badgeCodeId :: Int64,
    issuanceId :: Text,
    purchaseKey :: C.PublicKeyEd25519,
    masterKey :: BadgeMasterKey,
    badgeType :: BadgeType,
    credential :: BadgeCredential,
    periodStart :: UTCTime,
    periodEnd :: UTCTime,
    expiry :: UTCTime
  }

getBadgeCode :: DB.Connection -> ByteString -> IO (Maybe MintedCode)
getBadgeCode db codeHash =
  maybeFirstRow toCode $
    DB.query
      db
      [sql|
        SELECT c.badge_code_id, c.badge_type, p.purchase_key, i.credential
        FROM sx_badge_service_badge_codes c
        LEFT JOIN sx_badge_service_badge_purchases p ON p.badge_code_id = c.badge_code_id
        LEFT JOIN sx_badge_service_badge_issuances i ON i.badge_purchase_id = p.badge_purchase_id
        WHERE c.code_hash = ?
        ORDER BY i.created_at
        LIMIT 1
      |]
      (Only (Binary codeHash))
  where
    toCode (badgeCodeId, badgeType, purchaseKey_, credential_) =
      MintedCode {badgeCodeId, badgeType, redemption = codeRedemption purchaseKey_ credential_}
    codeRedemption purchaseKey_ credential_ = case purchaseKey_ of
      Nothing -> CodeUnredeemed
      Just purchaseKey -> case decodeCredential =<< credential_ of
        Just credential -> CodeRedeemed RedeemedCode {purchaseKey, credential}
        Nothing -> CodeRedeemedUnreadable
    decodeCredential (Binary bs) = J.decodeStrict' bs

purchaseKeyExists :: DB.Connection -> C.PublicKeyEd25519 -> IO Bool
purchaseKeyExists db key =
  maybeFirstRow' False (\(Only (_ :: Int64)) -> True) $
    DB.query db "SELECT badge_purchase_id FROM sx_badge_service_badge_purchases WHERE purchase_key = ?" (Only key)

-- one transaction: the caller has already signed, so no code is left spent without a credential
writeCodeRedemption :: DB.Connection -> CodeIssuance -> UTCTime -> IO ()
writeCodeRedemption db CodeIssuance {badgeCodeId, issuanceId, purchaseKey, masterKey = BadgeMasterKey mk, badgeType, credential, periodStart, periodEnd, expiry} now = do
  DB.execute
    db
    [sql|
      INSERT INTO sx_badge_service_badge_purchases
        (purchase_key, master_key, initial_badge_type, current_badge_type, status, badge_code_id, created_at, updated_at)
      VALUES (?,?,?,?,?,?,?,?)
    |]
    (purchaseKey, Binary mk, badgeType, badgeType, PSIssued, badgeCodeId, now, now)
  purchaseId <- insertedRowId db
  DB.execute
    db
    [sql|
      INSERT INTO sx_badge_service_badge_issuances
        (issuance_id, badge_purchase_id, badge_type, period_start, period_end, expiry, credential, created_at)
      VALUES (?,?,?,?,?,?,?,?)
    |]
    (issuanceId, purchaseId, badgeType, periodStart, periodEnd, expiry, Binary (LB.toStrict $ J.encode credential), now)
  DB.execute db "UPDATE sx_badge_service_badge_codes SET redeemed_at = ? WHERE badge_code_id = ?" (now, badgeCodeId)

insertBadgeCode :: DB.Connection -> ByteString -> BadgeType -> Int -> BadgeCodePaymentStatus -> UTCTime -> IO ()
insertBadgeCode db codeHash badgeType months paymentStatus now =
  DB.execute
    db
    [sql|
      INSERT INTO sx_badge_service_badge_codes (code_hash, badge_type, months, code_payment_status, created_at)
      VALUES (?,?,?,?,?)
    |]
    (Binary codeHash, badgeType, months, paymentStatus, now)

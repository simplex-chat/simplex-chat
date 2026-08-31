{-# LANGUAGE CPP #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.Badges
  ( BadgeCodeRedemption (..),
    getBadgeCodeRedemption,
    createBadgeCodeRedemption,
    deleteBadgeCodeRedemption,
    createCodeBadgePurchase,
  )
where

import Control.Concurrent.STM (TVar, atomically)
import Crypto.Random (ChaChaDRG)
import qualified Data.Aeson as J
import qualified Data.ByteString.Lazy.Char8 as LB
import Data.Int (Int64)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Simplex.Chat.Badges
import Simplex.Chat.Badges.Types (BadgePurchaseStatus (..))
import Simplex.Chat.Store.Shared (insertedRowId)
import Simplex.Chat.Types
import Simplex.Messaging.Agent.Store.DB (Binary (..))
import qualified Simplex.Messaging.Agent.Store.DB as DB
import qualified Simplex.Messaging.Crypto as C
import Simplex.Messaging.Encoding.String (strEncode)
import Simplex.Messaging.Util (maybeFirstRow, safeDecodeUtf8)

#if defined(dbPostgres)
import Database.PostgreSQL.Simple (Only (..))
import Database.PostgreSQL.Simple.SqlQQ (sql)
#else
import Database.SQLite.Simple (Only (..))
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
-- came from it - badge_purchases references this row, and a spent code can still be refused
-- later by a rebuilt service or a different one at the configured address.
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

-- | The purchase a redeemed code created, its issuance, and the profile's pointer to it.
--
-- Idempotent in the redemption: the service answers a repeat with the credential it already
-- issued, which must leave one purchase rather than add a second. The expiry is passed in
-- because badge_issuances requires one and the caller has already resolved it.
createCodeBadgePurchase :: DB.Connection -> TVar ChaChaDRG -> User -> BadgeCodeRedemption -> BadgeCredential -> UTCTime -> UTCTime -> IO ()
createCodeBadgePurchase db g User {userId} redemption credential expiry now = do
  purchaseId <- getCodeBadgePurchase db redemption >>= maybe insertPurchase pure
  DB.execute db "UPDATE users SET shown_badge_id = ? WHERE user_id = ?" (purchaseId, userId)
  where
    BadgeCodeRedemption {redemptionId, purchaseKey, purchasePrivKey, masterKey = BadgeMasterKey mk} = redemption
    BadgeCredential {badgeInfo = BadgeInfo {badgeType}} = credential
    insertPurchase = do
      DB.execute
        db
        [sql|
          INSERT INTO badge_purchases
            (user_id, purchase_key, purchase_priv_key, master_key, initial_badge_type, current_badge_type, status, badge_code_redemption_id, created_at, updated_at)
          VALUES (?,?,?,?,?,?,?,?,?,?)
        |]
        (userId, purchaseKey, purchasePrivKey, Binary mk, badgeType, badgeType, PSIssued, redemptionId, now, now)
      purchaseId <- insertedRowId db
      issuanceId <- safeDecodeUtf8 . strEncode <$> atomically (C.randomBytes 16 g)
      -- the service reports no period while the ledger is stubbed, so the credential's expiry
      -- stands in for the period end until the statement carries it
      DB.execute
        db
        [sql|
          INSERT INTO badge_issuances (issuance_id, badge_purchase_id, badge_type, period_start, period_end, expiry, credential, created_at)
          VALUES (?,?,?,?,?,?,?,?)
        |]
        (issuanceId, purchaseId, badgeType, now, expiry, expiry, Binary (LB.toStrict $ J.encode credential), now)
      pure purchaseId

getCodeBadgePurchase :: DB.Connection -> BadgeCodeRedemption -> IO (Maybe Int64)
getCodeBadgePurchase db BadgeCodeRedemption {redemptionId} =
  maybeFirstRow fromOnly $
    DB.query db "SELECT badge_purchase_id FROM badge_purchases WHERE badge_code_redemption_id = ?" (Only redemptionId)

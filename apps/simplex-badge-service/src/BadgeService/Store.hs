{-# LANGUAGE CPP #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | The badge service's own tables. They live in the client's database under the
-- @sx_badge_service_@ prefix the migration gives them (BadgeService.Store.SQLite.Migrations).
module BadgeService.Store
  ( MintedCode (..),
    CodeRedemption (..),
    RedeemedCode (..),
    CodeIssuance (..),
    withDB,
    withDB',
    getBadgeCode,
    purchaseKeyExists,
    writeCodeRedemption,
    insertBadgeCode,
  )
where

import Control.Logger.Simple (logError)
import Control.Monad.Except
import qualified Data.Aeson as J
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as LB
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (UTCTime)
import Simplex.Chat.Badges (BadgeCredential, BadgeMasterKey (..), BadgeType)
import Simplex.Chat.Badges.Types (BadgeCodePaymentStatus, BadgePurchaseStatus (..))
import Simplex.Chat.Store.Shared (insertedRowId)
import Simplex.Messaging.Agent.Store.Common (DBStore, withTransaction)
import Simplex.Messaging.Agent.Store.DB (Binary (..))
import qualified Simplex.Messaging.Agent.Store.DB as DB
import qualified Simplex.Messaging.Crypto as C
import Simplex.Messaging.Util (catchAll, maybeFirstRow, maybeFirstRow')

#if defined(dbPostgres)
import Database.PostgreSQL.Simple (Only (..))
import Database.PostgreSQL.Simple.SqlQQ (sql)
#else
import Database.SQLite.Simple (Only (..))
import Database.SQLite.Simple.QQ (sql)
#endif

-- The same shape as Directory.Util's helpers of these names; the two bots share no module.
-- Over a DBStore rather than a ChatController, so minting needs only a database.
withDB' :: Text -> DBStore -> (DB.Connection -> IO a) -> IO (Either String a)
withDB' cxt st a = withDB cxt st $ ExceptT . fmap Right . a

withDB :: Text -> DBStore -> (DB.Connection -> ExceptT String IO a) -> IO (Either String a)
withDB cxt chatStore action = do
  r_ <- withTransaction chatStore (runExceptT . action) `catchAll` (pure . Left . show)
  case r_ of
    Left e -> logError $ "Badge service database error: " <> cxt <> " " <> T.pack e
    Right _ -> pure ()
  pure r_

-- | A minted code as the service reads it back - never the code itself.
--
-- The month count the code was minted for is not here: redemption issues one credential and
-- writes no ledger, so nothing reads it yet.
data MintedCode = MintedCode
  { badgeCodeId :: Int64,
    badgeType :: BadgeType,
    redemption :: CodeRedemption
  }

-- | Whether a purchase claims this code, and if so whether its credential can be replayed.
-- The unreadable case is kept distinct from the unclaimed one: a claimed code is spent whether
-- or not its issuance can be read, and collapsing the two would issue it a second time.
data CodeRedemption
  = CodeUnredeemed
  | CodeRedeemed RedeemedCode
  | CodeRedeemedUnreadable

-- | The purchase a spent code created.
data RedeemedCode = RedeemedCode
  { purchaseKey :: C.PublicKeyEd25519,
    credential :: BadgeCredential
  }

-- | Everything one redemption writes, prepared before the transaction opens because the
-- credential is signed first.
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

-- | The purchase, its issuance and the code's redemption, in one transaction: the caller has
-- already signed, so nothing here can leave a code spent without a credential behind it.
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

{-# LANGUAGE CPP #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeOperators #-}

module StoreService.Names.Store
  ( PrepaidCode (..),
    NewPrepaidCode (..),
    CodeHashTaken (..),
    insertPrepaidCode,
    getPrepaidCode,
    revokePrepaidCode,
  )
where

import Data.ByteString (ByteString)
import Data.Int (Int64)
import Data.Maybe (isJust)
import Data.Time.Clock (UTCTime)
import Simplex.Chat.Badges.Types (BadgeCodePaymentStatus (..))
import Simplex.Chat.Store.Shared (insertedRowId)
import Simplex.Messaging.Agent.Store.DB (Binary (..))
import qualified Simplex.Messaging.Agent.Store.DB as DB
import Simplex.Messaging.Util (maybeFirstRow)
import StoreService.Store (RevokeResult (..))
import StoreService.Store.Invoices (executeChanging, truncateToSecond)

#if defined(dbPostgres)
import Database.PostgreSQL.Simple (Only (..), Query, (:.) (..))
import Database.PostgreSQL.Simple.SqlQQ (sql)
#else
import Database.SQLite.Simple (Only (..), Query, (:.) (..))
import Database.SQLite.Simple.QQ (sql)
#endif

data PrepaidCode = PrepaidCode
  { pcId :: Int64,
    pcCodeHash :: ByteString,
    pcMinLength :: Int,
    pcYears :: Int,
    pcPaymentStatus :: BadgeCodePaymentStatus,
    pcSpentAt :: Maybe UTCTime,
    pcRevokedAt :: Maybe UTCTime,
    pcExpiresAt :: Maybe UTCTime,
    pcCreatedAt :: UTCTime
  }
  deriving (Eq, Show)

data NewPrepaidCode = NewPrepaidCode
  { npcCodeHash :: ByteString,
    npcMinLength :: Int,
    npcYears :: Int,
    npcPaymentStatus :: BadgeCodePaymentStatus,
    npcExpiresAt :: Maybe UTCTime
  }

data CodeHashTaken = CodeHashTaken
  deriving (Eq, Show)

insertPrepaidCode :: DB.Connection -> NewPrepaidCode -> UTCTime -> IO (Either CodeHashTaken Int64)
insertPrepaidCode db NewPrepaidCode {npcCodeHash, npcMinLength, npcYears, npcPaymentStatus, npcExpiresAt} now = do
  inserted <-
    executeChanging
      db
      [sql|
        INSERT INTO sx_badge_service_name_prepaid_codes (code_hash, min_length, years, code_payment_status, expires_at, created_at)
        VALUES (?,?,?,?,?,?)
        ON CONFLICT DO NOTHING
      |]
      (Binary npcCodeHash, npcMinLength, npcYears, npcPaymentStatus, truncateToSecond <$> npcExpiresAt, truncateToSecond now)
  if inserted > 0 then Right <$> insertedRowId db else pure $ Left CodeHashTaken

prepaidCodeColumns :: Query
prepaidCodeColumns =
  [sql|
    SELECT name_prepaid_code_id, code_hash, min_length, years, code_payment_status, spent_at, revoked_at, expires_at, created_at
    FROM sx_badge_service_name_prepaid_codes
  |]

type PrepaidCodeRow = (Int64, Binary ByteString, Int, Int, BadgeCodePaymentStatus, Maybe UTCTime, Maybe UTCTime, Maybe UTCTime) :. Only UTCTime

toPrepaidCode :: PrepaidCodeRow -> PrepaidCode
toPrepaidCode ((pcId, Binary pcCodeHash, pcMinLength, pcYears, pcPaymentStatus, pcSpentAt, pcRevokedAt, pcExpiresAt) :. Only pcCreatedAt) =
  PrepaidCode {pcId, pcCodeHash, pcMinLength, pcYears, pcPaymentStatus, pcSpentAt, pcRevokedAt, pcExpiresAt, pcCreatedAt}

getPrepaidCode :: DB.Connection -> ByteString -> IO (Maybe PrepaidCode)
getPrepaidCode db codeHash =
  maybeFirstRow toPrepaidCode $ DB.query db (prepaidCodeColumns <> " WHERE code_hash = ?") (Only $ Binary codeHash)

-- | A spent code cannot be revoked.
revokePrepaidCode :: DB.Connection -> ByteString -> UTCTime -> IO RevokeResult
revokePrepaidCode db codeHash now = do
  revoked <-
    executeChanging
      db
      "UPDATE sx_badge_service_name_prepaid_codes SET revoked_at = ? WHERE code_hash = ? AND revoked_at IS NULL AND spent_at IS NULL"
      (truncateToSecond now, Binary codeHash)
  maybe NoSuchCode (result revoked) <$> getPrepaidCode db codeHash
  where
    result revoked PrepaidCode {pcId, pcRevokedAt}
      | revoked > 0 = Revoked pcId
      | isJust pcRevokedAt = AlreadyRevoked pcId
      | otherwise = AlreadyRedeemed

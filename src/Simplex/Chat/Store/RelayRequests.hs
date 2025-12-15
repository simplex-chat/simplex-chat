{-# LANGUAGE CPP #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Simplex.Chat.Store.RelayRequests
  ( hasPendingRelayRequests,
    getNextPendingRelayRequest,
    markRelayRequestFailed
  )
where

import Control.Monad.Except
import Data.Int (Int64)
import Data.Time.Clock (getCurrentTime)
import Simplex.Chat.Store.Groups (getGroupInfo)
import Simplex.Chat.Store.Shared
import Simplex.Chat.Types
import Simplex.Messaging.Agent.Protocol (InvitationId)
import Simplex.Messaging.Agent.Store.AgentStore (getWorkItem, maybeFirstRow)
import qualified Simplex.Messaging.Agent.Store.DB as DB
#if defined(dbPostgres)
import Database.PostgreSQL.Simple (Only (..))
import Database.PostgreSQL.Simple.SqlQQ (sql)
#else
import Database.SQLite.Simple (Only (..))
import Database.SQLite.Simple.QQ (sql)
#endif

hasPendingRelayRequests :: DB.Connection -> IO Bool
hasPendingRelayRequests db =
  fromOnly . head
    <$> DB.query
      db
      [sql|
        SELECT 1
        FROM groups
        WHERE relay_own_status = ?
          AND relay_request_failed = 0
        LIMIT 1
      |]
      (Only RSInvited)

getNextPendingRelayRequest :: DB.Connection -> VersionRangeChat -> User -> IO (Either StoreError (Maybe GroupInfo))
getNextPendingRelayRequest db vr user =
  getWorkItem "relay request" getGroupId getGroupInfo_ (markRelayRequestFailed db)
  where
    getGroupId :: IO (Maybe Int64)
    getGroupId =
      maybeFirstRow fromOnly $
        DB.query
          db
          [sql|
            SELECT group_id
            FROM groups
            WHERE relay_own_status = ?
              AND relay_request_failed = 0
            ORDER BY group_id ASC
            LIMIT 1
          |]
          (Only RSInvited)
    getGroupInfo_ :: Int64 -> IO (Either StoreError GroupInfo)
    getGroupInfo_ groupId = runExceptT $ getGroupInfo db vr user groupId

markRelayRequestFailed :: DB.Connection -> GroupId -> IO ()
markRelayRequestFailed db groupId = do
  currentTs <- getCurrentTime
  DB.execute
    db
    "UPDATE groups SET relay_request_failed = 1, updated_at = ? WHERE group_id = ?"
    (currentTs, groupId)

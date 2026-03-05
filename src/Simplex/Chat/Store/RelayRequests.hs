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
    setRelayRequestErr,
  )
where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Time.Clock (getCurrentTime)
import Simplex.Chat.Store.Shared
import Simplex.Chat.Types
import Simplex.Chat.Types.Shared
import Simplex.Messaging.Agent.Protocol (InvitationId)
import Simplex.Messaging.Agent.Store.AgentStore (getWorkItem, maybeFirstRow)
import qualified Simplex.Messaging.Agent.Store.DB as DB
import Simplex.Messaging.Util (firstRow')
import Simplex.Messaging.Version
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
        SELECT EXISTS (
          SELECT 1
          FROM groups
          WHERE relay_own_status = ?
            AND relay_request_failed = 0
            AND relay_request_err_reason IS NULL
          LIMIT 1
        )
      |]
      (Only RSInvited)

getNextPendingRelayRequest :: DB.Connection -> IO (Either StoreError (Maybe (GroupId, RelayRequestData)))
getNextPendingRelayRequest db =
  getWorkItem "relay request" getNextRequestGroupId getRelayRequestData (markRelayRequestFailed db)
  where
    getNextRequestGroupId :: IO (Maybe GroupId)
    getNextRequestGroupId =
      maybeFirstRow fromOnly $
        DB.query
          db
          [sql|
            SELECT group_id
            FROM groups
            WHERE relay_own_status = ?
              AND relay_request_failed = 0
              AND relay_request_err_reason IS NULL
            ORDER BY group_id ASC
            LIMIT 1
          |]
          (Only RSInvited)
    getRelayRequestData :: GroupId -> IO (Either StoreError (GroupId, RelayRequestData))
    getRelayRequestData groupId =
      firstRow' toRelayRequestData (SEGroupNotFound groupId) $
        DB.query
          db
          [sql|
            SELECT
              relay_request_inv_id, relay_request_group_link,
              relay_request_peer_chat_min_version, relay_request_peer_chat_max_version
            FROM groups
            WHERE group_id = ?
          |]
          (Only groupId)
      where
        toRelayRequestData :: (Maybe InvitationId, Maybe ShortLinkContact, Maybe VersionChat, Maybe VersionChat) -> Either StoreError (GroupId, RelayRequestData)
        toRelayRequestData = \case
          (Just relayInvId, Just reqGroupLink, Just minV, Just maxV) ->
            Right (groupId, RelayRequestData {relayInvId, reqGroupLink, reqChatVRange = fromMaybe (versionToRange maxV) $ safeVersionRange minV maxV})
          _ -> Left $ SEInternalError "missing relay request data"

markRelayRequestFailed :: DB.Connection -> GroupId -> IO ()
markRelayRequestFailed db groupId = do
  currentTs <- getCurrentTime
  DB.execute
    db
    "UPDATE groups SET relay_request_failed = 1, updated_at = ? WHERE group_id = ?"
    (currentTs, groupId)

setRelayRequestErr :: DB.Connection -> GroupId -> Text -> IO ()
setRelayRequestErr db groupId errReason = do
  currentTs <- getCurrentTime
  DB.execute
    db
    "UPDATE groups SET relay_request_err_reason = ?, updated_at = ? WHERE group_id = ?"
    (errReason, currentTs, groupId)

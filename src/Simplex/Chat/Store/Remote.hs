{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Simplex.Chat.Store.Remote where

import Data.ByteString.Char8 (ByteString)
import Data.Int (Int64)
import Data.Text (Text)
import qualified Database.SQLite.Simple as DB
import Simplex.Chat.Remote.Types (RemoteCtrl (..), RemoteCtrlId, RemoteHost (..), RemoteHostId)
import Simplex.Messaging.Agent.Store.SQLite (maybeFirstRow)
import qualified Simplex.Messaging.Crypto as C

getRemoteHosts :: DB.Connection -> IO [RemoteHost]
getRemoteHosts db =
  map toRemoteHost <$> DB.query_ db remoteHostQuery

getRemoteHost :: DB.Connection -> RemoteHostId -> IO (Maybe RemoteHost)
getRemoteHost db remoteHostId =
  maybeFirstRow toRemoteHost $
    DB.query db (remoteHostQuery <> "WHERE remote_host_id = ?") (DB.Only remoteHostId)

remoteHostQuery :: DB.Query
remoteHostQuery = "SELECT remote_host_id, display_name, store_path, ca_cert, ca_key FROM remote_hosts"

toRemoteHost :: (Int64, Text, FilePath, ByteString, C.Key) -> RemoteHost
toRemoteHost (remoteHostId, displayName, storePath, caCert, caKey) =
  RemoteHost {remoteHostId, displayName, storePath, caCert, caKey}

getRemoteCtrls :: DB.Connection -> IO [RemoteCtrl]
getRemoteCtrls db =
  map toRemoteCtrl <$> DB.query_ db remoteCtrlQuery

getRemoteCtrl :: DB.Connection -> RemoteCtrlId -> IO (Maybe RemoteCtrl)
getRemoteCtrl db remoteCtrlId =
  maybeFirstRow toRemoteCtrl $
    DB.query db (remoteCtrlQuery <> "WHERE remote_controller_id = ?") (DB.Only remoteCtrlId)

remoteCtrlQuery :: DB.Query
remoteCtrlQuery = "SELECT remote_controller_id, display_name, fingerprint, accepted FROM remote_controllers"

toRemoteCtrl :: (Int64, Text, C.KeyHash, Maybe Bool) -> RemoteCtrl
toRemoteCtrl (remoteCtrlId, displayName, fingerprint, accepted) =
  RemoteCtrl {remoteCtrlId, displayName, fingerprint, accepted}

markRemoteCtrlResolution :: DB.Connection -> RemoteCtrlId -> Bool -> IO ()
markRemoteCtrlResolution db remoteCtrlId accepted =
  DB.execute db "UPDATE remote_controllers SET accepted = ? WHERE remote_controller_id = ?" (accepted, remoteCtrlId)

deleteRemoteCtrl :: DB.Connection -> RemoteCtrlId -> IO ()
deleteRemoteCtrl db remoteCtrlId =
  DB.execute db "DELETE FROM remote_controllers WHERE remote_controller_id = ?" (DB.Only remoteCtrlId)

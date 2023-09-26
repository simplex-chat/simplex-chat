{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Simplex.Chat.Store.RemoteHosts where

import Data.ByteString.Char8 (ByteString)
import Data.Int (Int64)
import Data.Text (Text)
import qualified Database.SQLite.Simple as DB
import Simplex.Chat.Remote.Types (RemoteHostId, RemoteHost (..))
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

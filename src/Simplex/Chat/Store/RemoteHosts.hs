{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.RemoteHosts where

import Data.Maybe (listToMaybe)
import qualified Database.SQLite.Simple as DB
import Database.SQLite.Simple.QQ (sql)
import Simplex.Chat.Types (RemoteHostId, RemoteHostInfo)

getRemoteHosts :: DB.Connection -> IO [RemoteHostInfo]
getRemoteHosts db = DB.query_ db remoteHostQuery

getRemoteHost :: DB.Connection -> RemoteHostId -> IO (Maybe RemoteHostInfo)
getRemoteHost db remoteHostId = listToMaybe <$> DB.query db (remoteHostQuery <> [sql| WHERE remote_host_id = ? |]) (DB.Only remoteHostId)

remoteHostQuery :: DB.Query
remoteHostQuery = [sql| SELECT remote_host_id, display_name, path, properties FROM remote_hosts |]

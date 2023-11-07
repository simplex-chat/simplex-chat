{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Simplex.Chat.Store.Remote where

import Control.Monad.Except
import Data.Int (Int64)
import Data.Text (Text)
import Database.SQLite.Simple (Only (..))
import qualified Database.SQLite.Simple as SQL
import Simplex.Chat.Remote.Types
import Simplex.Chat.Store.Shared
import Simplex.Messaging.Agent.Store.SQLite (firstRow, maybeFirstRow)
import qualified Simplex.Messaging.Agent.Store.SQLite.DB as DB
import qualified Simplex.Messaging.Crypto as C

insertRemoteHost :: DB.Connection -> Text -> FilePath -> RCHostPairing -> IO RemoteHostId
insertRemoteHost db hostName storePath hostPairing = do
  DB.execute db
    [sql|
      INSERT INTO remote_hosts
        (host_name, store_path, ca_key, ca_cert, id_key, host_fingerprint, host_dh_pub, kem_shared)
      VALUES
        (?, ?, ?, ?, ?, ?, ?, ?)
    |]
    (hostName, storePath, caKey, caCert, idPrivKey, hostFingerprint, hostDHPublicKey, kemSharedKey)
  insertedRowId db
  where

getRemoteHosts :: DB.Connection -> IO [RemoteHost]
getRemoteHosts db =
  map toRemoteHost <$> DB.query_ db remoteHostQuery

getRemoteHost :: DB.Connection -> RemoteHostId -> ExceptT StoreError IO RemoteHost
getRemoteHost db remoteHostId =
  ExceptT . firstRow toRemoteHost (SERemoteHostNotFound remoteHostId) $
    DB.query db (remoteHostQuery <> " WHERE remote_host_id = ?") (Only remoteHostId)

getRemoteHostFingerprint :: DB.Connection -> C.KeyHash -> IO (Maybe RemoteHost)
getRemoteHostFingerprint db fingerprint =
  maybeFirstRow toRemoteHost $
    DB.query db (remoteHostQuery <> " WHERE fingerprint = ?") (Only fingerprint)

remoteHostQuery :: SQL.Query
remoteHostQuery = "SELECT remote_host_id, host_name, store_path, ca_key, ca_cert, id_key, host_fingerprint, host_dh_pub, kem_shared FROM remote_hosts"

toRemoteHost :: (Int64, Text, FilePath, C.APrivateSignKey, C.SignedCertificate, C.PrivateKeyEd25519, C.PublicKeyX25519, KEMSharedKey) -> RemoteHost
toRemoteHost (remoteHostId, hostName, storePath, caKey, caCert, idPrivKey, hostFingerprint, hostDHPublicKey, kemSharedKey) =
  RemoteHost {remoteHostId, hostName, storePath, hostPairing}
  where
    hostPairing = RCHostPairing {caKey, caCert, idPrivKey, knownHost = Just knownHostPairing}
    knownHostPairing = KnownHostPairing {hostFingerprint, storedSessKeys}
    storedSessKeys = StoredHostSessKeys {hostDHPublicKey, kemSharedKey}

deleteRemoteHostRecord :: DB.Connection -> RemoteHostId -> IO ()
deleteRemoteHostRecord db remoteHostId = DB.execute db "DELETE FROM remote_hosts WHERE remote_host_id = ?" (Only remoteHostId)

insertRemoteCtrl :: DB.Connection -> todoSignedOOB -> IO RemoteCtrlInfo
insertRemoteCtrl = undefined
-- insertRemoteCtrl db (SignedOOB OOB {deviceName, caFingerprint = fingerprint} _) = do
--   let displayName = fromMaybe "" deviceName
--   DB.execute db "INSERT INTO remote_controllers (display_name, fingerprint) VALUES (?,?)" (displayName, fingerprint)
--   remoteCtrlId <- insertedRowId db
--   pure RemoteCtrlInfo {remoteCtrlId, displayName, fingerprint, accepted = Nothing, sessionActive = False}

getRemoteCtrls :: DB.Connection -> IO [RemoteCtrl]
getRemoteCtrls db =
  map toRemoteCtrl <$> DB.query_ db remoteCtrlQuery

getRemoteCtrl :: DB.Connection -> RemoteCtrlId -> ExceptT StoreError IO RemoteCtrl
getRemoteCtrl db remoteCtrlId =
  ExceptT . firstRow toRemoteCtrl (SERemoteCtrlNotFound remoteCtrlId) $
    DB.query db (remoteCtrlQuery <> " WHERE remote_controller_id = ?") (Only remoteCtrlId)

getRemoteCtrlByFingerprint :: DB.Connection -> C.KeyHash -> IO (Maybe RemoteCtrl)
getRemoteCtrlByFingerprint db fingerprint =
  maybeFirstRow toRemoteCtrl $
    DB.query db (remoteCtrlQuery <> " WHERE fingerprint = ?") (Only fingerprint)

remoteCtrlQuery :: SQL.Query
remoteCtrlQuery = "SELECT remote_controller_id, display_name, fingerprint, accepted FROM remote_controllers"

toRemoteCtrl :: (Int64, Text, C.KeyHash, Maybe Bool) -> RemoteCtrl
toRemoteCtrl (remoteCtrlId, displayName, fingerprint, accepted) =
  RemoteCtrl {remoteCtrlId, displayName, fingerprint, accepted}

markRemoteCtrlResolution :: DB.Connection -> RemoteCtrlId -> Bool -> IO ()
markRemoteCtrlResolution db remoteCtrlId accepted =
  DB.execute db "UPDATE remote_controllers SET accepted = ? WHERE remote_controller_id = ? AND accepted IS NULL" (accepted, remoteCtrlId)

deleteRemoteCtrlRecord :: DB.Connection -> RemoteCtrlId -> IO ()
deleteRemoteCtrlRecord db remoteCtrlId =
  DB.execute db "DELETE FROM remote_controllers WHERE remote_controller_id = ?" (Only remoteCtrlId)

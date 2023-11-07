{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.Remote where

import Control.Monad.Except
import Data.Int (Int64)
import Data.Text (Text)
import Database.SQLite.Simple (Only (..))
import qualified Database.SQLite.Simple as SQL
import Database.SQLite.Simple.QQ (sql)
import Simplex.Chat.Remote.Types
import Simplex.Chat.Store.Shared
import Simplex.Messaging.Agent.Store.SQLite (firstRow, maybeFirstRow)
import qualified Simplex.Messaging.Agent.Store.SQLite.DB as DB
import qualified Simplex.Messaging.Crypto as C
import Simplex.Messaging.Crypto.SNTRUP761.Bindings (KEMSharedKey)
import Simplex.RemoteControl.Types
import UnliftIO

insertRemoteHost :: DB.Connection -> Text -> FilePath -> RCHostPairing -> ExceptT StoreError IO RemoteHostId
insertRemoteHost db hostName storePath RCHostPairing {caKey, caCert, idPrivKey, knownHost = kh_} = do
  KnownHostPairing {hostFingerprint, storedSessKeys = StoredHostSessKeys {hostDHPublicKey, kemSharedKey}} <-
    maybe (throwError SERemoteHostUnknown) pure kh_
  checkConstraint SERemoteHostDuplicateFingerprint . liftIO $
    DB.execute
      db
      [sql|
      INSERT INTO remote_hosts
        (host_name, store_path, ca_key, ca_cert, id_key, host_fingerprint, host_dh_pub, kem_shared)
      VALUES
        (?, ?, ?, ?, ?, ?, ?, ?)
    |]
      (hostName, storePath, caKey, C.SignedObject caCert, idPrivKey, hostFingerprint, hostDHPublicKey, kemSharedKey)
  liftIO $ insertedRowId db

getRemoteHosts :: DB.Connection -> IO [RemoteHost]
getRemoteHosts db =
  map toRemoteHost <$> DB.query_ db remoteHostQuery

getRemoteHost :: DB.Connection -> RemoteHostId -> ExceptT StoreError IO RemoteHost
getRemoteHost db remoteHostId =
  ExceptT . firstRow toRemoteHost (SERemoteHostNotFound remoteHostId) $
    DB.query db (remoteHostQuery <> " WHERE remote_host_id = ?") (Only remoteHostId)

getRemoteHostByFingerprint :: DB.Connection -> C.KeyHash -> IO (Maybe RemoteHost)
getRemoteHostByFingerprint db fingerprint =
  maybeFirstRow toRemoteHost $
    DB.query db (remoteHostQuery <> " WHERE host_fingerprint = ?") (Only fingerprint)

remoteHostQuery :: SQL.Query
remoteHostQuery =
  [sql|
    SELECT remote_host_id, host_name, store_path, ca_key, ca_cert, id_key, host_fingerprint, host_dh_pub, kem_shared
    FROM remote_hosts
  |]

toRemoteHost :: (Int64, Text, FilePath, C.APrivateSignKey, C.SignedObject C.Certificate, C.PrivateKeyEd25519, C.KeyHash, C.PublicKeyX25519, KEMSharedKey) -> RemoteHost
toRemoteHost (remoteHostId, hostName, storePath, caKey, C.SignedObject caCert, idPrivKey, hostFingerprint, hostDHPublicKey, kemSharedKey) =
  RemoteHost {remoteHostId, hostName, storePath, hostPairing}
  where
    hostPairing = RCHostPairing {caKey, caCert, idPrivKey, knownHost = Just knownHostPairing}
    knownHostPairing = KnownHostPairing {hostFingerprint, storedSessKeys}
    storedSessKeys = StoredHostSessKeys {hostDHPublicKey, kemSharedKey}

deleteRemoteHostRecord :: DB.Connection -> RemoteHostId -> IO ()
deleteRemoteHostRecord db remoteHostId = DB.execute db "DELETE FROM remote_hosts WHERE remote_host_id = ?" (Only remoteHostId)

insertRemoteCtrl :: DB.Connection -> Text -> RCCtrlPairing -> ExceptT StoreError IO RemoteCtrlId
insertRemoteCtrl db ctrlName RCCtrlPairing {caKey, caCert, ctrlFingerprint, idPubKey, storedSessKeys, prevStoredSessKeys} = do
  checkConstraint SERemoteCtrlDuplicateFingerprint . liftIO $
    DB.execute
      db
      [sql|
      INSERT INTO remote_controllers
        (ctrl_name, ca_key, ca_cert, ctrl_fingerprint, id_pub, sess_dh_key, sess_kem_shared, prev_dh_key, prev_kem_shared)
      VALUES
        (?, ?, ?, ?, ?, ?, ?, ?, ?)
    |]
      (ctrlName, caKey, C.SignedObject caCert, ctrlFingerprint, idPubKey, sessDhKey, sessKemShared, prevDhKey, prevKemShared)
  liftIO $ insertedRowId db
  where
    StoredCtrlSessKeys {dhPrivKey = sessDhKey, kemSharedKey = sessKemShared} = storedSessKeys
    (prevDhKey, prevKemShared) =
      maybe (Nothing, Nothing) (\StoredCtrlSessKeys {dhPrivKey, kemSharedKey} -> (Just dhPrivKey, kemSharedKey)) prevStoredSessKeys

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
    DB.query db (remoteCtrlQuery <> " WHERE ctrl_fingerprint = ?") (Only fingerprint)

remoteCtrlQuery :: SQL.Query
remoteCtrlQuery =
  [sql|
    SELECT remote_controller_id, ctrl_name, ca_key, ca_cert, ctrl_fingerprint, id_pub, sess_dh_key, sess_kem_shared, prev_dh_key, prev_kem_shared
    FROM remote_controllers
  |]

-- toRemoteCtrl :: (Int64, Text, C.KeyHash, Maybe Bool) -> RemoteCtrl
toRemoteCtrl ::
  ( RemoteCtrlId,
    Text,
    C.APrivateSignKey,
    C.SignedObject C.Certificate,
    C.KeyHash,
    C.PublicKeyEd25519,
    C.PrivateKeyX25519,
    KEMSharedKey,
    Maybe C.PrivateKeyX25519,
    Maybe KEMSharedKey
  ) ->
  RemoteCtrl
toRemoteCtrl (remoteCtrlId, ctrlName, caKey, C.SignedObject caCert, ctrlFingerprint, idPubKey, sess_dh_key, sess_kem_shared, prev_dh_key, prev_kem_shared) =
  RemoteCtrl
    { remoteCtrlId,
      ctrlName,
      ctrlPairing =
        RCCtrlPairing
          { caKey,
            caCert,
            ctrlFingerprint,
            idPubKey,
            storedSessKeys = StoredCtrlSessKeys {dhPrivKey = sess_dh_key, kemSharedKey = Just sess_kem_shared},
            prevStoredSessKeys = StoredCtrlSessKeys <$> prev_dh_key <*> Just prev_kem_shared
          }
    }

deleteRemoteCtrlRecord :: DB.Connection -> RemoteCtrlId -> IO ()
deleteRemoteCtrlRecord db remoteCtrlId =
  DB.execute db "DELETE FROM remote_controllers WHERE remote_controller_id = ?" (Only remoteCtrlId)

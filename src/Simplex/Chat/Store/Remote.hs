{-# LANGUAGE CPP #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.Remote where

import Control.Monad.Except
import Data.Int (Int64)
import Data.Text (Text)
import Data.Text.Encoding (decodeASCII, encodeUtf8)
import Data.Word (Word16)
import Simplex.Chat.Remote.Types
import Simplex.Chat.Store.Shared
import Simplex.Messaging.Agent.Store.AgentStore (firstRow, maybeFirstRow)
import qualified Simplex.Messaging.Agent.Store.DB as DB
import qualified Simplex.Messaging.Crypto as C
import Simplex.Messaging.Encoding.String (StrEncoding (..))
import Simplex.RemoteControl.Types
import UnliftIO
#if defined(dbPostgres)
import Database.PostgreSQL.Simple (Only (..), Query)
import Database.PostgreSQL.Simple.SqlQQ (sql)
#else
import Database.SQLite.Simple (Only (..), Query)
import Database.SQLite.Simple.QQ (sql)
#endif

insertRemoteHost :: DB.Connection -> Text -> FilePath -> Maybe RCCtrlAddress -> Maybe Word16 -> RCHostPairing -> ExceptT StoreError IO RemoteHostId
insertRemoteHost db hostDeviceName storePath rcAddr_ bindPort_ RCHostPairing {caKey, caCert, idPrivKey, knownHost = kh_} = do
  KnownHostPairing {hostFingerprint, hostDhPubKey} <-
    maybe (throwError SERemoteHostUnknown) pure kh_
  checkConstraint SERemoteHostDuplicateCA . liftIO $
    DB.execute
      db
      [sql|
        INSERT INTO remote_hosts
          (host_device_name, store_path, bind_addr, bind_iface, bind_port, ca_key, ca_cert, id_key, host_fingerprint, host_dh_pub)
        VALUES
          (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
      |]
      (hostDeviceName, storePath, bindAddr_, bindIface_, bindPort_, caKey, C.SignedObject caCert, idPrivKey, hostFingerprint, hostDhPubKey)
  liftIO $ insertedRowId db
  where
    (bindAddr_, bindIface_) = rcCtrlAddressFields_ rcAddr_

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

remoteHostQuery :: Query
remoteHostQuery =
  [sql|
    SELECT remote_host_id, host_device_name, store_path, ca_key, ca_cert, id_key, host_fingerprint, host_dh_pub, bind_iface, bind_addr, bind_port
    FROM remote_hosts
  |]

toRemoteHost :: (Int64, Text, FilePath, C.APrivateSignKey, C.SignedObject C.Certificate, C.PrivateKeyEd25519, C.KeyHash, C.PublicKeyX25519, Maybe Text, Maybe Text, Maybe Word16) -> RemoteHost
toRemoteHost (remoteHostId, hostDeviceName, storePath, caKey, C.SignedObject caCert, idPrivKey, hostFingerprint, hostDhPubKey, ifaceName_, ifaceAddr_, bindPort_) =
  RemoteHost {remoteHostId, hostDeviceName, storePath, hostPairing, bindAddress_, bindPort_}
  where
    hostPairing = RCHostPairing {caKey, caCert, idPrivKey, knownHost = Just knownHost}
    knownHost = KnownHostPairing {hostFingerprint, hostDhPubKey}
    bindAddress_ = RCCtrlAddress <$> (decodeAddr <$> ifaceAddr_) <*> ifaceName_
    decodeAddr = either (error "Error parsing TransportHost") id . strDecode . encodeUtf8

updateHostPairing :: DB.Connection -> RemoteHostId -> Text -> C.PublicKeyX25519 -> Maybe RCCtrlAddress -> Maybe Word16 -> IO ()
updateHostPairing db rhId hostDeviceName hostDhPubKey rcAddr_ bindPort_ =
  DB.execute
    db
    [sql|
      UPDATE remote_hosts
      SET host_device_name = ?, host_dh_pub = ?, bind_addr = ?, bind_iface = ?, bind_port = ?
      WHERE remote_host_id = ?
    |]
    (hostDeviceName, hostDhPubKey, bindAddr_, bindIface_, bindPort_, rhId)
  where
    (bindAddr_, bindIface_) = rcCtrlAddressFields_ rcAddr_

rcCtrlAddressFields_ :: Maybe RCCtrlAddress -> (Maybe Text, Maybe Text)
rcCtrlAddressFields_ = maybe (Nothing, Nothing) $ \RCCtrlAddress {address, interface} -> (Just . decodeASCII $ strEncode address, Just interface)

deleteRemoteHostRecord :: DB.Connection -> RemoteHostId -> IO ()
deleteRemoteHostRecord db remoteHostId = DB.execute db "DELETE FROM remote_hosts WHERE remote_host_id = ?" (Only remoteHostId)

insertRemoteCtrl :: DB.Connection -> Text -> RCCtrlPairing -> ExceptT StoreError IO RemoteCtrlId
insertRemoteCtrl db ctrlDeviceName RCCtrlPairing {caKey, caCert, ctrlFingerprint, idPubKey, dhPrivKey, prevDhPrivKey} = do
  checkConstraint SERemoteCtrlDuplicateCA . liftIO $
    DB.execute
      db
      [sql|
      INSERT INTO remote_controllers
        (ctrl_device_name, ca_key, ca_cert, ctrl_fingerprint, id_pub, dh_priv_key, prev_dh_priv_key)
      VALUES
        (?, ?, ?, ?, ?, ?, ?)
    |]
      (ctrlDeviceName, caKey, C.SignedObject caCert, ctrlFingerprint, idPubKey, dhPrivKey, prevDhPrivKey)
  liftIO $ insertedRowId db

getRemoteCtrls :: DB.Connection -> IO [RemoteCtrl]
getRemoteCtrls db =
  map toRemoteCtrl <$> DB.query_ db remoteCtrlQuery

getRemoteCtrl :: DB.Connection -> RemoteCtrlId -> ExceptT StoreError IO RemoteCtrl
getRemoteCtrl db remoteCtrlId =
  ExceptT . firstRow toRemoteCtrl (SERemoteCtrlNotFound remoteCtrlId) $
    DB.query db (remoteCtrlQuery <> " WHERE remote_ctrl_id = ?") (Only remoteCtrlId)

getRemoteCtrlByFingerprint :: DB.Connection -> C.KeyHash -> IO (Maybe RemoteCtrl)
getRemoteCtrlByFingerprint db fingerprint =
  maybeFirstRow toRemoteCtrl $
    DB.query db (remoteCtrlQuery <> " WHERE ctrl_fingerprint = ?") (Only fingerprint)

remoteCtrlQuery :: Query
remoteCtrlQuery =
  [sql|
    SELECT remote_ctrl_id, ctrl_device_name, ca_key, ca_cert, ctrl_fingerprint, id_pub, dh_priv_key, prev_dh_priv_key
    FROM remote_controllers
  |]

toRemoteCtrl ::
  ( RemoteCtrlId,
    Text,
    C.APrivateSignKey,
    C.SignedObject C.Certificate,
    C.KeyHash,
    C.PublicKeyEd25519,
    C.PrivateKeyX25519,
    Maybe C.PrivateKeyX25519
  ) ->
  RemoteCtrl
toRemoteCtrl (remoteCtrlId, ctrlDeviceName, caKey, C.SignedObject caCert, ctrlFingerprint, idPubKey, dhPrivKey, prevDhPrivKey) =
  let ctrlPairing = RCCtrlPairing {caKey, caCert, ctrlFingerprint, idPubKey, dhPrivKey, prevDhPrivKey}
   in RemoteCtrl {remoteCtrlId, ctrlDeviceName, ctrlPairing}

updateRemoteCtrl :: DB.Connection -> RemoteCtrl -> Text -> C.PrivateKeyX25519 -> IO ()
updateRemoteCtrl db RemoteCtrl {remoteCtrlId} ctrlDeviceName dhPrivKey =
  DB.execute
    db
    [sql|
      UPDATE remote_controllers
      SET ctrl_device_name = ?, dh_priv_key = ?, prev_dh_priv_key = dh_priv_key
      WHERE remote_ctrl_id = ?
    |]
    (ctrlDeviceName, dhPrivKey, remoteCtrlId)

deleteRemoteCtrlRecord :: DB.Connection -> RemoteCtrlId -> IO ()
deleteRemoteCtrlRecord db remoteCtrlId =
  DB.execute db "DELETE FROM remote_controllers WHERE remote_ctrl_id = ?" (Only remoteCtrlId)

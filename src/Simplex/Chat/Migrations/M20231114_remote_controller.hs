{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Migrations.M20231114_remote_controller where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20231114_remote_controller :: Query
m20231114_remote_controller =
  [sql|
CREATE TABLE remote_hosts ( -- hosts known to a controlling app
  remote_host_id INTEGER PRIMARY KEY AUTOINCREMENT,
  host_name TEXT NOT NULL,        -- remote device name provided in app info
  store_path TEXT NOT NULL,       -- file path relative to app store (must not contain "/")
  -- RCHostPairing
  ca_key BLOB NOT NULL,           -- private key for signing session certificates
  ca_cert BLOB NOT NULL,          -- root certificate, whose fingerprint is pinned on a remote
  id_key BLOB NOT NULL,           -- long-term/identity signing key
  -- KnownHostPairing
  host_fingerprint BLOB NOT NULL, -- pinned remote host CA, set when connected
  -- stored host session key
  host_dh_pub BLOB NOT NULL,      -- session DH key
  UNIQUE (host_fingerprint) ON CONFLICT FAIL
);

CREATE TABLE remote_controllers ( -- controllers known to a hosting app
  remote_controller_id INTEGER PRIMARY KEY AUTOINCREMENT,
  ctrl_name TEXT NOT NULL,        -- remote device name provided in app info
  -- RCCtrlPairing
  ca_key BLOB NOT NULL,           -- CA key
  ca_cert BLOB NOT NULL,          -- CA certificate for TLS clients
  ctrl_fingerprint BLOB NOT NULL, -- pinned remote controller CA, set when connected
  id_pub BLOB NOT NULL,           -- remote controller long-term/identity signing key
  -- stored session key, commited on connection confirmation
  dh_priv_key BLOB NOT NULL,      -- current DH key
  -- prev session key
  prev_dh_priv_key BLOB,          -- previous DH key
  UNIQUE (ctrl_fingerprint) ON CONFLICT FAIL
);
|]

down_m20231114_remote_controller :: Query
down_m20231114_remote_controller =
  [sql|
DROP TABLE remote_hosts;
DROP TABLE remote_controllers;
|]

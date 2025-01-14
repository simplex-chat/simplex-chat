{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.SQLite.Migrations.M20231114_remote_control where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20231114_remote_control :: Query
m20231114_remote_control =
  [sql|
CREATE TABLE remote_hosts ( -- e.g., mobiles known to a desktop app
  remote_host_id INTEGER PRIMARY KEY AUTOINCREMENT,
  host_device_name TEXT NOT NULL,
  store_path TEXT NOT NULL, -- relative folder name for host files
  ca_key BLOB NOT NULL,
  ca_cert BLOB NOT NULL,
  id_key BLOB NOT NULL, -- long-term/identity signing key
  host_fingerprint BLOB NOT NULL, -- remote host CA cert fingerprint, set when connected
  host_dh_pub BLOB NOT NULL -- last session DH key
);

CREATE UNIQUE INDEX idx_remote_hosts_host_fingerprint ON remote_hosts(host_fingerprint);

CREATE TABLE remote_controllers ( -- e.g., desktops known to a mobile app
  remote_ctrl_id INTEGER PRIMARY KEY AUTOINCREMENT,
  ctrl_device_name TEXT NOT NULL,
  ca_key BLOB NOT NULL,
  ca_cert BLOB NOT NULL,
  ctrl_fingerprint BLOB NOT NULL, -- remote controller CA cert fingerprint, set when connected
  id_pub BLOB NOT NULL, -- remote controller long-term/identity key to verify signatures
  dh_priv_key BLOB NOT NULL, -- last session DH key
  prev_dh_priv_key BLOB -- previous session DH key
);

CREATE UNIQUE INDEX idx_remote_controllers_ctrl_fingerprint ON remote_controllers(ctrl_fingerprint);
|]

down_m20231114_remote_control :: Query
down_m20231114_remote_control =
  [sql|
DROP INDEX idx_remote_hosts_host_fingerprint;
DROP INDEX idx_remote_controllers_ctrl_fingerprint;
DROP TABLE remote_hosts;
DROP TABLE remote_controllers;
|]

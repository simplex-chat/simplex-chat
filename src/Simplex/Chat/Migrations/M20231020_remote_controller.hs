{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Migrations.M20231020_remote_controller where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20231020_remote_controller :: Query
m20231020_remote_controller =
  [sql|
CREATE TABLE remote_hosts ( -- hosts known to a controlling app
  remote_host_id INTEGER PRIMARY KEY AUTOINCREMENT,
  store_path TEXT NOT NULL,            -- file path relative to app store (must not contain "/")
  display_name TEXT NOT NULL,          -- user-provided name for a remote host
  ca_key BLOB NOT NULL,                -- private key for signing session certificates
  ca_cert BLOB NOT NULL,               -- root certificate, whose fingerprint is pinned on a remote
  contacted INTEGER NOT NULL DEFAULT 0 -- 0 (first time), 1 (connected before)
);

CREATE TABLE remote_controllers ( -- controllers known to a hosting app
  remote_controller_id INTEGER PRIMARY KEY AUTOINCREMENT,
  display_name TEXT NOT NULL, -- user-provided name for a remote controller
  fingerprint BLOB NOT NULL,  -- remote controller CA fingerprint
  accepted INTEGER            -- NULL (unknown), 0 (rejected), 1 (confirmed)
);
|]

down_m20231020_remote_controller :: Query
down_m20231020_remote_controller =
  [sql|
DROP TABLE remote_hosts;
DROP TABLE remote_controllers;
|]

{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Migrations.M20230922_remote_controller where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20230922_remote_controller :: Query
m20230922_remote_controller =
  [sql|
CREATE TABLE remote_hosts ( -- hosts known to a controlling app
  remote_host_id INTEGER PRIMARY KEY,
  display_name TEXT NOT NULL,
  store_path TEXT NOT NULL,
  ca_cert BLOB NOT NULL,
  ca_key BLOB NOT NULL
);

CREATE TABLE remote_controllers ( -- controllers known to a hosting app
  remote_controller_id INTEGER PRIMARY KEY,
  display_name TEXT NOT NULL,
  fingerprint BLOB NOT NULL,
  accepted INTEGER
);
|]

down_m20230922_remote_controller :: Query
down_m20230922_remote_controller =
  [sql|
DROP TABLE remote_hosts;
DROP TABLE remote_controllers;
|]

{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.SQLite.Migrations.M20220301_smp_servers where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20220301_smp_servers :: Query
m20220301_smp_servers =
  [sql|
CREATE TABLE smp_servers (
  smp_server_id INTEGER PRIMARY KEY,
  host TEXT NOT NULL,
  port TEXT NOT NULL,
  key_hash BLOB NOT NULL,
  user_id INTEGER NOT NULL REFERENCES users ON DELETE CASCADE,
  created_at TEXT NOT NULL DEFAULT (datetime('now')),
  updated_at TEXT NOT NULL DEFAULT (datetime('now')),
  UNIQUE (host, port)
);
|]

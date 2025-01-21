{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.SQLite.Migrations.M20230118_recreate_smp_servers where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

-- UNIQUE constraint includes user_id
m20230118_recreate_smp_servers :: Query
m20230118_recreate_smp_servers =
  [sql|
DROP INDEX idx_smp_servers_user_id;

CREATE TABLE new_smp_servers (
  smp_server_id INTEGER PRIMARY KEY,
  host TEXT NOT NULL,
  port TEXT NOT NULL,
  key_hash BLOB NOT NULL,
  basic_auth TEXT,
  preset INTEGER NOT NULL DEFAULT 0,
  tested INTEGER,
  enabled INTEGER NOT NULL DEFAULT 1,
  user_id INTEGER NOT NULL REFERENCES users ON DELETE CASCADE,
  created_at TEXT NOT NULL DEFAULT (datetime('now')),
  updated_at TEXT NOT NULL DEFAULT (datetime('now')),
  UNIQUE (user_id, host, port)
);

INSERT INTO new_smp_servers
  (smp_server_id, host, port, key_hash, basic_auth, preset, tested, enabled, user_id, created_at, updated_at)
SELECT
  smp_server_id, host, port, key_hash, basic_auth, preset, tested, enabled, user_id, created_at, updated_at
  FROM smp_servers;

DROP TABLE smp_servers;
ALTER TABLE new_smp_servers RENAME TO smp_servers;

CREATE INDEX idx_smp_servers_user_id ON "smp_servers"(user_id);
|]

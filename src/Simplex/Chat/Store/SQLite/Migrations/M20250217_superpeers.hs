{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.SQLite.Migrations.M20250217_superpeers where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20250217_superpeers :: Query
m20250217_superpeers =
  [sql|
CREATE TABLE superpeers(
  superpeer_id INTEGER PRIMARY KEY,
  address TEXT NOT NULL,
  name TEXT NOT NULL,
  domains TEXT NOT NULL,
  preset INTEGER NOT NULL DEFAULT 0,
  tested INTEGER,
  enabled INTEGER NOT NULL DEFAULT 1,
  user_id INTEGER NOT NULL REFERENCES users ON DELETE CASCADE,
  created_at TEXT NOT NULL DEFAULT(datetime('now')),
  updated_at TEXT NOT NULL DEFAULT(datetime('now')),
  UNIQUE(user_id, address),
  UNIQUE(user_id, name)
);

CREATE INDEX idx_superpeers_user_id ON superpeers(user_id);

ALTER TABLE users ADD COLUMN user_superpeer INTEGER NOT NULL DEFAULT 0;

ALTER TABLE group_members ADD COLUMN superpeer INTEGER NOT NULL DEFAULT 0;
|]

down_m20250217_superpeers :: Query
down_m20250217_superpeers =
  [sql|
ALTER TABLE group_members DROP COLUMN superpeer;

ALTER TABLE users DROP COLUMN user_superpeer;

DROP INDEX idx_superpeers_user_id;

DROP TABLE superpeers;
|]

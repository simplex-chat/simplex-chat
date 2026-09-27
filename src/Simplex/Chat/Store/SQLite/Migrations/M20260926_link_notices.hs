{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.SQLite.Migrations.M20260926_link_notices where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20260926_link_notices :: Query
m20260926_link_notices =
  [sql|
CREATE TABLE link_notices(
  link_notice_id INTEGER PRIMARY KEY AUTOINCREMENT,
  link_hash BLOB NOT NULL,
  expires_at TEXT,
  reason TEXT,
  created_at TEXT NOT NULL,
  updated_at TEXT NOT NULL
) STRICT;

CREATE UNIQUE INDEX idx_link_notices_link_hash ON link_notices(link_hash);
|]

down_m20260926_link_notices :: Query
down_m20260926_link_notices =
  [sql|
DROP INDEX idx_link_notices_link_hash;
DROP TABLE link_notices;
|]

{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.SQLite.Migrations.M20261107_link_notices where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20261107_link_notices :: Query
m20261107_link_notices =
  [sql|
CREATE TABLE link_notices(
  link_notice_id INTEGER PRIMARY KEY AUTOINCREMENT,
  link_hash BLOB NOT NULL,
  expires_at TEXT,
  created_at TEXT NOT NULL,
  updated_at TEXT NOT NULL
) STRICT;

CREATE UNIQUE INDEX idx_link_notices_link_hash ON link_notices(link_hash);

ALTER TABLE groups ADD COLUMN silently_removed INTEGER NOT NULL DEFAULT 0;
|]

down_m20261107_link_notices :: Query
down_m20261107_link_notices =
  [sql|
ALTER TABLE groups DROP COLUMN silently_removed;

DROP INDEX idx_link_notices_link_hash;
DROP TABLE link_notices;
|]

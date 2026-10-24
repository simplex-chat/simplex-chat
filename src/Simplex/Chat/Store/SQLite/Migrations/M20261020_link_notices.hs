{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.SQLite.Migrations.M20261020_link_notices where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20261020_link_notices :: Query
m20261020_link_notices =
  [sql|
CREATE TABLE link_notices(
  link_notice_id INTEGER PRIMARY KEY AUTOINCREMENT,
  link_hash BLOB NOT NULL,
  expires_at TEXT,
  created_at TEXT NOT NULL,
  updated_at TEXT NOT NULL
) STRICT;

CREATE UNIQUE INDEX idx_link_notices_link_hash ON link_notices(link_hash);

ALTER TABLE group_members ADD COLUMN member_restriction_expires_at TEXT;
ALTER TABLE group_members ADD COLUMN member_restriction_by_user INTEGER NOT NULL DEFAULT 0;

CREATE INDEX idx_group_members_member_restriction_expires_at ON group_members(member_restriction_expires_at);
|]

down_m20261020_link_notices :: Query
down_m20261020_link_notices =
  [sql|
DROP INDEX idx_group_members_member_restriction_expires_at;

ALTER TABLE group_members DROP COLUMN member_restriction_by_user;
ALTER TABLE group_members DROP COLUMN member_restriction_expires_at;

DROP INDEX idx_link_notices_link_hash;
DROP TABLE link_notices;
|]

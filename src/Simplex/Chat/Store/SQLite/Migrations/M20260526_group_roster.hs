{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.SQLite.Migrations.M20260526_group_roster where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20260526_group_roster :: Query
m20260526_group_roster =
  [sql|
ALTER TABLE groups ADD COLUMN roster_version INTEGER;
ALTER TABLE groups ADD COLUMN roster_msg BLOB;

ALTER TABLE group_members ADD COLUMN delivered_roster_version INTEGER;
|]

down_m20260526_group_roster :: Query
down_m20260526_group_roster =
  [sql|
ALTER TABLE group_members DROP COLUMN delivered_roster_version;

ALTER TABLE groups DROP COLUMN roster_msg;
ALTER TABLE groups DROP COLUMN roster_version;
|]

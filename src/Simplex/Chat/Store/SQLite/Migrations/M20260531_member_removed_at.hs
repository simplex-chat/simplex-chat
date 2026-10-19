{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.SQLite.Migrations.M20260531_member_removed_at where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20260531_member_removed_at :: Query
m20260531_member_removed_at =
  [sql|
ALTER TABLE group_members ADD COLUMN removed_at TEXT;
|]

down_m20260531_member_removed_at :: Query
down_m20260531_member_removed_at =
  [sql|
ALTER TABLE group_members DROP COLUMN removed_at;
|]

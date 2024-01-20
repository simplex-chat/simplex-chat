{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Migrations.M20240115_block_member_for_all where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20240115_block_member_for_all :: Query
m20240115_block_member_for_all =
  [sql|
ALTER TABLE group_members ADD COLUMN member_restriction TEXT;
|]

down_m20240115_block_member_for_all :: Query
down_m20240115_block_member_for_all =
  [sql|
ALTER TABLE group_members DROP COLUMN member_restriction;
|]

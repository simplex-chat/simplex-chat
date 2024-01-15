{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Migrations.M20240115_group_show_messages where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)


m20240115_group_show_messages :: Query
m20240115_group_show_messages =
  [sql|
ALTER TABLE group_members ADD COLUMN group_show_messages INTEGER;
|]

down_m20240115_group_show_messages :: Query
down_m20240115_group_show_messages =
  [sql|
ALTER TABLE group_members DROP COLUMN group_show_messages;
|]

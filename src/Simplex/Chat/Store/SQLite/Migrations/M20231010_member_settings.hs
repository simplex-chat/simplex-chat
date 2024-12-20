{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.SQLite.Migrations.M20231010_member_settings where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20231010_member_settings :: Query
m20231010_member_settings =
  [sql|
ALTER TABLE group_members ADD COLUMN show_messages INTEGER NOT NULL DEFAULT 1;
|]

down_m20231010_member_settings :: Query
down_m20231010_member_settings =
  [sql|
ALTER TABLE group_members DROP COLUMN show_messages;
|]

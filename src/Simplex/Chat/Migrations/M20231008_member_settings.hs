{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Migrations.M20231008_member_settings where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20231008_member_settings :: Query
m20231008_member_settings =
  [sql|
ALTER TABLE group_members ADD COLUMN show_messages INTEGER;
|]

down_m20231008_member_settings :: Query
down_m20231008_member_settings =
  [sql|
ALTER TABLE group_members DROP COLUMN show_messages;
|]

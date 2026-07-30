{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.SQLite.Migrations.M20260727_member_key_sent where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20260727_member_key_sent :: Query
m20260727_member_key_sent =
  [sql|
ALTER TABLE group_members ADD COLUMN user_member_key_status TEXT;
ALTER TABLE group_members ADD COLUMN user_member_key_attempts INTEGER NOT NULL DEFAULT 0;
|]

down_m20260727_member_key_sent :: Query
down_m20260727_member_key_sent =
  [sql|
ALTER TABLE group_members DROP COLUMN user_member_key_status;
ALTER TABLE group_members DROP COLUMN user_member_key_attempts;
|]

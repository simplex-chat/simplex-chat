{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Migrations.M20240104_members_profile_update where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20240104_members_profile_update :: Query
m20240104_members_profile_update =
  [sql|
ALTER TABLE users ADD COLUMN last_profile_update_ts TEXT;
ALTER TABLE group_members ADD COLUMN last_profile_sent_ts TEXT;
|]

down_m20240104_members_profile_update :: Query
down_m20240104_members_profile_update =
  [sql|
ALTER TABLE group_members DROP COLUMN last_profile_sent_ts;
ALTER TABLE users DROP COLUMN last_profile_update_ts;
|]

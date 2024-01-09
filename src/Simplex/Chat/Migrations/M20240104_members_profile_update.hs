{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Migrations.M20240104_members_profile_update where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20240104_members_profile_update :: Query
m20240104_members_profile_update =
  [sql|
ALTER TABLE users ADD COLUMN memberships_profile_update_ts TEXT;
ALTER TABLE groups ADD COLUMN membership_profile_sent_ts TEXT;
|]

down_m20240104_members_profile_update :: Query
down_m20240104_members_profile_update =
  [sql|
ALTER TABLE groups DROP COLUMN membership_profile_sent_ts;
ALTER TABLE users DROP COLUMN memberships_profile_update_ts;
|]

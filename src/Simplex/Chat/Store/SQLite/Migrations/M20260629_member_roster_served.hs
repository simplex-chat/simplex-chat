{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.SQLite.Migrations.M20260629_member_roster_served where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20260629_member_roster_served :: Query
m20260629_member_roster_served =
  [sql|
ALTER TABLE group_members ADD COLUMN roster_served_version INTEGER;
ALTER TABLE groups ADD COLUMN stored_roster_version INTEGER;
|]

down_m20260629_member_roster_served :: Query
down_m20260629_member_roster_served =
  [sql|
ALTER TABLE group_members DROP COLUMN roster_served_version;
ALTER TABLE groups DROP COLUMN stored_roster_version;
|]

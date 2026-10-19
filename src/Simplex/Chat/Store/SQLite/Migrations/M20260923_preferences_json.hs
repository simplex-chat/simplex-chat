{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.SQLite.Migrations.M20260923_preferences_json where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20260923_preferences_json :: Query
m20260923_preferences_json =
  [sql|
ALTER TABLE contact_profiles ADD COLUMN preferences_json TEXT;
ALTER TABLE group_profiles ADD COLUMN preferences_json TEXT;
|]

down_m20260923_preferences_json :: Query
down_m20260923_preferences_json =
  [sql|
ALTER TABLE group_profiles DROP COLUMN preferences_json;
ALTER TABLE contact_profiles DROP COLUMN preferences_json;
|]

{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.SQLite.Migrations.M20250709_profile_short_descr where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20250709_profile_short_descr :: Query
m20250709_profile_short_descr =
  [sql|
ALTER TABLE contact_profiles ADD COLUMN short_descr TEXT;
ALTER TABLE group_profiles ADD COLUMN short_descr TEXT;
|]

down_m20250709_profile_short_descr :: Query
down_m20250709_profile_short_descr =
  [sql|
ALTER TABLE contact_profiles DROP COLUMN short_descr;
ALTER TABLE group_profiles DROP COLUMN short_descr;
|]

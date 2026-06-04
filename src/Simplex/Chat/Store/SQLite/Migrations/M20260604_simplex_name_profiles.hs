{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.SQLite.Migrations.M20260604_simplex_name_profiles where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20260604_simplex_name_profiles :: Query
m20260604_simplex_name_profiles =
  [sql|
ALTER TABLE contact_profiles ADD COLUMN simplex_name TEXT;
ALTER TABLE group_profiles ADD COLUMN simplex_name TEXT;

CREATE UNIQUE INDEX idx_contact_profiles_simplex_name
  ON contact_profiles(user_id, simplex_name)
  WHERE simplex_name IS NOT NULL;

CREATE UNIQUE INDEX idx_group_profiles_simplex_name
  ON group_profiles(user_id, simplex_name)
  WHERE simplex_name IS NOT NULL;
|]

down_m20260604_simplex_name_profiles :: Query
down_m20260604_simplex_name_profiles =
  [sql|
DROP INDEX idx_group_profiles_simplex_name;
DROP INDEX idx_contact_profiles_simplex_name;

ALTER TABLE group_profiles DROP COLUMN simplex_name;
ALTER TABLE contact_profiles DROP COLUMN simplex_name;
|]

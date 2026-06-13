{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.Postgres.Migrations.M20260604_simplex_name_profiles where

import Data.Text (Text)
import Text.RawString.QQ (r)

m20260604_simplex_name_profiles :: Text
m20260604_simplex_name_profiles =
  [r|
ALTER TABLE contact_profiles ADD COLUMN simplex_name TEXT;
ALTER TABLE group_profiles ADD COLUMN simplex_name TEXT;

CREATE UNIQUE INDEX idx_contact_profiles_simplex_name
  ON contact_profiles(user_id, simplex_name)
  WHERE simplex_name IS NOT NULL;

CREATE UNIQUE INDEX idx_group_profiles_simplex_name
  ON group_profiles(user_id, simplex_name)
  WHERE simplex_name IS NOT NULL;
|]

down_m20260604_simplex_name_profiles :: Text
down_m20260604_simplex_name_profiles =
  [r|
DROP INDEX idx_group_profiles_simplex_name;
DROP INDEX idx_contact_profiles_simplex_name;

ALTER TABLE group_profiles DROP COLUMN simplex_name;
ALTER TABLE contact_profiles DROP COLUMN simplex_name;
|]

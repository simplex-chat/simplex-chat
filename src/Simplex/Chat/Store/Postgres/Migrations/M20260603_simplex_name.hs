{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.Postgres.Migrations.M20260603_simplex_name where

import Data.Text (Text)
import Text.RawString.QQ (r)

m20260603_simplex_name :: Text
m20260603_simplex_name =
  [r|
ALTER TABLE contacts ADD COLUMN simplex_name TEXT;
ALTER TABLE groups ADD COLUMN simplex_name TEXT;
ALTER TABLE connections ADD COLUMN simplex_name TEXT;

CREATE UNIQUE INDEX idx_contacts_simplex_name
  ON contacts(user_id, simplex_name)
  WHERE simplex_name IS NOT NULL AND deleted = 0;

CREATE UNIQUE INDEX idx_groups_simplex_name
  ON groups(user_id, simplex_name)
  WHERE simplex_name IS NOT NULL;
|]

down_m20260603_simplex_name :: Text
down_m20260603_simplex_name =
  [r|
DROP INDEX idx_groups_simplex_name;
DROP INDEX idx_contacts_simplex_name;

ALTER TABLE connections DROP COLUMN simplex_name;
ALTER TABLE groups DROP COLUMN simplex_name;
ALTER TABLE contacts DROP COLUMN simplex_name;
|]

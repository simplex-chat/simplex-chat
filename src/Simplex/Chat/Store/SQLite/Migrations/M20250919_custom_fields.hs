{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.SQLite.Migrations.M20250919_custom_fields where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20250919_custom_fields :: Query
m20250919_custom_fields =
  [sql|
ALTER TABLE contacts ADD COLUMN custom_field1 TEXT;
ALTER TABLE groups ADD COLUMN custom_field1 TEXT;

CREATE INDEX idx_contacts_custom_field1 ON contacts(custom_field1);
CREATE INDEX idx_groups_custom_field1 ON groups(custom_field1);
|]

down_m20250919_custom_fields :: Query
down_m20250919_custom_fields =
  [sql|
DROP INDEX idx_contacts_custom_field1;
DROP INDEX idx_groups_custom_field1;

ALTER TABLE contacts DROP COLUMN custom_field1;
ALTER TABLE groups DROP COLUMN custom_field1;
|]

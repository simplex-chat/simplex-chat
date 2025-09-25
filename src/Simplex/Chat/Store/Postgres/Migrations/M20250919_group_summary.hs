{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.Postgres.Migrations.M20250919_group_summary where

import Data.Text (Text)
import qualified Data.Text as T
import Text.RawString.QQ (r)

m20250919_group_summary :: Text
m20250919_group_summary =
  T.pack
    [r|
ALTER TABLE contacts
  ADD COLUMN custom_field1 TEXT,
  ADD COLUMN custom_field2 INTEGER;
CREATE INDEX idx_contacts_custom_field1 ON contacts(custom_field1);
CREATE INDEX idx_contacts_custom_field2 ON contacts(custom_field2);

ALTER TABLE groups
  ADD COLUMN custom_field1 TEXT,
  ADD COLUMN custom_field2 INTEGER;
CREATE INDEX idx_groups_custom_field1 ON groups(custom_field1);
CREATE INDEX idx_groups_custom_field2 ON groups(custom_field2);
|]

down_m20250919_group_summary :: Text
down_m20250919_group_summary =
  T.pack
    [r|
DROP INDEX idx_contacts_custom_field1;
DROP INDEX idx_contacts_custom_field2;
ALTER TABLE contacts
  DROP COLUMN custom_field1,
  DROP COLUMN custom_field2;

DROP INDEX idx_groups_custom_field1;
DROP INDEX idx_groups_custom_field2;
ALTER TABLE groups
  DROP COLUMN custom_field1,
  DROP COLUMN custom_field2;
|]

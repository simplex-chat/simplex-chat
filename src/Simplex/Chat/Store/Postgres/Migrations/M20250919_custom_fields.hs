{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.Postgres.Migrations.M20250919_custom_fields where

import Data.Text (Text)
import qualified Data.Text as T
import Text.RawString.QQ (r)

m20250919_custom_fields :: Text
m20250919_custom_fields =
  T.pack
    [r|
ALTER TABLE contacts ADD COLUMN custom_field1 TEXT;
ALTER TABLE groups ADD COLUMN custom_field1 TEXT;

CREATE INDEX idx_contacts_custom_field1 ON contacts(custom_field1);
CREATE INDEX idx_groups_custom_field1 ON groups(custom_field1);
|]

down_m20250919_custom_fields :: Text
down_m20250919_custom_fields =
  T.pack
    [r|
DROP INDEX idx_contacts_custom_field1;
DROP INDEX idx_groups_custom_field1;

ALTER TABLE contacts DROP COLUMN custom_field1;
ALTER TABLE groups DROP COLUMN custom_field1;
|]

{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.SQLite.Migrations.M20250922_remove_contact_merge where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20250922_remove_contact_merge :: Query
m20250922_remove_contact_merge =
  [sql|
DROP INDEX idx_contacts_via_group;
ALTER TABLE contacts DROP COLUMN via_group;
|]

down_m20250922_remove_contact_merge :: Query
down_m20250922_remove_contact_merge =
  [sql|
ALTER TABLE contacts ADD COLUMN via_group INTEGER REFERENCES groups(group_id) ON DELETE SET NULL;
CREATE INDEX idx_contacts_via_group ON contacts(via_group);
|]

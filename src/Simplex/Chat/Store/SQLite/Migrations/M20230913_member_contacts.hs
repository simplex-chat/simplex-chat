{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.SQLite.Migrations.M20230913_member_contacts where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20230913_member_contacts :: Query
m20230913_member_contacts =
  [sql|
ALTER TABLE contacts ADD COLUMN contact_group_member_id INTEGER
  REFERENCES group_members(group_member_id) ON DELETE SET NULL;

CREATE INDEX idx_contacts_contact_group_member_id ON contacts(contact_group_member_id);

ALTER TABLE contacts ADD COLUMN contact_grp_inv_sent INTEGER NOT NULL DEFAULT 0;
|]

down_m20230913_member_contacts :: Query
down_m20230913_member_contacts =
  [sql|
ALTER TABLE contacts DROP COLUMN contact_grp_inv_sent;

DROP INDEX idx_contacts_contact_group_member_id;

ALTER TABLE contacts DROP COLUMN contact_group_member_id;
|]

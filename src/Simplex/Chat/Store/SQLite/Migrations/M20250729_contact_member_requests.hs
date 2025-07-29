{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.SQLite.Migrations.M20250729_contact_member_requests where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20250729_contact_member_requests :: Query
m20250729_contact_member_requests =
  [sql|
ALTER TABLE contacts ADD COLUMN contact_grp_inv_link BLOB;
ALTER TABLE contacts ADD COLUMN contact_grp_inv_from_group_id INTEGER REFERENCES groups(group_id) ON DELETE SET NULL;
ALTER TABLE contacts ADD COLUMN contact_grp_inv_from_group_member_id INTEGER REFERENCES group_members(group_member_id) ON DELETE SET NULL;

CREATE INDEX idx_contacts_contact_grp_inv_from_group_id ON contacts(contact_grp_inv_from_group_id);
CREATE INDEX idx_contacts_contact_grp_inv_from_group_member_id ON contacts(contact_grp_inv_from_group_member_id);

ALTER TABLE users ADD COLUMN auto_accept_grp_inv_links INTEGER NOT NULL DEFAULT 0;
|]

down_m20250729_contact_member_requests :: Query
down_m20250729_contact_member_requests =
  [sql|
ALTER TABLE users DROP COLUMN auto_accept_grp_inv_links;

DROP INDEX idx_contacts_contact_grp_inv_from_group_id;
DROP INDEX idx_contacts_contact_grp_inv_from_group_member_id;

ALTER TABLE contacts DROP COLUMN contact_grp_inv_link;
ALTER TABLE contacts DROP COLUMN contact_grp_inv_from_group_id;
ALTER TABLE contacts DROP COLUMN contact_grp_inv_from_group_member_id;
|]

{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.SQLite.Migrations.M20250729_member_contact_requests where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20250729_member_contact_requests :: Query
m20250729_member_contact_requests =
  [sql|
ALTER TABLE contacts ADD COLUMN grp_direct_inv_link BLOB;
ALTER TABLE contacts ADD COLUMN grp_direct_inv_from_group_id INTEGER REFERENCES groups(group_id) ON DELETE SET NULL;
ALTER TABLE contacts ADD COLUMN grp_direct_inv_from_group_member_id INTEGER REFERENCES group_members(group_member_id) ON DELETE SET NULL;
ALTER TABLE contacts ADD COLUMN grp_direct_inv_from_member_conn_id INTEGER REFERENCES connections(connection_id) ON DELETE SET NULL;
ALTER TABLE contacts ADD COLUMN grp_direct_inv_started_connection INTEGER NOT NULL DEFAULT 0;

CREATE INDEX idx_contacts_grp_direct_inv_from_group_id ON contacts(grp_direct_inv_from_group_id);
CREATE INDEX idx_contacts_grp_direct_inv_from_group_member_id ON contacts(grp_direct_inv_from_group_member_id);
CREATE INDEX idx_contacts_grp_direct_inv_from_member_conn_id ON contacts(grp_direct_inv_from_member_conn_id);

ALTER TABLE users ADD COLUMN auto_accept_member_contacts INTEGER NOT NULL DEFAULT 0;
|]

down_m20250729_member_contact_requests :: Query
down_m20250729_member_contact_requests =
  [sql|
ALTER TABLE users DROP COLUMN auto_accept_member_contacts;

DROP INDEX idx_contacts_grp_direct_inv_from_group_id;
DROP INDEX idx_contacts_grp_direct_inv_from_group_member_id;
DROP INDEX idx_contacts_grp_direct_inv_from_member_conn_id;

ALTER TABLE contacts DROP COLUMN grp_direct_inv_link;
ALTER TABLE contacts DROP COLUMN grp_direct_inv_from_group_id;
ALTER TABLE contacts DROP COLUMN grp_direct_inv_from_group_member_id;
ALTER TABLE contacts DROP COLUMN grp_direct_inv_from_member_conn_id;
ALTER TABLE contacts DROP COLUMN grp_direct_inv_started_connection;
|]

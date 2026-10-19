{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.Postgres.Migrations.M20250729_member_contact_requests where

import Data.Text (Text)
import qualified Data.Text as T
import Text.RawString.QQ (r)

m20250729_member_contact_requests :: Text
m20250729_member_contact_requests =
  T.pack
    [r|
ALTER TABLE contacts ADD COLUMN grp_direct_inv_link BYTEA;
ALTER TABLE contacts ADD COLUMN grp_direct_inv_from_group_id BIGINT REFERENCES groups(group_id) ON DELETE SET NULL;
ALTER TABLE contacts ADD COLUMN grp_direct_inv_from_group_member_id BIGINT REFERENCES group_members(group_member_id) ON DELETE SET NULL;
ALTER TABLE contacts ADD COLUMN grp_direct_inv_from_member_conn_id BIGINT REFERENCES connections(connection_id) ON DELETE SET NULL;
ALTER TABLE contacts ADD COLUMN grp_direct_inv_started_connection SMALLINT NOT NULL DEFAULT 0;

CREATE INDEX idx_contacts_grp_direct_inv_from_group_id ON contacts(grp_direct_inv_from_group_id);
CREATE INDEX idx_contacts_grp_direct_inv_from_group_member_id ON contacts(grp_direct_inv_from_group_member_id);
CREATE INDEX idx_contacts_grp_direct_inv_from_member_conn_id ON contacts(grp_direct_inv_from_member_conn_id);

ALTER TABLE users ADD COLUMN auto_accept_member_contacts SMALLINT NOT NULL DEFAULT 0;
|]

down_m20250729_member_contact_requests :: Text
down_m20250729_member_contact_requests =
  T.pack
    [r|
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

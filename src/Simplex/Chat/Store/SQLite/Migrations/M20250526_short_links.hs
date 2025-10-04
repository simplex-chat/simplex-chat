{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.SQLite.Migrations.M20250526_short_links where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20250526_short_links :: Query
m20250526_short_links =
  [sql|
ALTER TABLE contacts ADD COLUMN conn_full_link_to_connect BLOB;
ALTER TABLE contacts ADD COLUMN conn_short_link_to_connect BLOB;
ALTER TABLE contacts ADD COLUMN welcome_shared_msg_id BLOB;
ALTER TABLE contacts ADD COLUMN request_shared_msg_id BLOB;

ALTER TABLE contacts ADD COLUMN contact_request_id INTEGER REFERENCES contact_requests ON DELETE SET NULL;
CREATE INDEX idx_contacts_contact_request_id ON contacts(contact_request_id);

ALTER TABLE contact_requests ADD COLUMN business_group_id INTEGER REFERENCES groups(group_id) ON DELETE CASCADE;
CREATE INDEX idx_contact_requests_business_group_id ON contact_requests(business_group_id);
ALTER TABLE contact_requests ADD COLUMN welcome_shared_msg_id BLOB;
ALTER TABLE contact_requests ADD COLUMN request_shared_msg_id BLOB;

ALTER TABLE group_members ADD COLUMN member_xcontact_id BLOB;
ALTER TABLE group_members ADD COLUMN member_welcome_shared_msg_id BLOB;

ALTER TABLE user_contact_links ADD COLUMN short_link_data_set INTEGER NOT NULL DEFAULT 0;
ALTER TABLE user_contact_links ADD COLUMN short_link_large_data_set INTEGER NOT NULL DEFAULT 0;

ALTER TABLE groups ADD COLUMN conn_full_link_to_connect BLOB;
ALTER TABLE groups ADD COLUMN conn_short_link_to_connect BLOB;
ALTER TABLE groups ADD COLUMN conn_link_started_connection INTEGER NOT NULL DEFAULT 0;
ALTER TABLE groups ADD COLUMN welcome_shared_msg_id BLOB;
ALTER TABLE groups ADD COLUMN request_shared_msg_id BLOB;

ALTER TABLE chat_items ADD COLUMN show_group_as_sender INTEGER NOT NULL DEFAULT 0;
|]

down_m20250526_short_links :: Query
down_m20250526_short_links =
  [sql|
ALTER TABLE contacts DROP COLUMN conn_full_link_to_connect;
ALTER TABLE contacts DROP COLUMN conn_short_link_to_connect;
ALTER TABLE contacts DROP COLUMN welcome_shared_msg_id;
ALTER TABLE contacts DROP COLUMN request_shared_msg_id;

DROP INDEX idx_contacts_contact_request_id;
ALTER TABLE contacts DROP COLUMN contact_request_id;

DROP INDEX idx_contact_requests_business_group_id;
ALTER TABLE contact_requests DROP COLUMN business_group_id;
ALTER TABLE contact_requests DROP COLUMN welcome_shared_msg_id;
ALTER TABLE contact_requests DROP COLUMN request_shared_msg_id;

ALTER TABLE group_members DROP COLUMN member_xcontact_id;
ALTER TABLE group_members DROP COLUMN member_welcome_shared_msg_id;

ALTER TABLE user_contact_links DROP COLUMN short_link_data_set;
ALTER TABLE user_contact_links DROP COLUMN short_link_large_data_set;

ALTER TABLE groups DROP COLUMN conn_full_link_to_connect;
ALTER TABLE groups DROP COLUMN conn_short_link_to_connect;
ALTER TABLE groups DROP COLUMN conn_link_started_connection;
ALTER TABLE groups DROP COLUMN welcome_shared_msg_id;
ALTER TABLE groups DROP COLUMN request_shared_msg_id;

ALTER TABLE chat_items DROP COLUMN show_group_as_sender;
|]

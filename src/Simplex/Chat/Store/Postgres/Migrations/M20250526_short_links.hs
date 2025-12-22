{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.Postgres.Migrations.M20250526_short_links where

import Data.Text (Text)
import qualified Data.Text as T
import Text.RawString.QQ (r)

m20250526_short_links :: Text
m20250526_short_links =
  T.pack
    [r|
ALTER TABLE contacts
  ADD COLUMN conn_full_link_to_connect BYTEA,
  ADD COLUMN conn_short_link_to_connect BYTEA,
  ADD COLUMN welcome_shared_msg_id BYTEA,
  ADD COLUMN request_shared_msg_id BYTEA,
  ADD COLUMN contact_request_id BIGINT REFERENCES contact_requests ON DELETE SET NULL;
CREATE INDEX idx_contacts_contact_request_id ON contacts(contact_request_id);

ALTER TABLE contact_requests
  ADD COLUMN business_group_id BIGINT REFERENCES groups(group_id) ON DELETE CASCADE,
  ADD COLUMN welcome_shared_msg_id BYTEA,
  ADD COLUMN request_shared_msg_id BYTEA;
CREATE INDEX idx_contact_requests_business_group_id ON contact_requests(business_group_id);

ALTER TABLE group_members
  ADD COLUMN member_xcontact_id BYTEA,
  ADD COLUMN member_welcome_shared_msg_id BYTEA;

ALTER TABLE user_contact_links
  ADD COLUMN short_link_data_set SMALLINT NOT NULL DEFAULT 0,
  ADD COLUMN short_link_large_data_set SMALLINT NOT NULL DEFAULT 0;

ALTER TABLE groups
  ADD COLUMN conn_full_link_to_connect BYTEA,
  ADD COLUMN conn_short_link_to_connect BYTEA,
  ADD COLUMN conn_link_started_connection SMALLINT NOT NULL DEFAULT 0,
  ADD COLUMN welcome_shared_msg_id BYTEA,
  ADD COLUMN request_shared_msg_id BYTEA;

ALTER TABLE chat_items ADD COLUMN show_group_as_sender SMALLINT NOT NULL DEFAULT 0;
|]

down_m20250526_short_links :: Text
down_m20250526_short_links =
  T.pack
    [r|
DROP INDEX idx_contacts_contact_request_id;

ALTER TABLE contacts
  DROP COLUMN conn_full_link_to_connect,
  DROP COLUMN conn_short_link_to_connect,
  DROP COLUMN welcome_shared_msg_id,
  DROP COLUMN request_shared_msg_id,
  DROP COLUMN contact_request_id;

DROP INDEX idx_contact_requests_business_group_id;

ALTER TABLE contact_requests
  DROP COLUMN business_group_id,
  DROP COLUMN welcome_shared_msg_id,
  DROP COLUMN request_shared_msg_id;

ALTER TABLE group_members
  DROP COLUMN member_xcontact_id,
  DROP COLUMN member_welcome_shared_msg_id;

ALTER TABLE user_contact_links
  DROP COLUMN short_link_data_set,
  DROP COLUMN short_link_large_data_set;

ALTER TABLE groups
  DROP COLUMN conn_full_link_to_connect,
  DROP COLUMN conn_short_link_to_connect,
  DROP COLUMN conn_link_started_connection,
  DROP COLUMN welcome_shared_msg_id,
  DROP COLUMN request_shared_msg_id;

ALTER TABLE chat_items DROP COLUMN show_group_as_sender;
|]

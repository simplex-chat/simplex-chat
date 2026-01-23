{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.SQLite.Migrations.M20260222_chat_relays where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

-- TODO [relays] TBC schema improvement - relay_link is duplicate on group_relays and group_members for owner
-- - chat_relays - user's list of chat relays to choose from (similar to protocol_servers)
-- - users.is_user_chat_relay - indicates that the user can serve as a chat relay
--     (TBC usage, e.g. agree to invitations to be relay)
-- - group_relays - group owner's list of relays for a group
-- - group_relays.relay_link - links for all relays of a group are included in GroupShortLinkData
-- - group_relays.relay_status - group owner's status for each relay (RelayStatus)
-- - group_relays.chat_relay_id - associates group_relays record with a chat_relays record,
--     chat_relays.deleted is to keep associated record if user removes chat relay from configuration,
--     but has group relays using it
-- - group_members.relay_link - relay link, saved on member record for user joining group
-- - groups.relay_own_status - indicates for a relay client that it is chat relay for the group (RelayStatus)
-- - groups.relay_request_* - relay request "work item" fields
m20260222_chat_relays :: Query
m20260222_chat_relays =
  [sql|
CREATE TABLE chat_relays(
  chat_relay_id INTEGER PRIMARY KEY,
  address BLOB NOT NULL,
  name TEXT NOT NULL,
  domains TEXT NOT NULL,
  preset INTEGER NOT NULL DEFAULT 0,
  tested INTEGER,
  enabled INTEGER NOT NULL DEFAULT 1,
  user_id INTEGER NOT NULL REFERENCES users ON DELETE CASCADE,
  deleted INTEGER NOT NULL DEFAULT 0,
  created_at TEXT NOT NULL DEFAULT(datetime('now')),
  updated_at TEXT NOT NULL DEFAULT(datetime('now'))
) STRICT;
CREATE INDEX idx_chat_relays_user_id ON chat_relays(user_id);
CREATE UNIQUE INDEX idx_chat_relays_user_id_address ON chat_relays(user_id, address);
CREATE UNIQUE INDEX idx_chat_relays_user_id_name ON chat_relays(user_id, name);

ALTER TABLE users ADD COLUMN is_user_chat_relay INTEGER NOT NULL DEFAULT 0;

ALTER TABLE groups ADD COLUMN use_relays INTEGER NOT NULL DEFAULT 0;

ALTER TABLE groups ADD COLUMN creating_in_progress INTEGER NOT NULL DEFAULT 0;

ALTER TABLE groups ADD COLUMN relay_own_status TEXT;

ALTER TABLE groups ADD COLUMN relay_request_inv_id BLOB;
ALTER TABLE groups ADD COLUMN relay_request_group_link BLOB;
ALTER TABLE groups ADD COLUMN relay_request_peer_chat_min_version INTEGER;
ALTER TABLE groups ADD COLUMN relay_request_peer_chat_max_version INTEGER;
ALTER TABLE groups ADD COLUMN relay_request_failed INTEGER DEFAULT 0;
ALTER TABLE groups ADD COLUMN relay_request_err_reason TEXT;

ALTER TABLE group_profiles ADD COLUMN group_link BLOB;

CREATE TABLE group_relays(
  group_relay_id INTEGER PRIMARY KEY,
  group_id INTEGER NOT NULL REFERENCES groups ON DELETE CASCADE,
  group_member_id INTEGER NOT NULL REFERENCES group_members ON DELETE CASCADE,
  chat_relay_id INTEGER NOT NULL REFERENCES chat_relays ON DELETE CASCADE,
  relay_status TEXT NOT NULL,
  relay_link BLOB,
  created_at TEXT NOT NULL DEFAULT(datetime('now')),
  updated_at TEXT NOT NULL DEFAULT(datetime('now'))
) STRICT;
CREATE INDEX idx_group_relays_group_id ON group_relays(group_id);
CREATE UNIQUE INDEX idx_group_relays_group_member_id ON group_relays(group_member_id);
CREATE INDEX idx_group_relays_chat_relay_id ON group_relays(chat_relay_id);

ALTER TABLE group_members ADD COLUMN relay_link BLOB;
|]

down_m20260222_chat_relays :: Query
down_m20260222_chat_relays =
  [sql|
ALTER TABLE users DROP COLUMN is_user_chat_relay;

ALTER TABLE groups DROP COLUMN use_relays;

ALTER TABLE groups DROP COLUMN creating_in_progress;

ALTER TABLE groups DROP COLUMN relay_own_status;

ALTER TABLE groups DROP COLUMN relay_request_inv_id;
ALTER TABLE groups DROP COLUMN relay_request_group_link;
ALTER TABLE groups DROP COLUMN relay_request_peer_chat_min_version;
ALTER TABLE groups DROP COLUMN relay_request_peer_chat_max_version;
ALTER TABLE groups DROP COLUMN relay_request_failed;
ALTER TABLE groups DROP COLUMN relay_request_err_reason;

ALTER TABLE group_profiles DROP COLUMN group_link;

DROP INDEX idx_group_relays_group_id;
DROP INDEX idx_group_relays_group_member_id;
DROP INDEX idx_group_relays_chat_relay_id;
DROP TABLE group_relays;

DROP INDEX idx_chat_relays_user_id;
DROP INDEX idx_chat_relays_user_id_address;
DROP INDEX idx_chat_relays_user_id_name;
DROP TABLE chat_relays;

ALTER TABLE group_members DROP COLUMN relay_link;
|]

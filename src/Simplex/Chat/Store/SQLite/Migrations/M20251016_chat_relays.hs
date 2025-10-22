{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.SQLite.Migrations.M20251016_chat_relays where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

-- - chat_relays - user's list of chat relays to choose from (similar to protocol_servers)
-- - users.is_user_chat_relay - indicates that the user can serve as a chat relay
--     (TBC usage, e.g. agree to invitations to be relay)
-- - group_relays - group owner's list of relays for a group
-- - group_relays.relay_link - links for all relays of a group are included in GroupShortLinkData
-- - group_relays.relay_status - group owner's status for each relay (GroupRelayStatus)
-- - group_members.is_chat_relay - indicates that the member is a chat relay (to all group members)
-- - group_members.group_relay_id - associates group_members record with a group_relays record for a group owner;
--     receiving event to member connection, owner can match it to the relay
-- - TBC also inverse link from group_relays to group_members? (group_relays.group_member_id)
-- - groups.relay_own_status - indicates for a relay client that it is chat relay for the group (GroupRelayOwnStatus)
m20251016_chat_relays :: Query
m20251016_chat_relays =
  [sql|
CREATE TABLE chat_relays(
  chat_relay_id INTEGER PRIMARY KEY,
  address TEXT NOT NULL,
  name TEXT NOT NULL,
  domains TEXT NOT NULL,
  preset INTEGER NOT NULL DEFAULT 0,
  tested INTEGER,
  enabled INTEGER NOT NULL DEFAULT 1,
  user_id INTEGER NOT NULL REFERENCES users ON DELETE CASCADE,
  created_at TEXT NOT NULL DEFAULT(datetime('now')),
  updated_at TEXT NOT NULL DEFAULT(datetime('now')),
  UNIQUE(user_id, address),
  UNIQUE(user_id, name)
);
CREATE INDEX idx_chat_relays_user_id ON chat_relays(user_id);

ALTER TABLE users ADD COLUMN is_user_chat_relay INTEGER NOT NULL DEFAULT 0;

CREATE TABLE group_relays(
  group_relay_id INTEGER PRIMARY KEY,
  group_id INTEGER NOT NULL REFERENCES groups ON DELETE CASCADE,
  relay_status TEXT NOT NULL,
  relay_link BLOB
);
CREATE INDEX idx_group_relays_group_id ON group_relays(group_id);

ALTER TABLE group_members ADD COLUMN is_chat_relay INTEGER NOT NULL DEFAULT 0;

ALTER TABLE group_members ADD COLUMN group_relay_id INTEGER REFERENCES group_relays ON DELETE SET NULL;
CREATE INDEX idx_group_members_group_relay_id ON group_members(group_relay_id);

ALTER TABLE groups ADD COLUMN relay_own_status TEXT;

ALTER TABLE connections ADD COLUMN group_member_id_low_priority INTEGER REFERENCES group_members ON DELETE CASCADE;
CREATE INDEX idx_connections_group_member_id_low_priority ON connections(group_member_id_low_priority);
|]

down_m20251016_chat_relays :: Query
down_m20251016_chat_relays =
  [sql|
DROP INDEX idx_chat_relays_user_id;
DROP TABLE chat_relays;

ALTER TABLE users DROP COLUMN is_user_chat_relay;

DROP INDEX idx_group_relays_group_id;
DROP TABLE group_relays;

ALTER TABLE group_members DROP COLUMN is_chat_relay;

DROP INDEX idx_group_members_group_relay_id;
ALTER TABLE group_members DROP COLUMN group_relay_id;

ALTER TABLE groups DROP COLUMN relay_own_status;

DROP INDEX idx_connections_group_member_id_low_priority;
ALTER TABLE connections DROP COLUMN group_member_id_low_priority;
|]

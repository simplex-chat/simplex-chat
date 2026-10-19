{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.SQLite.Migrations.M20231113_group_forward where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20231113_group_forward :: Query
m20231113_group_forward =
  [sql|
ALTER TABLE group_member_intros ADD COLUMN intro_chat_protocol_version INTEGER NOT NULL DEFAULT 3;
CREATE INDEX idx_group_member_intros_re_group_member_id ON group_member_intros(re_group_member_id);

ALTER TABLE group_members ADD COLUMN invited_by_group_member_id INTEGER REFERENCES group_members ON DELETE SET NULL;
ALTER TABLE group_members ADD COLUMN peer_chat_min_version INTEGER NOT NULL DEFAULT 1;
ALTER TABLE group_members ADD COLUMN peer_chat_max_version INTEGER NOT NULL DEFAULT 1;
CREATE INDEX idx_group_members_invited_by_group_member_id ON group_members(invited_by_group_member_id);

UPDATE group_members
SET (peer_chat_min_version, peer_chat_max_version) = (c.peer_chat_min_version, c.peer_chat_max_version)
FROM connections c
WHERE c.group_member_id = group_members.group_member_id;

ALTER TABLE messages ADD COLUMN author_group_member_id INTEGER REFERENCES group_members ON DELETE SET NULL;
ALTER TABLE messages ADD COLUMN forwarded_by_group_member_id INTEGER REFERENCES group_members ON DELETE SET NULL;
CREATE INDEX idx_messages_author_group_member_id ON messages(author_group_member_id);
CREATE INDEX idx_messages_forwarded_by_group_member_id ON messages(forwarded_by_group_member_id);
CREATE INDEX idx_messages_group_id_shared_msg_id ON messages(group_id, shared_msg_id);

ALTER TABLE chat_items ADD COLUMN forwarded_by_group_member_id INTEGER REFERENCES group_members ON DELETE SET NULL;
CREATE INDEX idx_chat_items_forwarded_by_group_member_id ON chat_items(forwarded_by_group_member_id);
|]

down_m20231113_group_forward :: Query
down_m20231113_group_forward =
  [sql|
DROP INDEX idx_chat_items_forwarded_by_group_member_id;
ALTER TABLE chat_items DROP COLUMN forwarded_by_group_member_id;

DROP INDEX idx_messages_group_id_shared_msg_id;
DROP INDEX idx_messages_forwarded_by_group_member_id;
DROP INDEX idx_messages_author_group_member_id;
ALTER TABLE messages DROP COLUMN forwarded_by_group_member_id;
ALTER TABLE messages DROP COLUMN author_group_member_id;

DROP INDEX idx_group_members_invited_by_group_member_id;
ALTER TABLE group_members DROP COLUMN peer_chat_max_version;
ALTER TABLE group_members DROP COLUMN peer_chat_min_version;
ALTER TABLE group_members DROP COLUMN invited_by_group_member_id;

DROP INDEX idx_group_member_intros_re_group_member_id;
ALTER TABLE group_member_intros DROP COLUMN intro_chat_protocol_version;
|]

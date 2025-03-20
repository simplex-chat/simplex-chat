{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.SQLite.Migrations.M20230511_reactions where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20230511_reactions :: Query
m20230511_reactions =
  [sql|
CREATE TABLE chat_item_reactions (
  chat_item_reaction_id INTEGER PRIMARY KEY AUTOINCREMENT,
  item_member_id BLOB, -- member that created item, NULL for items in direct chats
  shared_msg_id BLOB NOT NULL,
  contact_id INTEGER REFERENCES contacts ON DELETE CASCADE,
  group_id INTEGER REFERENCES groups ON DELETE CASCADE,
  group_member_id INTEGER REFERENCES group_members ON DELETE SET NULL, -- member that sent reaction, NULL for items in direct chats
  created_by_msg_id INTEGER REFERENCES messages(message_id) ON DELETE SET NULL,
  reaction TEXT NOT NULL, -- JSON of MsgReaction
  reaction_sent INTEGER NOT NULL, -- 0 for received, 1 for sent
  reaction_ts TEXT NOT NULL, -- broker_ts of creating message for received, created_at for sent
  created_at TEXT NOT NULL DEFAULT(datetime('now')),
  updated_at TEXT NOT NULL DEFAULT(datetime('now'))
);

CREATE INDEX idx_chat_item_reactions_shared_msg_id ON chat_item_reactions(shared_msg_id);
CREATE INDEX idx_chat_item_reactions_contact_id ON chat_item_reactions(contact_id);
CREATE INDEX idx_chat_item_reactions_group_id ON chat_item_reactions(group_id);
CREATE INDEX idx_chat_item_reactions_group_member_id ON chat_item_reactions(group_member_id);

CREATE INDEX idx_chat_item_reactions_contact ON chat_item_reactions(contact_id, shared_msg_id);
CREATE INDEX idx_chat_item_reactions_group ON chat_item_reactions(group_id, shared_msg_id);
|]

down_m20230511_reactions :: Query
down_m20230511_reactions =
  [sql|
DROP INDEX idx_chat_item_reactions_group;
DROP INDEX idx_chat_item_reactions_contact;

DROP INDEX idx_chat_item_reactions_group_member_id;
DROP INDEX idx_chat_item_reactions_group_id;
DROP INDEX idx_chat_item_reactions_contact_id;
DROP INDEX idx_chat_item_reactions_shared_msg_id;

DROP TABLE chat_item_reactions;
|]

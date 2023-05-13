{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Migrations.M20230511_reactions where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20230511_reactions :: Query
m20230511_reactions =
  [sql|
CREATE TABLE chat_item_reactions (
  chat_item_reaction_id INTEGER PRIMARY KEY AUTOINCREMENT,
  shared_msg_id INTEGER NOT NULL,
  contact_id INTEGER REFERENCES contacts ON DELETE CASCADE,
  group_id INTEGER REFERENCES groups ON DELETE CASCADE,
  group_member_id INTEGER REFERENCES group_members ON DELETE SET NULL, -- NULL for sent even if group_id is not
  created_by_msg_id INTEGER UNIQUE REFERENCES messages(message_id) ON DELETE SET NULL,
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
|]

down_m20230511_reactions :: Query
down_m20230511_reactions =
  [sql|
DROP INDEX idx_chat_item_reactions_group_member_id;
DROP INDEX idx_chat_item_reactions_group_id;
DROP INDEX idx_chat_item_reactions_contact_id;
DROP INDEX idx_chat_item_reactions_shared_msg_id;

DROP TABLE chat_item_reactions;
|]

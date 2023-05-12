{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Migrations.M20230511_reactions where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20230511_reactions :: Query
m20230511_reactions =
  [sql|
CREATE TABLE chat_item_reactions (
  chat_item_reaction_id INTEGER PRIMARY KEY AUTOINCREMENT,
  chat_item_id INTEGER NOT NULL REFERENCES chat_items ON DELETE CASCADE,
  group_member_id INTEGER REFERENCES group_members ON DELETE SET NULL, -- NULL for sent even if group_id is not
  created_by_msg_id INTEGER UNIQUE REFERENCES messages(message_id) ON DELETE SET NULL,
  reaction_sent INTEGER NOT NULL, -- 0 for received, 1 for sent
  reaction_ts TEXT NOT NULL, -- broker_ts of creating message for received, created_at for sent
  reaction_text TEXT NOT NULL,
  created_at TEXT NOT NULL DEFAULT(datetime('now')),
  updated_at TEXT NOT NULL DEFAULT(datetime('now'))
);

CREATE INDEX idx_chat_item_reactions_chat_item_id ON chat_item_reactions(chat_item_id);
|]

down_m20230511_reactions :: Query
down_m20230511_reactions =
  [sql|
DROP INDEX idx_chat_item_reactions_chat_item_id;

DROP TABLE chat_item_reactions;
|]

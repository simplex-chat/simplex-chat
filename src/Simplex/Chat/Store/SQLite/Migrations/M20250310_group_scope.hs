{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.SQLite.Migrations.M20250310_group_scope where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

-- TODO [knocking] TBC schema
-- TODO            - chat_items.group_scope possibly not needed
-- TODO            - group_conversations.unread - don't persist, calculate on the fly?
m20250310_group_scope :: Query
m20250310_group_scope =
  [sql|
CREATE TABLE group_conversations (
  group_conversation_id INTEGER PRIMARY KEY AUTOINCREMENT,
  user_id INTEGER NOT NULL REFERENCES users ON DELETE CASCADE,
  group_id INTEGER NOT NULL REFERENCES groups ON DELETE CASCADE,
  conversation_tag TEXT NOT NULL,
  group_member_id INTEGER REFERENCES group_members ON DELETE CASCADE,
  chat_ts TEXT NOT NULL DEFAULT(datetime('now')),
  unread INTEGER NOT NULL DEFAULT 0,
  archived INTEGER NOT NULL DEFAULT 0,
  created_at TEXT NOT NULL DEFAULT(datetime('now')),
  updated_at TEXT NOT NULL DEFAULT(datetime('now'))
);

CREATE INDEX idx_group_conversations_user_id ON group_conversations(user_id);
CREATE INDEX idx_group_conversations_group_id ON group_conversations(group_id);
CREATE INDEX idx_group_conversations_group_member_id ON group_conversations(group_member_id);

ALTER TABLE chat_items ADD COLUMN group_scope TEXT;
ALTER TABLE chat_items ADD COLUMN group_conversation_id INTEGER REFERENCES group_conversations ON DELETE CASCADE;

CREATE INDEX idx_chat_items_group_conversation_id ON chat_items(group_conversation_id);
|]

down_m20250310_group_scope :: Query
down_m20250310_group_scope =
  [sql|
DROP INDEX idx_chat_items_group_conversation_id;

ALTER TABLE chat_items DROP COLUMN group_scope;
ALTER TABLE chat_items DROP COLUMN group_conversation_id;

DROP INDEX idx_group_conversations_user_id;
DROP INDEX idx_group_conversations_group_id;
DROP INDEX idx_group_conversations_group_member_id;

DROP TABLE group_conversations;
|]

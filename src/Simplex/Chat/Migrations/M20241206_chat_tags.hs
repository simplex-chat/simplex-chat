{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Migrations.M20241206_chat_tags where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20241206_chat_tags :: Query
m20241206_chat_tags =
  [sql|
CREATE TABLE chat_tags (
  chat_tag_id INTEGER PRIMARY KEY AUTOINCREMENT,
  user_id INTEGER REFERENCES users,
  chat_tag_text TEXT UNIQUE NOT NULL,
  chat_tag_emoji TEXT UNIQUE NOT NULL
);

CREATE TABLE chat_tags_chats (
  contact_id INTEGER REFERENCES contacts ON DELETE CASCADE, -- NULL for groups
  group_id INTEGER REFERENCES groups ON DELETE CASCADE, -- NULL for contacts
  chat_tag_id INTEGER NOT NULL REFERENCES chat_tags ON DELETE CASCADE,
  UNIQUE(chat_tag_id, group_id),
  UNIQUE(chat_tag_id, contact_id)
);

CREATE INDEX idx_chat_tags_user_id ON chat_tags_chat(user_id);
|]

down_m20241206_chat_tags :: Query
down_m20241206_chat_tags =
  [sql|
DROP INDEX idx_chat_tags_user_id;
DROP INDEX idx_chat_tags_user_id_contact_id;
DROP INDEX idx_chat_tags_group_id_contact_id;

DROP TABLE chat_tags_chats
DROP TABLE chat_tags;
|]
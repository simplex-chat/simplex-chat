{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.SQLite.Migrations.M20241223_chat_tags where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20241223_chat_tags :: Query
m20241223_chat_tags =
  [sql|
CREATE TABLE chat_tags (
  chat_tag_id INTEGER PRIMARY KEY AUTOINCREMENT,
  user_id INTEGER REFERENCES users,
  chat_tag_text TEXT NOT NULL,
  chat_tag_emoji TEXT,
  tag_order INTEGER NOT NULL
);

CREATE TABLE chat_tags_chats (
  contact_id INTEGER REFERENCES contacts ON DELETE CASCADE,
  group_id INTEGER REFERENCES groups ON DELETE CASCADE,
  chat_tag_id INTEGER NOT NULL REFERENCES chat_tags ON DELETE CASCADE
);

CREATE INDEX idx_chat_tags_user_id ON chat_tags(user_id);
CREATE UNIQUE INDEX idx_chat_tags_user_id_chat_tag_text ON chat_tags(user_id, chat_tag_text);
CREATE UNIQUE INDEX idx_chat_tags_user_id_chat_tag_emoji ON chat_tags(user_id, chat_tag_emoji);

CREATE INDEX idx_chat_tags_chats_chat_tag_id ON chat_tags_chats(chat_tag_id);
CREATE UNIQUE INDEX idx_chat_tags_chats_chat_tag_id_contact_id ON chat_tags_chats(contact_id, chat_tag_id);
CREATE UNIQUE INDEX idx_chat_tags_chats_chat_tag_id_group_id ON chat_tags_chats(group_id, chat_tag_id);
|]

down_m20241223_chat_tags :: Query
down_m20241223_chat_tags =
  [sql|
DROP INDEX idx_chat_tags_user_id;
DROP INDEX idx_chat_tags_user_id_chat_tag_text;
DROP INDEX idx_chat_tags_user_id_chat_tag_emoji;

DROP INDEX idx_chat_tags_chats_chat_tag_id;
DROP INDEX idx_chat_tags_chats_chat_tag_id_contact_id;
DROP INDEX idx_chat_tags_chats_chat_tag_id_group_id;

DROP TABLE chat_tags_chats;
DROP TABLE chat_tags;
|]

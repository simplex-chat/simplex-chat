{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Migrations.M20240102_note_folders where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20240102_note_folders :: Query
m20240102_note_folders =
  [sql|
CREATE TABLE note_folders (
  note_folder_id INTEGER PRIMARY KEY AUTOINCREMENT,
  user_id INTEGER NOT NULL REFERENCES users ON DELETE CASCADE,
  created_at TEXT NOT NULL DEFAULT(datetime('now')),
  updated_at TEXT NOT NULL DEFAULT(datetime('now')),
  chat_ts TEXT NOT NULL DEFAULT(datetime('now')),
  favorite INTEGER NOT NULL DEFAULT 0,
  unread_chat INTEGER NOT NULL DEFAULT 0
);

ALTER TABLE chat_items ADD COLUMN note_folder_id INTEGER DEFAULT NULL REFERENCES note_folders ON DELETE CASCADE;
ALTER TABLE files ADD COLUMN note_folder_id INTEGER DEFAULT NULL REFERENCES note_folders ON DELETE CASCADE;

CREATE INDEX chat_items_note_folder_id ON chat_items(note_folder_id);
CREATE INDEX files_note_folder_id ON files(note_folder_id);
CREATE INDEX note_folders_user_id ON note_folders(user_id);

INSERT INTO note_folders (user_id) SELECT user_id FROM users;
|]

down_m20240102_note_folders :: Query
down_m20240102_note_folders =
  [sql|
DROP INDEX chat_items_note_folder_id;
DROP INDEX files_note_folder_id;
DROP INDEX note_folders_user_id;

ALTER TABLE chat_items DROP COLUMN note_folder_id;
ALTER TABLE files DROP COLUMN note_folder_id;

DROP TABLE note_folders;
|]

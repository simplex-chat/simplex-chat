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
      created_at TEXT NOT NULL,
      updated_at TEXT NOT NULL,
      chat_ts TEXT NOT NULL,
      favorite INTEGER NOT NULL DEFAULT 0,
      unread_chat INTEGER DEFAULT 0 NOT NULL
    );

    ALTER TABLE chat_items ADD COLUMN note_folder_id INTEGER DEFAULT NULL REFERENCES note_folders ON DELETE CASCADE;
    ALTER TABLE files ADD COLUMN note_folder_id INTEGER DEFAULT NULL REFERENCES note_folders ON DELETE CASCADE;

    INSERT INTO note_folders
      SELECT
        NULL as note_folder_id,
        u.user_id as user_id,
        datetime('now') as created_at,
        datetime('now') as updated_at,
        datetime('now') as chat_ts,
        0 as favorite,
        0 as unread_chat
      FROM users u;
|]

down_m20240102_note_folders :: Query
down_m20240102_note_folders =
  [sql|
DROP TABLE note_folders;
ALTER TABLE chat_items DROP COLUMN note_folder_id;
ALTER TABLE files DROP COLUMN note_folder_id;
|]

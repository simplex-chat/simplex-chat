{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Migrations.M20231219_notes_folders where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20231219_notes_folders :: Query
m20231219_notes_folders =
  [sql|
    CREATE TABLE notes_folders (
      notes_folder_id INTEGER PRIMARY KEY AUTOINCREMENT,
      user_id INTEGER NOT NULL REFERENCES users ON DELETE CASCADE,
      local_display_name TEXT NOT NULL,
      created_at TEXT NOT NULL,
      updated_at TEXT NOT NULL,
      chat_ts TEXT,
      favorite INTEGER NOT NULL DEFAULT 0,
      unread_chat INTEGER DEFAULT 0 NOT NULL,
      chat_item_id INTEGER DEFAULT NULL REFERENCES chat_items ON DELETE SET NULL,
      FOREIGN KEY (user_id, local_display_name)
        REFERENCES display_names (user_id, local_display_name)
        ON DELETE CASCADE
        ON UPDATE CASCADE,
    );

    CREATE UNIQUE INDEX idx_notes_user_local_display_name ON notes_fodler (
      user_id,
      local_display_name
    );

    ALTER TABLE chat_items ADD COLUMN notes_folder_id INTEGER DEFAULT NULL REFERENCES notes_folders ON DELETE CASCADE;
|]

down_m20231219_notes_folders :: Query
down_m20231219_notes_folders =
  [sql|
DROP INDEX idx_notes_user_local_display_name;
DROP TABLE notes_folders;
ALTER TABLE chat_items DROP COLUMN notes_folder_id;
|]

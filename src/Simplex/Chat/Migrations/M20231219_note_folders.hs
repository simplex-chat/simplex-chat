{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Migrations.M20231219_note_folders where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20231219_note_folders :: Query
m20231219_note_folders =
  [sql|
    CREATE TABLE note_folders (
      note_folder_id INTEGER PRIMARY KEY AUTOINCREMENT,
      user_id INTEGER NOT NULL REFERENCES users ON DELETE CASCADE,
      display_name TEXT NOT NULL,
      local_display_name TEXT NOT NULL,
      created_at TEXT NOT NULL,
      updated_at TEXT NOT NULL,
      chat_ts TEXT NOT NULL,
      favorite INTEGER NOT NULL DEFAULT 0,
      unread_chat INTEGER DEFAULT 0 NOT NULL,
      FOREIGN KEY (user_id, local_display_name)
        REFERENCES display_names (user_id, local_display_name)
        ON DELETE CASCADE
        ON UPDATE CASCADE
    );

    CREATE UNIQUE INDEX idx_note_folders_user_id_local_display_name ON note_folders (
      user_id,
      local_display_name
    );

    ALTER TABLE chat_items ADD COLUMN note_folder_id INTEGER DEFAULT NULL REFERENCES note_folders ON DELETE CASCADE;
    ALTER TABLE chat_item_reactions ADD COLUMN note_folder_id INTEGER DEFAULT NULL REFERENCES note_folders ON DELETE CASCADE;
|]

down_m20231219_note_folders :: Query
down_m20231219_note_folders =
  [sql|
DROP INDEX idx_note_folders_user_id_local_display_name;
DROP TABLE note_folders;
ALTER TABLE chat_items DROP COLUMN note_folder_id;
ALTER TABLE chat_item_reactions DROP COLUMN note_folder_id;
|]

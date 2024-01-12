{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Migrations.M20240112_indexes where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20240112_indexes :: Query
m20240112_indexes =
  [sql|
DROP INDEX idx_chat_items_contacts;
CREATE INDEX idx_chat_items_contacts ON chat_items(user_id, contact_id, created_at);

CREATE INDEX idx_chat_items_note_folders ON chat_items(user_id, note_folder_id, created_at, chat_item_id);
|]

down_m20240112_indexes :: Query
down_m20240112_indexes =
  [sql|
DROP INDEX idx_chat_items_note_folders;

DROP INDEX idx_chat_items_contacts;
CREATE INDEX idx_chat_items_contacts ON chat_items(user_id, contact_id, chat_item_id);
|]

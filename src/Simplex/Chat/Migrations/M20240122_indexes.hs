{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Migrations.M20240122_indexes where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20240122_indexes :: Query
m20240122_indexes =
  [sql|
CREATE INDEX idx_chat_items_contacts_created_at on chat_items (user_id, contact_id, created_at);
CREATE INDEX idx_chat_items_contacts_item_status on chat_items (user_id, contact_id, item_status);
CREATE INDEX idx_chat_items_groups_item_status on chat_items (user_id, group_id, item_status);
CREATE INDEX idx_chat_items_notes_created_at on chat_items (user_id, note_folder_id, created_at);
CREATE INDEX idx_chat_items_notes_item_status on chat_items (user_id, note_folder_id, item_status);
|]

down_m20240122_indexes :: Query
down_m20240122_indexes =
  [sql|
DROP INDEX idx_chat_items_contacts_created_at;
DROP INDEX idx_chat_items_contacts_item_status;
DROP INDEX idx_chat_items_groups_item_status;
DROP INDEX idx_chat_items_notes_created_at;
DROP INDEX idx_chat_items_notes_item_status;
|]

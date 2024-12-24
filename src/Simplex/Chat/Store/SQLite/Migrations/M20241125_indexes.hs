{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.SQLite.Migrations.M20241125_indexes where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20241125_indexes :: Query
m20241125_indexes =
  [sql|
-- contacts
DROP INDEX idx_chat_items_contacts;
DROP INDEX idx_chat_items_contacts_item_status;

CREATE INDEX idx_chat_items_contacts ON chat_items(user_id, contact_id, item_status, created_at);

-- groups
DROP INDEX idx_chat_items_groups;
DROP INDEX idx_chat_items_groups_item_status;

CREATE INDEX idx_chat_items_groups ON chat_items(user_id, group_id, item_status, item_ts);
CREATE INDEX idx_chat_items_groups_item_ts ON chat_items(user_id, group_id, item_ts);

-- notes
DROP INDEX idx_chat_items_notes_item_status;

CREATE INDEX idx_chat_items_notes ON chat_items(user_id, note_folder_id, item_status, created_at);
|]

down_m20241125_indexes :: Query
down_m20241125_indexes =
  [sql|
-- contacts
DROP INDEX idx_chat_items_contacts;

CREATE INDEX idx_chat_items_contacts ON chat_items(user_id, contact_id, chat_item_id);
CREATE INDEX idx_chat_items_contacts_item_status on chat_items (user_id, contact_id, item_status);

-- groups
DROP INDEX idx_chat_items_groups;
DROP INDEX idx_chat_items_groups_item_ts;

CREATE INDEX idx_chat_items_groups ON chat_items(user_id, group_id, item_ts, chat_item_id);
CREATE INDEX idx_chat_items_groups_item_status on chat_items (user_id, group_id, item_status);

-- notes
DROP INDEX idx_chat_items_notes;

CREATE INDEX idx_chat_items_notes_item_status on chat_items (user_id, note_folder_id, item_status);
|]

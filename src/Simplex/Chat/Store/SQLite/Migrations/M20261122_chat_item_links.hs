{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.SQLite.Migrations.M20261122_chat_item_links where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20261122_chat_item_links :: Query
m20261122_chat_item_links =
  [sql|
CREATE TABLE chat_item_links(
  chat_item_link_id INTEGER PRIMARY KEY AUTOINCREMENT,
  chat_item_id INTEGER NOT NULL REFERENCES chat_items ON DELETE CASCADE,
  label TEXT NOT NULL,
  link_text TEXT NOT NULL,
  target_contact_id INTEGER REFERENCES contacts ON DELETE SET NULL,
  target_group_id INTEGER REFERENCES groups ON DELETE SET NULL,
  target_note_folder_id INTEGER REFERENCES note_folders ON DELETE SET NULL,
  target_chat_item_id INTEGER REFERENCES chat_items ON DELETE SET NULL
) STRICT;

CREATE UNIQUE INDEX idx_chat_item_links_chat_item_id_label ON chat_item_links(chat_item_id, label);
CREATE INDEX idx_chat_item_links_target_contact_id ON chat_item_links(target_contact_id);
CREATE INDEX idx_chat_item_links_target_group_id ON chat_item_links(target_group_id);
CREATE INDEX idx_chat_item_links_target_note_folder_id ON chat_item_links(target_note_folder_id);
CREATE INDEX idx_chat_item_links_target_chat_item_id ON chat_item_links(target_chat_item_id);
|]

down_m20261122_chat_item_links :: Query
down_m20261122_chat_item_links =
  [sql|
DROP INDEX idx_chat_item_links_target_chat_item_id;
DROP INDEX idx_chat_item_links_target_note_folder_id;
DROP INDEX idx_chat_item_links_target_group_id;
DROP INDEX idx_chat_item_links_target_contact_id;
DROP INDEX idx_chat_item_links_chat_item_id_label;
DROP TABLE chat_item_links;
|]

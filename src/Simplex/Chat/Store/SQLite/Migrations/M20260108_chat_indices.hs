{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.SQLite.Migrations.M20260108_chat_indices where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20260108_chat_indices :: Query
m20260108_chat_indices =
  [sql|
CREATE INDEX idx_chat_items_contacts_msg_content_tag_created_at ON chat_items(
  user_id,
  contact_id,
  msg_content_tag,
  created_at
);

CREATE INDEX idx_chat_items_note_folder_msg_content_tag_created_at ON chat_items(
  user_id,
  note_folder_id,
  msg_content_tag,
  created_at
);
|]

down_m20260108_chat_indices :: Query
down_m20260108_chat_indices =
  [sql|
DROP INDEX idx_chat_items_contacts_msg_content_tag_created_at;

DROP INDEX idx_chat_items_note_folder_msg_content_tag_created_at;
|]

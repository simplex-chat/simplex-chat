{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.SQLite.Migrations.M20260407_channel_comments where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20260407_channel_comments :: Query
m20260407_channel_comments =
  [sql|
ALTER TABLE chat_items ADD COLUMN parent_chat_item_id INTEGER REFERENCES chat_items ON DELETE CASCADE;
ALTER TABLE chat_items ADD COLUMN comments_total INTEGER NOT NULL DEFAULT 0;
ALTER TABLE chat_items ADD COLUMN comments_disabled INTEGER NOT NULL DEFAULT 0;

CREATE INDEX idx_chat_items_parent_chat_item_id ON chat_items(parent_chat_item_id);
CREATE INDEX idx_chat_items_parent_item_ts ON chat_items(user_id, group_id, parent_chat_item_id, item_ts);
|]

down_m20260407_channel_comments :: Query
down_m20260407_channel_comments =
  [sql|
DROP INDEX idx_chat_items_parent_chat_item_id;
DROP INDEX idx_chat_items_parent_item_ts;
ALTER TABLE chat_items DROP COLUMN parent_chat_item_id;
ALTER TABLE chat_items DROP COLUMN comments_total;
ALTER TABLE chat_items DROP COLUMN comments_disabled;

UPDATE group_members SET member_role = 'observer' WHERE member_role = 'commenter';
|]

{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.SQLite.Migrations.M20250122_chat_items_include_in_history where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20250122_chat_items_include_in_history :: Query
m20250122_chat_items_include_in_history =
  [sql|
ALTER TABLE chat_items ADD COLUMN include_in_history INTEGER NOT NULL DEFAULT 0;

CREATE INDEX idx_chat_items_groups_history ON chat_items(
  user_id,
  group_id,
  include_in_history,
  item_deleted,
  item_ts,
  chat_item_id
);

UPDATE chat_items
SET include_in_history = 1
WHERE group_id IS NOT NULL
  AND item_content_tag IN ('rcvMsgContent', 'sndMsgContent')
  AND msg_content_tag NOT IN ('report');

CREATE INDEX idx_group_snd_item_statuses_chat_item_id_group_member_id ON group_snd_item_statuses(chat_item_id, group_member_id);
|]

down_m20250122_chat_items_include_in_history :: Query
down_m20250122_chat_items_include_in_history =
  [sql|
DROP INDEX idx_group_snd_item_statuses_chat_item_id_group_member_id;

DROP INDEX idx_chat_items_groups_history;

ALTER TABLE chat_items DROP COLUMN include_in_history;
|]

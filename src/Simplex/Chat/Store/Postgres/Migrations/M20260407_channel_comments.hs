{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.Postgres.Migrations.M20260407_channel_comments where

import Data.Text (Text)
import Text.RawString.QQ (r)

m20260407_channel_comments :: Text
m20260407_channel_comments =
  [r|
ALTER TABLE chat_items ADD COLUMN parent_chat_item_id BIGINT REFERENCES chat_items ON DELETE CASCADE;
ALTER TABLE chat_items ADD COLUMN comments_total INTEGER NOT NULL DEFAULT 0;
ALTER TABLE chat_items ADD COLUMN comments_disabled SMALLINT NOT NULL DEFAULT 0;

CREATE INDEX idx_chat_items_parent_chat_item_id ON chat_items(parent_chat_item_id);
CREATE INDEX idx_chat_items_parent_item_ts ON chat_items(user_id, group_id, parent_chat_item_id, item_ts);
|]

down_m20260407_channel_comments :: Text
down_m20260407_channel_comments =
  [r|
DROP INDEX idx_chat_items_parent_chat_item_id;
DROP INDEX idx_chat_items_parent_item_ts;
ALTER TABLE chat_items DROP COLUMN parent_chat_item_id;
ALTER TABLE chat_items DROP COLUMN comments_total;
ALTER TABLE chat_items DROP COLUMN comments_disabled;

UPDATE group_members SET member_role = 'observer' WHERE member_role = 'commenter';
|]

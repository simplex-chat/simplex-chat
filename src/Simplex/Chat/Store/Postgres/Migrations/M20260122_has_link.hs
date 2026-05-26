{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.Postgres.Migrations.M20260122_has_link where

import Data.Text (Text)
import Text.RawString.QQ (r)

m20260122_has_link :: Text
m20260122_has_link =
  [r|
ALTER TABLE chat_items ADD COLUMN has_link SMALLINT NOT NULL DEFAULT 0;

UPDATE chat_items SET msg_content_tag = 'text' WHERE msg_content_tag = 'liveText';

UPDATE chat_items SET has_link = 1
WHERE msg_content_tag = 'link' OR item_text LIKE '%https://%';

CREATE INDEX idx_chat_items_groups_has_link_item_ts ON chat_items(user_id, group_id, has_link, item_ts);
CREATE INDEX idx_chat_items_contacts_has_link_created_at ON chat_items(user_id, contact_id, has_link, created_at);
CREATE INDEX idx_chat_items_note_folder_has_link_created_at ON chat_items(user_id, note_folder_id, has_link, created_at);
|]

down_m20260122_has_link :: Text
down_m20260122_has_link =
  [r|
DROP INDEX idx_chat_items_note_folder_has_link_created_at;
DROP INDEX idx_chat_items_contacts_has_link_created_at;
DROP INDEX idx_chat_items_groups_has_link_item_ts;

ALTER TABLE chat_items DROP COLUMN has_link;
|]

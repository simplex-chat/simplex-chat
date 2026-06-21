{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.Postgres.Migrations.M20260403_item_viewed where

import Data.Text (Text)
import Text.RawString.QQ (r)

m20260403_item_viewed :: Text
m20260403_item_viewed =
  [r|
ALTER TABLE chat_items ADD COLUMN item_viewed SMALLINT NOT NULL DEFAULT 0;
CREATE INDEX idx_chat_items_contacts_item_viewed ON chat_items(user_id, contact_id, item_viewed, created_at);
CREATE INDEX idx_chat_items_groups_item_viewed ON chat_items(user_id, group_id, item_viewed, item_ts);
|]

down_m20260403_item_viewed :: Text
down_m20260403_item_viewed =
  [r|
DROP INDEX idx_chat_items_contacts_item_viewed;
DROP INDEX idx_chat_items_groups_item_viewed;
ALTER TABLE chat_items DROP COLUMN item_viewed;
|]

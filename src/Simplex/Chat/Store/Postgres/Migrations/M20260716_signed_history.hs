{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.Postgres.Migrations.M20260716_signed_history where

import Data.Text (Text)
import Text.RawString.QQ (r)

m20260716_signed_history :: Text
m20260716_signed_history =
  [r|
ALTER TABLE chat_items ADD COLUMN item_msg_body BYTEA;
ALTER TABLE chat_items ADD COLUMN item_chat_binding TEXT;
ALTER TABLE chat_items ADD COLUMN item_signatures BYTEA;
ALTER TABLE chat_items ADD COLUMN item_signed_by_group_member_id BIGINT REFERENCES group_members ON DELETE SET NULL;

CREATE INDEX idx_chat_items_item_signed_by_group_member_id ON chat_items(item_signed_by_group_member_id);
|]

down_m20260716_signed_history :: Text
down_m20260716_signed_history =
  [r|
DROP INDEX idx_chat_items_item_signed_by_group_member_id;

ALTER TABLE chat_items DROP COLUMN item_msg_body;
ALTER TABLE chat_items DROP COLUMN item_chat_binding;
ALTER TABLE chat_items DROP COLUMN item_signatures;
ALTER TABLE chat_items DROP COLUMN item_signed_by_group_member_id;
|]

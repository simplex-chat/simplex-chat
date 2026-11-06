{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.Postgres.Migrations.M20261101_chat_items_unread_index where

import Data.Text (Text)
import Text.RawString.QQ (r)

m20261101_chat_items_unread_index :: Text
m20261101_chat_items_unread_index =
  [r|
DROP INDEX idx_chat_items_user_id_item_status;
CREATE INDEX idx_chat_items_user_id_item_status ON chat_items(user_id, item_status, contact_id, group_id, user_mention);
|]

down_m20261101_chat_items_unread_index :: Text
down_m20261101_chat_items_unread_index =
  [r|
DROP INDEX idx_chat_items_user_id_item_status;
CREATE INDEX idx_chat_items_user_id_item_status ON chat_items(user_id, item_status);
|]

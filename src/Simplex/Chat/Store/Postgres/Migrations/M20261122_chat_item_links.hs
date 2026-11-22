{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.Postgres.Migrations.M20261122_chat_item_links where

import Data.Text (Text)
import Text.RawString.QQ (r)

m20261122_chat_item_links :: Text
m20261122_chat_item_links =
  [r|
CREATE TABLE chat_item_links(
  chat_item_link_id BIGINT GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
  chat_item_id BIGINT NOT NULL REFERENCES chat_items ON DELETE CASCADE,
  label TEXT NOT NULL,
  link_text TEXT NOT NULL,
  target_chat_item_id BIGINT REFERENCES chat_items ON DELETE SET NULL
);

CREATE UNIQUE INDEX idx_chat_item_links_chat_item_id_label ON chat_item_links(chat_item_id, label);
CREATE INDEX idx_chat_item_links_target_chat_item_id ON chat_item_links(target_chat_item_id);
|]

down_m20261122_chat_item_links :: Text
down_m20261122_chat_item_links =
  [r|
DROP INDEX idx_chat_item_links_target_chat_item_id;
DROP INDEX idx_chat_item_links_chat_item_id_label;
DROP TABLE chat_item_links;
|]

{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Migrations.M20231214_item_content_tag where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20231214_item_content_tag :: Query
m20231214_item_content_tag =
  [sql|
ALTER TABLE chat_items ADD COLUMN item_content_tag TEXT;
|]

down_m20231214_item_content_tag :: Query
down_m20231214_item_content_tag =
  [sql|
ALTER TABLE chat_items DROP COLUMN item_content_tag;
|]

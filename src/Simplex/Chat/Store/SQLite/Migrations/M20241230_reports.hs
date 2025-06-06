{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.SQLite.Migrations.M20241230_reports where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20241230_reports :: Query
m20241230_reports =
  [sql|
ALTER TABLE chat_items ADD COLUMN msg_content_tag TEXT;
|]

down_m20241230_reports :: Query
down_m20241230_reports =
  [sql|
ALTER TABLE chat_items DROP COLUMN msg_content_tag;
|]

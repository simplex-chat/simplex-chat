{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.SQLite.Migrations.M20260121_msg_content_tag where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20260121_msg_content_tag :: Query
m20260121_msg_content_tag =
  [sql|
UPDATE chat_items SET msg_content_tag = CAST(msg_content_tag as TEXT) WHERE typeof(msg_content_tag) = 'blob';
|]

down_m20260121_msg_content_tag :: Query
down_m20260121_msg_content_tag =
  [sql|
SELECT 1;
|]

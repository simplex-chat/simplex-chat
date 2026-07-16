{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.SQLite.Migrations.M20260716_signed_history where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20260716_signed_history :: Query
m20260716_signed_history =
  [sql|
ALTER TABLE chat_items ADD COLUMN item_msg_body BLOB;
ALTER TABLE chat_items ADD COLUMN item_chat_binding TEXT;
ALTER TABLE chat_items ADD COLUMN item_signatures BLOB;
|]

down_m20260716_signed_history :: Query
down_m20260716_signed_history =
  [sql|
ALTER TABLE chat_items DROP COLUMN item_msg_body;
ALTER TABLE chat_items DROP COLUMN item_chat_binding;
ALTER TABLE chat_items DROP COLUMN item_signatures;
|]

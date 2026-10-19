{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.SQLite.Migrations.M20220321_chat_item_edited where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20220321_chat_item_edited :: Query
m20220321_chat_item_edited =
  [sql|
ALTER TABLE chat_items ADD COLUMN item_edited INTEGER; -- 1 for edited
|]

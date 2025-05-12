{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.SQLite.Migrations.M20221214_live_message where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20221214_live_message :: Query
m20221214_live_message =
  [sql|
ALTER TABLE chat_items ADD COLUMN item_live INTEGER;
|]

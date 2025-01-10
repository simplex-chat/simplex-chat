{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.SQLite.Migrations.M20240501_chat_deleted where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20240501_chat_deleted :: Query
m20240501_chat_deleted =
  [sql|
ALTER TABLE contacts ADD COLUMN chat_deleted INTEGER NOT NULL DEFAULT 0;
|]

down_m20240501_chat_deleted :: Query
down_m20240501_chat_deleted =
  [sql|
ALTER TABLE contacts DROP COLUMN chat_deleted;
|]

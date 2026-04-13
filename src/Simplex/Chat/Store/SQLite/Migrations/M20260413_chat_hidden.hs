{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.SQLite.Migrations.M20260413_chat_hidden where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20260413_chat_hidden :: Query
m20260413_chat_hidden =
  [sql|
ALTER TABLE groups ADD COLUMN chat_hidden INTEGER NOT NULL DEFAULT 0;
|]

down_m20260413_chat_hidden :: Query
down_m20260413_chat_hidden =
  [sql|
ALTER TABLE groups DROP COLUMN chat_hidden;
|]

{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Migrations.M20241223_chat_ttl where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20241223_chat_ttl :: Query
m20241223_chat_ttl =
  [sql|
ALTER TABLE contacts ADD COLUMN chat_items_ttl INTEGER;
ALTER TABLE groups ADD COLUMN chat_items_ttl INTEGER;
ALTER TABLE groups ADD COLUMN local_alias TEXT DEFAULT '';
|]

down_m20241223_chat_ttl :: Query
down_m20241223_chat_ttl =
  [sql|
ALTER TABLE contacts DROP COLUMN chat_items_ttl;
ALTER TABLE groups DROP COLUMN chat_items_ttl;
ALTER TABLE groups DROP COLUMN local_alias
|]

{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.SQLite.Migrations.M20230129_drop_chat_items_group_idx where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20230129_drop_chat_items_group_idx :: Query
m20230129_drop_chat_items_group_idx =
  [sql|
DROP INDEX idx_chat_items_group_id;
|]

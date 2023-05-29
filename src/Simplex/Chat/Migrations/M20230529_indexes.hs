{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Migrations.M20230529_indexes where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20230529_indexes :: Query
m20230529_indexes =
  [sql|
DROP INDEX idx_chat_items_timed_delete_at;

CREATE INDEX idx_chat_items_timed_delete_at ON chat_items(user_id, timed_delete_at);
|]

down_m20230529_indexes :: Query
down_m20230529_indexes =
  [sql|
DROP INDEX idx_chat_items_timed_delete_at;

CREATE INDEX idx_chat_items_timed_delete_at ON chat_items(timed_delete_at);
|]

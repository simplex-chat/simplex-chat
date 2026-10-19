{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.SQLite.Migrations.M20240920_user_order where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20240920_user_order :: Query
m20240920_user_order =
  [sql|
ALTER TABLE users ADD COLUMN active_order INTEGER NOT NULL DEFAULT 0;
|]

down_m20240920_user_order :: Query
down_m20240920_user_order =
  [sql|
ALTER TABLE users DROP COLUMN active_order;
|]

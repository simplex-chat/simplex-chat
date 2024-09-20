{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Migrations.M20240920_user_ts where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20240920_user_ts :: Query
m20240920_user_ts =
  [sql|
ALTER TABLE users ADD COLUMN active_at TEXT NOT NULL DEFAULT('1970-01-01 00:00:00');
|]

down_m20240920_user_ts :: Query
down_m20240920_user_ts =
  [sql|
ALTER TABLE users DROP COLUMN active_at;
|]

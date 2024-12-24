{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Migrations.M20241222_operator_conditions where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20241222_operator_conditions :: Query
m20241222_operator_conditions =
  [sql|
ALTER TABLE operator_usage_conditions ADD COLUMN auto_accepted BLOB INTEGER DEFAULT 0;
|]

down_m20241222_operator_conditions :: Query
down_m20241222_operator_conditions =
  [sql|
ALTER TABLE operator_usage_conditions DROP COLUMN auto_accepted;
|]

{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.SQLite.Migrations.M20230328_files_protocol where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20230328_files_protocol :: Query
m20230328_files_protocol =
  [sql|
ALTER TABLE files ADD COLUMN protocol TEXT NOT NULL DEFAULT 'smp';
|]

down_m20230328_files_protocol :: Query
down_m20230328_files_protocol =
  [sql|
ALTER TABLE files DROP COLUMN protocol;
|]

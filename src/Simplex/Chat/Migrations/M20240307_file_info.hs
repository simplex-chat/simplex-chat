{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Migrations.M20240307_file_info where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20240307_file_info :: Query
m20240307_file_info =
  [sql|
ALTER TABLE files ADD COLUMN file_info TEXT;
|]

down_m20240307_file_info :: Query
down_m20240307_file_info =
  [sql|
ALTER TABLE files DROP COLUMN file_info;
|]

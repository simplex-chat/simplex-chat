{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Migrations.M20240324_custom_data where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20240324_custom_data :: Query
m20240324_custom_data =
  [sql|
ALTER TABLE contacts ADD COLUMN custom_data BLOB;
ALTER TABLE groups ADD COLUMN custom_data BLOB;
|]

down_m20240324_custom_data :: Query
down_m20240324_custom_data =
  [sql|
ALTER TABLE contacts DROP COLUMN custom_data;
ALTER TABLE groups DROP COLUMN custom_data;
|]

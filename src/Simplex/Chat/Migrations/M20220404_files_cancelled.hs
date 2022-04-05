{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Migrations.M20220404_files_cancelled where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20220404_files_cancelled :: Query
m20220404_files_cancelled =
  [sql|
ALTER TABLE files ADD COLUMN cancelled INTEGER; -- 1 for cancelled
|]

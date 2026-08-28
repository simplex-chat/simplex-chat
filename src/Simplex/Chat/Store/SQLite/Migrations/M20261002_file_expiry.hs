{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.SQLite.Migrations.M20261002_file_expiry where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20261002_file_expiry :: Query
m20261002_file_expiry =
  [sql|
ALTER TABLE files ADD COLUMN file_expires_at TEXT;
|]

down_m20261002_file_expiry :: Query
down_m20261002_file_expiry =
  [sql|
ALTER TABLE files DROP COLUMN file_expires_at;
|]

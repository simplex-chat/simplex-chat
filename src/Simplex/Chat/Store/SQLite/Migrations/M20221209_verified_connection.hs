{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.SQLite.Migrations.M20221209_verified_connection where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20221209_verified_connection :: Query
m20221209_verified_connection =
  [sql|
ALTER TABLE connections ADD COLUMN security_code TEXT NULL;
ALTER TABLE connections ADD COLUMN security_code_verified_at TEXT NULL;
|]

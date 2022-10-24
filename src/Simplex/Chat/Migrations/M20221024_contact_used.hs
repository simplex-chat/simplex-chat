{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Migrations.M20221024_contact_used where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20221024_contact_used :: Query
m20221024_contact_used =
  [sql|
ALTER TABLE contacts ADD COLUMN contact_used INTEGER;
|]

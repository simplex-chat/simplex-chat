{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.SQLite.Migrations.M20230926_contact_status where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20230926_contact_status :: Query
m20230926_contact_status =
  [sql|
ALTER TABLE contacts ADD COLUMN contact_status TEXT NOT NULL DEFAULT 'active';
|]

down_m20230926_contact_status :: Query
down_m20230926_contact_status =
  [sql|
ALTER TABLE contacts DROP COLUMN contact_status;
|]

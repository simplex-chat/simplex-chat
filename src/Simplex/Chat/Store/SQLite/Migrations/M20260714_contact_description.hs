{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.SQLite.Migrations.M20260714_contact_description where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20260714_contact_description :: Query
m20260714_contact_description =
  [sql|
ALTER TABLE contact_profiles ADD COLUMN description TEXT;
|]

down_m20260714_contact_description :: Query
down_m20260714_contact_description =
  [sql|
ALTER TABLE contact_profiles DROP COLUMN description;
|]

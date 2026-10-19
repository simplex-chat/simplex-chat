{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.SQLite.Migrations.M20250801_via_group_link_uri where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20250801_via_group_link_uri :: Query
m20250801_via_group_link_uri =
  [sql|
ALTER TABLE groups ADD COLUMN via_group_link_uri BLOB;
ALTER TABLE connections ADD COLUMN via_contact_uri BLOB;
|]

down_m20250801_via_group_link_uri :: Query
down_m20250801_via_group_link_uri =
  [sql|
ALTER TABLE groups DROP COLUMN via_group_link_uri;
ALTER TABLE connections DROP COLUMN via_contact_uri;
|]

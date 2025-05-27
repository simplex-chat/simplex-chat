{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.SQLite.Migrations.M20250526_short_links where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20250526_short_links :: Query
m20250526_short_links =
  [sql|
ALTER TABLE contacts ADD COLUMN conn_req_to_connect BLOB;
ALTER TABLE contacts ADD COLUMN conn_req_to_connect_tag TEXT;
ALTER TABLE contacts ADD COLUMN is_contact_request INTEGER NOT NULL DEFAULT 0;
ALTER TABLE groups ADD COLUMN conn_req_to_connect BLOB;
|]

down_m20250526_short_links :: Query
down_m20250526_short_links =
  [sql|
ALTER TABLE contacts DROP COLUMN conn_req_to_connect;
ALTER TABLE contacts DROP COLUMN conn_req_to_connect_tag;
ALTER TABLE contacts DROP COLUMN is_contact_request;
ALTER TABLE groups DROP COLUMN conn_req_to_connect;
|]

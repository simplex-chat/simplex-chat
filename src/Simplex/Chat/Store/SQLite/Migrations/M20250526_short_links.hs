{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.SQLite.Migrations.M20250526_short_links where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

-- TODO [short links] contacts with contact requests
-- TODO  - contacts.is_contact_request flag?
-- TODO  - link contact_requests and contacts?
m20250526_short_links :: Query
m20250526_short_links =
  [sql|
ALTER TABLE contacts ADD COLUMN conn_full_link_to_connect BLOB;
ALTER TABLE contacts ADD COLUMN conn_short_link_to_connect BLOB;

ALTER TABLE groups ADD COLUMN conn_full_link_to_connect BLOB;
ALTER TABLE groups ADD COLUMN conn_short_link_to_connect BLOB;
ALTER TABLE groups ADD COLUMN conn_link_started_connection INTEGER NOT NULL DEFAULT 0;
|]

down_m20250526_short_links :: Query
down_m20250526_short_links =
  [sql|
ALTER TABLE contacts DROP COLUMN conn_full_link_to_connect;
ALTER TABLE contacts DROP COLUMN conn_short_link_to_connect;

ALTER TABLE groups DROP COLUMN conn_full_link_to_connect;
ALTER TABLE groups DROP COLUMN conn_short_link_to_connect;
ALTER TABLE groups DROP COLUMN conn_link_started_connection;
|]

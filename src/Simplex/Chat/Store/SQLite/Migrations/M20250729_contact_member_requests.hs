{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.SQLite.Migrations.M20250729_contact_member_requests where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20250729_contact_member_requests :: Query
m20250729_contact_member_requests =
  [sql|
ALTER TABLE contacts ADD COLUMN contact_grp_inv_link BLOB;

ALTER TABLE users ADD COLUMN auto_accept_grp_inv_links INTEGER NOT NULL DEFAULT 0;
|]

down_m20250729_contact_member_requests :: Query
down_m20250729_contact_member_requests =
  [sql|
ALTER TABLE users DROP COLUMN auto_accept_grp_inv_links;

ALTER TABLE contacts DROP COLUMN contact_grp_inv_link;
|]

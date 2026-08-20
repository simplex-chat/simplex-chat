{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.SQLite.Migrations.M20260723_contact_request_rejection where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20260723_contact_request_rejection :: Query
m20260723_contact_request_rejection =
  [sql|
ALTER TABLE contact_requests ADD COLUMN rejection_supported INTEGER NOT NULL DEFAULT 0;
|]

down_m20260723_contact_request_rejection :: Query
down_m20260723_contact_request_rejection =
  [sql|
ALTER TABLE contact_requests DROP COLUMN rejection_supported;
|]

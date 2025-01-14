{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.SQLite.Migrations.M20221024_contact_used where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20221024_contact_used :: Query
m20221024_contact_used =
  [sql|
PRAGMA ignore_check_constraints=ON;

ALTER TABLE contacts ADD COLUMN contact_used INTEGER DEFAULT 0 CHECK (contact_used NOT NULL);

UPDATE contacts SET contact_used = 0;

UPDATE contacts SET contact_used = 1 WHERE contact_id IN (
  SELECT DISTINCT contact_id FROM chat_items WHERE contact_id IS NOT NULL
);

PRAGMA ignore_check_constraints=OFF;
|]

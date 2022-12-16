{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Migrations.M20221216_contacts_unconfirmed_ttl_change where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20221216_contacts_unconfirmed_ttl_change :: Query
m20221216_contacts_unconfirmed_ttl_change =
  [sql|
PRAGMA ignore_check_constraints=ON;

ALTER TABLE contacts ADD COLUMN unconfirmed_ttl_change INTEGER DEFAULT 0 CHECK (unconfirmed_ttl_change NOT NULL);

PRAGMA ignore_check_constraints=OFF;
|]

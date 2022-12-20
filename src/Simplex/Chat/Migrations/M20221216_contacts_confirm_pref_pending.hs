{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Migrations.M20221216_contacts_confirm_pref_pending where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20221216_contacts_confirm_pref_pending :: Query
m20221216_contacts_confirm_pref_pending =
  [sql|
PRAGMA ignore_check_constraints=ON;

ALTER TABLE contacts ADD COLUMN confirm_pref_pending INTEGER DEFAULT 0 CHECK (confirm_pref_pending NOT NULL);

PRAGMA ignore_check_constraints=OFF;
|]

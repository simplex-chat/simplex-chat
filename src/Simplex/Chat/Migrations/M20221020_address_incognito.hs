{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Migrations.M20221020_address_incognito where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20221020_address_incognito :: Query
m20221020_address_incognito =
  [sql|
PRAGMA ignore_check_constraints=ON;

ALTER TABLE user_contact_links ADD column incognito INTEGER DEFAULT 0 CHECK (incognito NOT NULL);
UPDATE user_contact_links SET incognito = 0;

PRAGMA ignore_check_constraints=OFF;
|]

{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.SQLite.Migrations.M20221021_auto_accept__group_links where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20221021_auto_accept__group_links :: Query
m20221021_auto_accept__group_links =
  [sql|
PRAGMA ignore_check_constraints=ON;

ALTER TABLE connections ADD COLUMN via_group_link INTEGER DEFAULT 0 CHECK (via_group_link NOT NULL); -- flag, 1 for connections via group link
UPDATE connections SET via_group_link = 0;

ALTER TABLE user_contact_links ADD column auto_accept_incognito INTEGER DEFAULT 0 CHECK (auto_accept_incognito NOT NULL);
UPDATE user_contact_links SET auto_accept_incognito = 0;

PRAGMA ignore_check_constraints=OFF;
|]

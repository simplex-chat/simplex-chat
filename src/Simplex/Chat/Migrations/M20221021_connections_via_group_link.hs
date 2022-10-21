{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Migrations.M20221021_connections_via_group_link where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20221021_connections_via_group_link :: Query
m20221021_connections_via_group_link =
  [sql|
PRAGMA ignore_check_constraints=ON;

ALTER TABLE connections ADD COLUMN via_group_link INTEGER DEFAULT 0 CHECK (via_group_link NOT NULL); -- flag, 1 for connections via group link
UPDATE connections SET via_group_link = 0;

PRAGMA ignore_check_constraints=OFF;
|]

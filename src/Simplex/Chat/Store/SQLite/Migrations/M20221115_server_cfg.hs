{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.SQLite.Migrations.M20221115_server_cfg where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20221115_server_cfg :: Query
m20221115_server_cfg =
  [sql|
PRAGMA ignore_check_constraints=ON;

ALTER TABLE smp_servers ADD COLUMN preset INTEGER DEFAULT 0 CHECK (preset NOT NULL);
ALTER TABLE smp_servers ADD COLUMN tested INTEGER;
ALTER TABLE smp_servers ADD COLUMN enabled INTEGER DEFAULT 1 CHECK (enabled NOT NULL);
UPDATE smp_servers SET preset = 0, enabled = 1;

PRAGMA ignore_check_constraints=OFF;
|]

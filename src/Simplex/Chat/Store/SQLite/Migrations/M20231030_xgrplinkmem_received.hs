{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.SQLite.Migrations.M20231030_xgrplinkmem_received where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20231030_xgrplinkmem_received :: Query
m20231030_xgrplinkmem_received =
  [sql|
ALTER TABLE group_members ADD COLUMN xgrplinkmem_received INTEGER NOT NULL DEFAULT 0;
|]

down_m20231030_xgrplinkmem_received :: Query
down_m20231030_xgrplinkmem_received =
  [sql|
ALTER TABLE group_members DROP COLUMN xgrplinkmem_received;
|]

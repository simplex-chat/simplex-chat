{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.SQLite.Migrations.M20241008_indexes where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20241008_indexes :: Query
m20241008_indexes =
  [sql|
CREATE INDEX idx_received_probes_group_member_id on received_probes(group_member_id);
|]

down_m20241008_indexes :: Query
down_m20241008_indexes =
  [sql|
DROP INDEX idx_received_probes_group_member_id;
|]

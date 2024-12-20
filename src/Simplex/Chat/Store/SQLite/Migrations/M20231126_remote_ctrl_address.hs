{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.SQLite.Migrations.M20231126_remote_ctrl_address where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20231126_remote_ctrl_address :: Query
m20231126_remote_ctrl_address =
  [sql|
ALTER TABLE remote_hosts ADD COLUMN bind_addr TEXT;
ALTER TABLE remote_hosts ADD COLUMN bind_iface TEXT;
ALTER TABLE remote_hosts ADD COLUMN bind_port INTEGER;
|]

down_m20231126_remote_ctrl_address :: Query
down_m20231126_remote_ctrl_address =
  [sql|
ALTER TABLE remote_hosts DROP COLUMN bind_addr;
ALTER TABLE remote_hosts DROP COLUMN bind_iface;
ALTER TABLE remote_hosts DROP COLUMN bind_port;
|]

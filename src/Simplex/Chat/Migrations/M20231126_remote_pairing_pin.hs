{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Migrations.M20231126_remote_pairing_pin where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20231126_remote_pairing_pin :: Query
m20231126_remote_pairing_pin =
  [sql|
ALTER TABLE remote_hosts ADD COLUMN bind_iface TEXT;
ALTER TABLE remote_hosts ADD COLUMN bind_addr TEXT;
ALTER TABLE remote_hosts ADD COLUMN bind_port INTEGER;
|]

down_20231126_remote_pairing_pin :: Query
down_20231126_remote_pairing_pin =
  [sql|
ALTER TABLE remote_hosts DROP COLUMN bind_iface;
ALTER TABLE remote_hosts DROP COLUMN bind_addr;
ALTER TABLE remote_hosts DROP COLUMN bind_port;
|]

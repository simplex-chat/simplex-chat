{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Migrations.M20230924_remote_controllers where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20230924_remote_controllers :: Query
m20230924_remote_controllers =
  [sql|
CREATE TABLE remote_controllers ( -- controllers known to a hosting app
  remote_controller_id INTEGER PRIMARY KEY,
  display_name TEXT NOT NULL,
  fingerprint BLOB NOT NULL
);
|]

down_m20230924_remote_controllers :: Query
down_m20230924_remote_controllers =
  [sql| DROP TABLE remote_controllers; |]

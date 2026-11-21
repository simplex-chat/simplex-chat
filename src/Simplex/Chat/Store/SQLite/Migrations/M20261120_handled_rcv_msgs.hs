{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.SQLite.Migrations.M20261120_handled_rcv_msgs where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20261120_handled_rcv_msgs :: Query
m20261120_handled_rcv_msgs =
  [sql|
CREATE TABLE handled_rcv_msgs(
  connection_id INTEGER NOT NULL REFERENCES connections ON DELETE CASCADE,
  agent_msg_id INTEGER NOT NULL,
  rcpt INTEGER NOT NULL,
  created_at TEXT NOT NULL,
  PRIMARY KEY (connection_id, agent_msg_id)
) STRICT;
|]

down_m20261120_handled_rcv_msgs :: Query
down_m20261120_handled_rcv_msgs =
  [sql|
DROP TABLE handled_rcv_msgs;
|]

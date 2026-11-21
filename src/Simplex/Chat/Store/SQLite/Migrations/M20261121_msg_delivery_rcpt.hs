{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.SQLite.Migrations.M20261121_msg_delivery_rcpt where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20261121_msg_delivery_rcpt :: Query
m20261121_msg_delivery_rcpt =
  [sql|
ALTER TABLE msg_deliveries ADD COLUMN rcpt INTEGER NOT NULL DEFAULT 0;
|]

down_m20261121_msg_delivery_rcpt :: Query
down_m20261121_msg_delivery_rcpt =
  [sql|
ALTER TABLE msg_deliveries DROP COLUMN rcpt;
|]

{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.SQLite.Migrations.M20221004_idx_msg_deliveries_message_id where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20221004_idx_msg_deliveries_message_id :: Query
m20221004_idx_msg_deliveries_message_id =
  [sql|
CREATE INDEX idx_msg_deliveries_message_id ON msg_deliveries(message_id);
|]

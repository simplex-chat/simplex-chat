{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.SQLite.Migrations.M20230608_deleted_contacts where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20230608_deleted_contacts :: Query
m20230608_deleted_contacts =
  [sql|
ALTER TABLE contacts ADD COLUMN deleted INTEGER NOT NULL DEFAULT 0;

CREATE INDEX msg_delivery_events_msg_delivery_id ON msg_delivery_events(msg_delivery_id);
|]

down_m20230608_deleted_contacts :: Query
down_m20230608_deleted_contacts =
  [sql|
DROP INDEX msg_delivery_events_msg_delivery_id;

ALTER TABLE contacts DROP COLUMN deleted;
|]

{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.SQLite.Migrations.M20230504_recreate_msg_delivery_events_cleanup_messages where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20230504_recreate_msg_delivery_events_cleanup_messages :: Query
m20230504_recreate_msg_delivery_events_cleanup_messages =
  [sql|
DROP TABLE msg_delivery_events;

CREATE TABLE msg_delivery_events (
  msg_delivery_event_id INTEGER PRIMARY KEY,
  msg_delivery_id INTEGER NOT NULL REFERENCES msg_deliveries ON DELETE CASCADE,
  delivery_status TEXT NOT NULL,
  created_at TEXT NOT NULL DEFAULT (datetime('now')),
  updated_at TEXT NOT NULL DEFAULT (datetime('now'))
);

DELETE FROM messages WHERE created_at < datetime('now', '-30 days');
|]

down_m20230504_recreate_msg_delivery_events_cleanup_messages :: Query
down_m20230504_recreate_msg_delivery_events_cleanup_messages =
  [sql|
DROP TABLE msg_delivery_events;

CREATE TABLE msg_delivery_events (
  msg_delivery_event_id INTEGER PRIMARY KEY,
  msg_delivery_id INTEGER NOT NULL REFERENCES msg_deliveries ON DELETE CASCADE, -- non UNIQUE for multiple events per msg delivery
  delivery_status TEXT NOT NULL, -- see MsgDeliveryStatus for allowed values
  created_at TEXT NOT NULL DEFAULT (datetime('now')),
  updated_at TEXT NOT NULL DEFAULT (datetime('now')),
  UNIQUE (msg_delivery_id, delivery_status)
);
|]

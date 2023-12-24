{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Migrations.M20231215_recreate_msg_deliveries where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20231215_recreate_msg_deliveries :: Query
m20231215_recreate_msg_deliveries =
  [sql|
DROP VIEW IF EXISTS direct_messages;
DROP VIEW IF EXISTS direct_messages_plain;
DROP VIEW IF EXISTS group_messages;
DROP VIEW IF EXISTS group_messages_plain;
DROP VIEW IF EXISTS all_messages;
DROP VIEW IF EXISTS all_messages_plain;

DROP INDEX msg_delivery_events_msg_delivery_id;
DROP TABLE msg_delivery_events;

DROP INDEX idx_msg_deliveries_message_id;
DROP INDEX idx_msg_deliveries_agent_ack_cmd_id;

CREATE TABLE new_msg_deliveries(
  msg_delivery_id INTEGER PRIMARY KEY,
  message_id INTEGER NOT NULL REFERENCES messages ON DELETE CASCADE, -- non UNIQUE for group messages and for batched messages
  connection_id INTEGER NOT NULL REFERENCES connections ON DELETE CASCADE,
  agent_msg_id INTEGER, -- internal agent message ID (NULL while pending), non UNIQUE for batched messages
  agent_msg_meta TEXT, -- JSON with timestamps etc. sent in MSG, NULL for sent
  chat_ts TEXT NOT NULL DEFAULT(datetime('now')),
  created_at TEXT CHECK(created_at NOT NULL),
  updated_at TEXT CHECK(updated_at NOT NULL),
  agent_ack_cmd_id INTEGER, -- broker_ts for received, created_at for sent
  delivery_status TEXT -- MsgDeliveryStatus
);

INSERT INTO new_msg_deliveries (
  msg_delivery_id, message_id, connection_id, agent_msg_id, agent_msg_meta,
  chat_ts, created_at, updated_at, agent_ack_cmd_id
)
SELECT
  msg_delivery_id, message_id, connection_id, agent_msg_id, agent_msg_meta,
  chat_ts, created_at, updated_at, agent_ack_cmd_id
FROM msg_deliveries;

DROP TABLE msg_deliveries;
ALTER TABLE new_msg_deliveries RENAME TO msg_deliveries;

CREATE INDEX idx_msg_deliveries_message_id ON "msg_deliveries"(message_id);
CREATE INDEX idx_msg_deliveries_agent_ack_cmd_id ON "msg_deliveries"(connection_id, agent_ack_cmd_id);
CREATE INDEX idx_msg_deliveries_agent_msg_id ON "msg_deliveries"(connection_id, agent_msg_id);
|]

down_m20231215_recreate_msg_deliveries :: Query
down_m20231215_recreate_msg_deliveries =
  [sql|
DROP INDEX idx_msg_deliveries_message_id;
DROP INDEX idx_msg_deliveries_agent_ack_cmd_id;
DROP INDEX idx_msg_deliveries_agent_msg_id;

CREATE TABLE old_msg_deliveries(
  msg_delivery_id INTEGER PRIMARY KEY,
  message_id INTEGER NOT NULL REFERENCES messages ON DELETE CASCADE, -- non UNIQUE for group messages
  connection_id INTEGER NOT NULL REFERENCES connections ON DELETE CASCADE,
  agent_msg_id INTEGER, -- internal agent message ID(NULL while pending)
  agent_msg_meta TEXT, -- JSON with timestamps etc. sent in MSG, NULL for sent
  chat_ts TEXT NOT NULL DEFAULT(datetime('now')),
  created_at TEXT CHECK(created_at NOT NULL),
  updated_at TEXT CHECK(updated_at NOT NULL),
  agent_ack_cmd_id INTEGER, -- broker_ts for received, created_at for sent
  UNIQUE(connection_id, agent_msg_id)
);

INSERT INTO old_msg_deliveries (
  msg_delivery_id, message_id, connection_id, agent_msg_id, agent_msg_meta,
  chat_ts, created_at, updated_at, agent_ack_cmd_id
)
WITH unique_msg_deliveries AS (
  SELECT
    msg_delivery_id, message_id, connection_id, agent_msg_id, agent_msg_meta,
    chat_ts, created_at, updated_at, agent_ack_cmd_id,
    row_number() OVER connection_id_agent_msg_id_win AS row_number
  FROM msg_deliveries
  WINDOW connection_id_agent_msg_id_win AS (PARTITION BY connection_id, agent_msg_id ORDER BY created_at ASC, msg_delivery_id ASC)
)
SELECT
  msg_delivery_id, message_id, connection_id, agent_msg_id, agent_msg_meta,
  chat_ts, created_at, updated_at, agent_ack_cmd_id
FROM unique_msg_deliveries
WHERE row_number = 1;

DROP TABLE msg_deliveries;
ALTER TABLE old_msg_deliveries RENAME TO msg_deliveries;

CREATE INDEX idx_msg_deliveries_message_id ON "msg_deliveries"(message_id);
CREATE INDEX idx_msg_deliveries_agent_ack_cmd_id ON "msg_deliveries"(connection_id, agent_ack_cmd_id);

CREATE TABLE msg_delivery_events (
  msg_delivery_event_id INTEGER PRIMARY KEY,
  msg_delivery_id INTEGER NOT NULL REFERENCES msg_deliveries ON DELETE CASCADE, -- non UNIQUE for multiple events per msg delivery
  delivery_status TEXT NOT NULL, -- see MsgDeliveryStatus for allowed values
  created_at TEXT NOT NULL DEFAULT (datetime('now')),
  updated_at TEXT NOT NULL DEFAULT (datetime('now')),
  UNIQUE (msg_delivery_id, delivery_status)
);
CREATE INDEX msg_delivery_events_msg_delivery_id ON msg_delivery_events(msg_delivery_id);
|]

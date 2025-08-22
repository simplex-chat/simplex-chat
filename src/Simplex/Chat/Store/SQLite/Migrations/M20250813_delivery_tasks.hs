{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.SQLite.Migrations.M20250813_delivery_tasks where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20250813_delivery_tasks :: Query
m20250813_delivery_tasks =
  [sql|
CREATE TABLE delivery_tasks (
  delivery_task_id INTEGER PRIMARY KEY,
  group_id INTEGER NOT NULL REFERENCES groups ON DELETE CASCADE,
  delivery_scope TEXT, -- GroupForwardScope - tag? or, add support scope group_member_id?
  task_tag TEXT NOT NULL, -- DeliveryTaskTag = FCTMessage | FCTRelayRemoval | FCTReactionCount
  task_complete INTEGER NOT NULL DEFAULT 0, -- or task_status? e.g. "pending", "in_progress", "complete"
  cursor_group_member_id INTEGER,
  messages_encoding TEXT, -- or, instead save comma separated list of references to messages? (for MessageDeliveryTask)
  message_from_channel INTEGER NOT NULL DEFAULT 0, -- for MessageDeliveryTask (sender sent "message from channel")
  message_id INTEGER REFERENCES messages ON DELETE CASCADE -- for RelayRemovedTask
);

ALTER TABLE group_members ADD COLUMN last_profile_delivery_ts TEXT;
ALTER TABLE group_members ADD COLUMN join_ts TEXT;
|]

down_m20250813_delivery_tasks :: Query
down_m20250813_delivery_tasks =
  [sql|
ALTER TABLE group_members DROP COLUMN last_profile_delivery_ts;
ALTER TABLE group_members DROP COLUMN join_ts;

DROP TABLE delivery_tasks;
|]

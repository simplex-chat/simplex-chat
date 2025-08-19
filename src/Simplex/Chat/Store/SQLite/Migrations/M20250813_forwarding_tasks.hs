{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.SQLite.Migrations.M20250813_forwarding_tasks where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20250813_forwarding_tasks :: Query
m20250813_forwarding_tasks =
  [sql|
CREATE TABLE forwarding_tasks (
  forwarding_task_id INTEGER PRIMARY KEY,
  group_id INTEGER NOT NULL REFERENCES groups ON DELETE CASCADE,
  forward_scope TEXT, -- GroupForwardScope - tag? or, add support scope group_member_id?
  task_tag TEXT NOT NULL, -- ForwardingContextTag = FCTMessage | FCTRelayRemoval | FCTReactionCount
  task_complete INTEGER NOT NULL DEFAULT 0, -- or task_status? e.g. "pending", "in_progress", "complete"
  prev_sender_interaction_ts TEXT,
  cursor_group_member_id INTEGER, -- for members that joined before prev_sender_interaction_ts (MessageForwardingContext); or for all members
  post_interaction_cursor_group_member_id INTEGER, -- for members that joined after prev_sender_interaction_ts
  messages_encoding TEXT, -- or, instead save comma separated list of references to messages? (for MessageForwardingContext)
  group_as_sender INTEGER NOT NULL DEFAULT 0, -- for MessageForwardingContext (sender sent "message from channel")
  message_id INTEGER REFERENCES messages ON DELETE CASCADE, -- for RelayRemovalForwardingContext
)

ALTER TABLE group_members ADD COLUMN last_interaction_ts TEXT;
|]

down_m20250813_forwarding_tasks :: Query
down_m20250813_forwarding_tasks =
  [sql|
ALTER TABLE group_members DROP COLUMN last_interaction_ts;

DROP TABLE forwarding_tasks;
|]

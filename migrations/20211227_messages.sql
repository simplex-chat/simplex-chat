DROP TABLE event_body_parts;
DROP TABLE contact_profile_events;
DROP TABLE group_profile_events;
DROP TABLE group_event_parents;
DROP TABLE group_events;
DROP TABLE message_events;
DROP TABLE message_content;
DROP TABLE events;
DROP TABLE messages;

CREATE TABLE messages ( -- messages received by the agent, append only
  message_id INTEGER PRIMARY KEY,
  msg_sent INTEGER NOT NULL, -- 1 for sent, 0 for received
  chat_msg_event TEXT NOT NULL, -- message event type (the constructor of ChatMsgEvent)
  msg_body BLOB, -- agent message body as sent
  created_at TEXT NOT NULL DEFAULT (datetime('now'))
);

CREATE TABLE msg_deliveries (
  msg_delivery_id INTEGER PRIMARY KEY,
  message_id INTEGER NOT NULL REFERENCES messages ON DELETE CASCADE, -- non UNIQUE for groups
  agent_conn_id BLOB NOT NULL REFERENCES connections (agent_conn_id) ON DELETE CASCADE,
  agent_msg_id INTEGER NOT NULL, -- internal agent message ID (NULL while pending)
  agent_msg_meta TEXT, -- JSON with timestamps etc. sent in MSG, NULL for sent
  UNIQUE (agent_conn_id, agent_msg_id)
);

CREATE TABLE msg_delivery_events (
  msg_delivery_event_id INTEGER PRIMARY KEY,
  msg_delivery_id INTEGER NOT NULL REFERENCES msg_deliveries ON DELETE CASCADE, -- non UNIQUE for multiple events per msg delivery
  delivery_status TEXT NOT NULL DEFAULT 'pending', -- rcv/snd - "pending", "agent"; rcv - "acknowledged"; snd - "sent", "received", "read"
  created_at TEXT NOT NULL DEFAULT (datetime('now')),
  UNIQUE (msg_delivery_id, delivery_status)
);

-- TODO group message parents and chat items not to be implemented in current scope

-- CREATE TABLE group_message_parents (
--   group_message_parent_id INTEGER PRIMARY KEY,
--   message_id INTEGER NOT NULL REFERENCES group_messages (event_id),
--   parent_group_member_id INTEGER REFERENCES group_members (group_member_id), -- can be NULL if parent_member_id is incorrect
--   parent_member_id BLOB, -- shared member ID, unique per group
--   parent_message_id INTEGER REFERENCES messages (message_id) ON DELETE CASCADE, -- can be NULL if received message references another message that's not received yet
--   parent_chat_msg_id INTEGER NOT NULL,
--   parent_msg_body_hash BLOB NOT NULL
-- );

-- CREATE INDEX group_event_parents_parent_chat_event_id_index
--   ON group_message_parents (parent_member_id, parent_chat_msg_id);

-- CREATE TABLE chat_items ( -- mutable chat_items presented to user
--   chat_item_id INTEGER PRIMARY KEY,
--   chat_msg_id INTEGER NOT NULL, -- sent as part of the message that created the item
--   item_deleted INTEGER NOT NULL, -- 1 for deleted
--   item_type TEXT NOT NULL,
--   item_text TEXT NOT NULL, -- textual representation
--   item_props TEXT NOT NULL -- JSON
-- );

-- CREATE TABLE direct_chat_items (
--   chat_item_id INTEGER NOT NULL UNIQUE REFERENCES chat_items ON DELETE CASCADE,
--   contact_id INTEGER NOT NULL REFERENCES contacts ON DELETE RESTRICT,
--   item_sent INTEGER -- 1 for sent, 0 for received
-- );

-- CREATE TABLE group_chat_items (
--   chat_item_id INTEGER NOT NULL UNIQUE REFERENCES chat_items ON DELETE CASCADE,
--   group_member_id INTEGER REFERENCES group_members ON DELETE RESTRICT, -- NULL for sent
--   group_id INTEGER NOT NULL REFERENCES groups ON DELETE RESTRICT
-- );

-- CREATE TABLE chat_item_content (
--   chat_item_content_id INTEGER PRIMARY KEY,
--   chat_item_id INTEGER NOT NULL REFERENCES chat_items ON DELETE CASCADE,
--   content_type TEXT NOT NULL,
--   content_size INTEGER NOT NULL,
--   content BLOB NOT NULL
-- );

-- CREATE TABLE chat_item_messages (
--   message_id INTEGER NOT NULL UNIQUE REFERENCES messages,
--   chat_item_id INTEGER NOT NULL REFERENCES chat_items
-- );

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
  agent_msg_id INTEGER NOT NULL, -- internal agent message ID
  external_msg_id INTEGER NOT NULL, -- external message ID (sent or received)
  agent_meta TEXT, -- JSON with timestamps etc. sent in MSG, NULL for sent
  connection_id INTEGER NOT NULL REFERENCES connections,
  msg_sent INTEGER NOT NULL, -- 0 for received, 1 for sent
  chat_msg_id INTEGER NOT NULL, -- sent as part of the message
  chat_msg_event TEXT NOT NULL, -- message event type (the constructor of ChatMsgEvent)
  msg_body BLOB, -- agent message body as sent
  msg_body_hash BLOB NOT NULL,
  msg_integrity TEXT NOT NULL DEFAULT '',
  created_at TEXT NOT NULL DEFAULT (datetime('now'))
);

CREATE INDEX messages_agent_msg_id_index ON messages (connection_id, agent_msg_id);

CREATE INDEX messages_external_msg_id_index ON messages (connection_id, external_msg_id);

CREATE TABLE contact_profile_messages (
  message_id INTEGER NOT NULL UNIQUE REFERENCES messages,
  contact_profile_id INTEGER NOT NULL REFERENCES contact_profiles
);

CREATE TABLE group_profile_messages (
  message_id INTEGER NOT NULL UNIQUE REFERENCES messages,
  group_profile_id INTEGER NOT NULL REFERENCES group_profiles
);

CREATE TABLE direct_messages (
  message_id INTEGER NOT NULL UNIQUE REFERENCES messages,
  contact_id INTEGER NOT NULL REFERENCES contacts ON DELETE RESTRICT,
  msg_sent INTEGER -- 1 for sent, 0 for received
);

CREATE TABLE direct_msg_delivery_events (
  direct_msg_delivery_event_id INTEGER PRIMARY KEY,
  message_id INTEGER NOT NULL UNIQUE REFERENCES messages,
  delivery_status TEXT NOT NULL DEFAULT 'pending', -- pending, agent, sent, received, read
  created_at TEXT NOT NULL DEFAULT (datetime('now')),
  UNIQUE (message_id, delivery_status)
);

CREATE TABLE group_messages (
  message_id INTEGER NOT NULL UNIQUE REFERENCES messages,
  group_id INTEGER NOT NULL REFERENCES groups ON DELETE RESTRICT,
  group_member_id INTEGER REFERENCES group_members -- NULL for sent
);

CREATE TABLE group_msg_delivery_events (
  group_msg_delivery_event_id INTEGER PRIMARY KEY,
  group_member_id INTEGER NOT NULL REFERENCES group_members,
  message_id INTEGER NOT NULL UNIQUE REFERENCES messages,
  delivery_status TEXT NOT NULL DEFAULT 'pending', -- agent, sent, received, read
  created_at TEXT NOT NULL DEFAULT (datetime('now')),
  UNIQUE (group_member_id, message_id, delivery_status)
);

CREATE TABLE group_message_parents (
  group_message_parent_id INTEGER PRIMARY KEY,
  message_id INTEGER NOT NULL REFERENCES group_messages (event_id),
  parent_group_member_id INTEGER REFERENCES group_members (group_member_id), -- can be NULL if parent_member_id is incorrect
  parent_member_id BLOB, -- shared member ID, unique per group
  parent_message_id INTEGER REFERENCES messages (message_id) ON DELETE CASCADE, -- can be NULL if received message references another message that's not received yet
  parent_chat_msg_id INTEGER NOT NULL,
  parent_msg_body_hash BLOB NOT NULL
);

CREATE INDEX group_event_parents_parent_chat_event_id_index
  ON group_message_parents (parent_member_id, parent_chat_msg_id);

CREATE TABLE chat_items ( -- mutable chat_items presented to user
  chat_item_id INTEGER PRIMARY KEY,
  chat_msg_id INTEGER NOT NULL, -- sent as part of the message that created the item
  item_deleted INTEGER NOT NULL, -- 1 for deleted
  item_type TEXT NOT NULL,
  item_text TEXT NOT NULL, -- textual representation
  item_props TEXT NOT NULL -- JSON
);

CREATE TABLE direct_chat_items (
  chat_item_id INTEGER NOT NULL UNIQUE REFERENCES chat_items ON DELETE CASCADE,
  contact_id INTEGER NOT NULL REFERENCES contacts ON DELETE RESTRICT,
  item_sent INTEGER -- 1 for sent, 0 for received
);

CREATE TABLE group_chat_items (
  chat_item_id INTEGER NOT NULL UNIQUE REFERENCES chat_items ON DELETE CASCADE,
  group_member_id INTEGER REFERENCES group_members ON DELETE RESTRICT, -- NULL for sent
  group_id INTEGER NOT NULL REFERENCES groups ON DELETE RESTRICT
);

CREATE TABLE chat_item_content (
  chat_item_content_id INTEGER PRIMARY KEY,
  chat_item_id INTEGER NOT NULL REFERENCES chat_items ON DELETE CASCADE,
  content_type TEXT NOT NULL,
  content_size INTEGER NOT NULL,
  content BLOB NOT NULL
);

CREATE TABLE chat_item_messages (
  message_id INTEGER NOT NULL UNIQUE REFERENCES messages,
  chat_item_id INTEGER NOT NULL REFERENCES chat_items
);

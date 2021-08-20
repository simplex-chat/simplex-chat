CREATE TABLE contact_profiles ( -- remote user profile
  contact_profile_id INTEGER PRIMARY KEY,
  display_name TEXT NOT NULL, -- contact name set by remote user (not unique), this name must not contain spaces
  full_name TEXT NOT NULL,
  properties TEXT NOT NULL DEFAULT '{}' -- JSON with contact profile properties
);

CREATE INDEX contact_profiles_index ON contact_profiles (display_name, full_name);

CREATE TABLE users (
  user_id INTEGER PRIMARY KEY,
  contact_id INTEGER NOT NULL UNIQUE REFERENCES contacts ON DELETE CASCADE
    DEFERRABLE INITIALLY DEFERRED,
  local_display_name TEXT NOT NULL UNIQUE,
  active_user INTEGER NOT NULL DEFAULT 0, -- 1 for active user
  FOREIGN KEY (user_id, local_display_name)
    REFERENCES display_names (user_id, local_display_name)
    ON DELETE RESTRICT
    ON UPDATE CASCADE
    DEFERRABLE INITIALLY DEFERRED
);

CREATE TABLE display_names (
  user_id INTEGER NOT NULL REFERENCES users,
  local_display_name TEXT NOT NULL,
  ldn_base TEXT NOT NULL,
  ldn_suffix INTEGER NOT NULL DEFAULT 0,
  PRIMARY KEY (user_id, local_display_name) ON CONFLICT FAIL,
  UNIQUE (user_id, ldn_base, ldn_suffix) ON CONFLICT FAIL
) WITHOUT ROWID;

CREATE TABLE contacts (
  contact_id INTEGER PRIMARY KEY,
  contact_profile_id INTEGER REFERENCES contact_profiles, -- NULL if it's an incognito profile
  user_id INTEGER NOT NULL REFERENCES users,
  local_display_name TEXT NOT NULL,
  is_user INTEGER NOT NULL DEFAULT 0, -- 1 if this contact is a user
  via_group INTEGER REFERENCES groups (group_id) ON DELETE SET NULL,
  created_at TEXT NOT NULL DEFAULT (datetime('now')),
  FOREIGN KEY (user_id, local_display_name)
    REFERENCES display_names (user_id, local_display_name)
    ON DELETE RESTRICT
    ON UPDATE CASCADE,
  UNIQUE (user_id, local_display_name),
  UNIQUE (user_id, contact_profile_id)
);

CREATE TABLE sent_probes (
  sent_probe_id INTEGER PRIMARY KEY,
  contact_id INTEGER NOT NULL UNIQUE REFERENCES contacts ON DELETE CASCADE,
  probe BLOB NOT NULL,
  user_id INTEGER NOT NULL REFERENCES users,
  UNIQUE (user_id, probe)
);

CREATE TABLE sent_probe_hashes (
  sent_probe_hash_id INTEGER PRIMARY KEY,
  sent_probe_id INTEGER NOT NULL REFERENCES sent_probes ON DELETE CASCADE,
  contact_id INTEGER NOT NULL REFERENCES contacts ON DELETE CASCADE,
  user_id INTEGER NOT NULL REFERENCES users,
  UNIQUE (sent_probe_id, contact_id)
);

CREATE TABLE received_probes (
  received_probe_id INTEGER PRIMARY KEY,
  contact_id INTEGER NOT NULL REFERENCES contacts ON DELETE CASCADE,
  probe BLOB,
  probe_hash BLOB NOT NULL,
  user_id INTEGER NOT NULL REFERENCES users
);

CREATE TABLE known_servers(
  server_id INTEGER PRIMARY KEY,
  host TEXT NOT NULL,
  port TEXT NOT NULL,
  key_hash BLOB,
  user_id INTEGER NOT NULL REFERENCES users,
  UNIQUE (user_id, host, port)
) WITHOUT ROWID;

CREATE TABLE group_profiles ( -- shared group profiles
  group_profile_id INTEGER PRIMARY KEY,
  display_name TEXT NOT NULL, -- this name must not contain spaces
  full_name TEXT NOT NULL,
  properties TEXT NOT NULL DEFAULT '{}' -- JSON with user or contact profile
);

CREATE TABLE groups (
  group_id INTEGER PRIMARY KEY, -- local group ID
  user_id INTEGER NOT NULL REFERENCES users,
  local_display_name TEXT NOT NULL, -- local group name without spaces
  group_profile_id INTEGER REFERENCES group_profiles, -- shared group profile
  inv_queue_info BLOB,
  FOREIGN KEY (user_id, local_display_name)
    REFERENCES display_names (user_id, local_display_name)
    ON DELETE RESTRICT
    ON UPDATE CASCADE,
  UNIQUE (user_id, local_display_name),
  UNIQUE (user_id, group_profile_id)
);

CREATE TABLE group_members ( -- group members, excluding the local user
  group_member_id INTEGER PRIMARY KEY,
  group_id INTEGER NOT NULL REFERENCES groups ON DELETE RESTRICT,
  member_id BLOB NOT NULL, -- shared member ID, unique per group
  member_role TEXT NOT NULL, -- owner, admin, member
  member_category TEXT NOT NULL, -- see GroupMemberCategory
  member_status TEXT NOT NULL, -- see GroupMemberStatus
  invited_by INTEGER REFERENCES contacts (contact_id) ON DELETE RESTRICT, -- NULL for the members who joined before the current user and for the group creator
  group_queue_info BLOB,
  direct_queue_info BLOB,
  user_id INTEGER NOT NULL REFERENCES users,
  local_display_name TEXT NOT NULL, -- should be the same as contact
  contact_profile_id INTEGER NOT NULL REFERENCES contact_profiles ON DELETE RESTRICT,
  contact_id INTEGER REFERENCES contacts ON DELETE RESTRICT,
  FOREIGN KEY (user_id, local_display_name)
    REFERENCES display_names (user_id, local_display_name)
    ON DELETE RESTRICT
    ON UPDATE CASCADE,
  UNIQUE (group_id, member_id)
);

CREATE TABLE group_member_intros (
  group_member_intro_id INTEGER PRIMARY KEY,
  re_group_member_id INTEGER NOT NULL REFERENCES group_members (group_member_id) ON DELETE CASCADE,
  to_group_member_id INTEGER NOT NULL REFERENCES group_members (group_member_id) ON DELETE CASCADE,
  group_queue_info BLOB,
  direct_queue_info BLOB,
  intro_status TEXT NOT NULL, -- see GroupMemberIntroStatus
  UNIQUE (re_group_member_id, to_group_member_id)
);

CREATE TABLE files (
  file_id INTEGER PRIMARY KEY,
  contact_id INTEGER REFERENCES contacts ON DELETE RESTRICT,
  group_id INTEGER REFERENCES groups ON DELETE RESTRICT,
  file_name TEXT NOT NULL,
  file_path TEXT,
  file_size INTEGER NOT NULL,
  chunk_size INTEGER NOT NULL,
  created_at TEXT NOT NULL DEFAULT (datetime('now')),
  user_id INTEGER NOT NULL REFERENCES users
);

CREATE TABLE snd_files (
  file_id INTEGER NOT NULL REFERENCES files ON DELETE RESTRICT,
  connection_id INTEGER NOT NULL REFERENCES connections ON DELETE RESTRICT,
  file_status TEXT NOT NULL, -- new, accepted, connected, completed
  group_member_id INTEGER REFERENCES group_members ON DELETE RESTRICT,
  PRIMARY KEY (file_id, connection_id)
) WITHOUT ROWID;

CREATE TABLE rcv_files (
  file_id INTEGER PRIMARY KEY REFERENCES files ON DELETE RESTRICT,
  file_status TEXT NOT NULL, -- new, accepted, connected, completed
  group_member_id INTEGER REFERENCES group_members ON DELETE RESTRICT,
  file_queue_info BLOB
);

CREATE TABLE snd_file_chunks (
  file_id INTEGER NOT NULL,
  connection_id INTEGER NOT NULL,
  chunk_number INTEGER NOT NULL,
  chunk_agent_msg_id INTEGER,
  chunk_sent INTEGER NOT NULL DEFAULT 0, -- 0 (sent to agent), 1 (sent to server)
  FOREIGN KEY (file_id, connection_id) REFERENCES snd_files ON DELETE CASCADE,
  PRIMARY KEY (file_id, connection_id, chunk_number)
) WITHOUT ROWID;

CREATE TABLE rcv_file_chunks (
  file_id INTEGER NOT NULL REFERENCES rcv_files,
  chunk_number INTEGER NOT NULL,
  chunk_agent_msg_id INTEGER NOT NULL,
  chunk_stored INTEGER NOT NULL DEFAULT 0, -- 0 (received), 1 (appended to file)
  PRIMARY KEY (file_id, chunk_number)
) WITHOUT ROWID;

CREATE TABLE connections ( -- all SMP agent connections
  connection_id INTEGER PRIMARY KEY,
  agent_conn_id BLOB NOT NULL UNIQUE,
  conn_level INTEGER NOT NULL DEFAULT 0,
  via_contact INTEGER REFERENCES contacts (contact_id),
  conn_status TEXT NOT NULL,
  conn_type TEXT NOT NULL, -- contact, member, rcv_file, snd_file
  contact_id INTEGER REFERENCES contacts ON DELETE RESTRICT,
  group_member_id INTEGER REFERENCES group_members ON DELETE RESTRICT,
  snd_file_id INTEGER,
  rcv_file_id INTEGER REFERENCES rcv_files (file_id) ON DELETE RESTRICT,
  created_at TEXT NOT NULL DEFAULT (datetime('now')),
  user_id INTEGER NOT NULL REFERENCES users,
  FOREIGN KEY (snd_file_id, connection_id)
    REFERENCES snd_files (file_id, connection_id)
    ON DELETE RESTRICT
    DEFERRABLE INITIALLY DEFERRED
);

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

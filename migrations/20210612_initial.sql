CREATE TABLE contact_profiles ( -- remote user profile
  contact_profile_id INTEGER PRIMARY KEY,
  contact_ref TEXT NOT NULL, -- this name must not contain spaces
  properties TEXT NOT NULL DEFAULT '{}' -- JSON with user or contact profile
);

-- the first record (id = 1) is reserved for the local user
INSERT INTO contact_profiles (contact_profile_id, contact_ref) VALUES (1, '');

CREATE TABLE known_servers(
  server_id INTEGER PRIMARY KEY,
  host TEXT NOT NULL,
  port TEXT NOT NULL,
  key_hash BLOB,
  UNIQUE (host, port)
) WITHOUT ROWID;

CREATE TABLE contacts (
  contact_id INTEGER PRIMARY KEY,
  contact_ref TEXT NOT NULL UNIQUE, -- contact name set by local user - must be unique
  properties TEXT NOT NULL DEFAULT '{}', -- JSON set by local user
  contact_profile_id INTEGER REFERENCES contact_profiles, -- sent by remote contact
  contact_status TEXT NOT NULL DEFAULT ''
);

-- the first record (id = 1) is reserved for the local user
INSERT INTO contacts (contact_id, contact_ref, contact_profile_id) VALUES (1, '', 1);

CREATE TABLE connections (
  connection_id INTEGER PRIMARY KEY,
  contact_id INTEGER NOT NULL REFERENCES contacts ON DELETE RESTRICT, -- connection must be removed first via the agent
  group_member_id INTEGER REFERENCES group_members ON DELETE RESTRICT, -- connection must be removed first via the agent
  agent_conn_id BLOB NOT NULL UNIQUE,
  conn_level INTEGER NOT NULL DEFAULT 0,
  via_conn BLOB REFERENCES contact_connections (connection_id),
  conn_status TEXT NOT NULL DEFAULT ''
);

CREATE TABLE contact_invitations (
  invitation_id INTEGER PRIMARY KEY,
  agent_inv_id BLOB NOT NULL UNIQUE,
  contact_id INTEGER NOT NULL REFERENCES contacts ON DELETE RESTRICT,
  invitation_status TEXT NOT NULL DEFAULT ''
);

CREATE TABLE group_profiles ( -- shared group profiles
  group_profile_id INTEGER PRIMARY KEY,
  group_ref TEXT NOT NULL, -- this name must not contain spaces
  properties TEXT NOT NULL DEFAULT '{}' -- JSON with user or contact profile
);

CREATE TABLE groups (
  group_id INTEGER PRIMARY KEY, -- local group ID
  group_ref TEXT NOT NULL UNIQUE, -- local group name without spaces
  properties TEXT NOT NULL, -- local JSON group properties
  group_profile_id INTEGER REFERENCES group_profiles -- shared group profile
);

CREATE TABLE group_members ( -- group members, nullable fields are NULL for the local user
  group_member_id INTEGER PRIMARY KEY,
  group_id INTEGER NOT NULL REFERENCES groups ON DELETE RESTRICT,
  member_id BLOB NOT NULL, -- shared member ID, unique per group
  member_role TEXT NOT NULL DEFAULT '', -- owner, admin, moderator, ''
  member_status TEXT NOT NULL DEFAULT '', -- inv | con | full | off
  contact_id INTEGER NOT NULL REFERENCES contacts ON DELETE RESTRICT,
  invited_by INTEGER REFERENCES contacts ON DELETE RESTRICT,
  connection_id INTEGER UNIQUE REFERENCES connections,
  UNIQUE (group_id, member_id)
);

CREATE TABLE group_member_connections (
  connection_id INTEGER PRIMARY KEY REFERENCES connections ON DELETE RESTRICT,
  group_member_id INTEGER NOT NULL REFERENCES group_members ON DELETE RESTRICT -- connection must be removed first via the agent
);


CREATE TABLE events ( -- messages received by the agent, append only
  event_id INTEGER PRIMARY KEY,
  agent_id INTEGER NOT NULL, -- internal ID
  external_agent_id INTEGER NOT NULL, -- internal ID
  agent_meta TEXT NOT NULL, -- JSON with timestamps and IDs sent in MSG
  connection_id BLOB NOT NULL,
  received INTEGER NOT NULL, -- 1 for received, 0 for sent
  event_type TEXT NOT NULL, -- event type - see protocol/types.ts
  event_encoding INTEGER NOT NULL, -- format of event_body: 0 - binary, 1 - text utf8, 2 - JSON (utf8)
  content_type TEXT NOT NULL, -- content type - see protocol/types.ts
  event_body BLOB, -- agent message body as sent
  created_at TEXT NOT NULL DEFAULT (datetime('now'))
);

CREATE TABLE group_events (
  event_id INTEGER PRIMARY KEY,
  group_member_id INTEGER NOT NULL REFERENCES group_members
);

CREATE TABLE profile_events (
  event_id INTEGER NOT NULL UNIQUE REFERENCES events,
  profile_id INTEGER NOT NULL REFERENCES contact_profiles
);

CREATE TABLE group_profile_events (
  event_id INTEGER NOT NULL UNIQUE REFERENCES events,
  group_profile_id INTEGER NOT NULL REFERENCES group_profiles
);

CREATE TABLE group_event_parents (
  event_id INTEGER PRIMARY KEY,
  group_id INTEGER NOT NULL REFERENCES groups,
  parent_event_id INTEGER NOT NULL REFERENCES events (event_id),
  parent_member_id BLOB NOT NULL,
  parent_event_hash BLOB NOT NULL,
  FOREIGN KEY (group_id, parent_member_id)
    REFERENCES group_members (group_id, member_id)
);

CREATE TABLE blobs (
  blob_id INTEGER PRIMARY KEY,
  content BLOB NOT NULL
);

CREATE TABLE messages ( -- mutable messages presented to user
  message_id INTEGER PRIMARY KEY,
  contact_id INTEGER NOT NULL REFERENCES contacts ON DELETE RESTRICT, -- 1 for sent messages
  group_id INTEGER REFERENCES groups ON DELETE RESTRICT, -- only for group messages
  deleted INTEGER NOT NULL, -- 1 for deleted
  msg_type TEXT NOT NULL,
  content_type TEXT NOT NULL,
  msg_text TEXT NOT NULL, -- textual representation
  msg_props TEXT NOT NULL, -- JSON
  msg_blob_id INTEGER REFERENCES blobs (blob_id) ON DELETE RESTRICT -- optional binary content
);

CREATE TABLE message_events (
  event_id INTEGER NOT NULL UNIQUE REFERENCES events,
  message_id INTEGER NOT NULL REFERENCES messages
);

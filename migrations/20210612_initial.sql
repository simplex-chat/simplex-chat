CREATE TABLE profiles (
  profile_id INTEGER NOT NULL PRIMARY KEY,
  display_name TEXT NOT NULL,
  properties TEXT NOT NULL -- JSON with user or contact profile
);

CREATE TABLE known_servers(
  host TEXT NOT NULL,
  port TEXT NOT NULL,
  key_hash BLOB,
  PRIMARY KEY (host, port)
) WITHOUT ROWID;

CREATE TABLE contacts (
  contact_id INTEGER NOT NULL PRIMARY KEY,
  display_name TEXT NOT NULL, -- display name set by local user
  properties TEXT NOT NULL, -- JSON set by local user
  profile_id INTEGER REFERENCES profiles, -- sent by remote contact
  contact_status TEXT NOT NULL DEFAULT ''
);

CREATE TABLE contact_connections {
  connection_id BLOB NOT NULL PRIMARY KEY,
  contact_id INTEGER NOT NULL REFERENCES contacts ON DELETE RESTRICT,
  conn_level INTEGER NOT NULL DEFAULT 0,
  via_conn BLOB REFERENCES contact_connections (connection_id),
  conn_status TEXT NOT NULL DEFAULT ''
} WITHOUT ROWID;

CREATE TABLE contact_invitations {
  invitation_id BLOB NOT NULL PRIMARY KEY,
  contact_id INTEGER NOT NULL REFERENCES contacts ON DELETE RESTRICT,
  invitation_status TEXT NOT NULL DEFAULT ''
} WITHOUT ROWID;

CREATE TABLE groups (
  group_id INTEGER NOT NULL PRIMARY KEY, -- local group ID
  display_name TEXT NOT NULL, -- shared
  properties TEXT NOT NULL -- shared JSON group profile
);

CREATE TABLE group_members ( -- group members, nullable fields are NULL for the local user
  group_id INTEGER NOT NULL REFERENCES groups ON DELETE RESTRICT,
  member_id BLOB NOT NULL, -- shared member ID, unique per group
  member_role TEXT NOT NULL DEFAULT '', -- owner, admin, moderator, ''
  member_status TEXT NOT NULL DEFAULT '', -- inv | con | full | off
  contact_id INTEGER REFERENCES contacts ON DELETE RESTRICT,
  invited_by INTEGER REFERENCES contacts ON DELETE RESTRICT,
  connection_id BLOB UNIQUE,
  PRIMARY KEY (group_id, member_id)
) WITHOUT ROWID;

CREATE TABLE events ( -- messages received by the agent, append only
  event_id INTEGER NOT NULL PRIMARY KEY
  message_id INTEGER REFERENCES messages ON DELETE CASCADE
  agent_id INTEGER NOT NULL, -- internal ID
  external_agent_id INTEGER NOT NULL, -- internal ID
  agent_meta TEXT NOT NULL, -- JSON with timestamps and IDs sent in MSG
  connection_id BLOB NOT NULL,
  received INTEGER NOT NULL, -- 1 for received, 0 for sent
  event_type TEXT NOT NULL, -- event type - see protocol/types.ts
  event_encoding INTEGER NOT NULL, -- format of event_body: 0 - binary, 1 - text utf8, 2 - JSON (utf8)
  content_type TEXT NOT NULL, -- content type - see protocol/types.ts
  event_body BLOB, -- agent message body as sent
  created_at TEXT NOT NULL DEFAULT datetime('now'),
);

CREATE TABLE group_events (
  event_id INTEGER NOT NULL PRIMARY KEY,
  group_id INTEGER NOT NULL REFERENCES groups,
  member_id BLOB NOT NULL,
  FOREIGN KEY (group_id, member_id) REFERENCES group_members
);

CREATE TABLE group_event_parents (
  event_id INTEGER NOT NULL PRIMARY KEY,
  group_id INTEGER NOT NULL REFERENCES groups,
  parent_event_id INTEGER NOT NULL REFERENCES events (event_id),
  parent_member_id BLOB NOT NULL
  parent_event_hash BLOB NOT NULL
  FOREIGN KEY (group_id, parent_member_id)
    REFERENCES group_members (group_id, member_id)
);

CREATE TABLE messages ( -- mutable messages presented to user
  message_id INTEGER NOT NULL PRIMARY KEY
  contact_id INTEGER REFERENCES contacts ON DELETE RESTRICT -- NULL for sent messages
  group_id INTEGER REFERENCES groups ON DELETE RESTRICT -- only for group messages
  deleted INTEGER NOT NULL, -- 1 for deleted
  msg_type TEXT NOT NULL
  content_type TEXT NOT NULL
  msg_text TEXT NOT NULL, -- textual representation
  msg_props TEXT NOT NULL, -- JSON
  msg_blob_id INTEGER REFERENCES blobs (blob_id) ON DELETE RESTRICT, -- optional binary content
  created_at TEXT NOT NULL DEFAULT datetime('now'),
  created_event_id INTEGER NOT NULL REFERENCES events (event_id) ON DELETE RESTRICT,
  updated_at TEXT,
  updated_event_id INTEGER REFERENCES events (event_id) ON DELETE RESTRICT,
);

CREATE TABLE blobs (
  blob_id INTEGER NOT NULL PRIMARY KEY
  content BLOB NOT NULL
);

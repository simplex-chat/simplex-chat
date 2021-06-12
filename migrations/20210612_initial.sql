CREATE TABLE profiles (
  profile_id INTEGER NOT NULL PRIMARY KEY,
  display_name TEXT NOT NULL,
  properties TEXT NOT NULL -- JSON with user or contact profile
);

CREATE TABLE users (
  user_id INTEGER NOT NULL PRIMARY KEY, -- local user ID
  profile_id INTEGER REFERENCES profiles
);

CREATE TABLE known_servers(
  host TEXT NOT NULL,
  port TEXT NOT NULL,
  key_hash BLOB,
  user_id INTEGER NOT NULL PRIMARY KEY,
  PRIMARY KEY (host, port)
) WITHOUT ROWID;

CREATE TABLE contacts (
  contact_id INTEGER NOT NULL PRIMARY KEY,
  user_id INTEGER NOT NULL REFERENCES users,
  display_name TEXT NOT NULL, -- display name set by local user
  properties TEXT NOT NULL, -- JSON set by local user
  profile_id INTEGER REFERENCES profiles -- sent by remote contact
);

CREATE TABLE contact_connections {
  connection_id BLOB NOT NULL PRIMARY KEY,
  contact_id INTEGER NOT NULL REFERENCES contacts ON DELETE SET NULL,
  conn_level INTEGER NOT NULL DEFAULT 0,
  via_conn BLOB REFERENCES contact_connections (connection_id),
  conn_status TEXT NOT NULL DEFAULT ''
} WITHOUT ROWID;

CREATE TABLE groups (
  group_id INTEGER NOT NULL PRIMARY KEY, -- local group ID
  user_id INTEGER NOT NULL REFERENCES users,
  member_id BLOB NOT NULL, -- shared member ID of the user, unique per group
  display_name TEXT NOT NULL, -- shared
  properties TEXT NOT NULL -- shared JSON group profile
);

CREATE TABLE group_members (
  group_id INTEGER NOT NULL REFERENCES groups ON DELETE RESTRICT,
  group_member_id BLOB NOT NULL, -- shared member ID, unique per group
  contact_id INTEGER NOT NULL REFERENCES contacts ON DELETE RESTRICT,
  connection_id BLOB NOT NULL UNIQUE,
  member_role TEXT NOT NULL DEFAULT '', -- owner, admin, moderator, ''
  member_status TEXT NOT NULL DEFAULT '', -- inv | con | full | off
  PRIMARY KEY (group_id, group_member_id)
) WITHOUT ROWID;

CREATE TABLE user_contact_links (
  user_contact_link_id INTEGER PRIMARY KEY,
  conn_req_contact BLOB NOT NULL,
  local_display_name TEXT,
  created_at TEXT NOT NULL DEFAULT (datetime('now')),
  user_id INTEGER NOT NULL REFERENCES users,
  UNIQUE (user_id, local_display_name)
);

CREATE TABLE contact_requests (
  contact_request_id INTEGER PRIMARY KEY,
  user_contact_link_id INTEGER NOT NULL REFERENCES user_contact_links,
  agent_invitation_id  BLOB NOT NULL,
  contact_profile_id INTEGER REFERENCES contact_profiles, -- NULL if it's an incognito profile
  local_display_name TEXT NOT NULL,
  created_at TEXT NOT NULL DEFAULT (datetime('now')),
  user_id INTEGER NOT NULL REFERENCES users,
  FOREIGN KEY (user_id, local_display_name)
    REFERENCES display_names (user_id, local_display_name)
    ON DELETE RESTRICT
    ON UPDATE CASCADE,
  UNIQUE (user_id, local_display_name),
  UNIQUE (user_id, contact_profile_id)
);

ALTER TABLE connections ADD user_contact_link_id INTEGER
REFERENCES user_contact_links ON DELETE RESTRICT;

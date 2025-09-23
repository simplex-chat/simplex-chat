{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.SQLite.Migrations.M20250922_remove_contact_merge where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20250922_remove_contact_merge :: Query
m20250922_remove_contact_merge =
  [sql|
-- 1. Remove contacts.via_group field - logic it was used for has been deprecated in 5d18a49.

DROP INDEX idx_contacts_via_group;
ALTER TABLE contacts DROP COLUMN via_group;


-- 2. Remove duplicate contact and member connections, keeping only one connection
--    per entity according to old retrieval logic. See getContact_, groupMemberQuery in removed diff.

WITH ranked_contact_connections AS (
  SELECT
    c.connection_id,
    ROW_NUMBER() OVER (
      PARTITION BY c.user_id, c.contact_id
      ORDER BY
        CASE WHEN c.conn_status = 'ready' OR c.conn_status = 'snd-ready' THEN 1 ELSE 0 END DESC,
        c.created_at DESC
    ) AS rn
  FROM connections c
  WHERE c.contact_id IS NOT NULL
)
DELETE FROM connections
WHERE connection_id IN (
  SELECT connection_id
  FROM ranked_contact_connections
  WHERE rn > 1
);

WITH ranked_group_member_connections AS (
  SELECT
    c.connection_id,
    ROW_NUMBER() OVER (
      PARTITION BY c.user_id, c.group_member_id
      ORDER BY c.connection_id DESC
    ) AS rn
  FROM connections c
  WHERE c.group_member_id IS NOT NULL
)
DELETE FROM connections
WHERE connection_id IN (
  SELECT connection_id
  FROM ranked_group_member_connections
  WHERE rn > 1
);


-- 3. Remove obsolete file connections - they were used for old file protocol.

DELETE FROM connections WHERE snd_file_id IS NOT NULL;
DELETE FROM connections WHERE rcv_file_id IS NOT NULL;


-- 4. Recreate connections table without snd_file_id and rcv_file_id. Note that:
--    - primary key is recreated as AUTOINCREMENT,
--    - CHECK constraints are replaced with NOT NULL.

PRAGMA foreign_keys = OFF;

CREATE TABLE new_connections(
  connection_id INTEGER PRIMARY KEY AUTOINCREMENT,
  agent_conn_id BLOB NOT NULL UNIQUE,
  conn_level INTEGER NOT NULL DEFAULT 0,
  via_contact INTEGER REFERENCES contacts(contact_id) ON DELETE SET NULL,
  conn_status TEXT NOT NULL,
  conn_type TEXT NOT NULL,
  user_contact_link_id INTEGER REFERENCES user_contact_links ON DELETE CASCADE,
  contact_id INTEGER REFERENCES contacts ON DELETE CASCADE,
  group_member_id INTEGER REFERENCES group_members ON DELETE CASCADE,
  created_at TEXT NOT NULL DEFAULT(datetime('now')),
  user_id INTEGER NOT NULL REFERENCES users ON DELETE CASCADE,
  updated_at TEXT NOT NULL DEFAULT(datetime('now')),
  via_contact_uri_hash BLOB,
  xcontact_id BLOB,
  via_user_contact_link INTEGER DEFAULT NULL REFERENCES user_contact_links(user_contact_link_id) ON DELETE SET NULL,
  custom_user_profile_id INTEGER REFERENCES contact_profiles ON DELETE SET NULL,
  conn_req_inv BLOB,
  local_alias TEXT NOT NULL DEFAULT '',
  via_group_link INTEGER NOT NULL DEFAULT 0,
  group_link_id BLOB,
  security_code TEXT NULL,
  security_code_verified_at TEXT NULL,
  auth_err_counter INTEGER NOT NULL DEFAULT 0,
  peer_chat_min_version INTEGER NOT NULL DEFAULT 1,
  peer_chat_max_version INTEGER NOT NULL DEFAULT 1,
  to_subscribe INTEGER DEFAULT 0 NOT NULL,
  contact_conn_initiated INTEGER NOT NULL DEFAULT 0,
  conn_chat_version INTEGER,
  pq_support INTEGER NOT NULL DEFAULT 0,
  pq_encryption INTEGER NOT NULL DEFAULT 0,
  pq_snd_enabled INTEGER,
  pq_rcv_enabled INTEGER,
  quota_err_counter INTEGER NOT NULL DEFAULT 0,
  short_link_inv BLOB,
  via_short_link_contact BLOB,
  via_contact_uri BLOB
);

INSERT INTO new_connections (
  connection_id,
  agent_conn_id,
  conn_level,
  via_contact,
  conn_status,
  conn_type,
  user_contact_link_id,
  contact_id,
  group_member_id,
  created_at,
  user_id,
  updated_at,
  via_contact_uri_hash,
  xcontact_id,
  via_user_contact_link,
  custom_user_profile_id,
  conn_req_inv,
  local_alias,
  via_group_link,
  group_link_id,
  security_code,
  security_code_verified_at,
  auth_err_counter,
  peer_chat_min_version,
  peer_chat_max_version,
  to_subscribe,
  contact_conn_initiated,
  conn_chat_version,
  pq_support,
  pq_encryption,
  pq_snd_enabled,
  pq_rcv_enabled,
  quota_err_counter,
  short_link_inv,
  via_short_link_contact,
  via_contact_uri
)
SELECT
  connection_id,
  agent_conn_id,
  conn_level,
  via_contact,
  conn_status,
  conn_type,
  user_contact_link_id,
  contact_id,
  group_member_id,
  created_at,
  user_id,
  updated_at,
  via_contact_uri_hash,
  xcontact_id,
  via_user_contact_link,
  custom_user_profile_id,
  conn_req_inv,
  local_alias,
  via_group_link,
  group_link_id,
  security_code,
  security_code_verified_at,
  auth_err_counter,
  peer_chat_min_version,
  peer_chat_max_version,
  to_subscribe,
  contact_conn_initiated,
  conn_chat_version,
  pq_support,
  pq_encryption,
  pq_snd_enabled,
  pq_rcv_enabled,
  quota_err_counter,
  short_link_inv,
  via_short_link_contact,
  via_contact_uri
FROM connections;

DROP TABLE connections;

ALTER TABLE new_connections RENAME TO connections;

PRAGMA foreign_keys = ON;


-- 5. Recreate indexes on connections table - they were dropped when recreating table.
--    Note that indexes on contact_id and group_member_id are recreated as UNIQUE
--    to enforce the new one-connection-per-entity logic.

CREATE UNIQUE INDEX idx_connections_contact_id ON connections(contact_id);
CREATE UNIQUE INDEX idx_connections_group_member_id ON connections(group_member_id);
CREATE UNIQUE INDEX idx_connections_group_member ON connections(user_id, group_member_id);
CREATE INDEX idx_connections_custom_user_profile_id ON connections(custom_user_profile_id);
CREATE INDEX idx_connections_via_user_contact_link ON connections(via_user_contact_link);
CREATE INDEX idx_connections_user_contact_link_id ON connections(user_contact_link_id);
CREATE INDEX idx_connections_via_contact ON connections(via_contact);
CREATE INDEX idx_connections_to_subscribe ON connections(to_subscribe);
CREATE INDEX idx_connections_conn_req_inv ON connections(user_id, conn_req_inv);
CREATE INDEX idx_connections_via_contact_uri_hash ON connections(user_id, via_contact_uri_hash);
CREATE INDEX idx_connections_updated_at ON connections(user_id, updated_at);
|]

down_m20250922_remove_contact_merge :: Query
down_m20250922_remove_contact_merge =
  [sql|
-- 1. Restore contact.via_group field.

ALTER TABLE contacts ADD COLUMN via_group INTEGER REFERENCES groups(group_id) ON DELETE SET NULL;
CREATE INDEX idx_contacts_via_group ON contacts(via_group);


-- 2. Restore connections table with old definition.

PRAGMA foreign_keys = OFF;

CREATE TABLE old_connections(
  connection_id INTEGER PRIMARY KEY,
  agent_conn_id BLOB NOT NULL UNIQUE,
  conn_level INTEGER NOT NULL DEFAULT 0,
  via_contact INTEGER REFERENCES contacts(contact_id) ON DELETE SET NULL,
  conn_status TEXT NOT NULL,
  conn_type TEXT NOT NULL,
  user_contact_link_id INTEGER REFERENCES user_contact_links ON DELETE CASCADE,
  contact_id INTEGER REFERENCES contacts ON DELETE CASCADE,
  group_member_id INTEGER REFERENCES group_members ON DELETE CASCADE,
  snd_file_id INTEGER,
  rcv_file_id INTEGER REFERENCES rcv_files(file_id) ON DELETE CASCADE,
  created_at TEXT NOT NULL DEFAULT(datetime('now')),
  user_id INTEGER NOT NULL REFERENCES users ON DELETE CASCADE,
  updated_at TEXT CHECK(updated_at NOT NULL),
  via_contact_uri_hash BLOB,
  xcontact_id BLOB,
  via_user_contact_link INTEGER DEFAULT NULL REFERENCES user_contact_links(user_contact_link_id) ON DELETE SET NULL,
  custom_user_profile_id INTEGER REFERENCES contact_profiles ON DELETE SET NULL,
  conn_req_inv BLOB,
  local_alias DEFAULT '' CHECK(local_alias NOT NULL),
  via_group_link INTEGER DEFAULT 0 CHECK(via_group_link NOT NULL),
  group_link_id BLOB,
  security_code TEXT NULL,
  security_code_verified_at TEXT NULL,
  auth_err_counter INTEGER DEFAULT 0 CHECK(auth_err_counter NOT NULL),
  peer_chat_min_version INTEGER NOT NULL DEFAULT 1,
  peer_chat_max_version INTEGER NOT NULL DEFAULT 1,
  to_subscribe INTEGER DEFAULT 0 NOT NULL,
  contact_conn_initiated INTEGER NOT NULL DEFAULT 0,
  conn_chat_version INTEGER,
  pq_support INTEGER NOT NULL DEFAULT 0,
  pq_encryption INTEGER NOT NULL DEFAULT 0,
  pq_snd_enabled INTEGER,
  pq_rcv_enabled INTEGER,
  quota_err_counter INTEGER NOT NULL DEFAULT 0,
  short_link_inv BLOB,
  via_short_link_contact BLOB,
  via_contact_uri BLOB,
  FOREIGN KEY(snd_file_id, connection_id) REFERENCES snd_files(file_id, connection_id) ON DELETE CASCADE DEFERRABLE INITIALLY DEFERRED
);

INSERT INTO old_connections (
  connection_id,
  agent_conn_id,
  conn_level,
  via_contact,
  conn_status,
  conn_type,
  user_contact_link_id,
  contact_id,
  group_member_id,
  created_at,
  user_id,
  updated_at,
  via_contact_uri_hash,
  xcontact_id,
  via_user_contact_link,
  custom_user_profile_id,
  conn_req_inv,
  local_alias,
  via_group_link,
  group_link_id,
  security_code,
  security_code_verified_at,
  auth_err_counter,
  peer_chat_min_version,
  peer_chat_max_version,
  to_subscribe,
  contact_conn_initiated,
  conn_chat_version,
  pq_support,
  pq_encryption,
  pq_snd_enabled,
  pq_rcv_enabled,
  quota_err_counter,
  short_link_inv,
  via_short_link_contact,
  via_contact_uri
)
SELECT
  connection_id,
  agent_conn_id,
  conn_level,
  via_contact,
  conn_status,
  conn_type,
  user_contact_link_id,
  contact_id,
  group_member_id,
  created_at,
  user_id,
  updated_at,
  via_contact_uri_hash,
  xcontact_id,
  via_user_contact_link,
  custom_user_profile_id,
  conn_req_inv,
  local_alias,
  via_group_link,
  group_link_id,
  security_code,
  security_code_verified_at,
  auth_err_counter,
  peer_chat_min_version,
  peer_chat_max_version,
  to_subscribe,
  contact_conn_initiated,
  conn_chat_version,
  pq_support,
  pq_encryption,
  pq_snd_enabled,
  pq_rcv_enabled,
  quota_err_counter,
  short_link_inv,
  via_short_link_contact,
  via_contact_uri
FROM connections;

DROP TABLE connections;

ALTER TABLE old_connections RENAME TO connections;

PRAGMA foreign_keys = ON;


-- 3. Restore connections table indexes as they were prior to forward migration.
--    Note that compared to forward migration we additionally restore index on rcv_file_id.

CREATE INDEX idx_connections_contact_id ON connections(contact_id);
CREATE INDEX idx_connections_group_member_id ON connections(group_member_id);
CREATE INDEX idx_connections_group_member ON connections(user_id, group_member_id);
CREATE INDEX idx_connections_custom_user_profile_id ON connections(custom_user_profile_id);
CREATE INDEX idx_connections_via_user_contact_link ON connections(via_user_contact_link);
CREATE INDEX idx_connections_rcv_file_id ON connections(rcv_file_id);
CREATE INDEX idx_connections_user_contact_link_id ON connections(user_contact_link_id);
CREATE INDEX idx_connections_via_contact ON connections(via_contact);
CREATE INDEX idx_connections_to_subscribe ON connections(to_subscribe);
CREATE INDEX idx_connections_conn_req_inv ON connections(user_id, conn_req_inv);
CREATE INDEX idx_connections_via_contact_uri_hash ON connections(user_id, via_contact_uri_hash);
CREATE INDEX idx_connections_updated_at ON connections(user_id, updated_at);
|]

{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.SQLite.Migrations.M20250922_remove_unused_connections where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20250922_remove_unused_connections :: Query
m20250922_remove_unused_connections =
  [sql|
DELETE FROM connections WHERE snd_file_id IS NOT NULL;
DELETE FROM connections WHERE rcv_file_id IS NOT NULL;

DROP TABLE snd_file_chunks;

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

DROP INDEX idx_connections_contact_id;
DROP INDEX idx_connections_group_member_id;

CREATE UNIQUE INDEX idx_connections_contact_id ON connections(contact_id);
CREATE UNIQUE INDEX idx_connections_group_member_id ON connections(group_member_id);

DROP INDEX idx_connections_to_subscribe;
CREATE INDEX idx_connections_to_subscribe ON connections(user_id, to_subscribe);

DROP INDEX idx_contacts_via_group;
ALTER TABLE contacts DROP COLUMN via_group;
|]

down_m20250922_remove_unused_connections :: Query
down_m20250922_remove_unused_connections =
  [sql|
CREATE TABLE snd_file_chunks(
  file_id INTEGER NOT NULL,
  connection_id INTEGER NOT NULL,
  chunk_number INTEGER NOT NULL,
  chunk_agent_msg_id INTEGER,
  chunk_sent INTEGER NOT NULL DEFAULT 0,
  created_at TEXT CHECK(created_at NOT NULL),
  updated_at TEXT CHECK(updated_at NOT NULL),
  FOREIGN KEY(file_id, connection_id) REFERENCES snd_files ON DELETE CASCADE,
  PRIMARY KEY(file_id, connection_id, chunk_number)
) STRICT, WITHOUT ROWID;

CREATE INDEX idx_snd_file_chunks_file_id_connection_id ON snd_file_chunks(file_id, connection_id);

DROP INDEX idx_connections_contact_id;
DROP INDEX idx_connections_group_member_id;

CREATE INDEX idx_connections_contact_id ON connections(contact_id);
CREATE INDEX idx_connections_group_member_id ON connections(group_member_id);

DROP INDEX idx_connections_to_subscribe;
CREATE INDEX idx_connections_to_subscribe ON connections(to_subscribe);

ALTER TABLE contacts ADD COLUMN via_group INTEGER REFERENCES groups(group_id) ON DELETE SET NULL;
CREATE INDEX idx_contacts_via_group ON contacts(via_group);
|]

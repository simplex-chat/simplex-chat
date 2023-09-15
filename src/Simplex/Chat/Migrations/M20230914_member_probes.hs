{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Migrations.M20230914_member_probes where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20230903_connections_to_subscribe :: Query
m20230903_connections_to_subscribe =
  [sql|
-- sent_probes
ALTER TABLE sent_probes ADD COLUMN probe_contact_id INTEGER UNIQUE REFERENCES contacts ON DELETE CASCADE;
ALTER TABLE sent_probes ADD COLUMN probe_group_member_id INTEGER UNIQUE REFERENCES group_members ON DELETE CASCADE;
UPDATE sent_probes SET probe_contact_id = contact_id;

ALTER TABLE sent_probes DROP COLUMN contact_id;

CREATE INDEX idx_sent_probes_user_id ON sent_probes(user_id);
CREATE INDEX idx_sent_probes_probe_contact_id ON sent_probes(probe_contact_id);
CREATE INDEX idx_sent_probes_probe_group_member_id ON sent_probes(probe_group_member_id);

-- sent_probe_hashes
ALTER TABLE sent_probe_hashes ADD COLUMN probe_contact_id INTEGER UNIQUE REFERENCES contacts ON DELETE CASCADE,
ALTER TABLE sent_probe_hashes ADD COLUMN probe_group_member_id INTEGER UNIQUE REFERENCES group_members ON DELETE CASCADE,
UPDATE sent_probe_hashes SET probe_contact_id = contact_id;

DROP INDEX idx_sent_probe_hashes_contact_id ON sent_probe_hashes(contact_id);
ALTER TABLE sent_probe_hashes DROP COLUMN contact_id;

CREATE INDEX idx_sent_probe_hashes_probe_contact_id ON sent_probe_hashes(probe_contact_id);
CREATE INDEX idx_sent_probe_hashes_probe_group_member_id ON sent_probe_hashes(probe_group_member_id);

-- received_probes
ALTER TABLE received_probes ADD COLUMN probe_contact_id INTEGER UNIQUE REFERENCES contacts ON DELETE CASCADE,
ALTER TABLE received_probes ADD COLUMN probe_group_member_id INTEGER UNIQUE REFERENCES group_members ON DELETE CASCADE,
UPDATE received_probes SET probe_contact_id = contact_id;

DROP INDEX idx_received_probes_contact_id ON received_probes(contact_id);
ALTER TABLE received_probes DROP COLUMN contact_id;

CREATE INDEX idx_received_probes_probe_contact_id ON received_probes(probe_contact_id);
CREATE INDEX idx_received_probes_probe_group_member_id ON received_probes(probe_group_member_id);
|]

down_m20230903_connections_to_subscribe :: Query
down_m20230903_connections_to_subscribe =
  [sql|
DROP INDEX idx_sent_probes_user_id;
DROP INDEX idx_sent_probes_probe_contact_id;
DROP INDEX idx_sent_probes_probe_group_member_id;
-- DROP INDEX idx_sent_probe_hashes_probe_contact_id;
DROP INDEX idx_sent_probe_hashes_probe_group_member_id;
-- DROP INDEX idx_received_probes_probe_contact_id;
DROP INDEX idx_received_probes_probe_group_member_id;

DELETE FROM sent_probes WHERE probe_contact_id IS NULL;
DELETE FROM sent_probe_hashes WHERE probe_contact_id IS NULL;
DELETE FROM received_probes WHERE probe_contact_id IS NULL;

ALTER TABLE sent_probes RENAME COLUMN probe_contact_id TO contact_id;
ALTER TABLE sent_probe_hashes RENAME COLUMN probe_contact_id TO contact_id;
ALTER TABLE received_probes RENAME COLUMN probe_contact_id TO contact_id;

CREATE INDEX idx_received_probes_user_id ON received_probes(user_id);
-- CREATE INDEX idx_received_probes_contact_id ON received_probes(contact_id);
CREATE INDEX idx_sent_probe_hashes_user_id ON sent_probe_hashes(user_id);
-- CREATE INDEX idx_sent_probe_hashes_contact_id ON sent_probe_hashes(contact_id);
|]


CREATE TABLE sent_probes(
  sent_probe_id INTEGER PRIMARY KEY,
  contact_id INTEGER NOT NULL UNIQUE REFERENCES contacts ON DELETE CASCADE,
  probe BLOB NOT NULL,
  user_id INTEGER NOT NULL REFERENCES users ON DELETE CASCADE,
  created_at TEXT CHECK(created_at NOT NULL),
  updated_at TEXT CHECK(updated_at NOT NULL),
  UNIQUE(user_id, probe)
);
CREATE TABLE sent_probe_hashes(
  sent_probe_hash_id INTEGER PRIMARY KEY,
  sent_probe_id INTEGER NOT NULL REFERENCES sent_probes ON DELETE CASCADE,
  contact_id INTEGER NOT NULL REFERENCES contacts ON DELETE CASCADE,
  user_id INTEGER NOT NULL REFERENCES users ON DELETE CASCADE,
  created_at TEXT CHECK(created_at NOT NULL),
  updated_at TEXT CHECK(updated_at NOT NULL),
  UNIQUE(sent_probe_id, contact_id)
);
CREATE TABLE received_probes(
  received_probe_id INTEGER PRIMARY KEY,
  contact_id INTEGER NOT NULL REFERENCES contacts ON DELETE CASCADE,
  probe BLOB,
  probe_hash BLOB NOT NULL,
  user_id INTEGER NOT NULL REFERENCES users ON DELETE CASCADE
  ,
  created_at TEXT CHECK(created_at NOT NULL),
  updated_at TEXT CHECK(updated_at NOT NULL)
);
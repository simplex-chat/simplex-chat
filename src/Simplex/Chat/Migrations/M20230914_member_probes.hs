{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Migrations.M20230914_member_probes where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20230914_member_probes :: Query
m20230914_member_probes =
  [sql|
CREATE TABLE sent_probes_v2(
  sent_probe_id INTEGER PRIMARY KEY,
  contact_id INTEGER REFERENCES contacts ON DELETE CASCADE,
  group_member_id INTEGER REFERENCES group_members ON DELETE CASCADE,
  probe BLOB NOT NULL,
  user_id INTEGER NOT NULL REFERENCES users ON DELETE CASCADE,
  created_at TEXT CHECK(created_at NOT NULL),
  updated_at TEXT CHECK(updated_at NOT NULL),
  UNIQUE(user_id, probe)
);

CREATE TABLE sent_probe_hashes_v2(
  sent_probe_hash_id INTEGER PRIMARY KEY,
  sent_probe_id INTEGER NOT NULL REFERENCES sent_probes ON DELETE CASCADE,
  contact_id INTEGER REFERENCES contacts ON DELETE CASCADE,
  group_member_id INTEGER REFERENCES group_members ON DELETE CASCADE,
  user_id INTEGER NOT NULL REFERENCES users ON DELETE CASCADE,
  created_at TEXT CHECK(created_at NOT NULL),
  updated_at TEXT CHECK(updated_at NOT NULL)
);

CREATE TABLE received_probes_v2(
  received_probe_id INTEGER PRIMARY KEY,
  contact_id INTEGER REFERENCES contacts ON DELETE CASCADE,
  group_member_id INTEGER REFERENCES group_members ON DELETE CASCADE,
  probe BLOB,
  probe_hash BLOB NOT NULL,
  user_id INTEGER NOT NULL REFERENCES users ON DELETE CASCADE,
  created_at TEXT CHECK(created_at NOT NULL),
  updated_at TEXT CHECK(updated_at NOT NULL)
);

INSERT INTO sent_probes_v2
  (sent_probe_id, contact_id, probe, user_id, created_at, updated_at)
SELECT
  sent_probe_id, contact_id, probe, user_id, created_at, updated_at
  FROM sent_probes;

INSERT INTO sent_probe_hashes_v2
  (sent_probe_hash_id, sent_probe_id, contact_id, user_id, created_at, updated_at)
SELECT
  sent_probe_hash_id, sent_probe_id, contact_id, user_id, created_at, updated_at
  FROM sent_probe_hashes;

INSERT INTO received_probes_v2
  (received_probe_id, contact_id, probe, probe_hash, user_id, created_at, updated_at)
SELECT
  received_probe_id, contact_id, probe, probe_hash, user_id, created_at, updated_at
  FROM received_probes;

DROP INDEX idx_sent_probe_hashes_user_id;
DROP INDEX idx_sent_probe_hashes_contact_id;
DROP INDEX idx_received_probes_user_id;
DROP INDEX idx_received_probes_contact_id;

DROP TABLE sent_probes;
DROP TABLE sent_probe_hashes;
DROP TABLE received_probes;

CREATE INDEX idx_sent_probes_v2_user_id ON sent_probes_v2(user_id);
CREATE INDEX idx_sent_probes_v2_contact_id ON sent_probes_v2(contact_id);
CREATE INDEX idx_sent_probes_v2_group_member_id ON sent_probes_v2(group_member_id);
CREATE INDEX idx_sent_probes_v2_probe ON sent_probes_v2(probe);

CREATE INDEX idx_sent_probe_hashes_v2_user_id ON sent_probe_hashes_v2(user_id);
CREATE INDEX idx_sent_probe_hashes_v2_sent_probe_id ON sent_probe_hashes_v2(sent_probe_id);
CREATE INDEX idx_sent_probe_hashes_v2_contact_id ON sent_probe_hashes_v2(contact_id);
CREATE INDEX idx_sent_probe_hashes_v2_group_member_id ON sent_probe_hashes_v2(group_member_id);

CREATE INDEX idx_received_probes_v2_user_id ON received_probes_v2(user_id);
CREATE INDEX idx_received_probes_v2_contact_id ON received_probes_v2(contact_id);
CREATE INDEX idx_received_probes_v2_probe ON received_probes_v2(probe);
CREATE INDEX idx_received_probes_v2_probe_hash ON received_probes_v2(probe_hash);
|]

down_m20230914_member_probes :: Query
down_m20230914_member_probes =
  [sql|
CREATE TABLE sent_probes(
  sent_probe_id INTEGER PRIMARY KEY,
  contact_id INTEGER NOT NULL REFERENCES contacts ON DELETE CASCADE,
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
);

CREATE TABLE received_probes(
  received_probe_id INTEGER PRIMARY KEY,
  contact_id INTEGER NOT NULL REFERENCES contacts ON DELETE CASCADE,
  probe BLOB,
  probe_hash BLOB NOT NULL,
  user_id INTEGER NOT NULL REFERENCES users ON DELETE CASCADE,
  created_at TEXT CHECK(created_at NOT NULL),
  updated_at TEXT CHECK(updated_at NOT NULL)
);

DELETE FROM sent_probes_v2 WHERE contact_id IS NULL;
DELETE FROM sent_probe_hashes_v2 WHERE contact_id IS NULL;
DELETE FROM received_probes_v2 WHERE contact_id IS NULL;

INSERT INTO sent_probes
  (sent_probe_id, contact_id, probe, user_id, created_at, updated_at)
SELECT
  sent_probe_id, contact_id, probe, user_id, created_at, updated_at
  FROM sent_probes_v2;

INSERT INTO sent_probe_hashes
  (sent_probe_hash_id, sent_probe_id, contact_id, user_id, created_at, updated_at)
SELECT
  sent_probe_hash_id, sent_probe_id, contact_id, user_id, created_at, updated_at
  FROM sent_probe_hashes_v2;

INSERT INTO received_probes
  (received_probe_id, contact_id, probe, probe_hash, user_id, created_at, updated_at)
SELECT
  received_probe_id, contact_id, probe, probe_hash, user_id, created_at, updated_at
  FROM received_probes_v2;

DROP INDEX idx_sent_probes_v2_user_id;
DROP INDEX idx_sent_probes_v2_contact_id;
DROP INDEX idx_sent_probes_v2_group_member_id;
DROP INDEX idx_sent_probes_v2_probe;

DROP INDEX idx_sent_probe_hashes_v2_user_id;
DROP INDEX idx_sent_probe_hashes_v2_sent_probe_id;
DROP INDEX idx_sent_probe_hashes_v2_contact_id;
DROP INDEX idx_sent_probe_hashes_v2_group_member_id;

DROP INDEX idx_received_probes_v2_user_id;
DROP INDEX idx_received_probes_v2_contact_id;
DROP INDEX idx_received_probes_v2_probe;
DROP INDEX idx_received_probes_v2_probe_hash;

DROP TABLE sent_probes_v2;
DROP TABLE sent_probe_hashes_v2;
DROP TABLE received_probes_v2;

CREATE INDEX idx_received_probes_user_id ON received_probes(user_id);
CREATE INDEX idx_received_probes_contact_id ON received_probes(contact_id);
CREATE INDEX idx_sent_probe_hashes_user_id ON sent_probe_hashes(user_id);
CREATE INDEX idx_sent_probe_hashes_contact_id ON sent_probe_hashes(contact_id);
|]

{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.SQLite.Migrations.M20230914_member_probes where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20230914_member_probes :: Query
m20230914_member_probes =
  [sql|
CREATE TABLE new__sent_probes(
  sent_probe_id INTEGER PRIMARY KEY,
  contact_id INTEGER REFERENCES contacts ON DELETE CASCADE,
  group_member_id INTEGER REFERENCES group_members ON DELETE CASCADE,
  probe BLOB NOT NULL,
  user_id INTEGER NOT NULL REFERENCES users ON DELETE CASCADE,
  created_at TEXT CHECK(created_at NOT NULL),
  updated_at TEXT CHECK(updated_at NOT NULL),
  UNIQUE(user_id, probe)
);

CREATE TABLE new__sent_probe_hashes(
  sent_probe_hash_id INTEGER PRIMARY KEY,
  sent_probe_id INTEGER NOT NULL REFERENCES new__sent_probes ON DELETE CASCADE,
  contact_id INTEGER REFERENCES contacts ON DELETE CASCADE,
  group_member_id INTEGER REFERENCES group_members ON DELETE CASCADE,
  user_id INTEGER NOT NULL REFERENCES users ON DELETE CASCADE,
  created_at TEXT CHECK(created_at NOT NULL),
  updated_at TEXT CHECK(updated_at NOT NULL)
);

CREATE TABLE new__received_probes(
  received_probe_id INTEGER PRIMARY KEY,
  contact_id INTEGER REFERENCES contacts ON DELETE CASCADE,
  group_member_id INTEGER REFERENCES group_members ON DELETE CASCADE,
  probe BLOB,
  probe_hash BLOB NOT NULL,
  user_id INTEGER NOT NULL REFERENCES users ON DELETE CASCADE,
  created_at TEXT CHECK(created_at NOT NULL),
  updated_at TEXT CHECK(updated_at NOT NULL)
);

INSERT INTO new__sent_probes
  (sent_probe_id, contact_id, probe, user_id, created_at, updated_at)
SELECT
  sent_probe_id, contact_id, probe, user_id, created_at, updated_at
  FROM sent_probes;

INSERT INTO new__sent_probe_hashes
  (sent_probe_hash_id, sent_probe_id, contact_id, user_id, created_at, updated_at)
SELECT
  sent_probe_hash_id, sent_probe_id, contact_id, user_id, created_at, updated_at
  FROM sent_probe_hashes;

INSERT INTO new__received_probes
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

ALTER TABLE new__sent_probes RENAME TO sent_probes;
ALTER TABLE new__sent_probe_hashes RENAME TO sent_probe_hashes;
ALTER TABLE new__received_probes RENAME TO received_probes;

CREATE INDEX idx_sent_probes_user_id ON sent_probes(user_id);
CREATE INDEX idx_sent_probes_contact_id ON sent_probes(contact_id);
CREATE INDEX idx_sent_probes_group_member_id ON sent_probes(group_member_id);

CREATE INDEX idx_sent_probe_hashes_user_id ON sent_probe_hashes(user_id);
CREATE INDEX idx_sent_probe_hashes_sent_probe_id ON sent_probe_hashes(sent_probe_id);
CREATE INDEX idx_sent_probe_hashes_contact_id ON sent_probe_hashes(contact_id);
CREATE INDEX idx_sent_probe_hashes_group_member_id ON sent_probe_hashes(group_member_id);

CREATE INDEX idx_received_probes_user_id ON received_probes(user_id);
CREATE INDEX idx_received_probes_contact_id ON received_probes(contact_id);
CREATE INDEX idx_received_probes_probe ON received_probes(probe);
CREATE INDEX idx_received_probes_probe_hash ON received_probes(probe_hash);
|]

down_m20230914_member_probes :: Query
down_m20230914_member_probes =
  [sql|
CREATE TABLE old__sent_probes(
  sent_probe_id INTEGER PRIMARY KEY,
  contact_id INTEGER NOT NULL REFERENCES contacts ON DELETE CASCADE,
  probe BLOB NOT NULL,
  user_id INTEGER NOT NULL REFERENCES users ON DELETE CASCADE,
  created_at TEXT CHECK(created_at NOT NULL),
  updated_at TEXT CHECK(updated_at NOT NULL),
  UNIQUE(user_id, probe)
);

CREATE TABLE old__sent_probe_hashes(
  sent_probe_hash_id INTEGER PRIMARY KEY,
  sent_probe_id INTEGER NOT NULL REFERENCES old__sent_probes ON DELETE CASCADE,
  contact_id INTEGER NOT NULL REFERENCES contacts ON DELETE CASCADE,
  user_id INTEGER NOT NULL REFERENCES users ON DELETE CASCADE,
  created_at TEXT CHECK(created_at NOT NULL),
  updated_at TEXT CHECK(updated_at NOT NULL)
);

CREATE TABLE old__received_probes(
  received_probe_id INTEGER PRIMARY KEY,
  contact_id INTEGER NOT NULL REFERENCES contacts ON DELETE CASCADE,
  probe BLOB,
  probe_hash BLOB NOT NULL,
  user_id INTEGER NOT NULL REFERENCES users ON DELETE CASCADE,
  created_at TEXT CHECK(created_at NOT NULL),
  updated_at TEXT CHECK(updated_at NOT NULL)
);

DELETE FROM sent_probes WHERE contact_id IS NULL;
DELETE FROM sent_probe_hashes WHERE contact_id IS NULL;
DELETE FROM received_probes WHERE contact_id IS NULL;

INSERT INTO old__sent_probes
  (sent_probe_id, contact_id, probe, user_id, created_at, updated_at)
SELECT
  sent_probe_id, contact_id, probe, user_id, created_at, updated_at
  FROM sent_probes;

INSERT INTO old__sent_probe_hashes
  (sent_probe_hash_id, sent_probe_id, contact_id, user_id, created_at, updated_at)
SELECT
  sent_probe_hash_id, sent_probe_id, contact_id, user_id, created_at, updated_at
  FROM sent_probe_hashes;

INSERT INTO old__received_probes
  (received_probe_id, contact_id, probe, probe_hash, user_id, created_at, updated_at)
SELECT
  received_probe_id, contact_id, probe, probe_hash, user_id, created_at, updated_at
  FROM received_probes;

DROP INDEX idx_sent_probes_user_id;
DROP INDEX idx_sent_probes_contact_id;
DROP INDEX idx_sent_probes_group_member_id;

DROP INDEX idx_sent_probe_hashes_user_id;
DROP INDEX idx_sent_probe_hashes_sent_probe_id;
DROP INDEX idx_sent_probe_hashes_contact_id;
DROP INDEX idx_sent_probe_hashes_group_member_id;

DROP INDEX idx_received_probes_user_id;
DROP INDEX idx_received_probes_contact_id;
DROP INDEX idx_received_probes_probe;
DROP INDEX idx_received_probes_probe_hash;

DROP TABLE sent_probes;
DROP TABLE sent_probe_hashes;
DROP TABLE received_probes;

ALTER TABLE old__sent_probes RENAME TO sent_probes;
ALTER TABLE old__sent_probe_hashes RENAME TO sent_probe_hashes;
ALTER TABLE old__received_probes RENAME TO received_probes;

CREATE INDEX idx_received_probes_user_id ON received_probes(user_id);
CREATE INDEX idx_received_probes_contact_id ON received_probes(contact_id);
CREATE INDEX idx_sent_probe_hashes_user_id ON sent_probe_hashes(user_id);
CREATE INDEX idx_sent_probe_hashes_contact_id ON sent_probe_hashes(contact_id);
|]

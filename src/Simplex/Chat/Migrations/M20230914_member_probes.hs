{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Migrations.M20230914_member_probes where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20230914_member_probes :: Query
m20230914_member_probes =
  [sql|
ALTER TABLE group_members ADD COLUMN sent_probe BLOB;
ALTER TABLE group_members ADD COLUMN received_probe BLOB;
ALTER TABLE group_members ADD COLUMN received_probe_hash BLOB;

CREATE INDEX idx_group_members_sent_probe ON group_members(sent_probe);
CREATE INDEX idx_group_members_received_probe_hash ON group_members(received_probe_hash);
CREATE INDEX idx_sent_probes_probe ON sent_probes(probe);
CREATE INDEX idx_received_probes_probe_hash ON received_probes(probe_hash);
|]

down_m20230914_member_probes :: Query
down_m20230914_member_probes =
  [sql|
DROP INDEX idx_group_members_sent_probe;
DROP INDEX idx_group_members_received_probe_hash;
DROP INDEX idx_sent_probes_probe;
DROP INDEX idx_received_probes_probe_hash;

ALTER TABLE group_members DROP COLUMN sent_probe;
ALTER TABLE group_members DROP COLUMN received_probe;
ALTER TABLE group_members DROP COLUMN received_probe_hash;
|]

{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Migrations.M20230914_member_probes where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20230914_member_probes :: Query
m20230914_member_probes =
  [sql|
-- sent_probes
ALTER TABLE sent_probes ADD COLUMN probe_contact_id INTEGER REFERENCES contacts(contact_id) ON DELETE CASCADE;
ALTER TABLE sent_probes ADD COLUMN probe_group_member_id INTEGER REFERENCES group_members(group_member_id) ON DELETE CASCADE;
UPDATE sent_probes SET probe_contact_id = contact_id;

-- ALTER TABLE sent_probes DROP COLUMN contact_id;

CREATE INDEX idx_sent_probes_user_id ON sent_probes(user_id);
CREATE UNIQUE INDEX idx_sent_probes_probe_contact_id ON sent_probes(probe_contact_id);
CREATE UNIQUE INDEX idx_sent_probes_probe_group_member_id ON sent_probes(probe_group_member_id);
CREATE INDEX idx_sent_probes_probe ON sent_probes(probe);

-- received_probes
ALTER TABLE received_probes ADD COLUMN probe_contact_id INTEGER REFERENCES contacts(contact_id) ON DELETE CASCADE;
ALTER TABLE received_probes ADD COLUMN probe_group_member_id INTEGER REFERENCES group_members(group_member_id) ON DELETE CASCADE;
UPDATE received_probes SET probe_contact_id = contact_id;

DROP INDEX idx_received_probes_contact_id;
-- ALTER TABLE received_probes DROP COLUMN contact_id;

CREATE UNIQUE INDEX idx_received_probes_probe_contact_id ON received_probes(probe_contact_id);
CREATE UNIQUE INDEX idx_received_probes_probe_group_member_id ON received_probes(probe_group_member_id);
CREATE INDEX idx_received_probes_probe_hash ON received_probes(probe_hash);
|]

down_m20230914_member_probes :: Query
down_m20230914_member_probes =
  [sql|
DROP INDEX idx_sent_probes_user_id;
DROP INDEX idx_sent_probes_probe_contact_id;
DROP INDEX idx_sent_probes_probe_group_member_id;
DROP INDEX idx_sent_probes_probe;

DROP INDEX idx_received_probes_probe_contact_id;
DROP INDEX idx_received_probes_probe_group_member_id;
DROP INDEX idx_received_probes_probe_hash;

DELETE FROM sent_probes WHERE probe_contact_id IS NULL;
DELETE FROM received_probes WHERE probe_contact_id IS NULL;

-- ALTER TABLE sent_probes RENAME COLUMN probe_contact_id TO contact_id;
-- ALTER TABLE received_probes RENAME COLUMN probe_contact_id TO contact_id;

CREATE INDEX idx_received_probes_contact_id ON received_probes(contact_id);
|]

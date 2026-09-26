{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.SQLite.Migrations.M20260925_member_badge_proofs where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20260925_member_badge_proofs :: Query
m20260925_member_badge_proofs =
  [sql|
PRAGMA writable_schema=1;

UPDATE sqlite_master
SET sql = replace(sql, 'file_id INTEGER NOT NULL REFERENCES files', 'file_id INTEGER REFERENCES files')
WHERE name = 'file_badge_proofs' AND type = 'table';

PRAGMA writable_schema=RESET;

ALTER TABLE file_badge_proofs ADD COLUMN group_member_id INTEGER REFERENCES group_members ON DELETE CASCADE;

CREATE UNIQUE INDEX idx_file_badge_proofs_group_member_id ON file_badge_proofs(group_member_id);
|]

down_m20260925_member_badge_proofs :: Query
down_m20260925_member_badge_proofs =
  [sql|
DROP INDEX idx_file_badge_proofs_group_member_id;

DELETE FROM file_badge_proofs WHERE group_member_id IS NOT NULL;

ALTER TABLE file_badge_proofs DROP COLUMN group_member_id;

PRAGMA writable_schema=1;

UPDATE sqlite_master
SET sql = replace(sql, 'file_id INTEGER REFERENCES files', 'file_id INTEGER NOT NULL REFERENCES files')
WHERE name = 'file_badge_proofs' AND type = 'table';

PRAGMA writable_schema=RESET;
|]

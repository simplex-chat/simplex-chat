{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.SQLite.Migrations.M20251117_member_relations_vector where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20251117_member_relations_vector :: Query
m20251117_member_relations_vector =
  [sql|
ALTER TABLE group_members ADD COLUMN index_in_group INTEGER NOT NULL DEFAULT 0;

ALTER TABLE groups ADD COLUMN member_index INTEGER NOT NULL DEFAULT 0;

ALTER TABLE group_members ADD COLUMN member_relations_vector BLOB;

CREATE INDEX tmp_idx_group_members_group_id_group_member_id ON group_members(group_id, group_member_id);

CREATE TEMPORARY TABLE tmp_members_indexed AS
SELECT
  group_member_id,
  ROW_NUMBER() OVER (
    PARTITION BY group_id
    ORDER BY group_member_id ASC
  ) - 1 AS idx_in_group
FROM group_members;

CREATE INDEX tmp_idx_members_indexed ON tmp_members_indexed(group_member_id);

UPDATE group_members AS gm
SET index_in_group = (
  SELECT idx_in_group
  FROM tmp_members_indexed
  WHERE tmp_members_indexed.group_member_id = gm.group_member_id
);

DROP INDEX tmp_idx_group_members_group_id_group_member_id;
DROP INDEX tmp_idx_members_indexed;
DROP TABLE tmp_members_indexed;

CREATE UNIQUE INDEX idx_group_members_group_id_index_in_group ON group_members(group_id, index_in_group);

UPDATE groups AS g
SET member_index = COALESCE((
  SELECT MAX(index_in_group) + 1
  FROM group_members
  WHERE group_members.group_id = g.group_id
), 0);

UPDATE group_members
SET member_relations_vector = x''
WHERE group_id IN (
  SELECT mu.group_id
  FROM group_members mu
  WHERE mu.member_category = 'user'
    AND (
      mu.member_role NOT IN (CAST('admin' AS BLOB), CAST('owner' AS BLOB))
      OR mu.member_status IN ('removed', 'left', 'deleted')
    )
);
|]

down_m20251117_member_relations_vector :: Query
down_m20251117_member_relations_vector =
  [sql|
DROP INDEX idx_group_members_group_id_index_in_group;

ALTER TABLE group_members DROP COLUMN index_in_group;

ALTER TABLE groups DROP COLUMN member_index;

ALTER TABLE group_members DROP COLUMN member_relations_vector;
|]

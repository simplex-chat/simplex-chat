{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.Postgres.Migrations.M20251117_member_relations_vector where

import Data.Text (Text)
import qualified Data.Text as T
import Text.RawString.QQ (r)

m20251117_member_relations_vector :: Text
m20251117_member_relations_vector =
  T.pack
    [r|
ALTER TABLE group_members ADD COLUMN index_in_group BIGINT NOT NULL DEFAULT 0;

ALTER TABLE groups ADD COLUMN member_index BIGINT NOT NULL DEFAULT 0;

ALTER TABLE group_members ADD COLUMN member_relations_vector BYTEA;

CREATE INDEX tmp_idx_group_members_group_id_group_member_id ON group_members(group_id, group_member_id);

CREATE TEMPORARY TABLE tmp_members_numbered AS
SELECT
  group_member_id,
  ROW_NUMBER() OVER (
    PARTITION BY group_id
    ORDER BY group_member_id ASC
  ) AS rn
FROM group_members;

CREATE INDEX tmp_idx_members_numbered ON tmp_members_numbered(group_member_id);

UPDATE group_members AS gm
SET index_in_group = n.rn
FROM tmp_members_numbered n
WHERE n.group_member_id = gm.group_member_id;

DROP INDEX tmp_idx_group_members_group_id_group_member_id;
DROP INDEX tmp_idx_members_numbered;
DROP TABLE tmp_members_numbered;

CREATE UNIQUE INDEX idx_group_members_group_id_index_in_group ON group_members(group_id, index_in_group);

UPDATE groups g
SET member_index = COALESCE((
  SELECT MAX(index_in_group)
  FROM group_members
  WHERE group_members.group_id = g.group_id
), 0);
|]

down_m20251117_member_relations_vector :: Text
down_m20251117_member_relations_vector =
  T.pack
    [r|
DROP INDEX idx_group_members_group_id_index_in_group;

ALTER TABLE group_members DROP COLUMN index_in_group;

ALTER TABLE groups DROP COLUMN member_index;

ALTER TABLE group_members DROP COLUMN member_relations_vector;
|]

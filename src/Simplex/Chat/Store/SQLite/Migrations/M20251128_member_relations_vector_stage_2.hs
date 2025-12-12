{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.SQLite.Migrations.M20251128_member_relations_vector_stage_2 where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

-- Build member_relations_vector for all members that don't have it yet.
-- Uses custom aggregate function migrate_relations_vector defined in M20251117_member_relations_vector.
--
-- Query returns (idx, direction, intro_status) for each introduction:
-- - direction 0 (IDSubjectIntroduced): current member (subject) is re_group_member_id, was introduced to referenced member
-- - direction 1 (IDReferencedIntroduced): current member (subject) is to_group_member_id, referenced member was introduced to it

m20251128_member_relations_vector_stage_2 :: Query
m20251128_member_relations_vector_stage_2 =
  [sql|
UPDATE group_members
SET member_relations_vector = (
  SELECT migrate_relations_vector(idx, direction, intro_status)
  FROM (
    SELECT m.index_in_group AS idx, 0 AS direction, i.intro_status
    FROM group_member_intros i
    JOIN group_members m ON m.group_member_id = i.to_group_member_id
    WHERE i.re_group_member_id = group_members.group_member_id
    UNION ALL
    SELECT m.index_in_group AS idx, 1 AS direction, i.intro_status
    FROM group_member_intros i
    JOIN group_members m ON m.group_member_id = i.re_group_member_id
    WHERE i.to_group_member_id = group_members.group_member_id
  )
)
WHERE member_relations_vector IS NULL;

DROP INDEX idx_group_member_intros_to_group_member_id;
DROP INDEX idx_group_member_intros_re_group_member_id;
DROP INDEX idx_pending_group_messages_group_member_intro_id;
ALTER TABLE pending_group_messages DROP COLUMN group_member_intro_id;
DROP TABLE group_member_intros;
|]

down_m20251128_member_relations_vector_stage_2 :: Query
down_m20251128_member_relations_vector_stage_2 =
  [sql|
CREATE TABLE group_member_intros(
  group_member_intro_id INTEGER PRIMARY KEY,
  re_group_member_id INTEGER NOT NULL REFERENCES group_members(group_member_id) ON DELETE CASCADE,
  to_group_member_id INTEGER NOT NULL REFERENCES group_members(group_member_id) ON DELETE CASCADE,
  group_queue_info BLOB,
  direct_queue_info BLOB,
  intro_status TEXT NOT NULL,
  created_at TEXT CHECK(created_at NOT NULL),
  updated_at TEXT CHECK(updated_at NOT NULL),
  intro_chat_protocol_version INTEGER NOT NULL DEFAULT 3,
  UNIQUE(re_group_member_id, to_group_member_id)
);
ALTER TABLE pending_group_messages ADD COLUMN group_member_intro_id INTEGER REFERENCES group_member_intros ON DELETE CASCADE;
CREATE INDEX idx_group_member_intros_to_group_member_id ON group_member_intros(to_group_member_id);
CREATE INDEX idx_group_member_intros_re_group_member_id ON group_member_intros(re_group_member_id);
CREATE INDEX idx_pending_group_messages_group_member_intro_id ON pending_group_messages(group_member_intro_id);
|]

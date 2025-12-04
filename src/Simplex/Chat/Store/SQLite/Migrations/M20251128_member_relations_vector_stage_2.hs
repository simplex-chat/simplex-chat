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

-- TODO [relations vector] drop group_member_intros in the end of migration
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
|]

-- TODO [relations vector] re-create group_member_intros
down_m20251128_member_relations_vector_stage_2 :: Query
down_m20251128_member_relations_vector_stage_2 =
  [sql|

|]

{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.SQLite.Migrations.M20251128_member_relations_vector_stage_2 where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

-- TODO [relations vector] drop group_member_intros in the end of migration
-- Build member_relations_vector for all members that don't have it yet.
-- Vector encoding: byte at index i = relation value for member with index_in_group = i.
-- Values: 0 = MRNew, 1 = MRIntroduced, 2 = MRIntroducedTo, 3 = MRConnected.
--
-- Relations as mapped in UNION of introductions:
-- - Introduced: current member is to_group_member_id, status not connected;
-- - IntroducedTo: current member is re_group_member_id, status not connected;
-- - Connected: current member is to_group_member_id/re_group_member_id, status connected.
--
-- This migration uses custom aggregate function build_relations_vector(idx, relation).
-- It is registered on DB open in Simplex.Messaging.Agent.Store.SQLite `connectDB`.
m20251128_member_relations_vector_stage_2 :: Query
m20251128_member_relations_vector_stage_2 =
  [sql|
UPDATE group_members
SET member_relations_vector = COALESCE(
  (
    SELECT build_relations_vector(idx, relation)
    FROM (
      SELECT m.index_in_group AS idx, 1 AS relation
      FROM group_member_intros i
      JOIN group_members m ON m.group_member_id = i.re_group_member_id
      WHERE i.to_group_member_id = group_members.group_member_id
        AND i.intro_status NOT IN ('re-con', 'to-con', 'con')
      UNION ALL
      SELECT m.index_in_group AS idx, 2 AS relation
      FROM group_member_intros i
      JOIN group_members m ON m.group_member_id = i.to_group_member_id
      WHERE i.re_group_member_id = group_members.group_member_id
        AND i.intro_status NOT IN ('re-con', 'to-con', 'con')
      UNION ALL
      SELECT m.index_in_group AS idx, 3 AS relation
      FROM group_member_intros i
      JOIN group_members m ON m.group_member_id = i.re_group_member_id
      WHERE i.to_group_member_id = group_members.group_member_id
        AND i.intro_status IN ('re-con', 'to-con', 'con')
      UNION ALL
      SELECT m.index_in_group AS idx, 3 AS relation
      FROM group_member_intros i
      JOIN group_members m ON m.group_member_id = i.to_group_member_id
      WHERE i.re_group_member_id = group_members.group_member_id
        AND i.intro_status IN ('re-con', 'to-con', 'con')
    )
  ),
  x''
)
WHERE member_relations_vector IS NULL;
|]

-- TODO [relations vector] re-create group_member_intros
down_m20251128_member_relations_vector_stage_2 :: Query
down_m20251128_member_relations_vector_stage_2 =
  [sql|

|]

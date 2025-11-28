{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.SQLite.Migrations.M20251128_member_relations_vector_stage_2 where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

-- TODO [relations vector] drop group_member_intros in the end of migration
-- Build member_relations_vector for all members that don't have it yet
-- Vector encoding: byte at index i = relation value for member with index_in_group = i
-- Values: 0 = MRNew, 1 = MRIntroduced, 2 = MRIntroducedTo, 3 = MRConnected
m20251128_member_relations_vector_stage_2 :: Query
m20251128_member_relations_vector_stage_2 =
  [sql|
UPDATE group_members
SET member_relations_vector = (
  WITH member_relations AS (
    -- Introduced: current member is to_group_member_id, status not connected
    SELECT m.index_in_group AS idx, 1 AS relation
    FROM group_member_intros i
    JOIN group_members m ON m.group_member_id = i.re_group_member_id
    WHERE i.to_group_member_id = group_members.group_member_id
      AND i.intro_status NOT IN ('re-con', 'to-con', 'con')
    UNION ALL
    -- IntroducedTo: current member is re_group_member_id, status not connected
    SELECT m.index_in_group AS idx, 2 AS relation
    FROM group_member_intros i
    JOIN group_members m ON m.group_member_id = i.to_group_member_id
    WHERE i.re_group_member_id = group_members.group_member_id
      AND i.intro_status NOT IN ('re-con', 'to-con', 'con')
    UNION ALL
    -- Connected: current member is to_group_member_id, status connected
    SELECT m.index_in_group AS idx, 3 AS relation
    FROM group_member_intros i
    JOIN group_members m ON m.group_member_id = i.re_group_member_id
    WHERE i.to_group_member_id = group_members.group_member_id
      AND i.intro_status IN ('re-con', 'to-con', 'con')
    UNION ALL
    -- Connected: current member is re_group_member_id, status connected
    SELECT m.index_in_group AS idx, 3 AS relation
    FROM group_member_intros i
    JOIN group_members m ON m.group_member_id = i.to_group_member_id
    WHERE i.re_group_member_id = group_members.group_member_id
      AND i.intro_status IN ('re-con', 'to-con', 'con')
  ),
  max_idx AS (
    SELECT MAX(idx) AS max_val FROM member_relations
  ),
  -- Build blob recursively by concatenating single-byte blobs
  -- X'00' = MRNew, X'01' = MRIntroduced, X'02' = MRIntroducedTo, X'03' = MRConnected
  blob_build(i, blob_val) AS (
    SELECT 0, X''
    UNION ALL
    SELECT i + 1,
      blob_val || CASE COALESCE((SELECT relation FROM member_relations WHERE idx = i), 0)
        WHEN 0 THEN X'00'
        WHEN 1 THEN X'01'
        WHEN 2 THEN X'02'
        WHEN 3 THEN X'03'
      END
    FROM blob_build, max_idx
    WHERE i <= max_val
  )
  SELECT CASE
    WHEN max_val IS NULL THEN X''
    ELSE (SELECT blob_val FROM blob_build WHERE i = max_val + 1)
  END
  FROM max_idx
)
WHERE member_relations_vector IS NULL;
|]

-- TODO [relations vector] re-create group_member_intros
down_m20251128_member_relations_vector_stage_2 :: Query
down_m20251128_member_relations_vector_stage_2 =
  [sql|

|]

{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.SQLite.Migrations.M20251128_member_relations_vector_stage_2 where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

-- TODO [relations vector] drop group_member_intros in the end of migration
-- Build member_relations_vector for all members that don't have it yet.
-- Vector byte encoding: 4 reserved | 1 direction | 3 status
-- Direction: 0 = IDSubjectIntroduced, 1 = IDReferencedIntroduced
-- Status values: 0 = MRNew, 1 = MRIntroduced, 2 = MRSubjectConnected, 3 = MRReferencedConnected, 4 = MRConnected
--
-- Query returns (idx, direction, intro_status) for each introduction:
-- - direction 0 (IDSubjectIntroduced): current member (subject) is re_group_member_id, was introduced to referenced member
-- - direction 1 (IDReferencedIntroduced): current member (subject) is to_group_member_id, referenced member was introduced to it
--
-- This migration uses custom aggregate function migrate_relations_vector(idx, direction, intro_status).
-- It is registered on DB open in Simplex.Messaging.Agent.Store.SQLite `connectDB`.
-- The aggregate transforms (direction, intro_status) into the final byte value:
-- - intro_status 'new'/'sent'/'rcv'/'fwd': MRIntroduced (1)
-- - intro_status 're-con': if direction=0 then MRSubjectConnected (2), else MRReferencedConnected (3)
-- - intro_status 'to-con': if direction=0 then MRReferencedConnected (3), else MRSubjectConnected (2)
-- - intro_status 'con': MRConnected (4)
--
-- Aggregate function implementation in Simplex.Messaging.Agent.Store.SQLite `connectDB`:
--
-- createAggregate db' "migrate_relations_vector" (Just 3) [] migrateRelationsVectorStep migrateRelationsVectorFinal
--   >>= either (throwIO . userError . show) pure
--
-- migrateRelationsVectorStep :: FuncContext -> FuncArgs -> [(Int64, IntroductionDirection, MemberRelation)] -> IO [(Int64, IntroductionDirection, MemberRelation)]
-- migrateRelationsVectorStep _ args acc = do
--   idx <- funcArgInt64 args 0
--   direction <- fromIntroductionInt . fromIntegral <$> funcArgInt64 args 1
--   introStatus <- funcArgText args 2
--   let relation = introStatusToRelation direction introStatus
--   pure $ (idx, direction, relation) : acc
--   where
--     introStatusToRelation dir status = case status of
--       "re-con" -> if dir == IDSubjectIntroduced then MRSubjectConnected else MRReferencedConnected
--       "to-con" -> if dir == IDSubjectIntroduced then MRReferencedConnected else MRSubjectConnected
--       "con" -> MRConnected
--       _ -> MRIntroduced -- 'new', 'sent', 'rcv', 'fwd'
--
-- migrateRelationsVectorFinal :: FuncContext -> [(Int64, IntroductionDirection, MemberRelation)] -> IO ()
-- migrateRelationsVectorFinal ctx acc = funcResultBlob ctx $ setNewRelations acc B.empty
--
m20251128_member_relations_vector_stage_2 :: Query
m20251128_member_relations_vector_stage_2 =
  [sql|
UPDATE group_members
SET member_relations_vector = COALESCE(
  (
    SELECT migrate_relations_vector(idx, direction, intro_status)
    FROM (
      -- Subject is re_group_member_id, direction = 0 (IDSubjectIntroduced)
      SELECT m.index_in_group AS idx, 0 AS direction, i.intro_status
      FROM group_member_intros i
      JOIN group_members m ON m.group_member_id = i.to_group_member_id
      WHERE i.re_group_member_id = group_members.group_member_id
      UNION ALL
      -- Subject is to_group_member_id, direction = 1 (IDReferencedIntroduced)
      SELECT m.index_in_group AS idx, 1 AS direction, i.intro_status
      FROM group_member_intros i
      JOIN group_members m ON m.group_member_id = i.re_group_member_id
      WHERE i.to_group_member_id = group_members.group_member_id
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

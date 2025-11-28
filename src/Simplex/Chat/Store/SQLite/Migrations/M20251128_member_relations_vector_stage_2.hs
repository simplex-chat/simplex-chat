{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.SQLite.Migrations.M20251128_member_relations_vector_stage_2 where

import qualified Data.ByteString as B
import Data.Int (Int64)
import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)
import Database.SQLite3 (FuncArgs, FuncContext, funcArgInt64, funcResultBlob)
import Simplex.Chat.Types.MemberRelations (MemberRelation (..), setRelations)

-- This migration uses custom aggregate function build_relations_vector(idx, relation).
-- Register it on DB open before running this migration:
--
-- > import Database.SQLite3 (createAggregate)
-- >
-- > createAggregate db "build_relations_vector" (Just 2) [] buildRelationsVectorStep buildRelationsVectorFinal
-- >   >>= either (throwIO . userError . show) pure

-- Functions below also to be moved near function registration code.

-- | Step function for build_relations_vector aggregate.
-- Accumulates (idx, relation) pairs.
buildRelationsVectorStep :: FuncContext -> FuncArgs -> [(Int64, MemberRelation)] -> IO [(Int64, MemberRelation)]
buildRelationsVectorStep _ args acc = do
  idx <- funcArgInt64 args 0
  relation <- toMemberRelation <$> funcArgInt64 args 1
  pure $ (idx, relation) : acc

-- | Final function for build_relations_vector aggregate.
-- Builds the vector from accumulated pairs using setRelations.
buildRelationsVectorFinal :: FuncContext -> [(Int64, MemberRelation)] -> IO ()
buildRelationsVectorFinal ctx acc = funcResultBlob ctx $ setRelations acc B.empty

toMemberRelation :: Int64 -> MemberRelation
toMemberRelation = \case
  1 -> MRIntroduced
  2 -> MRIntroducedTo
  3 -> MRConnected
  _ -> MRNew

-- TODO [relations vector] drop group_member_intros in the end of migration

-- Build member_relations_vector for all members that don't have it yet
-- Vector encoding: byte at index i = relation value for member with index_in_group = i
-- Values: 0 = MRNew, 1 = MRIntroduced, 2 = MRIntroducedTo, 3 = MRConnected
m20251128_member_relations_vector_stage_2 :: Query
m20251128_member_relations_vector_stage_2 =
  [sql|
UPDATE group_members
SET member_relations_vector = COALESCE(
  (
    SELECT build_relations_vector(idx, relation)
    FROM (
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

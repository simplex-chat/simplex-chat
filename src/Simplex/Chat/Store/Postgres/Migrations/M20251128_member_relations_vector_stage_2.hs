{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.Postgres.Migrations.M20251128_member_relations_vector_stage_2 where

import Data.Text (Text)
import qualified Data.Text as T
import Text.RawString.QQ (r)

-- TODO [relations vector] drop group_member_intros in the end of migration
m20251128_member_relations_vector_stage_2 :: Text
m20251128_member_relations_vector_stage_2 =
  T.pack
    [r|
CREATE FUNCTION build_relations_vector_step(state BYTEA, idx BIGINT, relation INT)
RETURNS BYTEA AS $$
DECLARE
  new_len INT;
  result BYTEA;
BEGIN
  IF idx < 0 THEN
    RETURN state;
  END IF;
  new_len := GREATEST(length(state), idx + 1);
  IF new_len > length(state) THEN
    result := state || repeat(E'\\x00', new_len - length(state))::BYTEA;
  ELSE
    result := state;
  END IF;
  result := set_byte(result, idx::INT, relation);
  RETURN result;
END;
$$ LANGUAGE plpgsql IMMUTABLE;

CREATE AGGREGATE build_relations_vector(BIGINT, INT) (
  SFUNC = build_relations_vector_step,
  STYPE = BYTEA,
  INITCOND = ''
);

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
    ) AS relations
  ),
  ''::bytea
)
WHERE member_relations_vector IS NULL;
|]

-- TODO [relations vector] re-create group_member_intros
down_m20251128_member_relations_vector_stage_2 :: Text
down_m20251128_member_relations_vector_stage_2 =
  T.pack
    [r|
DROP AGGREGATE build_relations_vector(BIGINT, INT);
DROP FUNCTION build_relations_vector_step(BYTEA, BIGINT, INT);
|]

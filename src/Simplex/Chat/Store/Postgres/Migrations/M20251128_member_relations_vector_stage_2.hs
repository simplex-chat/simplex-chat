{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.Postgres.Migrations.M20251128_member_relations_vector_stage_2 where

import Data.Text (Text)
import qualified Data.Text as T
import Text.RawString.QQ (r)

-- Build member_relations_vector for all members that don't have it yet.
-- Vector byte encoding: 4 reserved | 1 direction | 3 status
-- Direction: 0 = IDSubjectIntroduced, 1 = IDReferencedIntroduced
-- Status values: 0 = MRNew, 1 = MRIntroduced, 2 = MRSubjectConnected, 3 = MRReferencedConnected, 4 = MRConnected
--
-- Query returns (idx, direction, intro_status) for each introduction:
-- - direction 0 (IDSubjectIntroduced): current member (subject) is re_group_member_id, was introduced to referenced member
-- - direction 1 (IDReferencedIntroduced): current member (subject) is to_group_member_id, referenced member was introduced to it
--
-- The aggregate transforms intro_status into relation status:
-- - intro_status 'new'/'sent'/'rcv'/'fwd': MRIntroduced (1)
-- - intro_status 're-con': if direction=0 then MRSubjectConnected (2), else MRReferencedConnected (3)
-- - intro_status 'to-con': if direction=0 then MRReferencedConnected (3), else MRSubjectConnected (2)
-- - intro_status 'con': MRConnected (4)
--
-- Final byte combines direction and status: byte = (direction << 3) | status

-- TODO [relations vector] drop group_member_intros in the end of migration
m20251128_member_relations_vector_stage_2 :: Text
m20251128_member_relations_vector_stage_2 =
  T.pack
    [r|
CREATE FUNCTION migrate_relations_vector_step(state BYTEA, idx BIGINT, direction INT, intro_status TEXT)
RETURNS BYTEA AS $$
DECLARE
  new_len INT;
  result BYTEA;
  status INT;
  byte_val INT;
BEGIN
  IF idx < 0 THEN
    RETURN state;
  END IF;
  IF intro_status = 're-con' THEN
    IF direction = 0 THEN status := 2; ELSE status := 3; END IF;
  ELSIF intro_status = 'to-con' THEN
    IF direction = 0 THEN status := 3; ELSE status := 2; END IF;
  ELSIF intro_status = 'con' THEN
    status := 4;
  ELSE
    status := 1;
  END IF;
  byte_val := (direction * 8) + status;
  new_len := GREATEST(length(state), idx + 1);
  IF new_len > length(state) THEN
    result := state || (SELECT string_agg('\x00'::BYTEA, ''::BYTEA) FROM generate_series(1, new_len - length(state)));
  ELSE
    result := state;
  END IF;
  result := set_byte(result, idx::INT, byte_val);
  RETURN result;
END;
$$ LANGUAGE plpgsql IMMUTABLE;

CREATE AGGREGATE migrate_relations_vector(BIGINT, INT, TEXT) (
  SFUNC = migrate_relations_vector_step,
  STYPE = BYTEA,
  INITCOND = ''::BYTEA
);

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
  ) AS relations
)
WHERE member_relations_vector IS NULL;
|]

-- TODO [relations vector] re-create group_member_intros
down_m20251128_member_relations_vector_stage_2 :: Text
down_m20251128_member_relations_vector_stage_2 =
  T.pack
    [r|
DROP AGGREGATE migrate_relations_vector(BIGINT, INT, TEXT);
DROP FUNCTION migrate_relations_vector_step(BYTEA, BIGINT, INT, TEXT);
|]

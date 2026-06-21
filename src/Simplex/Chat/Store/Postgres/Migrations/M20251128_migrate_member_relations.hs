{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.Postgres.Migrations.M20251128_migrate_member_relations where

import Data.Text (Text)
import Text.RawString.QQ (r)

-- Build member_relations_vector for all members that don't have it yet.
-- Uses custom aggregate function migrate_relations_vector defined in M20251117_member_relations_vector.
--
-- Query returns (idx, direction, intro_status) for each introduction:
-- - direction 0 (IDSubjectIntroduced): current member (subject) is re_group_member_id, was introduced to referenced member
-- - direction 1 (IDReferencedIntroduced): current member (subject) is to_group_member_id, referenced member was introduced to it

m20251128_migrate_member_relations :: Text
m20251128_migrate_member_relations =
  [r|
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

DROP INDEX idx_pending_group_messages_group_member_intro_id;
ALTER TABLE pending_group_messages DROP COLUMN group_member_intro_id;
|]

down_m20251128_migrate_member_relations :: Text
down_m20251128_migrate_member_relations =
  [r|
ALTER TABLE pending_group_messages ADD COLUMN group_member_intro_id BIGINT REFERENCES group_member_intros ON DELETE CASCADE;
CREATE INDEX idx_pending_group_messages_group_member_intro_id ON pending_group_messages(group_member_intro_id);
|]

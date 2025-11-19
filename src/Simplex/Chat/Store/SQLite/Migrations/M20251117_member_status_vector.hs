{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.SQLite.Migrations.M20251117_member_status_vector where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

-- to do list:
-- - directory migration
--   - background process to set member_status_vector based on group_member_intros
--   - also set member_status_vector on forward (recipient list for sender is known there)
--   - take member locks when updating member_status_vector
--   - for duration of migration forwarding operates in 2 modes simultaneously:
--     - if member_status_vector is set, use it
--     - otherwise, use existing logic based on group_member_intros
--     - new invitees start with member_status_vector = 0 for all existing (pre) members ->
--       member_status_vector immediately can be used when new invitee sends
--     - pre members are not updated right away for new invitee, if their member_status_vector is not set yet,
--       as it will be costly to update them all at once; instead it will be set once background process processes them;
--       also this means group_member_intros have to be maintained for them until then
--     - GroupMember.memberStatusVector is Maybe to make this differentiation
-- - user clients migration
--   - once directory service migrates to new state, member_status_vector can be updated in db migration
--     as user clients wouldn't have as large group_member_intros
--   - TBC migration SQL
-- - alternative approach for member_status_vector migration (both directory and user clients):
--   - set to 0 for all existing members right away in sql migration
--     (possibly limit to groups where user is admin or above, otherwise NULL)
--   - means that initially after migration new messages will be forwarded to all members,
--     however they will quickly report connected state via XGrpMemCon -> member_status_vector will self-adjust
--   - allows for simple migration path, with immediate switch from group_member_intros,
--     avoids complexity of dual-mode forwarding during migration for directory / complex sql migration for user clients
-- - rework forwarding logic to use member_status_vector:
--   - create new members with correct sequential_id = group's last_member_sequential_id + 1,
--     maintain groups.last_member_sequential_id
--   - when new invitee joins, set member_status_vector to all 0 for them, update for pre members (set 0 for invitee's seq id)
--   - on XGrpMemCon update bitvectors for sender and referenced member (set 1 for corresponding seq ids)
--   - don't maintain group_member_intros (don't create, update status)
--   - on forwarding, get recipients based on sender's member_status_vector
--     - for all 0s in bitvector, get members by sequential_id in corresponding positions
-- - second use of group_member_intros is targeted introductions of knocking member to "remaining" members
--   - has to be reworked to not rely on group_member_intros
--   - one approach could be to introduce accepted member to all (so, repeatedly introduce to moderators),
--     this idea was tested in PR 6327
-- - another use of group_member_intros - createIntroductions, checkInverseIntro logic
--   - TBC how to avoid making redundant introductions between concurrently joining members
--   - second vector - for member introductions, or track in same vector
--   - when introducing to moderators only, do nothing - new moderators are introduced only to current members,
--     no pending in progress members, so race can't happen there
--   - when introducing to all, filter out members who already were introduced to this member
--   - can also solve previous issue of introducing remaining members in same way - don't introduce
--     to members this member already was introduced to
m20251117_member_status_vector :: Query
m20251117_member_status_vector =
  [sql|
ALTER TABLE group_members ADD COLUMN sequential_id INTEGER NOT NULL DEFAULT 0;

ALTER TABLE groups ADD COLUMN last_member_sequential_id INTEGER NOT NULL DEFAULT 0;

ALTER TABLE group_members ADD COLUMN member_status_vector BLOB;

CREATE INDEX tmp_idx_group_members_group_id_group_member_id ON group_members(group_id, group_member_id);

CREATE TABLE tmp_members_numbered AS
SELECT
  group_member_id,
  ROW_NUMBER() OVER (
    PARTITION BY group_id
    ORDER BY group_member_id ASC
  ) AS rn
FROM group_members;

CREATE INDEX tmp_idx_members_numbered ON tmp_members_numbered(group_member_id);

UPDATE group_members AS gm
SET sequential_id = (
  SELECT rn
  FROM tmp_members_numbered
  WHERE tmp_members_numbered.group_member_id = gm.group_member_id
);

DROP INDEX tmp_idx_group_members_group_id_group_member_id;
DROP INDEX tmp_idx_members_numbered;
DROP TABLE tmp_members_numbered;

CREATE UNIQUE INDEX idx_group_members_group_id_sequential_id ON group_members(group_id, sequential_id);

UPDATE groups AS g
SET last_member_sequential_id = COALESCE((
  SELECT MAX(sequential_id)
  FROM group_members
  WHERE group_members.group_id = g.group_id
), 0);

CREATE TRIGGER tr_update_group_last_member_sequential_id
AFTER INSERT ON group_members
FOR EACH ROW
BEGIN
  UPDATE groups
  SET last_member_sequential_id = NEW.sequential_id
  WHERE group_id = NEW.group_id;
END;
|]

down_m20251117_member_status_vector :: Query
down_m20251117_member_status_vector =
  [sql|
DROP TRIGGER tr_update_group_last_member_sequential_id;

DROP INDEX idx_group_members_group_id_sequential_id;

ALTER TABLE group_members DROP COLUMN sequential_id;

ALTER TABLE groups DROP COLUMN last_member_sequential_id;

ALTER TABLE group_members DROP COLUMN member_status_vector;
|]

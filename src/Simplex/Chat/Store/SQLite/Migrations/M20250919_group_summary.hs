{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.SQLite.Migrations.M20250919_group_summary where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20250919_group_summary :: Query
m20250919_group_summary =
  [sql|
ALTER TABLE groups ADD COLUMN summary_current_members_count INTEGER NOT NULL DEFAULT 0;
CREATE INDEX idx_groups_summary_current_members_count ON groups(summary_current_members_count);

CREATE TABLE group_member_status_predicates(
  member_status TEXT NOT NULL PRIMARY KEY,
  current_member INTEGER NOT NULL DEFAULT 0
);

INSERT INTO group_member_status_predicates(member_status, current_member)
VALUES
  ('rejected', 0),
  ('removed', 0),
  ('left', 0),
  ('deleted', 0),
  ('unknown', 0),
  ('invited', 0),
  ('pending_approval', 0),
  ('pending_review', 0),
  ('introduced', 1),
  ('intro-inv', 1),
  ('accepted', 1),
  ('announced', 1),
  ('connected', 1),
  ('complete', 1),
  ('creator', 1);

UPDATE groups
SET summary_current_members_count = c.cnt
FROM (
  SELECT m.group_id, COUNT(m.group_member_id) AS cnt
  FROM group_members m
  JOIN group_member_status_predicates p ON m.member_status = p.member_status
  WHERE p.current_member = 1
  GROUP BY m.group_id
) AS c
WHERE groups.group_id = c.group_id;

CREATE TRIGGER on_group_members_insert_update_summary
AFTER INSERT ON group_members
FOR EACH ROW
WHEN EXISTS (SELECT 1 FROM group_member_status_predicates WHERE member_status = NEW.member_status AND current_member = 1)
BEGIN
  UPDATE groups
  SET summary_current_members_count = summary_current_members_count + 1
  WHERE group_id = NEW.group_id;
END;

CREATE TRIGGER on_group_members_delete_update_summary
AFTER DELETE ON group_members
FOR EACH ROW
WHEN EXISTS (SELECT 1 FROM group_member_status_predicates WHERE member_status = OLD.member_status AND current_member = 1)
BEGIN
  UPDATE groups
  SET summary_current_members_count = summary_current_members_count - 1
  WHERE group_id = OLD.group_id;
END;

CREATE TRIGGER on_group_members_update_update_summary
AFTER UPDATE ON group_members
FOR EACH ROW
WHEN EXISTS (SELECT 1 FROM group_member_status_predicates WHERE member_status = OLD.member_status AND current_member = 1)
     != EXISTS (SELECT 1 FROM group_member_status_predicates WHERE member_status = NEW.member_status AND current_member = 1)
BEGIN
    UPDATE groups
    SET summary_current_members_count = summary_current_members_count +
        (
          CASE WHEN EXISTS (SELECT 1 FROM group_member_status_predicates WHERE member_status = NEW.member_status AND current_member = 1)
          THEN 1 ELSE -1 END
        )
    WHERE group_id = NEW.group_id;
END;
|]

down_m20250919_group_summary :: Query
down_m20250919_group_summary =
  [sql|
DROP TRIGGER on_group_members_insert_update_summary;
DROP TRIGGER on_group_members_delete_update_summary;
DROP TRIGGER on_group_members_update_update_summary;

DROP TABLE group_member_status_predicates;

DROP INDEX idx_groups_summary_current_members_count;
ALTER TABLE groups DROP COLUMN summary_current_members_count;
|]

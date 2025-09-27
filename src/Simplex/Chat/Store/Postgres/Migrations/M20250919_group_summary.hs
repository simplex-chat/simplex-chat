{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.Postgres.Migrations.M20250919_group_summary where

import Data.Text (Text)
import qualified Data.Text as T
import Text.RawString.QQ (r)

m20250919_group_summary :: Text
m20250919_group_summary =
  T.pack
    [r|
ALTER TABLE groups ADD COLUMN summary_current_members_count BIGINT NOT NULL DEFAULT 0;
CREATE INDEX idx_groups_summary_current_members_count ON groups(summary_current_members_count);

CREATE TABLE group_member_status_predicates(
  member_status TEXT NOT NULL PRIMARY KEY,
  current_member BIGINT NOT NULL DEFAULT 0
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
SET summary_current_members_count = (
    SELECT COUNT(m.group_member_id)
    FROM group_members m
    JOIN group_member_status_predicates p ON m.member_status = p.member_status
    WHERE m.group_id = groups.group_id
      AND p.current_member = 1
);

CREATE FUNCTION on_group_members_insert_update_summary() RETURNS TRIGGER
LANGUAGE plpgsql AS $$
BEGIN
  IF EXISTS (SELECT 1 FROM group_member_status_predicates WHERE member_status = NEW.member_status AND current_member = 1) THEN
    UPDATE groups
    SET summary_current_members_count = summary_current_members_count + 1
    WHERE group_id = NEW.group_id;
  END IF;
  RETURN NEW;
END;
$$;

CREATE FUNCTION on_group_members_delete_update_summary() RETURNS TRIGGER
LANGUAGE plpgsql AS $$
BEGIN
  IF EXISTS (SELECT 1 FROM group_member_status_predicates WHERE member_status = OLD.member_status AND current_member = 1) THEN
    UPDATE groups
    SET summary_current_members_count = summary_current_members_count - 1
    WHERE group_id = OLD.group_id;
  END IF;
  RETURN OLD;
END;
$$;

CREATE FUNCTION on_group_members_update_update_summary() RETURNS TRIGGER
LANGUAGE plpgsql AS $$
BEGIN
  IF EXISTS (SELECT 1 FROM group_member_status_predicates WHERE member_status = OLD.member_status AND current_member = 1)
      != EXISTS (SELECT 1 FROM group_member_status_predicates WHERE member_status = NEW.member_status AND current_member = 1) THEN
    UPDATE groups
    SET summary_current_members_count = summary_current_members_count +
        (
          CASE WHEN EXISTS (SELECT 1 FROM group_member_status_predicates WHERE member_status = NEW.member_status AND current_member = 1)
          THEN 1 ELSE -1 END
        )
    WHERE group_id = NEW.group_id;
  END IF;
  RETURN NEW;
END;
$$;

CREATE TRIGGER tr_group_members_insert_update_summary
AFTER INSERT ON group_members
FOR EACH ROW
EXECUTE FUNCTION on_group_members_insert_update_summary();

CREATE TRIGGER tr_group_members_delete_update_summary
AFTER DELETE ON group_members
FOR EACH ROW
EXECUTE FUNCTION on_group_members_delete_update_summary();

CREATE TRIGGER tr_group_members_update_update_summary
AFTER UPDATE ON group_members
FOR EACH ROW
EXECUTE FUNCTION on_group_members_update_update_summary();
|]

down_m20250919_group_summary :: Text
down_m20250919_group_summary =
  T.pack
    [r|
DROP TRIGGER tr_group_members_insert_update_summary ON group_members;
DROP TRIGGER tr_group_members_delete_update_summary ON group_members;
DROP TRIGGER tr_group_members_update_update_summary ON group_members;

DROP FUNCTION on_group_members_insert_update_summary;
DROP FUNCTION on_group_members_delete_update_summary;
DROP FUNCTION on_group_members_update_update_summary;

DROP TABLE group_member_status_predicates;

DROP INDEX idx_groups_summary_current_members_count;
ALTER TABLE groups DROP COLUMN summary_current_members_count;
|]

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

CREATE FUNCTION is_current_member(p_status TEXT) RETURNS BOOLEAN
LANGUAGE plpgsql AS $$
BEGIN
  RETURN p_status IN (
    'introduced',
    'intro-inv',
    'accepted',
    'announced',
    'connected',
    'complete',
    'creator'
  );
END;
$$;

UPDATE groups g
SET summary_current_members_count = COALESCE(c.cnt, 0)
FROM (
  SELECT group_id, COUNT(group_member_id) AS cnt
  FROM group_members
  WHERE is_current_member(member_status) = TRUE
  GROUP BY group_id
) c
WHERE g.group_id = c.group_id;

CREATE FUNCTION on_group_members_insert_update_summary() RETURNS TRIGGER
LANGUAGE plpgsql AS $$
BEGIN
  IF is_current_member(NEW.member_status) THEN
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
  IF is_current_member(OLD.member_status) THEN
    UPDATE groups
    SET summary_current_members_count = summary_current_members_count - 1
    WHERE group_id = OLD.group_id;
  END IF;
  RETURN OLD;
END;
$$;

CREATE FUNCTION on_group_members_update_update_summary() RETURNS TRIGGER
LANGUAGE plpgsql AS $$
DECLARE
  was_active BOOLEAN;
  is_active BOOLEAN;
BEGIN
  was_active := is_current_member(OLD.member_status);
  is_active := is_current_member(NEW.member_status);

  IF was_active != is_active THEN
    UPDATE groups
    SET summary_current_members_count = summary_current_members_count +
        (CASE WHEN is_active THEN 1 ELSE -1 END)
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

DROP FUNCTION is_current_member;

DROP INDEX idx_groups_summary_current_members_count;
ALTER TABLE groups DROP COLUMN summary_current_members_count;
|]

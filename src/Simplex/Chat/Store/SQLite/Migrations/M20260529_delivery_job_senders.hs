{-# LANGUAGE QuasiQuotes #-}

-- delivery_jobs.sender_group_member_ids: comma-separated decimal GroupMemberIds.
-- NULL means [] (sender-less jobs, e.g. DJRelayRemoved). One column carries
-- single- and multi-sender jobs uniformly; the per-job introduction bits live
-- in group_members.member_relations_vector (MRIntroduced).
module Simplex.Chat.Store.SQLite.Migrations.M20260529_delivery_job_senders where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20260529_delivery_job_senders :: Query
m20260529_delivery_job_senders =
  [sql|
DROP INDEX idx_delivery_jobs_single_sender_group_member_id;

ALTER TABLE delivery_jobs ADD COLUMN sender_group_member_ids TEXT;

UPDATE delivery_jobs
SET sender_group_member_ids = CAST(single_sender_group_member_id AS TEXT)
WHERE single_sender_group_member_id IS NOT NULL;

ALTER TABLE delivery_jobs DROP COLUMN single_sender_group_member_id;
|]

down_m20260529_delivery_job_senders :: Query
down_m20260529_delivery_job_senders =
  [sql|
-- Pre-up the FK was ON DELETE CASCADE, so orphan delivery_jobs cannot
-- exist. After up the FK was dropped and orphans may accumulate. Drop
-- them here, matching pre-up semantics, before re-adding the FK column.
DELETE FROM delivery_jobs
WHERE sender_group_member_ids IS NOT NULL
  AND length(sender_group_member_ids) > 0
  AND instr(sender_group_member_ids, ',') = 0
  AND NOT EXISTS (
    SELECT 1 FROM group_members
    WHERE group_member_id = CAST(sender_group_member_ids AS INTEGER)
  );

ALTER TABLE delivery_jobs ADD COLUMN single_sender_group_member_id INTEGER REFERENCES group_members(group_member_id) ON DELETE CASCADE;

UPDATE delivery_jobs
SET single_sender_group_member_id =
  CASE
    WHEN sender_group_member_ids IS NULL THEN NULL
    WHEN instr(sender_group_member_ids, ',') > 0 THEN NULL
    WHEN length(sender_group_member_ids) = 0 THEN NULL
    ELSE CAST(sender_group_member_ids AS INTEGER)
  END;

ALTER TABLE delivery_jobs DROP COLUMN sender_group_member_ids;

CREATE INDEX idx_delivery_jobs_single_sender_group_member_id ON delivery_jobs(single_sender_group_member_id);
|]

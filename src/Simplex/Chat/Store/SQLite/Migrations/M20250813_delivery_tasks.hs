{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.SQLite.Migrations.M20250813_delivery_tasks where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

-- TODO [channels fwd] indexes for faster search of the next work item
-- TODO [channels fwd] add for "member profile" delivery jobs:
-- TODO   - ALTER TABLE group_members ADD COLUMN last_profile_delivery_ts TEXT;
-- TODO   - ALTER TABLE group_members ADD COLUMN join_ts TEXT;

-- How columns correspond to types:
-- - <group_id, delivery_job_scope> <-> DeliveryWorkerScope,
-- - delivery_job_scope <-> DeliveryJobScope,
-- - delivery_job_tag <-> DeliveryJobTag,
-- - forward_scope_tag <-> GroupForwardScopeTag,
-- - forward_scope_group_member_id <-> GroupMemberId (for GFSMemberSupport forward scope),
-- - sender_group_member_id <-> GroupMemberId (sender of the original message that created task),
-- - single_sender_group_member_id <-> GroupMemberId (set when all messages in job's delivery body are from the same sender),
-- - task_status <-> DeliveryTaskStatus,
-- - message_from_channel <-> MessageFromChannel (for DJTMessageForward task),
-- - job_status <-> DeliveryJobStatus,
-- - cursor_group_member_id <-> GroupMemberId (for tracking progress of job processing buckets of recipient members).
--
-- Pair of columns <group_id, delivery_job_scope> defines the scope of work for a worker.
--
-- Both tasks and jobs are defined by the same range of delivery scopes, with a delivery task worker
-- converting tasks into jobs for a delivery job worker of the same scope.
-- That's why delivery_tasks table has a delivery_job_scope column.
--
-- See definition of DeliveryJobScope:
-- `type DeliveryJobScope = DJSGroup | DJSMemberSupport | DJSMemberProfile`
-- So, each group can have up to 3 delivery task workers and 3 delivery job workers - 1 task and
-- 1 job worker for each delivery job scope.
--
-- Column forward_scope_tag specifies the exact forwarding scope, in a sense it narrows down delivery scope for a job.
-- For example, for a DJSGroup (delivery job scope "group") forward scope may be GFSAll or GFSMain.
-- Or, for a DJSMemberSupport (delivery job scope "member support") forward scope specifies
-- the exact support member (forward_scope_group_member_id). See also forwardToJobScope.
--
-- Multiple tasks of the same job tag (delivery_job_tag) and forward scope may be converted into a single job.
-- For example, messages referenced in DJTMessageForward tasks of the same scope can be batched for a single delivery,
-- Some tasks, for example DJTRelayRemoved, are converted one-to-one into jobs.
--
-- Forward scope can be NULL, which allows creating more specialized jobs.
-- For example DJSMemberProfile workers are planned to deliver senders' profiles to
-- all members satisfying criteria: sender last_profile_delivery_ts < recipient join_ts.
-- Tasks for these jobs are planned to be created based on sender and group state, rather than per message,
-- so member profiles will be delivered outside of message forwarding scope framework.
-- Requires adding "profile_job_group_member_id" column to delivery_jobs table (absent in current schema).
-- See rfc doc for more details.
m20250813_delivery_tasks :: Query
m20250813_delivery_tasks =
  [sql|
CREATE TABLE delivery_tasks (
  delivery_task_id INTEGER PRIMARY KEY,
  group_id INTEGER NOT NULL REFERENCES groups ON DELETE CASCADE,
  delivery_job_scope TEXT NOT NULL,
  delivery_job_tag TEXT NOT NULL,
  forward_scope_tag TEXT,
  forward_scope_group_member_id INTEGER REFERENCES group_members(group_member_id) ON DELETE CASCADE,
  sender_group_member_id INTEGER NOT NULL REFERENCES group_members(group_member_id) ON DELETE CASCADE,
  message_id INTEGER REFERENCES messages ON DELETE CASCADE,
  message_from_channel INTEGER NOT NULL DEFAULT 0,
  task_status TEXT NOT NULL,
  failed INTEGER DEFAULT 0,
  created_at TEXT NOT NULL DEFAULT (datetime('now')),
  updated_at TEXT NOT NULL DEFAULT (datetime('now'))
);

CREATE INDEX idx_delivery_tasks_group_id ON delivery_tasks(group_id);
CREATE INDEX idx_delivery_tasks_sender_group_member_id ON delivery_tasks(sender_group_member_id);
CREATE INDEX idx_delivery_tasks_forward_scope_group_member_id ON delivery_tasks(forward_scope_group_member_id);
CREATE INDEX idx_delivery_tasks_message_id ON delivery_tasks(message_id);

CREATE TABLE delivery_jobs (
  delivery_job_id INTEGER PRIMARY KEY,
  group_id INTEGER NOT NULL REFERENCES groups ON DELETE CASCADE,
  delivery_job_scope TEXT NOT NULL,
  delivery_job_tag TEXT NOT NULL,
  forward_scope_tag TEXT,
  forward_scope_group_member_id INTEGER REFERENCES group_members(group_member_id) ON DELETE CASCADE,
  single_sender_group_member_id INTEGER REFERENCES group_members(group_member_id) ON DELETE CASCADE,
  delivery_body BLOB,
  cursor_group_member_id INTEGER,
  job_status TEXT NOT NULL,
  failed INTEGER DEFAULT 0,
  created_at TEXT NOT NULL DEFAULT (datetime('now')),
  updated_at TEXT NOT NULL DEFAULT (datetime('now'))
);

CREATE INDEX idx_delivery_jobs_group_id ON delivery_jobs(group_id);
CREATE INDEX idx_delivery_jobs_forward_scope_group_member_id ON delivery_jobs(forward_scope_group_member_id);
CREATE INDEX idx_delivery_jobs_single_sender_group_member_id ON delivery_jobs(single_sender_group_member_id);

ALTER TABLE groups ADD COLUMN group_type TEXT NOT NULL DEFAULT 'small_group';

ALTER TABLE messages ADD COLUMN broker_ts TEXT;
|]

down_m20250813_delivery_tasks :: Query
down_m20250813_delivery_tasks =
  [sql|
ALTER TABLE messages DROP COLUMN broker_ts;

ALTER TABLE groups DROP COLUMN group_type;

DROP INDEX idx_delivery_jobs_group_id;
DROP INDEX idx_delivery_jobs_forward_scope_group_member_id;
DROP INDEX idx_delivery_jobs_single_sender_group_member_id;

DROP TABLE delivery_jobs;

DROP INDEX idx_delivery_tasks_group_id;
DROP INDEX idx_delivery_tasks_sender_group_member_id;
DROP INDEX idx_delivery_tasks_forward_scope_group_member_id;
DROP INDEX idx_delivery_tasks_message_id;

DROP TABLE delivery_tasks;
|]

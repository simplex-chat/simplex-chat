{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.SQLite.Migrations.M20250813_delivery_tasks where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

-- TODO [relays] add later in new migration for MemberProfileUpdate delivery jobs:
-- TODO   - ALTER TABLE group_members ADD COLUMN last_profile_delivery_ts TEXT;
-- TODO   - ALTER TABLE group_members ADD COLUMN join_ts TEXT;

-- How columns correspond to types:

-- both tables:
-- - <group_id, worker_scope> <-> DeliveryWorkerKey = (GroupId, DeliveryWorkerScope),
-- - job_scope_spec_tag <-> Maybe DeliveryJobSpecTag (for DJSGroup job scope),
-- - job_scope_include_pending <-> Maybe Bool (for DJDeliveryJob job spec),
-- - job_scope_support_gm_id <-> Maybe GroupMemberId (for DJSMemberSupport job scope),
-- - failed <-> Bool (for internal worker use, to mark failed work items).

-- delivery_tasks table:
-- - sender_group_member_id <-> GroupMemberId (sender of the original message that created task),
-- - message_id <-> MessageId (reference to the original message that created task),
-- - message_from_channel <-> ShowGroupAsSender (for MessageDeliveryTask),
-- - task_status <-> DeliveryTaskStatus,
-- - task_err_reason <-> Maybe Text (set when task status is DTSError, not encoded in status to allow filtering by DTSError in queries).

-- delivery_jobs table:
-- - single_sender_group_member_id <-> Maybe GroupMemberId (set when all messages in job's delivery body are from the same sender),
-- - body <-> ByteString (JSON encoded batch of messages or single message packed with forwarding metadata (XGrpMsgForward)),
-- - cursor_group_member_id <-> Maybe GroupMemberId (for tracking progress of job processing buckets of recipient members),
-- - job_status <-> DeliveryJobStatus,
-- - job_err_reason <-> Maybe Text (set when job status is DJSError, not encoded in status to allow filtering by DJSError in queries).
--
-- Pair of columns <group_id, worker_scope> defines the scope of work for a worker.
--
-- Both tasks and jobs are defined by the same range of worker scopes, with a delivery task worker
-- converting tasks into jobs for a delivery job worker of the same scope.
-- Each group can have up to 1 task worker and 1 job worker for each worker scope.
-- See DeliveryWorkerScope.
--
-- Columns job_scope_spec_tag, job_scope_include_pending, job_scope_support_gm_id narrow down delivery scope for a job.
-- See DeliveryJobScope.
--
-- In some cases multiple tasks of the same job scope may be converted into a single job.
-- For example, messages referenced in DJDeliveryJob tasks of the same scope/spec can be batched for a single delivery.
-- Some tasks, for example of DJRelayRemoved specialization, are converted one-to-one into jobs.
--
-- Delivery scopes can be expanded to create more specialized jobs.
-- For example DWSMemberProfileUpdate workers are planned to deliver senders' profiles to
-- all members satisfying criteria: sender last_profile_delivery_ts < recipient join_ts.
-- Tasks for these jobs are planned to be created based on sender and group state, rather than per message,
-- so member profile updates will be delivered separately from message deliveries.
-- See rfc doc for more details.
m20250813_delivery_tasks :: Query
m20250813_delivery_tasks =
  [sql|
CREATE TABLE delivery_tasks (
  delivery_task_id INTEGER PRIMARY KEY AUTOINCREMENT,
  group_id INTEGER NOT NULL REFERENCES groups ON DELETE CASCADE,
  worker_scope TEXT NOT NULL,
  job_scope_spec_tag TEXT,
  job_scope_include_pending INTEGER,
  job_scope_support_gm_id INTEGER REFERENCES group_members(group_member_id) ON DELETE CASCADE,
  sender_group_member_id INTEGER NOT NULL REFERENCES group_members(group_member_id) ON DELETE CASCADE,
  message_id INTEGER REFERENCES messages ON DELETE CASCADE,
  message_from_channel INTEGER NOT NULL DEFAULT 0,
  task_status TEXT NOT NULL,
  task_err_reason TEXT,
  failed INTEGER DEFAULT 0,
  created_at TEXT NOT NULL DEFAULT (datetime('now')),
  updated_at TEXT NOT NULL DEFAULT (datetime('now'))
);

CREATE INDEX idx_delivery_tasks_group_id ON delivery_tasks(group_id);
CREATE INDEX idx_delivery_tasks_job_scope_support_gm_id ON delivery_tasks(job_scope_support_gm_id);
CREATE INDEX idx_delivery_tasks_sender_group_member_id ON delivery_tasks(sender_group_member_id);
CREATE INDEX idx_delivery_tasks_message_id ON delivery_tasks(message_id);

CREATE INDEX idx_delivery_tasks_next ON delivery_tasks(
  group_id,
  worker_scope,
  failed,
  task_status
);
CREATE INDEX idx_delivery_tasks_next_for_job_scope ON delivery_tasks(
  group_id,
  worker_scope,
  job_scope_spec_tag,
  job_scope_include_pending,
  job_scope_support_gm_id,
  failed,
  task_status
);
CREATE INDEX idx_delivery_tasks_next_for_job_scope_sender ON delivery_tasks(
  group_id,
  worker_scope,
  job_scope_spec_tag,
  job_scope_include_pending,
  job_scope_support_gm_id,
  sender_group_member_id,
  failed,
  task_status
);
CREATE INDEX idx_delivery_tasks_created_at ON delivery_tasks(created_at);

CREATE TABLE delivery_jobs (
  delivery_job_id INTEGER PRIMARY KEY AUTOINCREMENT,
  group_id INTEGER NOT NULL REFERENCES groups ON DELETE CASCADE,
  worker_scope TEXT NOT NULL,
  job_scope_spec_tag TEXT,
  job_scope_include_pending INTEGER,
  job_scope_support_gm_id INTEGER REFERENCES group_members(group_member_id) ON DELETE CASCADE,
  single_sender_group_member_id INTEGER REFERENCES group_members(group_member_id) ON DELETE CASCADE,
  body BLOB,
  cursor_group_member_id INTEGER,
  job_status TEXT NOT NULL,
  job_err_reason TEXT,
  failed INTEGER DEFAULT 0,
  created_at TEXT NOT NULL DEFAULT (datetime('now')),
  updated_at TEXT NOT NULL DEFAULT (datetime('now'))
);

CREATE INDEX idx_delivery_jobs_group_id ON delivery_jobs(group_id);
CREATE INDEX idx_delivery_jobs_job_scope_support_gm_id ON delivery_jobs(job_scope_support_gm_id);
CREATE INDEX idx_delivery_jobs_single_sender_group_member_id ON delivery_jobs(single_sender_group_member_id);

CREATE INDEX idx_delivery_jobs_next ON delivery_jobs(
  group_id,
  worker_scope,
  failed,
  job_status
);
CREATE INDEX idx_delivery_jobs_created_at ON delivery_jobs(created_at);

ALTER TABLE messages ADD COLUMN broker_ts TEXT;
|]

down_m20250813_delivery_tasks :: Query
down_m20250813_delivery_tasks =
  [sql|
ALTER TABLE messages DROP COLUMN broker_ts;

DROP INDEX idx_delivery_jobs_group_id;
DROP INDEX idx_delivery_jobs_job_scope_support_gm_id;
DROP INDEX idx_delivery_jobs_single_sender_group_member_id;

DROP INDEX idx_delivery_jobs_next;
DROP INDEX idx_delivery_jobs_created_at;

DROP TABLE delivery_jobs;

DROP INDEX idx_delivery_tasks_group_id;
DROP INDEX idx_delivery_tasks_job_scope_support_gm_id;
DROP INDEX idx_delivery_tasks_sender_group_member_id;
DROP INDEX idx_delivery_tasks_message_id;

DROP INDEX idx_delivery_tasks_next;
DROP INDEX idx_delivery_tasks_next_for_job_scope;
DROP INDEX idx_delivery_tasks_next_for_job_scope_sender;
DROP INDEX idx_delivery_tasks_created_at;

DROP TABLE delivery_tasks;
|]

{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.Postgres.Migrations.M20250813_delivery_tasks where

import Data.Text (Text)
import qualified Data.Text as T
import Text.RawString.QQ (r)

m20250813_delivery_tasks :: Text
m20250813_delivery_tasks =
  T.pack
    [r|
CREATE TABLE delivery_tasks (
  delivery_task_id BIGINT PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
  group_id BIGINT NOT NULL REFERENCES groups ON DELETE CASCADE,
  worker_scope TEXT NOT NULL,
  job_scope_spec_tag TEXT,
  job_scope_include_pending SMALLINT,
  job_scope_support_gm_id BIGINT REFERENCES group_members(group_member_id) ON DELETE CASCADE,
  sender_group_member_id BIGINT NOT NULL REFERENCES group_members(group_member_id) ON DELETE CASCADE,
  message_id BIGINT REFERENCES messages ON DELETE CASCADE,
  message_from_channel SMALLINT NOT NULL DEFAULT 0,
  task_status TEXT NOT NULL,
  task_err_reason TEXT,
  failed SMALLINT DEFAULT 0,
  created_at TIMESTAMPTZ NOT NULL DEFAULT (now()),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT (now())
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
  delivery_job_id BIGINT PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
  group_id BIGINT NOT NULL REFERENCES groups ON DELETE CASCADE,
  worker_scope TEXT NOT NULL,
  job_scope_spec_tag TEXT,
  job_scope_include_pending SMALLINT,
  job_scope_support_gm_id BIGINT REFERENCES group_members(group_member_id) ON DELETE CASCADE,
  single_sender_group_member_id BIGINT REFERENCES group_members(group_member_id) ON DELETE CASCADE,
  body BYTEA,
  cursor_group_member_id BIGINT,
  job_status TEXT NOT NULL,
  job_err_reason TEXT,
  failed SMALLINT DEFAULT 0,
  created_at TIMESTAMPTZ NOT NULL DEFAULT (now()),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT (now())
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



ALTER TABLE messages ADD COLUMN broker_ts TIMESTAMPTZ;
|]

down_m20250813_delivery_tasks :: Text
down_m20250813_delivery_tasks =
  T.pack
    [r|
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

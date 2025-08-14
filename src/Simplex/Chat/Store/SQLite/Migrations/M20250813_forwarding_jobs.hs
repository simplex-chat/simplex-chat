{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.SQLite.Migrations.M20250813_forwarding_jobs where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20250813_forwarding_jobs :: Query
m20250813_forwarding_jobs =
  [sql|
CREATE TABLE forwarding_jobs (
  forwarding_job_id INTEGER PRIMARY KEY,
  msg_batch_encoding TEXT,
  cursor_group_member_id INTEGER
)

ALTER TABLE messages ADD COLUMN chat_message_json TEXT;
ALTER TABLE messages ADD COLUMN forward_scope TEXT;
ALTER TABLE messages ADD COLUMN forward_complete INTEGER NOT NULL DEFAULT 0;
ALTER TABLE messages ADD COLUMN forwarding_job_id INTEGER REFERENCES forwarding_jobs ON DELETE SET NULL;
|]

down_m20250813_forwarding_jobs :: Query
down_m20250813_forwarding_jobs =
  [sql|
ALTER TABLE MESSAGES DROP COLUMN chat_message_json;
ALTER TABLE MESSAGES DROP COLUMN forward_scope;
ALTER TABLE MESSAGES DROP COLUMN forward_complete;
ALTER TABLE MESSAGES DROP COLUMN forwarding_job_id;

DROP TABLE forwarding_jobs;
|]

{-# LANGUAGE QuasiQuotes #-}

-- Member profile dissemination in relay-mediated groups (task 001):
--   group_members.sent_profile_vector      — per-recipient byte vector of "did this member's profile reach you?"
--   delivery_jobs.sender_group_member_ids  — multi-sender batch: senders whose body fragments are in this job
-- Both columns serve the same feature; the migration name only references the primary column.
module Simplex.Chat.Store.SQLite.Migrations.M20260513_sent_profile_vector where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20260513_sent_profile_vector :: Query
m20260513_sent_profile_vector =
  [sql|
ALTER TABLE group_members ADD COLUMN sent_profile_vector BLOB NOT NULL DEFAULT x'';
ALTER TABLE delivery_jobs ADD COLUMN sender_group_member_ids BLOB;
|]

down_m20260513_sent_profile_vector :: Query
down_m20260513_sent_profile_vector =
  [sql|
ALTER TABLE delivery_jobs DROP COLUMN sender_group_member_ids;
ALTER TABLE group_members DROP COLUMN sent_profile_vector;
|]

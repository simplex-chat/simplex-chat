{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- Member profile dissemination in relay-mediated groups (task 001):
--   delivery_jobs.sender_group_member_ids replaces delivery_jobs.single_sender_group_member_id.
--   One column carries either [s] (single-sender job) or [s1, s2, ...] (multi-sender batch).
--   The (group_members.member_relations_vector, MRIntroduced) bit tracks
--   which recipients have been announced for each sender — reused, not duplicated.
module Simplex.Chat.Store.Postgres.Migrations.M20260515_delivery_job_senders where

import Data.Text (Text)
import Text.RawString.QQ (r)

-- Backfill rule: '\x01' is the smpEncodeList length prefix (lenEncode 1);
-- int8send produces the 8-byte big-endian wire form that smpEncode @Int64
-- (Word32 high BE ++ Word32 low BE) emits for a single Int64. The
-- concatenation matches smpEncodeList [s] byte-for-byte.
m20260515_delivery_job_senders :: Text
m20260515_delivery_job_senders =
  [r|
DROP INDEX idx_delivery_jobs_single_sender_group_member_id;

ALTER TABLE delivery_jobs ADD COLUMN sender_group_member_ids BYTEA;

UPDATE delivery_jobs
SET sender_group_member_ids = '\x01'::bytea || int8send(single_sender_group_member_id::bigint)
WHERE single_sender_group_member_id IS NOT NULL;

ALTER TABLE delivery_jobs DROP COLUMN single_sender_group_member_id;
|]

-- Down reverses the up backfill for the only shape it can undo: a
-- singleton list. Anything else (NULL, multi-element, malformed) restores
-- as NULL; multi-sender jobs do not survive a downgrade.
down_m20260515_delivery_job_senders :: Text
down_m20260515_delivery_job_senders =
  [r|
ALTER TABLE delivery_jobs ADD COLUMN single_sender_group_member_id BIGINT REFERENCES group_members(group_member_id) ON DELETE CASCADE;

UPDATE delivery_jobs
SET single_sender_group_member_id = CASE
  WHEN sender_group_member_ids IS NOT NULL
       AND octet_length(sender_group_member_ids) = 9
       AND get_byte(sender_group_member_ids, 0) = 1
    THEN ('x' || encode(substring(sender_group_member_ids FROM 2 FOR 8), 'hex'))::bit(64)::bigint
  ELSE NULL
END;

ALTER TABLE delivery_jobs DROP COLUMN sender_group_member_ids;

CREATE INDEX idx_delivery_jobs_single_sender_group_member_id ON delivery_jobs(single_sender_group_member_id);
|]

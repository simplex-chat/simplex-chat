{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.Postgres.Migrations.M20260629_roster_catchup where

import Data.Text (Text)
import Text.RawString.QQ (r)

-- Roster catch-up bookkeeping. Three monotonic per-group roster versions with distinct roles - normally equal,
-- diverging across gaps and failed transfers (applied_complete <= stored <= roster_version):
--   roster_version (added in M20260602) - the GATE: highest version seen. Advanced by every accepted owner delta
--     and by a roster apply. Revert protection: a completing roster older than this is rejected, since its
--     snapshot would undo a newer applied delta.
--   stored_roster_version - the blob HELD: the version of the roster blob actually stored (written with the blob).
--     What a relay can re-serve; a failed blob receive leaves it behind the gate (which the delta still advances)
--     until a later roster completes. Relay-side; on a member it is set but the blob is unused.
--   applied_complete_roster_version - the COMPLETE frontier: highest version up to which the picture is contiguous.
--     Advances by 1 on a contiguous delta and to the roster's version on apply, but stays put on a gapped delta.
--     The subscriber's "what I have" for gap detection and the catch-up request: a value below the gate means
--     missed versions, so each following delta re-asks the forwarding relay until a roster fills the frontier.
-- Also adds group_members.roster_served_version - the newest version a relay re-served a given member, bounding
-- reflected amplification (a member can't re-trigger a full serve at a version it was already served).
-- Backfill an existing roster's stored and complete versions from roster_version: pre-upgrade the picture is
-- contiguous up to roster_version (no gap detection existed), so a fresh NULL frontier would read every group's
-- next delta as a gap and make every subscriber request a re-serve at once.
m20260629_roster_catchup :: Text
m20260629_roster_catchup =
  [r|
ALTER TABLE group_members ADD COLUMN roster_served_version BIGINT;
ALTER TABLE groups ADD COLUMN stored_roster_version BIGINT;
ALTER TABLE groups ADD COLUMN applied_complete_roster_version BIGINT;
UPDATE groups SET stored_roster_version = roster_version, applied_complete_roster_version = roster_version WHERE roster_version IS NOT NULL;
|]

down_m20260629_roster_catchup :: Text
down_m20260629_roster_catchup =
  [r|
ALTER TABLE group_members DROP COLUMN roster_served_version;
ALTER TABLE groups DROP COLUMN stored_roster_version;
ALTER TABLE groups DROP COLUMN applied_complete_roster_version;
|]

{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Migrations.M20231113_group_invitees_forwards where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20231113_group_invitees_forwards :: Query
m20231113_group_invitees_forwards =
  [sql|
CREATE TABLE group_invitees_forwards (
  group_invitee_forward_id INTEGER PRIMARY KEY,
  invitee_group_member_id INTEGER NOT NULL REFERENCES group_members ON DELETE CASCADE,
  forward_group_member_id INTEGER NOT NULL REFERENCES group_members ON DELETE CASCADE,
  created_at TEXT NOT NULL DEFAULT(datetime('now')),
  updated_at TEXT NOT NULL DEFAULT(datetime('now')),
  UNIQUE(invitee_group_member_id, forward_group_member_id)
);

CREATE INDEX idx_group_invitees_forwards_invitee_group_member_id ON group_invitees_forwards(invitee_group_member_id);
CREATE INDEX idx_group_invitees_forwards_forward_group_member_id ON group_invitees_forwards(forward_group_member_id);

ALTER TABLE group_members ADD COLUMN invited_by_group_member_id INTEGER REFERENCES group_members ON DELETE SET NULL;

CREATE INDEX idx_group_members_invited_by_group_member_id ON group_members(invited_by_group_member_id);
|]

down_m20231113_group_invitees_forwards :: Query
down_m20231113_group_invitees_forwards =
  [sql|
DROP INDEX idx_group_invitees_forwards_invitee_group_member_id;
DROP INDEX idx_group_invitees_forwards_forward_group_member_id;

DROP TABLE group_invitees_forwards;

DROP INDEX idx_group_members_invited_by_group_member_id;

ALTER TABLE group_members DROP COLUMN invited_by_group_member_id;
|]

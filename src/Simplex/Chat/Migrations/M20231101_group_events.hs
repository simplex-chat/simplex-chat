{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Migrations.M20231101_group_events where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20231101_group_events :: Query
m20231101_group_events =
  [sql|
CREATE TABLE group_events (
  group_event_id INTEGER PRIMARY KEY,
  user_id INTEGER NOT NULL REFERENCES users ON DELETE CASCADE,
  chat_item_id INTEGER REFERENCES chat_items ON DELETE SET NULL,
  chat_min_version INTEGER NOT NULL DEFAULT 1,  -- chatVRange :: VersionRange
  chat_max_version INTEGER NOT NULL DEFAULT 1,
  shared_msg_id BLOB NOT NULL,                  -- msgId :: SharedMsgId
  event_data TEXT NOT NULL,                     -- eventData :: StoredGroupEventData
  shared_hash BLOB NOT NULL,                    -- sharedHash :: ByteString
  event_sent INTEGER NOT NULL,                  -- 0 for received, 1 for sent; below `rcvd_` fields are null for sent
  rcvd_author_member_id BLOB,                   -- ReceivedEventInfo authorMemberId :: MemberId
  rcvd_author_member_name TEXT,                 -- ReceivedEventInfo authorMemberName :: ContactName
  -- ReceivedEventInfo authorMember :: Maybe GroupMemberRef; can be null even for received event
  rcvd_author_group_member_id INTEGER REFERENCES group_members ON DELETE SET NULL,
  rcvd_author_contact_profile_id INTEGER REFERENCES contact_profiles ON DELETE SET NULL,
  -- rcvd_author_role TEXT NOT NULL,            -- ReceivedFromRole - store in case it changes?
  -- ReceivedEventInfo receivedFrom :: GroupMemberRef
  rcvd_from_group_member_id INTEGER REFERENCES group_members ON DELETE SET NULL,
  rcvd_from_contact_profile_id INTEGER REFERENCES contact_profiles ON DELETE SET NULL,
  rcvd_processing TEXT NOT NULL,                -- ReceivedEventInfo processing :: EventProcessing
  rcvd_processed INTEGER NOT NULL DEFAULT 0,    -- 1 for processed; when retrieving unprocessed
  -- rcvd_scheduled_at TEXT,                    -- EPScheduled UTCTime; when retrieving scheduled at near time?
  created_at TEXT NOT NULL DEFAULT (datetime('now')),
  updated_at TEXT NOT NULL DEFAULT (datetime('now'))
);

CREATE INDEX idx_group_events_user_id ON group_events(user_id);
CREATE INDEX idx_group_events_chat_item_id ON group_events(chat_item_id);
CREATE INDEX idx_group_events_shared_msg_id ON group_events(shared_msg_id);
CREATE INDEX idx_group_events_rcvd_author_group_member_id ON group_events(rcvd_author_group_member_id);
CREATE INDEX idx_group_events_rcvd_author_contact_profile_id ON group_events(rcvd_author_contact_profile_id);
CREATE INDEX idx_group_events_rcvd_from_group_member_id ON group_events(rcvd_from_group_member_id);
CREATE INDEX idx_group_events_rcvd_from_contact_profile_id ON group_events(rcvd_from_contact_profile_id);

CREATE TABLE group_events_availabilities (
  group_events_availability_id INTEGER PRIMARY KEY,
  group_event_id INTEGER NOT NULL REFERENCES group_events ON DELETE CASCADE,
  available_at_group_member_id INTEGER REFERENCES group_members ON DELETE CASCADE,
  created_at TEXT NOT NULL DEFAULT (datetime('now')),
  updated_at TEXT NOT NULL DEFAULT (datetime('now'))
);

CREATE INDEX idx_group_events_availabilities_group_event_id ON group_events_availabilities(group_event_id);
CREATE INDEX idx_group_events_availabilities_available_at_group_member_id ON group_events_availabilities(available_at_group_member_id);

CREATE TABLE group_events_dag_errors (
  group_event_dag_error_id INTEGER PRIMARY KEY,
  group_event_id INTEGER NOT NULL REFERENCES group_events ON DELETE CASCADE,
  dag_error TEXT NOT NULL,
  created_at TEXT NOT NULL DEFAULT (datetime('now')),
  updated_at TEXT NOT NULL DEFAULT (datetime('now'))
);

CREATE INDEX idx_group_events_dag_errors_group_event_id ON group_events_dag_errors(group_event_id);

CREATE TABLE group_events_parents (
  group_event_parent_id INTEGER NOT NULL REFERENCES group_events ON DELETE CASCADE,
  group_event_child_id INTEGER NOT NULL REFERENCES group_events ON DELETE CASCADE,
  created_at TEXT NOT NULL DEFAULT(datetime('now')),
  updated_at TEXT NOT NULL DEFAULT(datetime('now')),
  UNIQUE(group_event_parent_id, group_event_child_id)
);

CREATE INDEX idx_group_events_parents_group_event_parent_id ON group_events_parents(group_event_parent_id);
CREATE INDEX idx_group_events_parents_group_event_child_id ON group_events_parents(group_event_child_id);
|]

down_m20231101_group_events :: Query
down_m20231101_group_events =
  [sql|
DROP INDEX idx_group_events_parents_group_event_parent_id;
DROP INDEX idx_group_events_parents_group_event_child_id;

DROP TABLE group_events_parents;

DROP INDEX idx_group_events_dag_errors_group_event_id;

DROP TABLE group_events_dag_errors;

DROP INDEX idx_group_events_availabilities_group_event_id;
DROP INDEX idx_group_events_availabilities_available_at_group_member_id;

DROP TABLE group_events_availabilities;

DROP INDEX idx_group_events_user_id;
DROP INDEX idx_group_events_chat_item_id;
DROP INDEX idx_group_events_shared_msg_id;
DROP INDEX idx_group_events_rcvd_author_group_member_id;
DROP INDEX idx_group_events_rcvd_author_contact_profile_id;
DROP INDEX idx_group_events_rcvd_from_group_member_id;
DROP INDEX idx_group_events_rcvd_from_contact_profile_id;

DROP TABLE group_events;
|]

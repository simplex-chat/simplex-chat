{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.SQLite.Migrations.M20220122_v1_1 where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20220122_v1_1 :: Query
m20220122_v1_1 =
  [sql|
-- * pending group messages

-- pending messages for announced (memberCurrent) but not yet connected (memberActive) group members
CREATE TABLE pending_group_messages (
  pending_group_message_id INTEGER PRIMARY KEY,
  group_member_id INTEGER NOT NULL REFERENCES group_members ON DELETE CASCADE,
  message_id INTEGER NOT NULL REFERENCES messages ON DELETE CASCADE,
  group_member_intro_id INTEGER REFERENCES group_member_intros ON DELETE CASCADE,
  created_at TEXT NOT NULL DEFAULT (datetime('now')),
  updated_at TEXT NOT NULL DEFAULT (datetime('now'))
);

-- * chat items

-- mutable chat_items presented to user
CREATE TABLE chat_items (
  chat_item_id INTEGER PRIMARY KEY,
  user_id INTEGER NOT NULL REFERENCES users ON DELETE CASCADE,
  contact_id INTEGER REFERENCES contacts ON DELETE CASCADE,
  group_id INTEGER REFERENCES groups ON DELETE CASCADE,
  group_member_id INTEGER REFERENCES group_members ON DELETE SET NULL, -- NULL for sent even if group_id is not
  chat_msg_id INTEGER, -- sent as part of the message that created the item
  created_by_msg_id INTEGER UNIQUE REFERENCES messages (message_id) ON DELETE SET NULL,
  item_sent INTEGER NOT NULL, -- 0 for received, 1 for sent
  item_ts TEXT NOT NULL, -- broker_ts of creating message for received, created_at for sent
  item_deleted INTEGER NOT NULL DEFAULT 0, -- 1 for deleted
  item_content TEXT NOT NULL, -- JSON
  item_text TEXT NOT NULL, -- textual representation
  created_at TEXT NOT NULL DEFAULT (datetime('now')),
  updated_at TEXT NOT NULL DEFAULT (datetime('now'))
);

CREATE TABLE chat_item_messages (
  chat_item_id INTEGER NOT NULL REFERENCES chat_items ON DELETE CASCADE,
  message_id INTEGER NOT NULL UNIQUE REFERENCES messages ON DELETE CASCADE,
  created_at TEXT NOT NULL DEFAULT (datetime('now')),
  updated_at TEXT NOT NULL DEFAULT (datetime('now')),
  UNIQUE (chat_item_id, message_id)
);

ALTER TABLE files ADD COLUMN chat_item_id INTEGER DEFAULT NULL REFERENCES chat_items ON DELETE CASCADE;

-- * created_at & updated_at for all tables

PRAGMA ignore_check_constraints=ON;

-- ** contact_profiles

ALTER TABLE contact_profiles ADD COLUMN created_at TEXT CHECK (created_at NOT NULL);
UPDATE contact_profiles SET created_at = '1970-01-01 00:00:00';

ALTER TABLE contact_profiles ADD COLUMN updated_at TEXT CHECK (updated_at NOT NULL);
UPDATE contact_profiles SET updated_at = '1970-01-01 00:00:00';

-- ** users

ALTER TABLE users ADD COLUMN created_at TEXT CHECK (created_at NOT NULL);
UPDATE users SET created_at = '1970-01-01 00:00:00';

ALTER TABLE users ADD COLUMN updated_at TEXT CHECK (updated_at NOT NULL);
UPDATE users SET updated_at = '1970-01-01 00:00:00';

-- ** display_names

ALTER TABLE display_names ADD COLUMN created_at TEXT CHECK (created_at NOT NULL);
UPDATE display_names SET created_at = '1970-01-01 00:00:00';

ALTER TABLE display_names ADD COLUMN updated_at TEXT CHECK (updated_at NOT NULL);
UPDATE display_names SET updated_at = '1970-01-01 00:00:00';

-- ** contacts

ALTER TABLE contacts ADD COLUMN updated_at TEXT CHECK (updated_at NOT NULL);
UPDATE contacts SET updated_at = '1970-01-01 00:00:00';

-- ** sent_probes

ALTER TABLE sent_probes ADD COLUMN created_at TEXT CHECK (created_at NOT NULL);
UPDATE sent_probes SET created_at = '1970-01-01 00:00:00';

ALTER TABLE sent_probes ADD COLUMN updated_at TEXT CHECK (updated_at NOT NULL);
UPDATE sent_probes SET updated_at = '1970-01-01 00:00:00';

-- ** sent_probe_hashes

ALTER TABLE sent_probe_hashes ADD COLUMN created_at TEXT CHECK (created_at NOT NULL);
UPDATE sent_probe_hashes SET created_at = '1970-01-01 00:00:00';

ALTER TABLE sent_probe_hashes ADD COLUMN updated_at TEXT CHECK (updated_at NOT NULL);
UPDATE sent_probe_hashes SET updated_at = '1970-01-01 00:00:00';

-- ** received_probes

ALTER TABLE received_probes ADD COLUMN created_at TEXT CHECK (created_at NOT NULL);
UPDATE received_probes SET created_at = '1970-01-01 00:00:00';

ALTER TABLE received_probes ADD COLUMN updated_at TEXT CHECK (updated_at NOT NULL);
UPDATE received_probes SET updated_at = '1970-01-01 00:00:00';

-- ** known_servers

ALTER TABLE known_servers ADD COLUMN created_at TEXT CHECK (created_at NOT NULL);
UPDATE known_servers SET created_at = '1970-01-01 00:00:00';

ALTER TABLE known_servers ADD COLUMN updated_at TEXT CHECK (updated_at NOT NULL);
UPDATE known_servers SET updated_at = '1970-01-01 00:00:00';

-- ** group_profiles

ALTER TABLE group_profiles ADD COLUMN created_at TEXT CHECK (created_at NOT NULL);
UPDATE group_profiles SET created_at = '1970-01-01 00:00:00';

ALTER TABLE group_profiles ADD COLUMN updated_at TEXT CHECK (updated_at NOT NULL);
UPDATE group_profiles SET updated_at = '1970-01-01 00:00:00';

-- ** groups

ALTER TABLE groups ADD COLUMN created_at TEXT CHECK (created_at NOT NULL);
UPDATE groups SET created_at = '1970-01-01 00:00:00';

ALTER TABLE groups ADD COLUMN updated_at TEXT CHECK (updated_at NOT NULL);
UPDATE groups SET updated_at = '1970-01-01 00:00:00';

-- ** group_members

ALTER TABLE group_members ADD COLUMN created_at TEXT CHECK (created_at NOT NULL);
UPDATE group_members SET created_at = '1970-01-01 00:00:00';

ALTER TABLE group_members ADD COLUMN updated_at TEXT CHECK (updated_at NOT NULL);
UPDATE group_members SET updated_at = '1970-01-01 00:00:00';

-- ** group_member_intros

ALTER TABLE group_member_intros ADD COLUMN created_at TEXT CHECK (created_at NOT NULL);
UPDATE group_member_intros SET created_at = '1970-01-01 00:00:00';

ALTER TABLE group_member_intros ADD COLUMN updated_at TEXT CHECK (updated_at NOT NULL);
UPDATE group_member_intros SET updated_at = '1970-01-01 00:00:00';

-- ** files

ALTER TABLE files ADD COLUMN updated_at TEXT CHECK (updated_at NOT NULL);
UPDATE files SET updated_at = '1970-01-01 00:00:00';

-- ** snd_files

ALTER TABLE snd_files ADD COLUMN created_at TEXT CHECK (created_at NOT NULL);
UPDATE snd_files SET created_at = '1970-01-01 00:00:00';

ALTER TABLE snd_files ADD COLUMN updated_at TEXT CHECK (updated_at NOT NULL);
UPDATE snd_files SET updated_at = '1970-01-01 00:00:00';

-- ** rcv_files

ALTER TABLE rcv_files ADD COLUMN created_at TEXT CHECK (created_at NOT NULL);
UPDATE rcv_files SET created_at = '1970-01-01 00:00:00';

ALTER TABLE rcv_files ADD COLUMN updated_at TEXT CHECK (updated_at NOT NULL);
UPDATE rcv_files SET updated_at = '1970-01-01 00:00:00';

-- ** snd_file_chunks

ALTER TABLE snd_file_chunks ADD COLUMN created_at TEXT CHECK (created_at NOT NULL);
UPDATE snd_file_chunks SET created_at = '1970-01-01 00:00:00';

ALTER TABLE snd_file_chunks ADD COLUMN updated_at TEXT CHECK (updated_at NOT NULL);
UPDATE snd_file_chunks SET updated_at = '1970-01-01 00:00:00';

-- ** rcv_file_chunks

ALTER TABLE rcv_file_chunks ADD COLUMN created_at TEXT CHECK (created_at NOT NULL);
UPDATE rcv_file_chunks SET created_at = '1970-01-01 00:00:00';

ALTER TABLE rcv_file_chunks ADD COLUMN updated_at TEXT CHECK (updated_at NOT NULL);
UPDATE rcv_file_chunks SET updated_at = '1970-01-01 00:00:00';

-- ** connections

ALTER TABLE connections ADD COLUMN updated_at TEXT CHECK (updated_at NOT NULL);
UPDATE connections SET updated_at = '1970-01-01 00:00:00';

-- ** user_contact_links

ALTER TABLE user_contact_links ADD COLUMN updated_at TEXT CHECK (updated_at NOT NULL);
UPDATE user_contact_links SET updated_at = '1970-01-01 00:00:00';

-- ** contact_requests

ALTER TABLE contact_requests ADD COLUMN updated_at TEXT CHECK (updated_at NOT NULL);
UPDATE contact_requests SET updated_at = '1970-01-01 00:00:00';

-- ** messages

ALTER TABLE messages ADD COLUMN updated_at TEXT CHECK (updated_at NOT NULL);
UPDATE messages SET updated_at = '1970-01-01 00:00:00';

-- ** msg_deliveries

ALTER TABLE msg_deliveries ADD COLUMN created_at TEXT CHECK (created_at NOT NULL);
UPDATE msg_deliveries SET created_at = '1970-01-01 00:00:00';

ALTER TABLE msg_deliveries ADD COLUMN updated_at TEXT CHECK (updated_at NOT NULL);
UPDATE msg_deliveries SET updated_at = '1970-01-01 00:00:00';

-- ** msg_delivery_events

ALTER TABLE msg_delivery_events ADD COLUMN updated_at TEXT CHECK (updated_at NOT NULL);
UPDATE msg_delivery_events SET updated_at = '1970-01-01 00:00:00';

PRAGMA ignore_check_constraints=OFF;
|]

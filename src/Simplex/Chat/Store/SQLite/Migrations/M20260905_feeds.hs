{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.SQLite.Migrations.M20260905_feeds where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20260905_feeds :: Query
m20260905_feeds =
  [sql|
CREATE TABLE feeds(
  feed_id INTEGER PRIMARY KEY AUTOINCREMENT,
  user_id INTEGER NOT NULL REFERENCES users ON DELETE CASCADE,
  created_at TEXT NOT NULL DEFAULT(datetime('now')),
  updated_at TEXT NOT NULL DEFAULT(datetime('now')),
  chat_ts TEXT NOT NULL DEFAULT(datetime('now')),
  favorite INTEGER NOT NULL DEFAULT 0,
  unread_chat INTEGER NOT NULL DEFAULT 0
) STRICT;
CREATE INDEX idx_feeds_user_id ON feeds(user_id);
INSERT INTO feeds (user_id) SELECT user_id FROM users;

ALTER TABLE chat_items ADD COLUMN feed_id INTEGER DEFAULT NULL REFERENCES feeds ON DELETE CASCADE;
ALTER TABLE chat_items ADD COLUMN feed_item_id INTEGER DEFAULT NULL REFERENCES chat_items ON DELETE SET NULL;
ALTER TABLE chat_items ADD COLUMN item_feed INTEGER NOT NULL DEFAULT 0;
CREATE INDEX idx_chat_items_feed_id ON chat_items(feed_id);
CREATE INDEX idx_chat_items_feeds_created_at ON chat_items(user_id, feed_id, created_at);
CREATE INDEX idx_chat_items_feed_item_contact ON chat_items(feed_item_id, contact_id);
CREATE INDEX idx_chat_items_feed_item_group ON chat_items(feed_item_id, group_id);

ALTER TABLE messages ADD COLUMN feed_id INTEGER DEFAULT NULL REFERENCES feeds ON DELETE CASCADE;
CREATE INDEX idx_messages_feed_id ON messages(feed_id);

ALTER TABLE files ADD COLUMN feed_id INTEGER DEFAULT NULL REFERENCES feeds ON DELETE CASCADE;
CREATE INDEX idx_files_feed_id ON files(feed_id);

ALTER TABLE contacts ADD COLUMN drop_feed INTEGER NOT NULL DEFAULT 0;
ALTER TABLE groups ADD COLUMN drop_feed INTEGER NOT NULL DEFAULT 0;
CREATE INDEX idx_contacts_user_id ON contacts(user_id);
CREATE INDEX idx_groups_user_id_business_chat ON groups(user_id, business_chat);

PRAGMA writable_schema=1;

UPDATE sqlite_master
SET sql = replace(sql, 'group_id INTEGER NOT NULL REFERENCES groups ON DELETE CASCADE', 'group_id INTEGER REFERENCES groups ON DELETE CASCADE')
WHERE type = 'table' AND name = 'delivery_jobs';

PRAGMA writable_schema=RESET;

ALTER TABLE delivery_jobs ADD COLUMN feed_id INTEGER REFERENCES feeds ON DELETE CASCADE;
ALTER TABLE delivery_jobs ADD COLUMN chat_item_id INTEGER REFERENCES chat_items ON DELETE CASCADE;
ALTER TABLE delivery_jobs ADD COLUMN delete_mode TEXT;
ALTER TABLE delivery_jobs ADD COLUMN message_ids TEXT;
ALTER TABLE delivery_jobs ADD COLUMN cursor_contact_id INTEGER;
ALTER TABLE delivery_jobs ADD COLUMN cursor_group_id INTEGER;
CREATE INDEX idx_delivery_jobs_feed_next ON delivery_jobs(feed_id, worker_scope, failed, job_status);
CREATE INDEX idx_delivery_jobs_chat_item_id ON delivery_jobs(chat_item_id);
|]

down_m20260905_feeds :: Query
down_m20260905_feeds =
  [sql|
DROP INDEX idx_delivery_jobs_chat_item_id;
DROP INDEX idx_delivery_jobs_feed_next;
DELETE FROM delivery_jobs WHERE group_id IS NULL;
ALTER TABLE delivery_jobs DROP COLUMN cursor_group_id;
ALTER TABLE delivery_jobs DROP COLUMN cursor_contact_id;
ALTER TABLE delivery_jobs DROP COLUMN message_ids;
ALTER TABLE delivery_jobs DROP COLUMN delete_mode;
ALTER TABLE delivery_jobs DROP COLUMN chat_item_id;
ALTER TABLE delivery_jobs DROP COLUMN feed_id;

PRAGMA writable_schema=1;

UPDATE sqlite_master
SET sql = replace(sql, 'group_id INTEGER REFERENCES groups ON DELETE CASCADE', 'group_id INTEGER NOT NULL REFERENCES groups ON DELETE CASCADE')
WHERE type = 'table' AND name = 'delivery_jobs';

PRAGMA writable_schema=RESET;

DROP INDEX idx_groups_user_id_business_chat;
DROP INDEX idx_contacts_user_id;
ALTER TABLE groups DROP COLUMN drop_feed;
ALTER TABLE contacts DROP COLUMN drop_feed;

DROP INDEX idx_files_feed_id;
ALTER TABLE files DROP COLUMN feed_id;

DROP INDEX idx_messages_feed_id;
ALTER TABLE messages DROP COLUMN feed_id;

DROP INDEX idx_chat_items_feed_item_group;
DROP INDEX idx_chat_items_feed_item_contact;
DROP INDEX idx_chat_items_feeds_created_at;
DROP INDEX idx_chat_items_feed_id;
ALTER TABLE chat_items DROP COLUMN item_feed;
ALTER TABLE chat_items DROP COLUMN feed_item_id;
ALTER TABLE chat_items DROP COLUMN feed_id;

DROP INDEX idx_feeds_user_id;
DROP TABLE feeds;
|]

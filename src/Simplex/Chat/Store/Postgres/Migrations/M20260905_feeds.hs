{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.Postgres.Migrations.M20260905_feeds where

import Data.Text (Text)
import qualified Data.Text as T
import Text.RawString.QQ (r)

m20260905_feeds :: Text
m20260905_feeds =
  T.pack
    [r|
CREATE TABLE feeds (
  feed_id BIGINT PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
  user_id BIGINT NOT NULL REFERENCES users ON DELETE CASCADE,
  created_at TIMESTAMPTZ NOT NULL DEFAULT (now()),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT (now()),
  chat_ts TIMESTAMPTZ NOT NULL DEFAULT (now()),
  favorite SMALLINT NOT NULL DEFAULT 0,
  unread_chat SMALLINT NOT NULL DEFAULT 0
);
CREATE INDEX idx_feeds_user_id ON feeds(user_id);
INSERT INTO feeds (user_id) SELECT user_id FROM users;

ALTER TABLE chat_items ADD COLUMN feed_id BIGINT REFERENCES feeds ON DELETE CASCADE;
ALTER TABLE chat_items ADD COLUMN feed_item_id BIGINT REFERENCES chat_items ON DELETE SET NULL;
ALTER TABLE chat_items ADD COLUMN item_feed SMALLINT NOT NULL DEFAULT 0;
CREATE INDEX idx_chat_items_feed_id ON chat_items(feed_id);
CREATE INDEX idx_chat_items_feeds_created_at ON chat_items(user_id, feed_id, created_at);
CREATE INDEX idx_chat_items_feed_item_contact ON chat_items(feed_item_id, contact_id);
CREATE INDEX idx_chat_items_feed_item_group ON chat_items(feed_item_id, group_id);

ALTER TABLE messages ADD COLUMN feed_id BIGINT REFERENCES feeds ON DELETE CASCADE;
CREATE INDEX idx_messages_feed_id ON messages(feed_id);

ALTER TABLE files ADD COLUMN feed_id BIGINT REFERENCES feeds ON DELETE CASCADE;
CREATE INDEX idx_files_feed_id ON files(feed_id);

ALTER TABLE contacts ADD COLUMN drop_feed SMALLINT NOT NULL DEFAULT 0;
ALTER TABLE groups ADD COLUMN drop_feed SMALLINT NOT NULL DEFAULT 0;
CREATE INDEX idx_contacts_user_id ON contacts(user_id, contact_id);
CREATE INDEX idx_groups_user_id_business_chat ON groups(user_id, business_chat, group_id);

CREATE TABLE feed_jobs (
  feed_job_id BIGINT PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
  feed_id BIGINT NOT NULL REFERENCES feeds ON DELETE CASCADE,
  chat_item_id BIGINT NOT NULL REFERENCES chat_items ON DELETE CASCADE,
  worker_scope TEXT NOT NULL,
  action_tag TEXT NOT NULL,
  message_ids TEXT,
  cursor_id BIGINT,
  job_status TEXT NOT NULL,
  job_err_reason TEXT,
  failed SMALLINT NOT NULL DEFAULT 0,
  created_at TIMESTAMPTZ NOT NULL DEFAULT (now()),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT (now())
);
CREATE INDEX idx_feed_jobs_next ON feed_jobs(feed_id, worker_scope, failed, job_status, feed_job_id);
CREATE INDEX idx_feed_jobs_chat_item_id ON feed_jobs(chat_item_id, action_tag);
|]

down_m20260905_feeds :: Text
down_m20260905_feeds =
  T.pack
    [r|
DROP INDEX idx_feed_jobs_chat_item_id;
DROP INDEX idx_feed_jobs_next;
DROP TABLE feed_jobs;

DROP INDEX idx_groups_user_id_business_chat;
DROP INDEX idx_contacts_user_id;
ALTER TABLE groups DROP COLUMN drop_feed;
ALTER TABLE contacts DROP COLUMN drop_feed;

DELETE FROM files WHERE feed_id IS NOT NULL;
DROP INDEX idx_files_feed_id;
ALTER TABLE files DROP COLUMN feed_id;

DELETE FROM messages WHERE feed_id IS NOT NULL;
DROP INDEX idx_messages_feed_id;
ALTER TABLE messages DROP COLUMN feed_id;

DELETE FROM chat_items WHERE feed_id IS NOT NULL;
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

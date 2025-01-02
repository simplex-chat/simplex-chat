{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Migrations.M20241230_reports where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20241230_reports :: Query
m20241230_reports =
  [sql|
ALTER TABLE chat_items ADD COLUMN msg_content_tag TEXT;
ALTER TABLE chat_items ADD COLUMN processed_by_group_member_id BLOB;

ALTER TABLE groups ADD COLUMN unprocessed_reports_count INTEGER NOT NULL DEFAULT 0;

ALTER TABLE groups ADD COLUMN unread_count INTEGER;
ALTER TABLE groups ADD COLUMN last_read_item_id INTEGER;
ALTER TABLE groups ADD COLUMN last_read_item_ts TEXT;

ALTER TABLE contacts ADD COLUMN unread_count INTEGER;
ALTER TABLE contacts ADD COLUMN last_read_item_id INTEGER;
ALTER TABLE contacts ADD COLUMN last_read_created_at TEXT;

-- TODO create indices
|]

down_m20241230_reports :: Query
down_m20241230_reports =
  [sql|
ALTER TABLE chat_items DROP COLUMN msg_content_tag;
ALTER TABLE chat_items DROP COLUMN processed_by_group_member_id;

ALTER TABLE groups DROP COLUMN unprocessed_reports_count;

ALTER TABLE groups DROP COLUMN unread_count;
ALTER TABLE groups DROP COLUMN last_read_item_id;
ALTER TABLE groups DROP COLUMN last_read_item_ts;

ALTER TABLE contacts DROP COLUMN unread_count;
ALTER TABLE contacts DROP COLUMN last_read_item_id;
ALTER TABLE contacts DROP COLUMN last_read_created_at;

-- TODO remove indices
|]

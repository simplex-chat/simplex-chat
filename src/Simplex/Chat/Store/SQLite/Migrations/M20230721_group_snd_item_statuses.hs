{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.SQLite.Migrations.M20230721_group_snd_item_statuses where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20230721_group_snd_item_statuses :: Query
m20230721_group_snd_item_statuses =
  [sql|
CREATE TABLE group_snd_item_statuses (
  group_snd_item_status_id INTEGER PRIMARY KEY,
  chat_item_id INTEGER NOT NULL REFERENCES chat_items ON DELETE CASCADE,
  group_member_id INTEGER NOT NULL REFERENCES group_members ON DELETE CASCADE,
  group_snd_item_status TEXT NOT NULL,
  created_at TEXT NOT NULL DEFAULT(datetime('now')),
  updated_at TEXT NOT NULL DEFAULT(datetime('now'))
);

CREATE INDEX idx_group_snd_item_statuses_chat_item_id ON group_snd_item_statuses(chat_item_id);
CREATE INDEX idx_group_snd_item_statuses_group_member_id ON group_snd_item_statuses(group_member_id);

UPDATE users SET send_rcpts_small_groups = 1 WHERE send_rcpts_contacts = 1;
|]

down_m20230721_group_snd_item_statuses :: Query
down_m20230721_group_snd_item_statuses =
  [sql|
DROP INDEX idx_group_snd_item_statuses_group_member_id;
DROP INDEX idx_group_snd_item_statuses_chat_item_id;

DROP TABLE group_snd_item_statuses;
|]

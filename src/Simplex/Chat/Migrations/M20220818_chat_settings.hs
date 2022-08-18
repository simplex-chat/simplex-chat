{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Migrations.M20220818_chat_settings where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20220818_chat_settings :: Query
m20220818_chat_settings =
  [sql|
CREATE TABLE chat_settings (
  chat_setting_id INTEGER PRIMARY KEY,
  setting_name TEXT UNIQUE,
  value_type TEXT, -- bool, string, int
  created_at TEXT NOT NULL DEFAULT (datetime('now')),
  updated_at TEXT NOT NULL DEFAULT (datetime('now'))
);

INSERT INTO chat_settings (setting_name, value_type)
VALUES ("auto_receive_images", "bool");

CREATE TABLE chat_setting_values (
  chat_setting_value_id INTEGER PRIMARY KEY,
  chat_setting_id INTEGER NOT NULL REFERENCES chat_settings ON DELETE CASCADE,
  -- setting contact_id / group_id to NULL defines a global setting
  contact_id INTEGER REFERENCES contacts ON DELETE CASCADE,
  group_id INTEGER REFERENCES groups ON DELETE CASCADE,
  setting_value TEXT NOT NULL,
  UNIQUE (chat_setting_id, contact_id, group_id)
);
|]

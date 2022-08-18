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
  value_type TEXT NOT NULL, -- bool, string, int
  value_default TEXT NOT NULL,
  created_at TEXT NOT NULL DEFAULT (datetime('now')),
  updated_at TEXT NOT NULL DEFAULT (datetime('now'))
);

INSERT INTO chat_settings (setting_name, value_type, value_default)
VALUES ('notifications', 'bool', 'true');

CREATE TABLE chat_setting_values (
  chat_setting_value_id INTEGER PRIMARY KEY,
  chat_setting_id INTEGER NOT NULL REFERENCES chat_settings ON DELETE CASCADE,
  user_id INTEGER NOT NULL REFERENCES users ON DELETE CASCADE,
  contact_id INTEGER REFERENCES contacts ON DELETE CASCADE,
  group_id INTEGER REFERENCES groups ON DELETE CASCADE,
  setting_value TEXT NOT NULL,
  created_at TEXT NOT NULL DEFAULT (datetime('now')),
  updated_at TEXT NOT NULL DEFAULT (datetime('now')),
  UNIQUE (chat_setting_id, user_id, contact_id, group_id)
);
|]

{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Migrations.M20220728_group_profile_versions where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20220728_group_profile_versions :: Query
m20220728_group_profile_versions =
  [sql|
CREATE TABLE group_profile_versions (
  group_profile_version_id INTEGER PRIMARY KEY,
  group_profile_id INTEGER NOT NULL REFERENCES group_profiles ON DELETE CASCADE,
  group_id INTEGER NOT NULL REFERENCES groups ON DELETE CASCADE,
  user_id INTEGER DEFAULT NULL REFERENCES users ON DELETE CASCADE,
  prev_profile_version BLOB,
  profile_version BLOB,
  display_name TEXT NOT NULL, -- this name must not contain spaces
  full_name TEXT NOT NULL,
  image TEXT,
  properties TEXT NOT NULL DEFAULT '{}', -- JSON with user or contact profile
  created_at TEXT NOT NULL DEFAULT (datetime('now')),
  updated_at TEXT NOT NULL DEFAULT (datetime('now'))
);
|]

{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}

module Directory.Store.SQLite.Migrations (directorySchemaMigrations) where

import Data.List (sortOn)
import Database.SQLite.Simple (Query (..))
import Database.SQLite.Simple.QQ (sql)
import Simplex.Messaging.Agent.Store.Shared (Migration (..))

directorySchemaMigrations :: [Migration]
directorySchemaMigrations = sortOn name $ map migration schemaMigrations
  where
    migration (name, up, down) = Migration {name, up = fromQuery up, down = fromQuery <$> down}

schemaMigrations :: [(String, Query, Maybe Query)]
schemaMigrations =
  [ ("20250924_initial", m20250924_initial, Nothing)
  ]

m20250924_initial :: Query
m20250924_initial =
  [sql|
CREATE TABLE sx_directory_store_info(
  version INTEGER NOT NULL,
  updated_at TEXT NOT NULL DEFAULT(datetime('now'))
);

INSERT INTO sx_directory_store_info (version) VALUES (1);

CREATE TABLE sx_directory_group_regs(
  group_reg_id INTEGER PRIMARY KEY AUTOINCREMENT,
  group_id INTEGER NOT NULL REFERENCES groups ON UPDATE RESTRICT ON DELETE CASCADE,
  user_group_reg_id INTEGER NOT NULL,
  contact_id INTEGER NOT NULL REFERENCES contacts(contact_id) ON UPDATE RESTRICT ON DELETE CASCADE,
  owner_member_id INTEGER REFERENCES group_members(group_member_id) ON UPDATE RESTRICT ON DELETE CASCADE,
  group_reg_status TEXT NOT NULL,
  group_promoted INTEGER NOT NULL,
  acceptance_reject_names TEXT, -- reject long names and names with profanity
  acceptance_pass_captcha TEXT, -- run captcha challenge with joining members
  acceptance_make_observer TEXT, -- the role assigned in the end, after captcha challenge
  created_at TEXT NOT NULL DEFAULT(datetime('now')),
  updated_at TEXT NOT NULL DEFAULT(datetime('now'))
);

CREATE UNIQUE INDEX idx_sx_directory_group_registrations_group_id ON sx_directory_group_regs(group_id);

CREATE UNIQUE INDEX idx_sx_directory_group_registrations_owner_member_id ON sx_directory_group_regs(owner_member_id);

CREATE INDEX idx_sx_directory_group_registrations_owner_contact_id ON sx_directory_group_regs(contact_id);

CREATE UNIQUE INDEX idx_sx_directory_group_registrations_owner_contact_id_user_group_reg_id ON sx_directory_group_regs(contact_id, user_group_reg_id);
  |]

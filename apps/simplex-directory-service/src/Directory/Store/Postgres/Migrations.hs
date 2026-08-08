{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}

module Directory.Store.Postgres.Migrations where

import Data.List (sortOn)
import Data.Text (Text)
import qualified Data.Text as T
import Simplex.Messaging.Agent.Store.Shared (Migration (..))
import Text.RawString.QQ (r)

directorySchemaMigrations :: [Migration]
directorySchemaMigrations = sortOn name $ map migration schemaMigrations
  where
    migration (name, up, down) = Migration {name, up, down}

schemaMigrations :: [(String, Text, Maybe Text)]
schemaMigrations =
  [ ("20250924_directory_schema", m20250924_directory_schema, Just down_m20250924_directory_schema),
    ("20260801_directory_contact_regs", m20260801_directory_contact_regs, Just down_m20260801_directory_contact_regs)
  ]

m20250924_directory_schema :: Text
m20250924_directory_schema =
  T.pack
    [r|
CREATE TABLE sx_directory_group_regs(
  group_reg_id BIGINT PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
  group_id BIGINT NOT NULL REFERENCES groups ON UPDATE RESTRICT ON DELETE CASCADE,
  user_group_reg_id BIGINT NOT NULL,
  contact_id BIGINT NOT NULL REFERENCES contacts(contact_id) ON UPDATE RESTRICT ON DELETE CASCADE,
  owner_member_id BIGINT REFERENCES group_members(group_member_id) ON UPDATE RESTRICT ON DELETE CASCADE,
  group_reg_status TEXT NOT NULL,
  group_promoted SMALLINT NOT NULL,
  created_at TIMESTAMPTZ NOT NULL DEFAULT (now()),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT (now())
);

CREATE UNIQUE INDEX idx_sx_directory_group_regs_group_id ON sx_directory_group_regs(group_id);
CREATE UNIQUE INDEX idx_sx_directory_group_regs_owner_member_id ON sx_directory_group_regs(owner_member_id);
CREATE UNIQUE INDEX idx_sx_directory_group_regs_owner_contact_id_user_group_reg_id ON sx_directory_group_regs(contact_id, user_group_reg_id);
  |]

down_m20250924_directory_schema :: Text
down_m20250924_directory_schema =
  T.pack
    [r|
DROP INDEX idx_sx_directory_group_regs_group_id;
DROP INDEX idx_sx_directory_group_regs_owner_member_id;
DROP INDEX idx_sx_directory_group_regs_owner_contact_id_user_group_reg_id;

DROP TABLE sx_directory_group_regs;
  |]

m20260801_directory_contact_regs :: Text
m20260801_directory_contact_regs =
  T.pack
    [r|
CREATE TABLE sx_directory_contact_regs(
  contact_reg_id BIGINT PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
  contact_id BIGINT REFERENCES contacts(contact_id) ON UPDATE RESTRICT ON DELETE CASCADE,
  peer_type TEXT NOT NULL,
  contact_reg_status TEXT NOT NULL,
  contact_promoted SMALLINT NOT NULL DEFAULT 0,
  created_at TIMESTAMPTZ NOT NULL DEFAULT (now()),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT (now())
);

CREATE UNIQUE INDEX idx_sx_directory_contact_regs_contact_id ON sx_directory_contact_regs(contact_id);
  |]

down_m20260801_directory_contact_regs :: Text
down_m20260801_directory_contact_regs =
  T.pack
    [r|
DROP INDEX idx_sx_directory_contact_regs_contact_id;

DROP TABLE sx_directory_contact_regs;
  |]

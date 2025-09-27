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
  [ ("20250924_directory_schema", m20250924_directory_schema, Just down_m20250924_directory_schema)
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

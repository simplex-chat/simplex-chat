{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.Postgres.Migrations.M20261020_link_notices where

import Data.Text (Text)
import Text.RawString.QQ (r)

m20261020_link_notices :: Text
m20261020_link_notices =
  [r|
CREATE TABLE link_notices(
  link_notice_id BIGINT GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
  link_hash BYTEA NOT NULL,
  expires_at TIMESTAMPTZ,
  created_at TIMESTAMPTZ NOT NULL,
  updated_at TIMESTAMPTZ NOT NULL
);

CREATE UNIQUE INDEX idx_link_notices_link_hash ON link_notices(link_hash);

ALTER TABLE group_members ADD COLUMN member_restriction_expires_at TIMESTAMPTZ;
ALTER TABLE group_members ADD COLUMN member_restriction_by_user SMALLINT NOT NULL DEFAULT 0;

CREATE INDEX idx_group_members_member_restriction_expires_at ON group_members(member_restriction_expires_at);
|]

down_m20261020_link_notices :: Text
down_m20261020_link_notices =
  [r|
DROP INDEX idx_group_members_member_restriction_expires_at;

ALTER TABLE group_members DROP COLUMN member_restriction_by_user;
ALTER TABLE group_members DROP COLUMN member_restriction_expires_at;

DROP INDEX idx_link_notices_link_hash;
DROP TABLE link_notices;
|]

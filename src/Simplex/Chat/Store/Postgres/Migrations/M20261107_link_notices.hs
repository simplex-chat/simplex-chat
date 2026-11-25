{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.Postgres.Migrations.M20261107_link_notices where

import Data.Text (Text)
import Text.RawString.QQ (r)

m20261107_link_notices :: Text
m20261107_link_notices =
  [r|
CREATE TABLE link_notices(
  link_notice_id BIGINT GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
  link_hash BYTEA NOT NULL,
  expires_at TIMESTAMPTZ,
  created_at TIMESTAMPTZ NOT NULL,
  updated_at TIMESTAMPTZ NOT NULL
);

CREATE UNIQUE INDEX idx_link_notices_link_hash ON link_notices(link_hash);

ALTER TABLE groups ADD COLUMN silently_removed SMALLINT NOT NULL DEFAULT 0;
|]

down_m20261107_link_notices :: Text
down_m20261107_link_notices =
  [r|
ALTER TABLE groups DROP COLUMN silently_removed;

DROP INDEX idx_link_notices_link_hash;
DROP TABLE link_notices;
|]

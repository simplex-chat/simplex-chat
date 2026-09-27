{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.Postgres.Migrations.M20260926_link_notices where

import Data.Text (Text)
import Text.RawString.QQ (r)

m20260926_link_notices :: Text
m20260926_link_notices =
  [r|
CREATE TABLE link_notices(
  link_notice_id BIGINT GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
  link_hash BYTEA NOT NULL,
  expires_at TIMESTAMPTZ,
  reason TEXT,
  created_at TIMESTAMPTZ NOT NULL,
  updated_at TIMESTAMPTZ NOT NULL
);

CREATE UNIQUE INDEX idx_link_notices_link_hash ON link_notices(link_hash);
|]

down_m20260926_link_notices :: Text
down_m20260926_link_notices =
  [r|
DROP INDEX idx_link_notices_link_hash;
DROP TABLE link_notices;
|]

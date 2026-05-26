{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.Postgres.Migrations.M20260429_relay_request_retries where

import Data.Text (Text)
import Text.RawString.QQ (r)

m20260429_relay_request_retries :: Text
m20260429_relay_request_retries =
  [r|
ALTER TABLE groups ADD COLUMN relay_request_retries BIGINT NOT NULL DEFAULT 0;
ALTER TABLE groups ADD COLUMN relay_request_delay BIGINT NOT NULL DEFAULT 0;
ALTER TABLE groups ADD COLUMN relay_request_execute_at TIMESTAMPTZ NOT NULL DEFAULT '1970-01-01 00:00:00+00';
|]

down_m20260429_relay_request_retries :: Text
down_m20260429_relay_request_retries =
  [r|
ALTER TABLE groups DROP COLUMN relay_request_retries;
ALTER TABLE groups DROP COLUMN relay_request_delay;
ALTER TABLE groups DROP COLUMN relay_request_execute_at;
|]

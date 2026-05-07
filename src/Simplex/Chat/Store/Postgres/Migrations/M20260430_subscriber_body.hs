{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.Postgres.Migrations.M20260430_subscriber_body where

import Data.Text (Text)
import Text.RawString.QQ (r)

m20260430_subscriber_body :: Text
m20260430_subscriber_body =
  [r|
ALTER TABLE delivery_jobs ADD COLUMN subscriber_body BYTEA;
|]

down_m20260430_subscriber_body :: Text
down_m20260430_subscriber_body =
  [r|
ALTER TABLE delivery_jobs DROP COLUMN subscriber_body;
|]

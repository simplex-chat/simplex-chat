{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.Postgres.Migrations.M20260531_relay_sent_web_url where

import Data.Text (Text)
import Text.RawString.QQ (r)

m20260531_relay_sent_web_url :: Text
m20260531_relay_sent_web_url =
  [r|
ALTER TABLE groups ADD COLUMN relay_sent_web_url TEXT;
|]

down_m20260531_relay_sent_web_url :: Text
down_m20260531_relay_sent_web_url =
  [r|
ALTER TABLE groups DROP COLUMN relay_sent_web_url;
|]

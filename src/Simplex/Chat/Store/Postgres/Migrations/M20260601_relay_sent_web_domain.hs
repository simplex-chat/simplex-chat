{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.Postgres.Migrations.M20260601_relay_sent_web_domain where

import Data.Text (Text)
import Text.RawString.QQ (r)

m20260601_relay_sent_web_domain :: Text
m20260601_relay_sent_web_domain =
  [r|
ALTER TABLE groups ADD COLUMN relay_sent_web_domain TEXT;
|]

down_m20260601_relay_sent_web_domain :: Text
down_m20260601_relay_sent_web_domain =
  [r|
ALTER TABLE groups DROP COLUMN relay_sent_web_domain;
|]

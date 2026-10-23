{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.Postgres.Migrations.M20260507_relay_inactive_at where

import Data.Text (Text)
import Text.RawString.QQ (r)

m20260507_relay_inactive_at :: Text
m20260507_relay_inactive_at =
  [r|
ALTER TABLE groups ADD COLUMN relay_inactive_at TIMESTAMPTZ;
|]

down_m20260507_relay_inactive_at :: Text
down_m20260507_relay_inactive_at =
  [r|
ALTER TABLE groups DROP COLUMN relay_inactive_at;
|]

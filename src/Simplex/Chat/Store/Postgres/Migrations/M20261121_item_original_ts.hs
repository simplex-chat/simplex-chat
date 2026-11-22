{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.Postgres.Migrations.M20261121_item_original_ts where

import Data.Text (Text)
import Text.RawString.QQ (r)

m20261121_item_original_ts :: Text
m20261121_item_original_ts =
  [r|
ALTER TABLE chat_items ADD COLUMN item_original_ts TIMESTAMPTZ;
|]

down_m20261121_item_original_ts :: Text
down_m20261121_item_original_ts =
  [r|
ALTER TABLE chat_items DROP COLUMN item_original_ts;
|]

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.Postgres.Migrations.M20261113_item_priority where

import Data.Text (Text)
import Text.RawString.QQ (r)

m20261113_item_priority :: Text
m20261113_item_priority =
  [r|
ALTER TABLE chat_items ADD COLUMN item_priority SMALLINT NOT NULL DEFAULT 0;
|]

down_m20261113_item_priority :: Text
down_m20261113_item_priority =
  [r|
ALTER TABLE chat_items DROP COLUMN item_priority;
|]

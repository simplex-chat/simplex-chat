{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.Postgres.Migrations.M20260716_signed_history where

import Data.Text (Text)
import Text.RawString.QQ (r)

m20260716_signed_history :: Text
m20260716_signed_history =
  [r|
ALTER TABLE chat_items ADD COLUMN item_msg_body BYTEA;
ALTER TABLE chat_items ADD COLUMN item_chat_binding TEXT;
ALTER TABLE chat_items ADD COLUMN item_signatures BYTEA;
|]

down_m20260716_signed_history :: Text
down_m20260716_signed_history =
  [r|
ALTER TABLE chat_items DROP COLUMN item_msg_body;
ALTER TABLE chat_items DROP COLUMN item_chat_binding;
ALTER TABLE chat_items DROP COLUMN item_signatures;
|]

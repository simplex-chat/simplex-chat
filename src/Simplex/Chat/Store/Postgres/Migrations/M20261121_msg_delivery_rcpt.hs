{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.Postgres.Migrations.M20261121_msg_delivery_rcpt where

import Data.Text (Text)
import Text.RawString.QQ (r)

m20261121_msg_delivery_rcpt :: Text
m20261121_msg_delivery_rcpt =
  [r|
ALTER TABLE msg_deliveries ADD COLUMN rcpt SMALLINT NOT NULL DEFAULT 0;
|]

down_m20261121_msg_delivery_rcpt :: Text
down_m20261121_msg_delivery_rcpt =
  [r|
ALTER TABLE msg_deliveries DROP COLUMN rcpt;
|]

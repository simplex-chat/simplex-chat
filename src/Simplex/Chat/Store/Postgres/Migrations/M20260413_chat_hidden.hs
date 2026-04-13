{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.Postgres.Migrations.M20260413_chat_hidden where

import Data.Text (Text)
import Text.RawString.QQ (r)

m20260413_chat_hidden :: Text
m20260413_chat_hidden =
  [r|
ALTER TABLE groups ADD COLUMN chat_hidden SMALLINT NOT NULL DEFAULT 0;
ALTER TABLE groups ADD COLUMN owner_sig TEXT;
|]

down_m20260413_chat_hidden :: Text
down_m20260413_chat_hidden =
  [r|
ALTER TABLE groups DROP COLUMN chat_hidden;
ALTER TABLE groups DROP COLUMN owner_sig;
|]

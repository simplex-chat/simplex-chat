{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.Postgres.Migrations.M20261019_chat_layout where

import Data.Text (Text)
import Text.RawString.QQ (r)

m20261019_chat_layout :: Text
m20261019_chat_layout =
  [r|
ALTER TABLE contacts ADD COLUMN chat_layout TEXT;
ALTER TABLE group_profiles ADD COLUMN chat_layout TEXT;
ALTER TABLE user_contact_links ADD COLUMN chat_layout TEXT;
|]

down_m20261019_chat_layout :: Text
down_m20261019_chat_layout =
  [r|
ALTER TABLE contacts DROP COLUMN chat_layout;
ALTER TABLE group_profiles DROP COLUMN chat_layout;
ALTER TABLE user_contact_links DROP COLUMN chat_layout;
|]

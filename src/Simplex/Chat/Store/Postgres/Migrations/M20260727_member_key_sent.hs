{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.Postgres.Migrations.M20260727_member_key_sent where

import Data.Text (Text)
import Text.RawString.QQ (r)

m20260727_member_key_sent :: Text
m20260727_member_key_sent =
  [r|
ALTER TABLE group_members ADD COLUMN user_member_key_status TEXT;
ALTER TABLE group_members ADD COLUMN user_member_key_attempts BIGINT NOT NULL DEFAULT 0;
|]

down_m20260727_member_key_sent :: Text
down_m20260727_member_key_sent =
  [r|
ALTER TABLE group_members DROP COLUMN user_member_key_status;
ALTER TABLE group_members DROP COLUMN user_member_key_attempts;
|]

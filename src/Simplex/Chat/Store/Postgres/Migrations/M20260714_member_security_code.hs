{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.Postgres.Migrations.M20260714_member_security_code where

import Data.Text (Text)
import Text.RawString.QQ (r)

m20260714_member_security_code :: Text
m20260714_member_security_code =
  [r|
ALTER TABLE group_members ADD COLUMN member_security_code TEXT;
ALTER TABLE group_members ADD COLUMN member_security_code_verified_at TEXT;
|]

down_m20260714_member_security_code :: Text
down_m20260714_member_security_code =
  [r|
ALTER TABLE group_members DROP COLUMN member_security_code;
ALTER TABLE group_members DROP COLUMN member_security_code_verified_at;
|]

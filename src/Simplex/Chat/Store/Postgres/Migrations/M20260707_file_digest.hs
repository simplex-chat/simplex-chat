{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.Postgres.Migrations.M20260707_file_digest where

import Data.Text (Text)
import Text.RawString.QQ (r)

m20260707_file_digest :: Text
m20260707_file_digest =
  [r|
ALTER TABLE files ADD COLUMN file_digest BYTEA;
|]

down_m20260707_file_digest :: Text
down_m20260707_file_digest =
  [r|
ALTER TABLE files DROP COLUMN file_digest;
|]

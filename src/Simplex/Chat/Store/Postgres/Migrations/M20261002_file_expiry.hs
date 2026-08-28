{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.Postgres.Migrations.M20261002_file_expiry where

import Data.Text (Text)
import Text.RawString.QQ (r)

m20261002_file_expiry :: Text
m20261002_file_expiry =
  [r|
ALTER TABLE files ADD COLUMN file_expires_at TIMESTAMPTZ;
|]

down_m20261002_file_expiry :: Text
down_m20261002_file_expiry =
  [r|
ALTER TABLE files DROP COLUMN file_expires_at;
|]

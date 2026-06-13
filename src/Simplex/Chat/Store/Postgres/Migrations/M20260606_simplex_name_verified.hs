{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.Postgres.Migrations.M20260606_simplex_name_verified where

import Data.Text (Text)
import Text.RawString.QQ (r)

m20260606_simplex_name_verified :: Text
m20260606_simplex_name_verified =
  [r|
ALTER TABLE contacts ADD COLUMN simplex_name_verified_at TIMESTAMPTZ;
ALTER TABLE groups ADD COLUMN simplex_name_verified_at TIMESTAMPTZ;
|]

down_m20260606_simplex_name_verified :: Text
down_m20260606_simplex_name_verified =
  [r|
ALTER TABLE groups DROP COLUMN simplex_name_verified_at;
ALTER TABLE contacts DROP COLUMN simplex_name_verified_at;
|]

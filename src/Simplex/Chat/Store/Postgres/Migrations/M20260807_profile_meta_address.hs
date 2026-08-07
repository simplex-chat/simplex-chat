{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.Postgres.Migrations.M20260807_profile_meta_address where

import Data.Text (Text)
import Text.RawString.QQ (r)

-- | See the SQLite migration for what this holds and why it is safe to publish.
m20260807_profile_meta_address :: Text
m20260807_profile_meta_address =
  [r|
ALTER TABLE contact_profiles ADD COLUMN meta_address TEXT;
|]

down_m20260807_profile_meta_address :: Text
down_m20260807_profile_meta_address =
  [r|
ALTER TABLE contact_profiles DROP COLUMN meta_address;
|]

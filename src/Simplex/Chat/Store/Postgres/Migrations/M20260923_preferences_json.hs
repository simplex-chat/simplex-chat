{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.Postgres.Migrations.M20260923_preferences_json where

import Data.Text (Text)
import Text.RawString.QQ (r)

m20260923_preferences_json :: Text
m20260923_preferences_json =
  [r|
ALTER TABLE contact_profiles ADD COLUMN preferences_json TEXT;
ALTER TABLE group_profiles ADD COLUMN preferences_json TEXT;
|]

down_m20260923_preferences_json :: Text
down_m20260923_preferences_json =
  [r|
ALTER TABLE group_profiles DROP COLUMN preferences_json;
ALTER TABLE contact_profiles DROP COLUMN preferences_json;
|]

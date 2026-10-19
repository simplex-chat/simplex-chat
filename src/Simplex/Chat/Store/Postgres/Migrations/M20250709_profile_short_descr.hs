{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.Postgres.Migrations.M20250709_profile_short_descr where

import Data.Text (Text)
import qualified Data.Text as T
import Text.RawString.QQ (r)

m20250709_profile_short_descr :: Text
m20250709_profile_short_descr =
  T.pack
    [r|
ALTER TABLE contact_profiles ADD COLUMN short_descr TEXT;
ALTER TABLE group_profiles ADD COLUMN short_descr TEXT;
|]

down_m20250709_profile_short_descr :: Text
down_m20250709_profile_short_descr =
  T.pack
    [r|
ALTER TABLE contact_profiles DROP COLUMN short_descr;
ALTER TABLE group_profiles DROP COLUMN short_descr;
|]

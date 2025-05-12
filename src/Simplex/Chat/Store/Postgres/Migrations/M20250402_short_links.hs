{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.Postgres.Migrations.M20250402_short_links where

import Data.Text (Text)
import qualified Data.Text as T
import Text.RawString.QQ (r)

m20250402_short_links :: Text
m20250402_short_links =
  T.pack
    [r|
ALTER TABLE user_contact_links ADD COLUMN short_link_contact BYTEA;
ALTER TABLE connections ADD COLUMN short_link_inv BYTEA;
ALTER TABLE connections ADD COLUMN via_short_link_contact BYTEA;
|]

down_m20250402_short_links :: Text
down_m20250402_short_links =
  T.pack
    [r|
ALTER TABLE user_contact_links DROP COLUMN short_link_contact;
ALTER TABLE connections DROP COLUMN short_link_inv;
ALTER TABLE connections DROP COLUMN via_short_link_contact;
|]

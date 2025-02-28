{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.Postgres.Migrations.M20250227_member_acceptance where

import Data.Text (Text)
import qualified Data.Text as T
import Text.RawString.QQ (r)

m20250227_member_acceptance :: Text
m20250227_member_acceptance =
  T.pack
    [r|
ALTER TABLE user_contact_links ADD COLUMN group_link_auto_accept TEXT NULL;
|]

down_m20250227_member_acceptance :: Text
down_m20250227_member_acceptance =
  T.pack
    [r|
ALTER TABLE user_contact_links DROP COLUMN group_link_auto_accept;
|]

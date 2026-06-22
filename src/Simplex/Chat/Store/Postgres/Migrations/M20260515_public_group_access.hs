{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.Postgres.Migrations.M20260515_public_group_access where

import Data.Text (Text)
import Text.RawString.QQ (r)

m20260515_public_group_access :: Text
m20260515_public_group_access =
  [r|
ALTER TABLE group_profiles ADD COLUMN group_web_page TEXT;
ALTER TABLE group_profiles ADD COLUMN group_domain TEXT;
ALTER TABLE group_profiles ADD COLUMN domain_web_page BIGINT;
ALTER TABLE group_profiles ADD COLUMN allow_embedding BIGINT;

ALTER TABLE group_relays ADD COLUMN base_web_url TEXT;
|]

down_m20260515_public_group_access :: Text
down_m20260515_public_group_access =
  [r|
ALTER TABLE group_relays DROP COLUMN base_web_url;

ALTER TABLE group_profiles DROP COLUMN allow_embedding;
ALTER TABLE group_profiles DROP COLUMN domain_web_page;
ALTER TABLE group_profiles DROP COLUMN group_domain;
ALTER TABLE group_profiles DROP COLUMN group_web_page;
|]

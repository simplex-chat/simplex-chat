{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.SQLite.Migrations.M20260515_public_group_access where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20260515_public_group_access :: Query
m20260515_public_group_access =
  [sql|
ALTER TABLE group_profiles ADD COLUMN group_web_page TEXT;
ALTER TABLE group_profiles ADD COLUMN group_domain TEXT;
ALTER TABLE group_profiles ADD COLUMN domain_web_page INTEGER;
ALTER TABLE group_profiles ADD COLUMN allow_embedding INTEGER;

ALTER TABLE group_relays ADD COLUMN base_web_url TEXT;
|]

down_m20260515_public_group_access :: Query
down_m20260515_public_group_access =
  [sql|
ALTER TABLE group_relays DROP COLUMN base_web_url;

ALTER TABLE group_profiles DROP COLUMN allow_embedding;
ALTER TABLE group_profiles DROP COLUMN domain_web_page;
ALTER TABLE group_profiles DROP COLUMN group_domain;
ALTER TABLE group_profiles DROP COLUMN group_web_page;
|]

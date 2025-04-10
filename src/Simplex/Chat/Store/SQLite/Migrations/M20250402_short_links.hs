{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.SQLite.Migrations.M20250402_short_links where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20250402_short_links :: Query
m20250402_short_links =
  [sql|
ALTER TABLE user_contact_links ADD COLUMN short_link_contact BLOB;
ALTER TABLE connections ADD COLUMN short_link_inv BLOB;

|]

down_m20250402_short_links :: Query
down_m20250402_short_links =
  [sql|
ALTER TABLE user_contact_links DROP COLUMN short_link_contact;
ALTER TABLE connections DROP COLUMN short_link_inv;
|]

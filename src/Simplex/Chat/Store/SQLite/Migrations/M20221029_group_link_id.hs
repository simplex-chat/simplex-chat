{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.SQLite.Migrations.M20221029_group_link_id where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20221029_group_link_id :: Query
m20221029_group_link_id =
  [sql|
ALTER TABLE user_contact_links ADD COLUMN group_link_id BLOB;

ALTER TABLE connections ADD COLUMN group_link_id BLOB;
|]

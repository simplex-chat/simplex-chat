{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.SQLite.Migrations.M20221011_user_contact_links_group_id where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20221011_user_contact_links_group_id :: Query
m20221011_user_contact_links_group_id =
  [sql|
ALTER TABLE user_contact_links ADD COLUMN group_id INTEGER REFERENCES groups ON DELETE CASCADE;

CREATE UNIQUE INDEX idx_user_contact_links_group_id ON user_contact_links(group_id);
|]

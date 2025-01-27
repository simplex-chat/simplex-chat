{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.SQLite.Migrations.M20220822_groups_host_conn_custom_user_profile_id where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20220822_groups_host_conn_custom_user_profile_id :: Query
m20220822_groups_host_conn_custom_user_profile_id =
  [sql|
ALTER TABLE groups ADD COLUMN host_conn_custom_user_profile_id INTEGER REFERENCES contact_profiles ON DELETE SET NULL; -- id of custom user profile used in direct connection with host
|]

{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Migrations.M20220820_invited_by_conn_custom_user_profile_id where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20220820_invited_by_conn_custom_user_profile_id :: Query
m20220820_invited_by_conn_custom_user_profile_id =
  [sql|
ALTER TABLE group_members ADD COLUMN invited_by_conn_custom_user_profile_id INTEGER REFERENCES contact_profiles ON DELETE SET NULL; -- id of custom user profile used in direct connection with host
|]

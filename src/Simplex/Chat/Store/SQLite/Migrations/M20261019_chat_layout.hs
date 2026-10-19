{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.SQLite.Migrations.M20261019_chat_layout where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20261019_chat_layout :: Query
m20261019_chat_layout =
  [sql|
ALTER TABLE contacts ADD COLUMN chat_layout TEXT;
ALTER TABLE contacts ADD COLUMN user_chat_layout TEXT;
ALTER TABLE groups ADD COLUMN chat_layout TEXT;
ALTER TABLE groups ADD COLUMN user_chat_layout TEXT;
ALTER TABLE group_profiles ADD COLUMN chat_layout TEXT;
ALTER TABLE user_contact_links ADD COLUMN chat_layout TEXT;
ALTER TABLE users ADD COLUMN chat_layout TEXT;
|]

down_m20261019_chat_layout :: Query
down_m20261019_chat_layout =
  [sql|
ALTER TABLE contacts DROP COLUMN chat_layout;
ALTER TABLE contacts DROP COLUMN user_chat_layout;
ALTER TABLE groups DROP COLUMN chat_layout;
ALTER TABLE groups DROP COLUMN user_chat_layout;
ALTER TABLE group_profiles DROP COLUMN chat_layout;
ALTER TABLE user_contact_links DROP COLUMN chat_layout;
ALTER TABLE users DROP COLUMN chat_layout;
|]

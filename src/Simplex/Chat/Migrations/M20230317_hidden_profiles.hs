{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Migrations.M20230317_hidden_profiles where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20230317_hidden_profiles :: Query
m20230317_hidden_profiles =
  [sql|
ALTER TABLE users ADD COLUMN view_pwd_hash BLOB NULL;

ALTER TABLE users ADD COLUMN view_pwd_salt BLOB NULL;

ALTER TABLE users ADD COLUMN wipe_pwd_hash BLOB NULL;

ALTER TABLE users ADD COLUMN wipe_pwd_salt BLOB NULL;

ALTER TABLE users ADD COLUMN hide_ntfs INTEGER NULL DEFAULT 0;
|]

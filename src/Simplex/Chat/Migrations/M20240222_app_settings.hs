{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Migrations.M20240222_app_settings where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20240222_app_settings :: Query
m20240222_app_settings =
  [sql|
CREATE TABLE app_settings (
  app_settings TEXT NOT NULL
);
|]

down_m20240222_app_settings :: Query
down_m20240222_app_settings =
  [sql|
DROP TABLE app_settings;
|]

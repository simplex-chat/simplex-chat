{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Migrations.M20240430_wallpapers where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20240430_wallpapers :: Query
m20240430_wallpapers =
  [sql|
ALTER TABLE users ADD COLUMN wallpaper TEXT; -- JSON
ALTER TABLE contacts ADD COLUMN wallpaper TEXT;
ALTER TABLE groups ADD COLUMN wallpaper TEXT;
|]

down_m20240430_wallpapers :: Query
down_m20240430_wallpapers =
  [sql|
ALTER TABLE users DROP COLUMN wallpaper;
ALTER TABLE contacts DROP COLUMN wallpaper;
ALTER TABLE groups DROP COLUMN wallpaper;
|]

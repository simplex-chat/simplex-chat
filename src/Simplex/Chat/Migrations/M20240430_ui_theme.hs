{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Migrations.M20240430_ui_theme where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20240430_ui_theme :: Query
m20240430_ui_theme =
  [sql|
ALTER TABLE contacts ADD COLUMN ui_theme TEXT;
ALTER TABLE groups ADD COLUMN ui_theme TEXT;
|]

down_m20240430_ui_theme :: Query
down_m20240430_ui_theme =
  [sql|
ALTER TABLE contacts DROP COLUMN ui_theme;
ALTER TABLE groups DROP COLUMN ui_theme;
|]

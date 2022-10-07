{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Migrations.M20221007_inline_files where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20221007_inline_files :: Query
m20221007_inline_files =
  [sql|
ALTER TABLE rcv_files ADD COLUMN file_inline TEXT;
ALTER TABLE snd_files ADD COLUMN file_inline TEXT;
|]

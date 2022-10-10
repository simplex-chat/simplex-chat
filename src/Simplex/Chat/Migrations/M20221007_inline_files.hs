{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Migrations.M20221007_inline_files where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20221007_inline_files :: Query
m20221007_inline_files =
  [sql|
PRAGMA ignore_check_constraints=ON;

ALTER TABLE rcv_files ADD COLUMN file_inline INTEGER DEFAULT 0 CHECK (file_inline NOT NULL);
UPDATE rcv_files SET file_inline = 0;

ALTER TABLE snd_files ADD COLUMN file_inline INTEGER DEFAULT 0 CHECK (file_inline NOT NULL);
UPDATE snd_files SET file_inline = 0;

PRAGMA ignore_check_constraints=OFF;
|]

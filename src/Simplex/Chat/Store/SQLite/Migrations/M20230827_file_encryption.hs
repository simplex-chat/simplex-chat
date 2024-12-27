{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.SQLite.Migrations.M20230827_file_encryption where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20230827_file_encryption :: Query
m20230827_file_encryption =
  [sql|
ALTER TABLE files ADD COLUMN file_crypto_key BLOB;
ALTER TABLE files ADD COLUMN file_crypto_nonce BLOB;
|]

down_m20230827_file_encryption :: Query
down_m20230827_file_encryption =
  [sql|
ALTER TABLE files DROP COLUMN file_crypto_key;
ALTER TABLE files DROP COLUMN file_crypto_nonce;
|]

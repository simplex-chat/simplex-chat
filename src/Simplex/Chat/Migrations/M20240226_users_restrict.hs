{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Migrations.M20240226_users_restrict where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20240226_users_restrict :: Query
m20240226_users_restrict =
  [sql|
PRAGMA writable_schema=1;

UPDATE sqlite_master
SET sql = replace(sql, 'ON DELETE CASCADE', 'ON DELETE RESTRICT')
WHERE name = 'users' AND type = 'table';

PRAGMA writable_schema=0;
|]

down_m20240226_users_restrict :: Query
down_m20240226_users_restrict =
  [sql|
PRAGMA writable_schema=1;

UPDATE sqlite_master
SET sql = replace(sql, 'ON DELETE RESTRICT', 'ON DELETE CASCADE')
WHERE name = 'users' AND type = 'table';

PRAGMA writable_schema=0;
|]

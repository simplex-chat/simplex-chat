{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.SQLite.Migrations.M20251231_strict_tables_2 where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20251231_strict_tables_2 :: Query
m20251231_strict_tables_2 =
  [sql|
PRAGMA writable_schema=1;

UPDATE sqlite_master
SET sql = replace(sql, 'call_state BLOB NOT NULL', 'call_state TEXT NOT NULL')
WHERE type = 'table' AND name = 'calls';

UPDATE sqlite_master
SET sql = replace(sql, 'local_alias DEFAULT', 'local_alias TEXT DEFAULT')
WHERE type = 'table' AND name = 'connections';

UPDATE sqlite_master
SET sql = CASE
  WHEN LOWER(SUBSTR(sql, -15)) = ') without rowid' THEN sql || ', STRICT'
  WHEN SUBSTR(sql, -1) = ')' THEN sql || ' STRICT'
  ELSE sql
END
WHERE type = 'table' AND name != 'sqlite_sequence';

PRAGMA writable_schema=RESET;
|]

down_m20251231_strict_tables_2 :: Query
down_m20251231_strict_tables_2 =
  [sql|
PRAGMA writable_schema=1;

UPDATE sqlite_master
SET sql = CASE
  WHEN LOWER(SUBSTR(sql, -8)) = ', strict' THEN SUBSTR(sql, 1, LENGTH(sql) - 8)
  WHEN LOWER(SUBSTR(sql, -7)) = ' strict' THEN SUBSTR(sql, 1, LENGTH(sql) - 7)
  ELSE sql
END
WHERE type = 'table' AND name != 'sqlite_sequence';

UPDATE sqlite_master
SET sql = replace(sql, 'call_state TEXT NOT NULL', 'call_state BLOB NOT NULL')
WHERE type = 'table' AND name = 'calls';

UPDATE sqlite_master
SET sql = replace(sql, 'local_alias TEXT DEFAULT', 'local_alias DEFAULT')
WHERE type = 'table' AND name = 'connections';

PRAGMA writable_schema=RESET;
|]

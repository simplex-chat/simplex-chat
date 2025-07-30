{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.SQLite.Migrations.M20241023_chat_item_autoincrement_id where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20241023_chat_item_autoincrement_id :: Query
m20241023_chat_item_autoincrement_id =
    [sql|
INSERT INTO sqlite_sequence (name, seq)
SELECT 'chat_items', MAX(ROWID) FROM chat_items;

PRAGMA writable_schema=1;

UPDATE sqlite_master SET sql = replace(sql, 'INTEGER PRIMARY KEY', 'INTEGER PRIMARY KEY AUTOINCREMENT')
WHERE name = 'chat_items' AND type = 'table';

PRAGMA writable_schema=0;
|]

down_m20241023_chat_item_autoincrement_id :: Query
down_m20241023_chat_item_autoincrement_id =
    [sql|
DELETE FROM sqlite_sequence WHERE name = 'chat_items';

PRAGMA writable_schema=1;

UPDATE sqlite_master
SET sql = replace(sql, 'INTEGER PRIMARY KEY AUTOINCREMENT', 'INTEGER PRIMARY KEY')
WHERE name = 'chat_items' AND type = 'table';

PRAGMA writable_schema=0;
|]

{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.SQLite.Migrations.M20261018_autoincrement_ids where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20261018_autoincrement_ids :: Query
m20261018_autoincrement_ids =
  [sql|
INSERT INTO sqlite_sequence (name, seq)
SELECT 'users', MAX(ROWID) FROM users;

INSERT INTO sqlite_sequence (name, seq)
SELECT 'contacts', MAX(ROWID) FROM contacts;

INSERT INTO sqlite_sequence (name, seq)
SELECT 'groups', MAX(ROWID) FROM groups;

INSERT INTO sqlite_sequence (name, seq)
SELECT 'group_members', MAX(ROWID) FROM group_members;

INSERT INTO sqlite_sequence (name, seq)
SELECT 'files', MAX(ROWID) FROM files;

INSERT INTO sqlite_sequence (name, seq)
SELECT 'connections', MAX(ROWID) FROM connections;

INSERT INTO sqlite_sequence (name, seq)
SELECT 'contact_requests', MAX(ROWID) FROM contact_requests;

INSERT INTO sqlite_sequence (name, seq)
SELECT 'rcv_roster_transfers', MAX(ROWID) FROM rcv_roster_transfers;

PRAGMA writable_schema=1;

UPDATE sqlite_master SET sql = replace(sql, 'user_id INTEGER PRIMARY KEY,', 'user_id INTEGER PRIMARY KEY AUTOINCREMENT,')
WHERE name = 'users' AND type = 'table';

UPDATE sqlite_master SET sql = replace(sql, 'contact_id INTEGER PRIMARY KEY,', 'contact_id INTEGER PRIMARY KEY AUTOINCREMENT,')
WHERE name = 'contacts' AND type = 'table';

UPDATE sqlite_master SET sql = replace(sql, 'group_id INTEGER PRIMARY KEY,', 'group_id INTEGER PRIMARY KEY AUTOINCREMENT,')
WHERE name = 'groups' AND type = 'table';

UPDATE sqlite_master SET sql = replace(sql, 'group_member_id INTEGER PRIMARY KEY,', 'group_member_id INTEGER PRIMARY KEY AUTOINCREMENT,')
WHERE name = 'group_members' AND type = 'table';

UPDATE sqlite_master SET sql = replace(sql, 'file_id INTEGER PRIMARY KEY,', 'file_id INTEGER PRIMARY KEY AUTOINCREMENT,')
WHERE name = 'files' AND type = 'table';

UPDATE sqlite_master SET sql = replace(sql, 'connection_id INTEGER PRIMARY KEY,', 'connection_id INTEGER PRIMARY KEY AUTOINCREMENT,')
WHERE name = 'connections' AND type = 'table';

UPDATE sqlite_master SET sql = replace(sql, 'contact_request_id INTEGER PRIMARY KEY,', 'contact_request_id INTEGER PRIMARY KEY AUTOINCREMENT,')
WHERE name = 'contact_requests' AND type = 'table';

UPDATE sqlite_master SET sql = replace(sql, 'roster_transfer_id INTEGER PRIMARY KEY,', 'roster_transfer_id INTEGER PRIMARY KEY AUTOINCREMENT,')
WHERE name = 'rcv_roster_transfers' AND type = 'table';

PRAGMA writable_schema=RESET;
|]

down_m20261018_autoincrement_ids :: Query
down_m20261018_autoincrement_ids =
  [sql|
DELETE FROM sqlite_sequence WHERE name = 'users';

DELETE FROM sqlite_sequence WHERE name = 'contacts';

DELETE FROM sqlite_sequence WHERE name = 'groups';

DELETE FROM sqlite_sequence WHERE name = 'group_members';

DELETE FROM sqlite_sequence WHERE name = 'files';

DELETE FROM sqlite_sequence WHERE name = 'connections';

DELETE FROM sqlite_sequence WHERE name = 'contact_requests';

DELETE FROM sqlite_sequence WHERE name = 'rcv_roster_transfers';

PRAGMA writable_schema=1;

UPDATE sqlite_master SET sql = replace(sql, 'user_id INTEGER PRIMARY KEY AUTOINCREMENT,', 'user_id INTEGER PRIMARY KEY,')
WHERE name = 'users' AND type = 'table';

UPDATE sqlite_master SET sql = replace(sql, 'contact_id INTEGER PRIMARY KEY AUTOINCREMENT,', 'contact_id INTEGER PRIMARY KEY,')
WHERE name = 'contacts' AND type = 'table';

UPDATE sqlite_master SET sql = replace(sql, 'group_id INTEGER PRIMARY KEY AUTOINCREMENT,', 'group_id INTEGER PRIMARY KEY,')
WHERE name = 'groups' AND type = 'table';

UPDATE sqlite_master SET sql = replace(sql, 'group_member_id INTEGER PRIMARY KEY AUTOINCREMENT,', 'group_member_id INTEGER PRIMARY KEY,')
WHERE name = 'group_members' AND type = 'table';

UPDATE sqlite_master SET sql = replace(sql, 'file_id INTEGER PRIMARY KEY AUTOINCREMENT,', 'file_id INTEGER PRIMARY KEY,')
WHERE name = 'files' AND type = 'table';

UPDATE sqlite_master SET sql = replace(sql, 'connection_id INTEGER PRIMARY KEY AUTOINCREMENT,', 'connection_id INTEGER PRIMARY KEY,')
WHERE name = 'connections' AND type = 'table';

UPDATE sqlite_master SET sql = replace(sql, 'contact_request_id INTEGER PRIMARY KEY AUTOINCREMENT,', 'contact_request_id INTEGER PRIMARY KEY,')
WHERE name = 'contact_requests' AND type = 'table';

UPDATE sqlite_master SET sql = replace(sql, 'roster_transfer_id INTEGER PRIMARY KEY AUTOINCREMENT,', 'roster_transfer_id INTEGER PRIMARY KEY,')
WHERE name = 'rcv_roster_transfers' AND type = 'table';

PRAGMA writable_schema=RESET;
|]

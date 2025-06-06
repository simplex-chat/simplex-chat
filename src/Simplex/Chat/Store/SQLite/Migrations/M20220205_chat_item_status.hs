{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.SQLite.Migrations.M20220205_chat_item_status where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20220205_chat_item_status :: Query
m20220205_chat_item_status =
  [sql|
PRAGMA ignore_check_constraints=ON;

ALTER TABLE chat_items ADD COLUMN item_status TEXT CHECK (item_status NOT NULL);

UPDATE chat_items SET item_status = 'rcv_read' WHERE item_sent = 0;

UPDATE chat_items SET item_status = 'snd_sent' WHERE item_sent = 1;

PRAGMA ignore_check_constraints=OFF;
|]

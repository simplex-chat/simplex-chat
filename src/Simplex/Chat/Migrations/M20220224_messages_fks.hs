{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Migrations.M20220224_messages_fks where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20220224_messages_fks :: Query
m20220224_messages_fks =
  [sql|
ALTER TABLE messages ADD COLUMN connection_id INTEGER DEFAULT NULL REFERENCES connections ON DELETE CASCADE;
ALTER TABLE messages ADD COLUMN group_id INTEGER DEFAULT NULL REFERENCES groups ON DELETE CASCADE;

UPDATE messages
SET connection_id = (
  SELECT connection_id
  FROM msg_deliveries
  WHERE message_id = messages.message_id
);

DELETE FROM messages WHERE connection_id IS NULL;
|]

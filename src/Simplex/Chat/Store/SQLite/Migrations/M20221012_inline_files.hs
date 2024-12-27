{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.SQLite.Migrations.M20221012_inline_files where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20221012_inline_files :: Query
m20221012_inline_files =
  [sql|
DROP INDEX idx_messages_direct_shared_msg_id;

ALTER TABLE files ADD COLUMN file_inline TEXT; -- based on offer, determined by file sender for both sides
ALTER TABLE rcv_files ADD COLUMN rcv_file_inline TEXT; -- actual mode when receiving file, determined when invitation is accepted
ALTER TABLE rcv_files ADD COLUMN file_inline TEXT; -- based on offer, determined when invitation is processed
ALTER TABLE snd_files ADD COLUMN file_inline TEXT; -- actual mode when sending file, determined when invitation is accepted
ALTER TABLE snd_files ADD COLUMN last_inline_msg_delivery_id INTEGER;

CREATE UNIQUE INDEX idx_snd_files_last_inline_msg_delivery_id ON snd_files(last_inline_msg_delivery_id);
|]

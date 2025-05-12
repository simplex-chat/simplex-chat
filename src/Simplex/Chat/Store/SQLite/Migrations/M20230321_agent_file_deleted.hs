{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.SQLite.Migrations.M20230321_agent_file_deleted where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20230321_agent_file_deleted :: Query
m20230321_agent_file_deleted =
  [sql|
PRAGMA ignore_check_constraints=ON;

ALTER TABLE files ADD COLUMN agent_snd_file_deleted INTEGER DEFAULT 0 CHECK (agent_snd_file_deleted NOT NULL);
UPDATE files SET agent_snd_file_deleted = 0;

ALTER TABLE rcv_files ADD COLUMN agent_rcv_file_deleted INTEGER DEFAULT 0 CHECK (agent_rcv_file_deleted NOT NULL);
UPDATE rcv_files SET agent_rcv_file_deleted = 0;

PRAGMA ignore_check_constraints=OFF;
|]

down_m20230321_agent_file_deleted :: Query
down_m20230321_agent_file_deleted =
  [sql|
ALTER TABLE rcv_files DROP COLUMN agent_rcv_file_deleted;

ALTER TABLE files DROP COLUMN agent_snd_file_deleted;
|]

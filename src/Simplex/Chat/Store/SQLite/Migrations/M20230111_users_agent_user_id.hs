{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.SQLite.Migrations.M20230111_users_agent_user_id where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20230111_users_agent_user_id :: Query
m20230111_users_agent_user_id =
  [sql|
PRAGMA ignore_check_constraints=ON;

ALTER TABLE users ADD COLUMN agent_user_id INTEGER CHECK (agent_user_id NOT NULL);
UPDATE users SET agent_user_id = 1;

PRAGMA ignore_check_constraints=OFF;
|]

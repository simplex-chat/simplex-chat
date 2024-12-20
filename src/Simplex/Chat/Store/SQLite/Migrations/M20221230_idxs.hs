{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.SQLite.Migrations.M20221230_idxs where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20221230_idxs :: Query
m20221230_idxs =
  [sql|
CREATE INDEX idx_connections_group_member ON connections(user_id, group_member_id);

CREATE INDEX idx_commands_connection_id ON commands(connection_id);
|]

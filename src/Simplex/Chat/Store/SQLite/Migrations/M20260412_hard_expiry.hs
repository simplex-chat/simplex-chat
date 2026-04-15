{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.SQLite.Migrations.M20260412_hard_expiry where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20260412_hard_expiry :: Query
m20260412_hard_expiry =
  [sql|
ALTER TABLE chat_items ADD COLUMN hard_expiry_at TEXT;
CREATE INDEX idx_chat_items_hard_expiry_at ON chat_items(user_id, hard_expiry_at);
|]

down_m20260412_hard_expiry :: Query
down_m20260412_hard_expiry =
  [sql|
DROP INDEX IF EXISTS idx_chat_items_hard_expiry_at;
ALTER TABLE chat_items DROP COLUMN hard_expiry_at;
|]

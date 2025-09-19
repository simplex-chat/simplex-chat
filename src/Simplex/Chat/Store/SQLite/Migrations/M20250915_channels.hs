{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.SQLite.Migrations.M20250915_channels where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

-- TODO [channels fwd] add group_members.is_relay
m20250915_channels :: Query
m20250915_channels =
  [sql|
ALTER TABLE groups ADD COLUMN group_type TEXT NOT NULL DEFAULT 'group';
ALTER TABLE groups ADD COLUMN use_relays INTEGER NOT NULL DEFAULT 0;
|]

down_m20250915_channels :: Query
down_m20250915_channels =
  [sql|
ALTER TABLE groups DROP COLUMN group_type;
ALTER TABLE groups DROP COLUMN use_relays;
|]

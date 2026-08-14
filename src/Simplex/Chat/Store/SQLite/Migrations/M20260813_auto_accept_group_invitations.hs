{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.SQLite.Migrations.M20260813_auto_accept_group_invitations where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20260813_auto_accept_group_invitations :: Query
m20260813_auto_accept_group_invitations =
  [sql|
ALTER TABLE users ADD COLUMN auto_accept_group_invitations INTEGER NOT NULL DEFAULT 0;
|]

down_m20260813_auto_accept_group_invitations :: Query
down_m20260813_auto_accept_group_invitations =
  [sql|
ALTER TABLE users DROP COLUMN auto_accept_group_invitations;
|]

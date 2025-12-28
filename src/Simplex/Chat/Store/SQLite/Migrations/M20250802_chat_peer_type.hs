{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.SQLite.Migrations.M20250802_chat_peer_type where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20250802_chat_peer_type :: Query
m20250802_chat_peer_type =
  [sql|
ALTER TABLE contact_profiles ADD COLUMN chat_peer_type TEXT;
|]

down_m20250802_chat_peer_type :: Query
down_m20250802_chat_peer_type =
  [sql|
ALTER TABLE contact_profiles DROP COLUMN chat_peer_type;
|]

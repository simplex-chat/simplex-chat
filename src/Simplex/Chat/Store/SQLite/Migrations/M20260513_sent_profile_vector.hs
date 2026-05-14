{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.SQLite.Migrations.M20260513_sent_profile_vector where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20260513_sent_profile_vector :: Query
m20260513_sent_profile_vector =
  [sql|
ALTER TABLE group_members ADD COLUMN sent_profile_vector BLOB NOT NULL DEFAULT x'';
ALTER TABLE delivery_jobs ADD COLUMN sender_group_member_ids BLOB;
|]

down_m20260513_sent_profile_vector :: Query
down_m20260513_sent_profile_vector =
  [sql|
ALTER TABLE delivery_jobs DROP COLUMN sender_group_member_ids;
ALTER TABLE group_members DROP COLUMN sent_profile_vector;
|]

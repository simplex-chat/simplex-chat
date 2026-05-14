{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.Postgres.Migrations.M20260513_sent_profile_vector where

import Data.Text (Text)
import Text.RawString.QQ (r)

m20260513_sent_profile_vector :: Text
m20260513_sent_profile_vector =
  [r|
ALTER TABLE group_members ADD COLUMN sent_profile_vector BYTEA NOT NULL DEFAULT ''::BYTEA;
ALTER TABLE delivery_jobs ADD COLUMN sender_group_member_ids BYTEA;
|]

down_m20260513_sent_profile_vector :: Text
down_m20260513_sent_profile_vector =
  [r|
ALTER TABLE delivery_jobs DROP COLUMN sender_group_member_ids;
ALTER TABLE group_members DROP COLUMN sent_profile_vector;
|]

{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.Postgres.Migrations.M20250802_chat_peer_type where

import Data.Text (Text)
import qualified Data.Text as T
import Text.RawString.QQ (r)

m20250802_chat_peer_type :: Text
m20250802_chat_peer_type =
  T.pack
    [r|
ALTER TABLE contact_profiles ADD COLUMN chat_peer_type TEXT;
|]

down_m20250802_chat_peer_type :: Text
down_m20250802_chat_peer_type =
  T.pack
    [r|
ALTER TABLE contact_profiles DROP COLUMN chat_peer_type;
|]

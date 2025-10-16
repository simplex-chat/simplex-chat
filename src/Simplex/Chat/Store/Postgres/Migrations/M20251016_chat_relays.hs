{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.Postgres.Migrations.M20251016_chat_relays where

import Data.Text (Text)
import qualified Data.Text as T
import Text.RawString.QQ (r)

m20251016_chat_relays :: Text
m20251016_chat_relays =
  T.pack
    [r|
CREATE TABLE chat_relays(
  chat_relay_id BIGINT PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
  address TEXT NOT NULL,
  name TEXT NOT NULL,
  domains TEXT NOT NULL,
  preset SMALLINT NOT NULL DEFAULT 0,
  tested SMALLINT,
  enabled SMALLINT NOT NULL DEFAULT 1,
  user_id BIGINT NOT NULL REFERENCES users ON DELETE CASCADE,
  created_at TEXT NOT NULL DEFAULT (now()),
  updated_at TEXT NOT NULL DEFAULT (now()),
  UNIQUE(user_id, address),
  UNIQUE(user_id, name)
);

CREATE INDEX idx_chat_relays_user_id ON chat_relays(user_id);

ALTER TABLE users ADD COLUMN is_user_chat_relay SMALLINT NOT NULL DEFAULT 0;

ALTER TABLE group_members ADD COLUMN is_chat_relay SMALLINT NOT NULL DEFAULT 0;
|]

down_m20251016_chat_relays :: Text
down_m20251016_chat_relays =
  T.pack
    [r|
ALTER TABLE group_members DROP COLUMN is_chat_relay;

ALTER TABLE users DROP COLUMN is_user_chat_relay;

DROP INDEX idx_chat_relays_user_id;

DROP TABLE chat_relays;
|]

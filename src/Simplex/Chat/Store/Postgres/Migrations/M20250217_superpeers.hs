{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.Postgres.Migrations.M20250217_superpeers where

import Data.Text (Text)
import qualified Data.Text as T
import Text.RawString.QQ (r)

m20250217_superpeers :: Text
m20250217_superpeers =
  T.pack
    [r|
CREATE TABLE superpeers(
  superpeer_id BIGINT PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
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

CREATE INDEX idx_superpeers_user_id ON superpeers(user_id);

ALTER TABLE users ADD COLUMN user_superpeer SMALLINT NOT NULL DEFAULT 0;

ALTER TABLE group_members ADD COLUMN superpeer SMALLINT NOT NULL DEFAULT 0;
|]

down_m20250217_superpeers :: Text
down_m20250217_superpeers =
  T.pack
    [r|
ALTER TABLE group_members DROP COLUMN superpeer;

ALTER TABLE users DROP COLUMN user_superpeer;

DROP INDEX idx_superpeers_user_id;

DROP TABLE superpeers;
|]

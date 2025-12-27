{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.Postgres.Migrations.M20251225_client_services where

import Data.Text (Text)
import Text.RawString.QQ (r)

m20251225_client_services :: Text
m20251225_client_services =
  [r|
ALTER TABLE users ADD COLUMN client_service SMALLINT NOT NULL DEFAULT 0;
|]

down_m20251225_client_services :: Text
down_m20251225_client_services =
  [r|
ALTER TABLE users DROP COLUMN client_service;
|]

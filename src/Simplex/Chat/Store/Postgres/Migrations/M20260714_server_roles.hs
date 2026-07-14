{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.Postgres.Migrations.M20260714_server_roles where

import Data.Text (Text)
import Text.RawString.QQ (r)

m20260714_server_roles :: Text
m20260714_server_roles =
  [r|
ALTER TABLE protocol_servers ADD COLUMN role_storage SMALLINT;
ALTER TABLE protocol_servers ADD COLUMN role_proxy SMALLINT;
ALTER TABLE protocol_servers ADD COLUMN role_names SMALLINT;
|]

down_m20260714_server_roles :: Text
down_m20260714_server_roles =
  [r|
ALTER TABLE protocol_servers DROP COLUMN role_storage;
ALTER TABLE protocol_servers DROP COLUMN role_proxy;
ALTER TABLE protocol_servers DROP COLUMN role_names;
|]

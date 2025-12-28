{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.Postgres.Migrations.M20250704_groups_conn_link_prepared_connection where

import Data.Text (Text)
import qualified Data.Text as T
import Text.RawString.QQ (r)

m20250704_groups_conn_link_prepared_connection :: Text
m20250704_groups_conn_link_prepared_connection =
  T.pack
    [r|
ALTER TABLE groups ADD COLUMN conn_link_prepared_connection SMALLINT NOT NULL DEFAULT 0;
|]

down_m20250704_groups_conn_link_prepared_connection :: Text
down_m20250704_groups_conn_link_prepared_connection =
  T.pack
    [r|
ALTER TABLE groups DROP COLUMN conn_link_prepared_connection;
|]

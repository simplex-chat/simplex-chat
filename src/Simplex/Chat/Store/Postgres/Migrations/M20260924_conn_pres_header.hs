{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.Postgres.Migrations.M20260924_conn_pres_header where

import Data.Text (Text)
import Text.RawString.QQ (r)

m20260924_conn_pres_header :: Text
m20260924_conn_pres_header =
  [r|
ALTER TABLE connections ADD COLUMN pres_header BYTEA;
|]

down_m20260924_conn_pres_header :: Text
down_m20260924_conn_pres_header =
  [r|
ALTER TABLE connections DROP COLUMN pres_header;
|]

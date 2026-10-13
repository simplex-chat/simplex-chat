{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.Postgres.Migrations.M20260723_contact_request_rejection where

import Data.Text (Text)
import Text.RawString.QQ (r)

m20260723_contact_request_rejection :: Text
m20260723_contact_request_rejection =
  [r|
ALTER TABLE contact_requests ADD COLUMN rejection_supported SMALLINT NOT NULL DEFAULT 0;
|]

down_m20260723_contact_request_rejection :: Text
down_m20260723_contact_request_rejection =
  [r|
ALTER TABLE contact_requests DROP COLUMN rejection_supported;
|]

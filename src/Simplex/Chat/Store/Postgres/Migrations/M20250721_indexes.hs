{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.Postgres.Migrations.M20250721_indexes where

import Data.Text (Text)
import qualified Data.Text as T
import Text.RawString.QQ (r)

m20250721_indexes :: Text
m20250721_indexes =
  T.pack
    [r|
DROP INDEX idx_contact_requests_xcontact_id;

CREATE INDEX idx_contact_requests_xcontact_id ON contact_requests(user_id, xcontact_id);
|]

down_m20250721_indexes :: Text
down_m20250721_indexes =
  T.pack
    [r|
DROP INDEX idx_contact_requests_xcontact_id;

CREATE INDEX idx_contact_requests_xcontact_id ON contact_requests(xcontact_id);
|]

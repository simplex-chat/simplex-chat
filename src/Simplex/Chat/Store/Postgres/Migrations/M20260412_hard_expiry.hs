{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.Postgres.Migrations.M20260412_hard_expiry where

import Data.Text (Text)
import qualified Data.Text as T
import Text.RawString.QQ (r)

m20260412_hard_expiry :: Text
m20260412_hard_expiry =
  T.pack
    [r|
ALTER TABLE chat_items ADD COLUMN hard_expiry_at TIMESTAMPTZ;
CREATE INDEX idx_chat_items_hard_expiry_at ON chat_items(user_id, hard_expiry_at);
|]

down_m20260412_hard_expiry :: Text
down_m20260412_hard_expiry =
  T.pack
    [r|
DROP INDEX IF EXISTS idx_chat_items_hard_expiry_at;
ALTER TABLE chat_items DROP COLUMN hard_expiry_at;
|]

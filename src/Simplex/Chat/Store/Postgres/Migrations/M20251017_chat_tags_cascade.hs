{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.Postgres.Migrations.M20251017_chat_tags_cascade where

import Data.Text (Text)
import qualified Data.Text as T
import Text.RawString.QQ (r)

m20251017_chat_tags_cascade :: Text
m20251017_chat_tags_cascade =
  T.pack
    [r|
ALTER TABLE chat_tags DROP CONSTRAINT chat_tags_user_id_fkey;

ALTER TABLE chat_tags
  ADD CONSTRAINT chat_tags_user_id_fkey
  FOREIGN KEY (user_id)
  REFERENCES users(user_id)
  ON DELETE CASCADE;
|]

down_m20251017_chat_tags_cascade :: Text
down_m20251017_chat_tags_cascade =
  T.pack
    [r|
ALTER TABLE chat_tags DROP CONSTRAINT chat_tags_user_id_fkey;

ALTER TABLE chat_tags
  ADD CONSTRAINT chat_tags_user_id_fkey
  FOREIGN KEY (user_id)
  REFERENCES users(user_id);
|]

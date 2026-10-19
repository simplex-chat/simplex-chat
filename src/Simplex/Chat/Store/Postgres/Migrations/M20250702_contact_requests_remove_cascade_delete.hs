{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.Postgres.Migrations.M20250702_contact_requests_remove_cascade_delete where

import Data.Text (Text)
import qualified Data.Text as T
import Text.RawString.QQ (r)

m20250702_contact_requests_remove_cascade_delete :: Text
m20250702_contact_requests_remove_cascade_delete =
  T.pack
    [r|
ALTER TABLE contact_requests DROP CONSTRAINT contact_requests_user_contact_link_id_fkey;

ALTER TABLE contact_requests ALTER COLUMN user_contact_link_id DROP NOT NULL;

ALTER TABLE contact_requests
  ADD CONSTRAINT contact_requests_user_contact_link_id_fkey
  FOREIGN KEY (user_contact_link_id)
  REFERENCES user_contact_links(user_contact_link_id)
  ON UPDATE CASCADE
  ON DELETE SET NULL;
|]

down_m20250702_contact_requests_remove_cascade_delete :: Text
down_m20250702_contact_requests_remove_cascade_delete =
  T.pack
    [r|
ALTER TABLE contact_requests DROP CONSTRAINT contact_requests_user_contact_link_id_fkey;

ALTER TABLE contact_requests ALTER COLUMN user_contact_link_id SET NOT NULL;

ALTER TABLE contact_requests
  ADD CONSTRAINT contact_requests_user_contact_link_id_fkey
  FOREIGN KEY (user_contact_link_id)
  REFERENCES user_contact_links(user_contact_link_id)
  ON UPDATE CASCADE
  ON DELETE CASCADE;
|]

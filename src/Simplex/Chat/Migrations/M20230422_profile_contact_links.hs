{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Migrations.M20230422_profile_contact_links where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20230422_profile_contact_links :: Query
m20230422_profile_contact_links =
  [sql|
ALTER TABLE group_profiles ADD COLUMN group_link BLOB;
ALTER TABLE group_profiles ADD COLUMN user_contact_link_id INTEGER REFERENCES user_contact_links ON DELETE SET NULL;
CREATE INDEX idx_group_profiles_user_contact_link_id ON group_profiles (user_contact_link_id);

ALTER TABLE contact_profiles ADD COLUMN contact_link BLOB;
ALTER TABLE contact_profiles ADD COLUMN user_contact_link_id INTEGER REFERENCES user_contact_links ON DELETE SET NULL;
CREATE INDEX idx_contact_profiles_user_contact_link_id ON contact_profiles (user_contact_link_id);
|]

down_m20230422_profile_contact_links :: Query
down_m20230422_profile_contact_links =
  [sql|
DROP INDEX idx_contact_profiles_user_contact_link_id;
ALTER TABLE contact_profiles DROP COLUMN user_contact_link_id;
ALTER TABLE contact_profiles DROP COLUMN contact_link;

DROP INDEX idx_group_profiles_user_contact_link_id;
ALTER TABLE group_profiles DROP COLUMN user_contact_link_id;
ALTER TABLE group_profiles DROP COLUMN group_link;
|]

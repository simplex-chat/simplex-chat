{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.SQLite.Migrations.M20250129_delete_unused_contacts where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20250129_delete_unused_contacts :: Query
m20250129_delete_unused_contacts =
  [sql|
CREATE TEMPORARY TABLE temp_delete_contacts (
    contact_id INTEGER PRIMARY KEY,
    contact_profile_id INTEGER NOT NULL,
    local_display_name TEXT NOT NULL
);

INSERT INTO temp_delete_contacts(contact_id, contact_profile_id, local_display_name)
SELECT contact_id, contact_profile_id, local_display_name
FROM contacts WHERE contact_used = 0 AND is_user = 0;

CREATE TEMPORARY TABLE temp_delete_profiles (contact_profile_id INTEGER PRIMARY KEY);

INSERT OR IGNORE INTO temp_delete_profiles(contact_profile_id)
SELECT custom_user_profile_id FROM connections
WHERE contact_id IN (SELECT contact_id FROM temp_delete_contacts)
 AND custom_user_profile_id IS NOT NULL;

UPDATE group_members SET contact_id = NULL
WHERE contact_id IN (SELECT contact_id FROM temp_delete_contacts);

DELETE FROM connections
WHERE contact_id IN (SELECT contact_id FROM temp_delete_contacts);

DELETE FROM contacts
WHERE contact_id IN (SELECT contact_id FROM temp_delete_contacts)
  AND contact_id NOT IN (SELECT contact_id FROM users);

DELETE FROM contact_profiles
WHERE
  (contact_profile_id IN (SELECT contact_profile_id FROM temp_delete_profiles)
    OR contact_profile_id IN (SELECT contact_profile_id FROM temp_delete_contacts))
  AND contact_profile_id NOT IN (SELECT contact_profile_id FROM group_members)
  AND contact_profile_id NOT IN (SELECT member_profile_id FROM group_members)
  AND contact_profile_id NOT IN (SELECT contact_profile_id FROM contact_requests)
  AND contact_profile_id NOT IN (SELECT custom_user_profile_id FROM connections);

DELETE FROM display_names
WHERE local_display_name IN (SELECT local_display_name FROM temp_delete_contacts)
  AND local_display_name NOT IN (SELECT local_display_name FROM group_members)
  AND local_display_name NOT IN (SELECT local_display_name FROM users)
  AND local_display_name NOT IN (SELECT local_display_name FROM groups)
  AND local_display_name NOT IN (SELECT local_display_name FROM user_contact_links)
  AND local_display_name NOT IN (SELECT local_display_name FROM contact_requests);

DROP TABLE temp_delete_contacts;
DROP TABLE temp_delete_profiles;

ALTER TABLE contacts DROP COLUMN contact_used;
|]

down_m20250129_delete_unused_contacts :: Query
down_m20250129_delete_unused_contacts =
  [sql|
ALTER TABLE contacts ADD COLUMN contact_used INTEGER NOT NULL DEFAULT 0;

UPDATE contacts SET contact_used = 1
WHERE contact_id IN (
    SELECT DISTINCT contact_id FROM chat_items WHERE contact_id IS NOT NULL
  ) OR contact_id = (
    SELECT contact_id FROM connections
    WHERE conn_level = 0 AND via_group_link = 0
  );
|]

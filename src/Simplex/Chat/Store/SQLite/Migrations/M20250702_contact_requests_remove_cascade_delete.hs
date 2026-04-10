{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.SQLite.Migrations.M20250702_contact_requests_remove_cascade_delete where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20250702_contact_requests_remove_cascade_delete :: Query
m20250702_contact_requests_remove_cascade_delete =
  [sql|
PRAGMA writable_schema=1;

UPDATE sqlite_master
SET sql = replace(
            replace(
              sql,
              'user_contact_link_id INTEGER NOT NULL REFERENCES user_contact_links',
              'user_contact_link_id INTEGER REFERENCES user_contact_links'
            ),
            'ON UPDATE CASCADE ON DELETE CASCADE,',
            'ON UPDATE CASCADE ON DELETE SET NULL,'
          )
WHERE name = 'contact_requests' AND type = 'table';

PRAGMA writable_schema=RESET;
|]

down_m20250702_contact_requests_remove_cascade_delete :: Query
down_m20250702_contact_requests_remove_cascade_delete =
  [sql|
PRAGMA writable_schema=1;

UPDATE sqlite_master
SET sql = replace(
            replace(
              sql,
              'ON UPDATE CASCADE ON DELETE SET NULL,',
              'ON UPDATE CASCADE ON DELETE CASCADE,'
            ),
            'user_contact_link_id INTEGER REFERENCES user_contact_links',
            'user_contact_link_id INTEGER NOT NULL REFERENCES user_contact_links'
          )
WHERE name = 'contact_requests' AND type = 'table';

PRAGMA writable_schema=RESET;
|]

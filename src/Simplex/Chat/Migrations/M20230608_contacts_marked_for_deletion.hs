{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Migrations.M20230608_contacts_marked_for_deletion where

import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.QQ (sql)

m20230608_contacts_marked_for_deletion :: Query
m20230608_contacts_marked_for_deletion =
  [sql|
ALTER TABLE contacts ADD COLUMN marked_for_deletion INTEGER NOT NULL DEFAULT 0;

CREATE INDEX msg_delivery_events_msg_delivery_id ON msg_delivery_events(msg_delivery_id);
|]

down_m20230608_contacts_marked_for_deletion :: Query
down_m20230608_contacts_marked_for_deletion =
  [sql|
DROP INDEX msg_delivery_events_msg_delivery_id;

ALTER TABLE contacts DROP COLUMN marked_for_deletion;
|]

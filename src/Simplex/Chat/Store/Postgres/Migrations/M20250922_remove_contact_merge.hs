{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.Postgres.Migrations.M20250922_remove_contact_merge where

import Data.Text (Text)
import qualified Data.Text as T
import Text.RawString.QQ (r)

m20250922_remove_contact_merge :: Text
m20250922_remove_contact_merge =
  T.pack
    [r|
WITH ranked_contact_connections AS (
  SELECT
    c.connection_id,
    ROW_NUMBER() OVER (
      PARTITION BY c.user_id, c.contact_id
      ORDER BY
        CASE WHEN c.conn_status = 'ready' OR c.conn_status = 'snd-ready' THEN 1 ELSE 0 END DESC,
        c.created_at DESC
    ) AS rn
  FROM connections c
  WHERE c.contact_id IS NOT NULL
)
DELETE FROM connections
WHERE connection_id IN (
  SELECT connection_id
  FROM ranked_contact_connections
  WHERE rn > 1
);

WITH ranked_group_member_connections AS (
  SELECT
    c.connection_id,
    ROW_NUMBER() OVER (
      PARTITION BY c.user_id, c.group_member_id
      ORDER BY c.connection_id DESC
    ) AS rn
  FROM connections c
  WHERE c.group_member_id IS NOT NULL
)
DELETE FROM connections
WHERE connection_id IN (
  SELECT connection_id
  FROM ranked_group_member_connections
  WHERE rn > 1
);

DROP INDEX idx_connections_contact_id;
DROP INDEX idx_connections_group_member_id;

CREATE UNIQUE INDEX idx_connections_contact_id ON connections(contact_id);
CREATE UNIQUE INDEX idx_connections_group_member_id ON connections(group_member_id);

DROP INDEX idx_contacts_via_group;
ALTER TABLE contacts DROP COLUMN via_group;
|]

down_m20250922_remove_contact_merge :: Text
down_m20250922_remove_contact_merge =
  T.pack
    [r|
ALTER TABLE contacts ADD COLUMN via_group INTEGER REFERENCES groups(group_id) ON DELETE SET NULL;
CREATE INDEX idx_contacts_via_group ON contacts(via_group);

DROP INDEX idx_connections_contact_id;
DROP INDEX idx_connections_group_member_id;

CREATE INDEX idx_connections_contact_id ON connections(contact_id);
CREATE INDEX idx_connections_group_member_id ON connections(group_member_id);
|]

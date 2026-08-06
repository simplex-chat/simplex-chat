{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}

module BadgeService.Store.SQLite.Migrations (badgeServiceSchemaMigrations) where

import Data.List (sortOn)
import Database.SQLite.Simple (Query (..))
import Database.SQLite.Simple.QQ (sql)
import Simplex.Messaging.Agent.Store.Shared (Migration (..))

badgeServiceSchemaMigrations :: [Migration]
badgeServiceSchemaMigrations = sortOn name $ map migration schemaMigrations
  where
    migration (name, up, down) = Migration {name, up = fromQuery up, down = fromQuery <$> down}

schemaMigrations :: [(String, Query, Maybe Query)]
schemaMigrations =
  [ ("20260806_badge_service_schema", m20260806_badge_service_schema, Just down_m20260806_badge_service_schema)
  ]

m20260806_badge_service_schema :: Query
m20260806_badge_service_schema =
  [sql|
CREATE TABLE sx_badge_service_test(
  test_id INTEGER PRIMARY KEY AUTOINCREMENT,
  created_at TEXT NOT NULL DEFAULT(datetime('now'))
);
  |]

down_m20260806_badge_service_schema :: Query
down_m20260806_badge_service_schema =
  [sql|
DROP TABLE sx_badge_service_test;
  |]

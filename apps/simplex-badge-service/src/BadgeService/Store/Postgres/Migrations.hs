{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}

module BadgeService.Store.Postgres.Migrations (badgeServiceSchemaMigrations) where

import Data.List (sortOn)
import Data.Text (Text)
import qualified Data.Text as T
import Simplex.Messaging.Agent.Store.Shared (Migration (..))
import Text.RawString.QQ (r)

badgeServiceSchemaMigrations :: [Migration]
badgeServiceSchemaMigrations = sortOn name $ map migration schemaMigrations
  where
    migration (name, up, down) = Migration {name, up, down}

schemaMigrations :: [(String, Text, Maybe Text)]
schemaMigrations =
  [ ("20260806_badge_service_schema", m20260806_badge_service_schema, Just down_m20260806_badge_service_schema)
  ]

m20260806_badge_service_schema :: Text
m20260806_badge_service_schema =
  T.pack
    [r|
CREATE TABLE sx_badge_service_test(
  test_id BIGINT PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
  created_at TIMESTAMPTZ NOT NULL DEFAULT (now())
);
  |]

down_m20260806_badge_service_schema :: Text
down_m20260806_badge_service_schema =
  T.pack
    [r|
DROP TABLE sx_badge_service_test;
  |]

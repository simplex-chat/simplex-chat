{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module BadgeService.Store.Postgres.Migrations (badgeServiceSchemaMigrations) where

import Data.List (sortOn)
import Data.Text (Text)
import Simplex.Chat.Store.Postgres.Migrations.M20260731_user_badges (badgeSchema, badgeSchemaDown, withPrefix)
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

-- the client tables are in the same database, so the service tables are the same names with this prefix
servicePrefix :: Text
servicePrefix = "sx_badge_service_"

m20260806_badge_service_schema :: Text
m20260806_badge_service_schema =
  badgeSchema servicePrefix
    <> withPrefix
      servicePrefix
      [r|
ALTER TABLE @payments ADD COLUMN receipt_hash BYTEA;
|]

down_m20260806_badge_service_schema :: Text
down_m20260806_badge_service_schema = badgeSchemaDown servicePrefix

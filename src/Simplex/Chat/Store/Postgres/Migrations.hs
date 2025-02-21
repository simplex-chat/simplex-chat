{-# LANGUAGE NamedFieldPuns #-}

module Simplex.Chat.Store.Postgres.Migrations (migrations) where

import Data.List (sortOn)
import Data.Text (Text)
import Simplex.Chat.Store.Postgres.Migrations.M20241220_initial
import Simplex.Chat.Store.Postgres.Migrations.M20250217_superpeers
import Simplex.Messaging.Agent.Store.Shared (Migration (..))

schemaMigrations :: [(String, Text, Maybe Text)]
schemaMigrations =
  [ ("20241220_initial", m20241220_initial, Nothing),
    ("20250217_superpeers", m20250217_superpeers, Just down_m20250217_superpeers)
  ]

-- | The list of migrations in ascending order by date
migrations :: [Migration]
migrations = sortOn name $ map migration schemaMigrations
  where
    migration (name, up, down) = Migration {name, up, down}

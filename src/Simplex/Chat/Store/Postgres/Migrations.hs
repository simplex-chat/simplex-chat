module Simplex.Chat.Store.Postgres.Migrations where

import Data.List (sortOn)
import Data.Text (Text)
import Simplex.Chat.Store.Postgres.Migrations.M20241220_initial

schemaMigrations :: [(String, Text, Maybe Text)]
schemaMigrations =
  [ ("20241220_initial", m20241220_initial, Nothing)
  ]

-- | The list of migrations in ascending order by date
migrations :: [Migration]
migrations = sortOn name $ map migration schemaMigrations
  where
    migration (name, up, down) = Migration {name, up, down = down}

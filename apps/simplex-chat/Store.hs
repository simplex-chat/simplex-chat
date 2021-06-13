{-# LANGUAGE TemplateHaskell #-}

module Store where

import Data.FileEmbed (embedDir, makeRelativeToProject)
import Data.Function (on)
import Data.List (sortBy)
import Data.Text.Encoding (decodeUtf8)
import Simplex.Messaging.Agent.Store.SQLite
import Simplex.Messaging.Agent.Store.SQLite.Migrations (Migration (..))
import System.FilePath (takeBaseName, takeExtension)

-- | The list of migrations in ascending order by date
migrations :: [Migration]
migrations =
  sortBy (compare `on` name) . map migration . filter sqlFile $
    $(makeRelativeToProject "migrations" >>= embedDir)
  where
    sqlFile (file, _) = takeExtension file == ".sql"
    migration (file, qStr) = Migration {name = takeBaseName file, up = decodeUtf8 qStr}

createStore :: FilePath -> IO SQLiteStore
createStore dbFile = createSQLiteStore dbFile migrations

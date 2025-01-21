{-# LANGUAGE CPP #-}

module Simplex.Chat.Options.DB

#if defined(dbPostgres)
  ( module Simplex.Chat.Options.Postgres,
    FromField (..),
    ToField (..),
  )
  where
import Simplex.Chat.Options.Postgres
import Database.PostgreSQL.Simple.FromField (FromField (..))
import Database.PostgreSQL.Simple.ToField (ToField (..))

#else
  ( module Simplex.Chat.Options.SQLite,
    FromField (..),
    ToField (..),
  )
  where
import Simplex.Chat.Options.SQLite
import Database.SQLite.Simple.FromField (FromField (..))
import Database.SQLite.Simple.ToField (ToField (..))

#endif

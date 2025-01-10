{-# LANGUAGE CPP #-}

module Simplex.Chat.Options.DB
#if defined(dbPostgres)
  ( module Simplex.Chat.Options.Postgres,
  )
  where
import Simplex.Chat.Options.Postgres
#else
  ( module Simplex.Chat.Options.SQLite,
  )
  where
import Simplex.Chat.Options.SQLite
#endif

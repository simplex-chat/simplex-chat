{-# LANGUAGE CPP #-}

module Simplex.Chat.Mobile.Options.DB
#if defined(dbPostgres)
  ( module Simplex.Chat.Mobile.Options.Postgres,
  )
  where
import Simplex.Chat.Mobile.Options.Postgres
#else
  ( module Simplex.Chat.Mobile.Options.SQLite,
  )
  where
import Simplex.Chat.Mobile.Options.SQLite
#endif

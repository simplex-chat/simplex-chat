{-# LANGUAGE CPP #-}

module ChatTests.DBUtils

#if defined(dbPostgres)
  ( module ChatTests.DBUtils.Postgres,
  )
  where
import ChatTests.DBUtils.Postgres
#else
  ( module ChatTests.DBUtils.SQLite,
  )
  where
import ChatTests.DBUtils.SQLite
#endif

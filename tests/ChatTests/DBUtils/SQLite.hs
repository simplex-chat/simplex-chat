module ChatTests.DBUtils.SQLite where

import Database.SQLite.Simple (Query)
import Simplex.Messaging.Agent.Store.SQLite.DB
import Simplex.Messaging.TMap (TMap)

data TestParams = TestParams
  { tmpPath :: FilePath,
    chatQueryStats :: TMap Query SlowQueryStats,
    agentQueryStats :: TMap Query SlowQueryStats
  }

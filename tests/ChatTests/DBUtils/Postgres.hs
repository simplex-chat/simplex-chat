module ChatTests.DBUtils.Postgres where

data TestParams = TestParams
  { tmpPath :: FilePath,
    portBase :: Int,
    printOutput :: Bool
  }

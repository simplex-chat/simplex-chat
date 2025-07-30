module ChatTests.DBUtils.Postgres where

data TestParams = TestParams
  { tmpPath :: FilePath,
    printOutput :: Bool
  }

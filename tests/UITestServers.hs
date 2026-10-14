{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module Main (main) where

import ChatClient (persistentServerStoreCfg, smpServerCfg, testServerKeyHash)
import ChatTests.DBUtils
import Control.Concurrent.Async (async, cancel)
import Control.Concurrent.STM
import qualified Simplex.Messaging.TMap as TM
import Simplex.Messaging.Server (runSMPServerBlocking)
import Simplex.Messaging.Server.Env.STM (ServerConfig (..))
import System.Directory (createDirectoryIfMissing)
import System.Environment (getArgs)
import System.Exit (die)
import System.IO

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  (dir, base) <-
    getArgs >>= \case
      [d, b] -> pure (d, read b :: Int)
      _ -> die "usage: uitest-servers <servers dir> <port base>"
  chatQueryStats <- TM.emptyIO
  agentQueryStats <- TM.emptyIO
  let tmpPath = dir <> "/smp1"
      ps = TestParams {tmpPath, portBase = base, printOutput = False, chatQueryStats, agentQueryStats}
      cfg =
        (smpServerCfg ps)
          { serverStoreCfg = persistentServerStoreCfg tmpPath,
            serverStatsLogFile = tmpPath <> "/smp-server-stats.daily.log",
            prometheusMetricsFile = tmpPath <> "/smp-server-metrics.txt"
          }
  createDirectoryIfMissing True tmpPath
  started <- newEmptyTMVarIO
  server <- async $ runSMPServerBlocking started cfg Nothing
  ok <- atomically $ takeTMVar started
  putStrLn $
    if ok
      then "ready smp1 smp://" <> testServerKeyHash <> "@localhost:" <> show (base + 1)
      else "error smp1 did not start"
  waitForEOF
  cancel server
  where
    waitForEOF = hIsEOF stdin >>= \eof -> if eof then pure () else getLine >> waitForEOF

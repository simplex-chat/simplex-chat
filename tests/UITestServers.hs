{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import ChatClient (persistentServerStoreCfg, smpServerCfg, testServerKeyHash, xftpServerConfig, xftpServerFiles)
import ChatTests.DBUtils
import Control.Concurrent.Async (Async, async, cancel, poll, waitCatchSTM)
import Control.Concurrent.STM
import Control.Exception (SomeException, try)
import Data.IORef
import qualified Data.Map.Strict as M
import Data.Maybe (isNothing)
import qualified Simplex.Messaging.TMap as TM
import Simplex.FileTransfer.Server (runXFTPServerBlocking)
import Simplex.Messaging.Server (runSMPServerBlocking)
import Simplex.Messaging.Server.Env.STM (ServerConfig (..))
import System.Directory (createDirectoryIfMissing, removePathForcibly)
import System.Environment (getArgs)
import System.Exit (die)
import System.IO

data Protocol = SMP | XFTP

data ServerDef = ServerDef {srvProtocol :: Protocol, srvPortOffset :: Int, srvHost :: String}

serverDefs :: M.Map String ServerDef
serverDefs =
  M.fromList
    [ ("smp1", ServerDef SMP 0 "localhost"),
      ("xftp1", ServerDef XFTP 0 "localhost"),
      ("smp2", ServerDef SMP 10 "127.0.0.1"),
      ("xftp2", ServerDef XFTP 10 "127.0.0.1")
    ]

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  (dir, base) <-
    getArgs >>= \case
      [d, b] -> pure (d, read b :: Int)
      _ -> die "usage: uitest-servers <servers dir> <port base>"
  chatQueryStats <- TM.emptyIO
  agentQueryStats <- TM.emptyIO
  running <- newIORef (M.empty :: M.Map String (Async ()))
  let serverDir name = dir <> "/" <> name
      params name ServerDef {srvPortOffset} = TestParams {tmpPath = serverDir name, portBase = base + srvPortOffset, printOutput = False, chatQueryStats, agentQueryStats}
      address ServerDef {srvProtocol, srvPortOffset, srvHost} = case srvProtocol of
        SMP -> "smp://" <> testServerKeyHash <> "@" <> srvHost <> ":" <> show (base + srvPortOffset + 1)
        XFTP -> "xftp://" <> testServerKeyHash <> "@" <> srvHost <> ":" <> show (base + srvPortOffset + 2)
      runServer name d@ServerDef {srvProtocol} = do
        let ps = params name d
            path = tmpPath ps
        started <- newEmptyTMVarIO
        a <- case srvProtocol of
          SMP -> do
            createDirectoryIfMissing True path
            let cfg =
                  (smpServerCfg ps)
                    { serverStoreCfg = persistentServerStoreCfg path,
                      serverStatsLogFile = path <> "/smp-server-stats.daily.log",
                      prometheusMetricsFile = path <> "/smp-server-metrics.txt"
                    }
            async $ runSMPServerBlocking started cfg Nothing
          XFTP -> do
            createDirectoryIfMissing True (xftpServerFiles ps)
            async $ runXFTPServerBlocking started (xftpServerConfig ps)
        atomically ((Right <$> takeTMVar started) `orElse` (Left <$> waitCatchSTM a)) >>= \case
          Right True -> modifyIORef' running (M.insert name a) >> pure ("ready " <> name <> " " <> address d)
          Right False -> cancel a >> pure ("error " <> name <> " did not start")
          Left (Left e) -> pure ("error " <> name <> " " <> oneLine (show e))
          Left (Right ()) -> pure ("error " <> name <> " exited while starting")
      isRunning name = readIORef running >>= maybe (pure False) (fmap isNothing . poll) . M.lookup name
      stopServer name = do
        readIORef running >>= mapM_ cancel . M.lookup name
        modifyIORef' running (M.delete name)
      startServer name d = isRunning name >>= \up -> if up then pure ("ready " <> name <> " " <> address d) else stopServer name >> runServer name d
      command line = case words line of
        [cmd, name] | Just d <- M.lookup name serverDefs -> case cmd of
          "start" -> startServer name d
          "stop" -> stopServer name >> pure ("stopped " <> name)
          "reset" -> stopServer name >> removePathForcibly (serverDir name) >> runServer name d
          _ -> pure ("error " <> name <> " unknown command " <> cmd)
        [_, name] -> pure ("error " <> name <> " unknown server")
        _ -> pure ("error - unknown command " <> line)
      respond line = try (command line) >>= putStrLn . either (\(e :: SomeException) -> "error " <> serverName line <> " " <> oneLine (show e)) id
  respond "start smp1"
  respond "start xftp1"
  let loop = hIsEOF stdin >>= \eof -> if eof then pure () else getLine >>= respond >> loop
  loop
  readIORef running >>= mapM_ cancel . M.elems
  where
    serverName line = case words line of
      [_, name] -> name
      _ -> "-"
    oneLine = map (\c -> if c == '\n' then ' ' else c)

{-# LANGUAGE CPP #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module Simplex.Chat.Terminal.Main where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM
import Control.Monad
import Data.Maybe (fromMaybe)
import Network.Socket
import Simplex.Chat.Controller (ArchiveConfig (..), ChatCommand (..), ChatConfig (..), ChatController (..), ChatError, ChatEvent (..), ChatResponse (..), PresetServers (..), SimpleNetCfg (..), currentRemoteHost, versionNumber, versionString)
import Simplex.Chat.Core
import Simplex.Chat.Options
import Simplex.Chat.Options.DB
import Simplex.Chat.Terminal
import Simplex.Chat.View (ChatResponseEvent, smpProxyModeStr)
import Simplex.Messaging.Client (NetworkConfig (..), SocksMode (..))
import System.Directory (getAppUserDataDirectory)
import System.Exit (exitFailure, exitSuccess)
import System.Terminal (withTerminal)

simplexChatCLI :: ChatConfig -> Maybe (ServiceName -> ChatConfig -> ChatOpts -> IO ()) -> IO ()
simplexChatCLI cfg server_ = do
  appDir <- getAppUserDataDirectory "simplex"
  opts <- getChatOpts appDir "simplex_v1"
  simplexChatCLI' cfg opts server_

simplexChatCLI' :: ChatConfig -> ChatOpts -> Maybe (ServiceName -> ChatConfig -> ChatOpts -> IO ()) -> IO ()
simplexChatCLI' cfg opts@ChatOpts {chatCmd, chatCmdLog, chatCmdDelay, chatServerPort, optExportArchive, optImportArchive} server_ = do
  -- Handle archive operations first (they exit after completion)
#if !defined(dbPostgres)
  case (optExportArchive, optImportArchive) of
    (Just archivePath, Nothing) -> runArchiveExport cfg opts archivePath
    (Nothing, Just archivePath) -> runArchiveImport cfg opts archivePath
    (Just _, Just _) -> putStrLn "Error: Cannot specify both --export-archive and --import-archive" >> exitFailure
    (Nothing, Nothing) ->
#else
  case (optExportArchive, optImportArchive) of
    (Just _, _) -> putStrLn "Error: Archive export is not supported with PostgreSQL backend" >> exitFailure
    (_, Just _) -> putStrLn "Error: Archive import is not supported with PostgreSQL backend" >> exitFailure
    (Nothing, Nothing) ->
#endif
      if null chatCmd
        then case chatServerPort of
          Just chatPort -> case server_ of
            Just server -> server chatPort cfg opts
            Nothing -> putStrLn "Not allowed to run as a WebSockets server" >> exitFailure
          _ -> runCLI
        else simplexChatCore cfg opts runCommand
  where
    runCLI = do
      welcome cfg opts
      t <- withTerminal pure
      simplexChatTerminal cfg opts t
    runCommand user cc = do
      when (chatCmdLog /= CCLNone) . void . forkIO . forever $ do
        (_, r) <- atomically . readTBQueue $ outputQ cc
        case r of
          Right CEvtNewChatItems {} -> printResponse r
          _ -> when (chatCmdLog == CCLAll) $ printResponse r
      sendChatCmdStr cc chatCmd >>= printResponse
      threadDelay $ chatCmdDelay * 1000000
      where
        printResponse :: ChatResponseEvent r => Either ChatError r -> IO ()
        printResponse r = do
          rh <- readTVarIO $ currentRemoteHost cc
          printResponseEvent (rh, Just user) cfg r

#if !defined(dbPostgres)
runArchiveExport :: ChatConfig -> ChatOpts -> FilePath -> IO ()
runArchiveExport cfg opts archivePath = do
  putStrLn $ "Exporting chat database to: " <> archivePath
  simplexChatCore cfg opts $ \_ cc -> do
    let archiveCfg = ArchiveConfig archivePath Nothing Nothing
    result <- sendChatCmd cc $ APIExportArchive archiveCfg
    case result of
      Right (CRArchiveExported []) -> do
        putStrLn "Archive exported successfully"
        exitSuccess
      Right (CRArchiveExported errs) -> do
        putStrLn $ "Archive exported with " <> show (length errs) <> " file errors:"
        mapM_ (putStrLn . ("  - " <>) . show) errs
        exitSuccess
      Right r -> do
        putStrLn $ "Unexpected response: " <> show r
        exitFailure
      Left err -> do
        putStrLn $ "Export failed: " <> show err
        exitFailure

runArchiveImport :: ChatConfig -> ChatOpts -> FilePath -> IO ()
runArchiveImport cfg opts archivePath = do
  putStrLn $ "Importing chat database from: " <> archivePath
  putStrLn "Warning: This will replace your current database"
  putStrLn "   (Existing databases will be backed up with .bak extension)"
  simplexChatCore cfg opts $ \_ cc -> do
    let archiveCfg = ArchiveConfig archivePath Nothing Nothing
    result <- sendChatCmd cc $ APIImportArchive archiveCfg
    case result of
      Right (CRArchiveImported []) -> do
        putStrLn "Archive imported successfully"
        exitSuccess
      Right (CRArchiveImported errs) -> do
        putStrLn $ "Archive imported with " <> show (length errs) <> " file errors:"
        mapM_ (putStrLn . ("  - " <>) . show) errs
        exitSuccess
      Right r -> do
        putStrLn $ "Unexpected response: " <> show r
        exitFailure
      Left err -> do
        putStrLn $ "Import failed: " <> show err
        exitFailure
#endif

welcome :: ChatConfig -> ChatOpts -> IO ()
welcome ChatConfig {presetServers = PresetServers {netCfg}} ChatOpts {coreOptions = CoreChatOpts {dbOptions, simpleNetCfg = SimpleNetCfg {socksProxy, socksMode, smpProxyMode_, smpProxyFallback_}}} =
  mapM_
    putStrLn
    [ versionString versionNumber,
      "db: " <> dbString dbOptions,
      maybe
        "direct network connection - use `/network` command or `-x` CLI option to connect via SOCKS5 at :9050"
        ((\sp -> "using SOCKS5 proxy " <> sp <> if socksMode == SMOnion then " for onion servers ONLY." else " for ALL servers.") . show)
        socksProxy,
      smpProxyModeStr
        (fromMaybe (smpProxyMode netCfg) smpProxyMode_)
        (fromMaybe (smpProxyFallback netCfg) smpProxyFallback_),
      "type \"/help\" or \"/h\" for usage info"
    ]

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}

module Simplex.Chat.Terminal.Main where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM
import Control.Monad
import Data.Maybe (fromMaybe)
import Data.Time.Clock (getCurrentTime)
import Data.Time.LocalTime (getCurrentTimeZone)
import Network.Socket
import Simplex.Chat.Controller (ChatConfig (..), ChatController (..), ChatResponse (..), DefaultAgentServers (DefaultAgentServers, netCfg), SimpleNetCfg (..), currentRemoteHost, versionNumber, versionString)
import Simplex.Chat.Core
import Simplex.Chat.Options
import Simplex.Chat.Terminal
import Simplex.Chat.View (serializeChatResponse, smpProxyModeStr)
import Simplex.Messaging.Client (NetworkConfig (..), SocksMode (..))
import System.Directory (getAppUserDataDirectory)
import System.Exit (exitFailure)
import System.Terminal (withTerminal)

simplexChatCLI :: ChatConfig -> Maybe (ServiceName -> ChatConfig -> ChatOpts -> IO ()) -> IO ()
simplexChatCLI cfg server_ = do
  appDir <- getAppUserDataDirectory "simplex"
  opts <- getChatOpts appDir "simplex_v1"
  simplexChatCLI' cfg opts server_

simplexChatCLI' :: ChatConfig -> ChatOpts -> Maybe (ServiceName -> ChatConfig -> ChatOpts -> IO ()) -> IO ()
simplexChatCLI' cfg opts@ChatOpts {chatCmd, chatCmdLog, chatCmdDelay, chatServerPort} server_ = do
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
        (_, _, r') <- atomically . readTBQueue $ outputQ cc
        case r' of
          CRNewChatItem {} -> printResponse r'
          _ -> when (chatCmdLog == CCLAll) $ printResponse r'
      sendChatCmdStr cc chatCmd >>= printResponse
      threadDelay $ chatCmdDelay * 1000000
      where
        printResponse r = do
          ts <- getCurrentTime
          tz <- getCurrentTimeZone
          rh <- readTVarIO $ currentRemoteHost cc
          putStrLn $ serializeChatResponse (rh, Just user) ts tz rh r

welcome :: ChatConfig -> ChatOpts -> IO ()
welcome ChatConfig {defaultServers = DefaultAgentServers {netCfg}} ChatOpts {coreOptions = CoreChatOpts {dbFilePrefix, simpleNetCfg = SimpleNetCfg {socksProxy, socksMode, smpProxyMode_, smpProxyFallback_}}} =
  mapM_
    putStrLn
    [ versionString versionNumber,
      "db: " <> dbFilePrefix <> "_chat.db, " <> dbFilePrefix <> "_agent.db",
      maybe
        "direct network connection - use `/network` command or `-x` CLI option to connect via SOCKS5 at :9050"
        ((\sp -> "using SOCKS5 proxy " <> sp <> if socksMode == SMOnion then " for onion servers ONLY." else " for ALL servers.") . show)
        socksProxy,
      smpProxyModeStr
        (fromMaybe (smpProxyMode netCfg) smpProxyMode_)
        (fromMaybe (smpProxyFallback netCfg) smpProxyFallback_),
      "type \"/help\" or \"/h\" for usage info"
    ]

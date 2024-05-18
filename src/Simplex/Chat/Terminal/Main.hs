{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}

module Simplex.Chat.Terminal.Main where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM
import Control.Monad
import Data.Time.Clock (getCurrentTime)
import Data.Time.LocalTime (getCurrentTimeZone)
import Network.Socket
import Simplex.Chat.Controller (ChatConfig, ChatController (..), ChatResponse (..), currentRemoteHost, versionNumber, versionString)
import Simplex.Chat.Core
import Simplex.Chat.Options
import Simplex.Chat.Terminal
import Simplex.Chat.View (serializeChatResponse)
import Simplex.Messaging.Client (NetworkConfig (..))
import System.Directory (getAppUserDataDirectory)
import System.Exit (exitFailure)
import System.Terminal (withTerminal)

simplexChatCLI :: ChatConfig -> Maybe (ServiceName -> ChatConfig -> ChatOpts -> IO ()) -> IO ()
simplexChatCLI cfg server_ = do
  appDir <- getAppUserDataDirectory "simplex"
  opts@ChatOpts {chatCmd, chatServerPort} <- getChatOpts appDir "simplex_v1"
  if null chatCmd
    then case chatServerPort of
      Just chatPort -> case server_ of
        Just server -> server chatPort cfg opts
        Nothing -> putStrLn "Not allowed to run as a WebSockets server" >> exitFailure
      _ -> runCLI opts
    else simplexChatCore cfg opts $ runCommand opts
  where
    runCLI opts = do
      welcome opts
      t <- withTerminal pure
      simplexChatTerminal cfg opts t
    runCommand ChatOpts {chatCmd, chatCmdLog, chatCmdDelay} user cc = do
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
          ll <- readTVarIO $ appLogLevel cc
          putStrLn $ serializeChatResponse (rh, Just user) ll ts tz rh r

welcome :: ChatOpts -> IO ()
welcome ChatOpts {coreOptions = CoreChatOpts {dbFilePrefix, networkConfig}} =
  mapM_
    putStrLn
    [ versionString versionNumber,
      "db: " <> dbFilePrefix <> "_chat.db, " <> dbFilePrefix <> "_agent.db",
      maybe
        "direct network connection - use `/network` command or `-x` CLI option to connect via SOCKS5 at :9050"
        (("using SOCKS5 proxy " <>) . show)
        (socksProxy networkConfig),
      "type \"/help\" or \"/h\" for usage info"
    ]

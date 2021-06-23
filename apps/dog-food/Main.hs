{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import ChatOptions
import Control.Concurrent.STM
import Control.Logger.Simple
import Control.Monad.Reader
import Simplex.Chat
import Simplex.Chat.Controller
import Simplex.Input
import Simplex.Messaging.Agent (getSMPAgentClient)
import Simplex.Messaging.Agent.Env.SQLite
import Simplex.Messaging.Client (smpDefaultConfig)
import Simplex.Terminal
import System.Directory (getAppUserDataDirectory)
import UnliftIO.Async (race_)

cfg :: AgentConfig
cfg =
  AgentConfig
    { tcpPort = undefined, -- agent does not listen to TCP
      smpServers = undefined, -- filled in from options
      rsaKeySize = 2048 `div` 8,
      connIdBytes = 12,
      tbqSize = 16,
      dbFile = "smp-chat.db",
      smpCfg = smpDefaultConfig
    }

logCfg :: LogConfig
logCfg = LogConfig {lc_file = Nothing, lc_stderr = True}

main :: IO ()
main = do
  ChatOpts {dbFile, smpServers} <- welcomeGetOpts
  ct <- newChatTerminal
  smpAgent <- getSMPAgentClient cfg {dbFile, smpServers}
  cc <- atomically $ newChatController smpAgent ct $ tbqSize cfg
  -- setLogLevel LogInfo -- LogError
  -- withGlobalLogging logCfg $ do
  dogFoodChat cc ct

welcomeGetOpts :: IO ChatOpts
welcomeGetOpts = do
  appDir <- getAppUserDataDirectory "simplex"
  opts@ChatOpts {dbFile} <- getChatOpts appDir
  putStrLn "SimpleX chat prototype v0.3.1"
  putStrLn $ "db: " <> dbFile
  putStrLn "type \"/help\" or \"/h\" for usage info"
  pure opts

dogFoodChat :: ChatController -> ChatTerminal -> IO ()
dogFoodChat cc ct =
  race_
    (runTerminalInput cc ct)
    (runReaderT runChatController cc)

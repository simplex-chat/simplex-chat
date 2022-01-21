{-# LANGUAGE NamedFieldPuns #-}

module Simplex.Chat.Terminal where

import Control.Logger.Simple
import Control.Monad.Reader
import Simplex.Chat
import Simplex.Chat.Controller
import Simplex.Chat.Help (chatWelcome)
import Simplex.Chat.Options
import Simplex.Chat.Store
import Simplex.Chat.Terminal.Input
import Simplex.Chat.Terminal.Notification
import Simplex.Chat.Terminal.Output
import Simplex.Chat.Types (User)
import Simplex.Chat.Util (whenM)
import Simplex.Messaging.Util (raceAny_)

simplexChat :: WithTerminal t => ChatConfig -> ChatOpts -> t -> IO ()
simplexChat cfg opts t
  | logging opts = do
    setLogLevel LogInfo -- LogError
    withGlobalLogging logCfg initRun
  | otherwise = initRun
  where
    initRun = do
      sendNotification <- initializeNotifications
      let f = chatStoreFile $ dbFilePrefix opts
      st <- createStore f $ dbPoolSize cfg
      user <- getCreateActiveUser st
      ct <- newChatTerminal t
      cc <- newChatController st user cfg opts sendNotification
      runSimplexChat user ct cc

runSimplexChat :: User -> ChatTerminal -> ChatController -> IO ()
runSimplexChat user ct = runReaderT $ do
  whenM (asks firstTime) . liftIO . printToTerminal ct $ chatWelcome user
  raceAny_ [runTerminalInput ct, runTerminalOutput ct, runChatController]

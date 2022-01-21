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
import Simplex.Chat.Util (whenM)
import Simplex.Messaging.Util (raceAny_)
import UnliftIO.STM

simplexChat :: WithTerminal t => ChatConfig -> ChatOpts -> t -> IO ()
simplexChat cfg opts t
  | logging opts = do
    setLogLevel LogInfo -- LogError
    withGlobalLogging logCfg initRun
  | otherwise = initRun
  where
    initRun = do
      sendNotification <- initializeNotifications
      ct <- newChatTerminal t
      cc <- newChatControllerTerminal cfg opts sendNotification
      runSimplexChat ct cc

newChatControllerTerminal :: ChatConfig -> ChatOpts -> (Notification -> IO ()) -> IO ChatController
newChatControllerTerminal config@ChatConfig {dbPoolSize} opts@ChatOpts {dbFilePrefix} sendNotification = do
  let f = chatStoreFile dbFilePrefix
  st <- createStore f dbPoolSize
  user <- getCreateActiveUser st
  newChatController st user config opts sendNotification

runSimplexChat :: ChatTerminal -> ChatController -> IO ()
runSimplexChat ct = runReaderT $ do
  user <- readTVarIO =<< asks currentUser
  whenM (asks firstTime) . liftIO . printToTerminal ct $ chatWelcome user
  raceAny_ [runTerminalInput ct, runTerminalOutput ct, runChatController]

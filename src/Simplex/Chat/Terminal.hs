{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}

module Simplex.Chat.Terminal where

import Control.Monad.Except
import Simplex.Chat.Controller
import Simplex.Chat.Core
import Simplex.Chat.Help (chatWelcome)
import Simplex.Chat.Options
import Simplex.Chat.Terminal.Input
import Simplex.Chat.Terminal.Notification
import Simplex.Chat.Terminal.Output
import Simplex.Messaging.Util (raceAny_)

simplexChatTerminal :: WithTerminal t => ChatConfig -> ChatOpts -> t -> IO ()
simplexChatTerminal cfg opts t = do
  sendToast <- initializeNotifications
  simplexChatCore cfg opts (Just sendToast) $ \u cc -> do
    ct <- newChatTerminal t
    when (firstTime cc) . printToTerminal ct $ chatWelcome u
    runChatTerminal ct cc

runChatTerminal :: ChatTerminal -> ChatController -> IO ()
runChatTerminal ct cc = raceAny_ [runTerminalInput ct cc, runTerminalOutput ct cc, runInputLoop ct cc]

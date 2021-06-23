{-# LANGUAGE NamedFieldPuns #-}

module Simplex.Chat.Controller where

import ChatTerminal.Core (ChatTerminal)
import Control.Concurrent.STM
import Numeric.Natural
import Simplex.Input (InputEvent)
import Simplex.Messaging.Agent (AgentClient)

data ChatController = ChatController
  { smpAgent :: AgentClient,
    chatTerminal :: ChatTerminal,
    inputEventQ :: TBQueue InputEvent
  }

newChatController :: AgentClient -> ChatTerminal -> Natural -> STM ChatController
newChatController smpAgent chatTerminal qSize = do
  inputEventQ <- newTBQueue qSize
  pure ChatController {smpAgent, chatTerminal, inputEventQ}

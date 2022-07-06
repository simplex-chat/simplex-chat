{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Simplex.Chat.Terminal where

import Control.Monad.Except
import qualified Data.List.NonEmpty as L
import Simplex.Chat (defaultChatConfig)
import Simplex.Chat.Controller
import Simplex.Chat.Core
import Simplex.Chat.Help (chatWelcome)
import Simplex.Chat.Options
import Simplex.Chat.Terminal.Input
import Simplex.Chat.Terminal.Notification
import Simplex.Chat.Terminal.Output
import Simplex.Messaging.Agent.Env.SQLite (InitialAgentServers (..))
import Simplex.Messaging.Util (raceAny_)

terminalChatConfig :: ChatConfig
terminalChatConfig =
  defaultChatConfig
    { defaultServers =
        InitialAgentServers
          { smp =
              L.fromList
                [ "smp://u2dS9sG8nMNURyZwqASV4yROM28Er0luVTx5X1CsMrU=@smp4.simplex.im",
                  "smp://hpq7_4gGJiilmz5Rf-CswuU5kZGkm_zOIooSw6yALRg=@smp5.simplex.im",
                  "smp://PQUV2eL0t7OStZOoAsPEV2QYWt4-xilbakvGUGOItUo=@smp6.simplex.im"
                ],
            ntf = ["ntf://FB-Uop7RTaZZEG0ZLD2CIaTjsPh-Fw0zFAnb7QyA8Ks=@ntf2.simplex.im"]
          }
    }

simplexChatTerminal :: WithTerminal t => ChatConfig -> ChatOpts -> t -> IO ()
simplexChatTerminal cfg opts t = do
  sendToast <- initializeNotifications
  simplexChatCore cfg opts (Just sendToast) $ \u cc -> do
    ct <- newChatTerminal t
    when (firstTime cc) . printToTerminal ct $ chatWelcome u
    runChatTerminal ct cc

runChatTerminal :: ChatTerminal -> ChatController -> IO ()
runChatTerminal ct cc = raceAny_ [runTerminalInput ct cc, runTerminalOutput ct cc, runInputLoop ct cc]

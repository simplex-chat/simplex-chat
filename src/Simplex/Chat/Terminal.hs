{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Simplex.Chat.Terminal where

import Control.Exception (handle, throwIO)
import Control.Monad
import qualified Data.List.NonEmpty as L
import Database.SQLite.Simple (SQLError (..))
import qualified Database.SQLite.Simple as DB
import Simplex.Chat (defaultChatConfig)
import Simplex.Chat.Controller
import Simplex.Chat.Core
import Simplex.Chat.Help (chatWelcome)
import Simplex.Chat.Options
import Simplex.Chat.Terminal.Input
import Simplex.Chat.Terminal.Notification
import Simplex.Chat.Terminal.Output
import Simplex.FileTransfer.Client.Presets (defaultXFTPServers)
import Simplex.Messaging.Client (defaultNetworkConfig)
import Simplex.Messaging.Util (raceAny_)
import System.Exit (exitFailure)

terminalChatConfig :: ChatConfig
terminalChatConfig =
  defaultChatConfig
    { defaultServers =
        DefaultAgentServers
          { smp =
              L.fromList
                [ "smp://u2dS9sG8nMNURyZwqASV4yROM28Er0luVTx5X1CsMrU=@smp4.simplex.im,o5vmywmrnaxalvz6wi3zicyftgio6psuvyniis6gco6bp6ekl4cqj4id.onion",
                  "smp://hpq7_4gGJiilmz5Rf-CswuU5kZGkm_zOIooSw6yALRg=@smp5.simplex.im,jjbyvoemxysm7qxap7m5d5m35jzv5qq6gnlv7s4rsn7tdwwmuqciwpid.onion",
                  "smp://PQUV2eL0t7OStZOoAsPEV2QYWt4-xilbakvGUGOItUo=@smp6.simplex.im,bylepyau3ty4czmn77q4fglvperknl4bi2eb2fdy2bh4jxtf32kf73yd.onion"
                ],
            ntf = ["ntf://FB-Uop7RTaZZEG0ZLD2CIaTjsPh-Fw0zFAnb7QyA8Ks=@ntf2.simplex.im,ntg7jdjy2i3qbib3sykiho3enekwiaqg3icctliqhtqcg6jmoh6cxiad.onion"],
            xftp = defaultXFTPServers,
            netCfg = defaultNetworkConfig
          }
    }

simplexChatTerminal :: WithTerminal t => ChatConfig -> ChatOpts -> t -> IO ()
simplexChatTerminal cfg opts t = do
  sendToast <- if muteNotifications opts then pure Nothing else Just <$> initializeNotifications
  handle checkDBKeyError . simplexChatCore cfg opts sendToast $ \u cc -> do
    ct <- newChatTerminal t
    when (firstTime cc) . printToTerminal ct $ chatWelcome u
    runChatTerminal ct cc

checkDBKeyError :: SQLError -> IO ()
checkDBKeyError e = case sqlError e of
  DB.ErrorNotADatabase -> do
    putStrLn "Database file is invalid or you passed an incorrect encryption key"
    exitFailure
  _ -> throwIO e

runChatTerminal :: ChatTerminal -> ChatController -> IO ()
runChatTerminal ct cc = raceAny_ [runTerminalInput ct cc, runTerminalOutput ct cc, runInputLoop ct cc]

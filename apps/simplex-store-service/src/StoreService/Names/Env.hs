{-# LANGUAGE NamedFieldPuns #-}

module StoreService.Names.Env (NamesEnv (..), newNamesEnv) where

import Simplex.Chat.Controller (ChatController)
import Simplex.Messaging.Agent.Protocol (UserId)
import Simplex.Messaging.Agent.Store.Common (DBStore)
import StoreService.Config (NamesConfig)
import StoreService.Names.Phases (Phases, newPhases)
import StoreService.Names.Turns (Turns, newTurns)

data NamesEnv = NamesEnv
  { neChat :: ChatController,
    neUserId :: UserId,
    neConfig :: NamesConfig,
    neStore :: DBStore,
    nePhases :: Phases,
    neTurns :: Turns
  }

newNamesEnv :: ChatController -> UserId -> NamesConfig -> DBStore -> IO NamesEnv
newNamesEnv neChat neUserId neConfig neStore = do
  nePhases <- newPhases
  neTurns <- newTurns
  pure NamesEnv {neChat, neUserId, neConfig, neStore, nePhases, neTurns}

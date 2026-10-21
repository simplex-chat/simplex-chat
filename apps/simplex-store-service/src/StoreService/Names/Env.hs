{-# LANGUAGE NamedFieldPuns #-}

module StoreService.Names.Env (NamesEnv (..), newNamesEnv) where

import Simplex.Chat.Controller (ChatController)
import Simplex.Messaging.Agent.Protocol (UserId)
import Simplex.Messaging.Agent.Store.Common (DBStore)
import StoreService.Config (NamesConfig)
import StoreService.Names.Phases (Phases, newPhases)
import StoreService.Names.Profiles (Profiles, chatProfile, defaultProfileLimits, monotonicSeconds, newProfiles, systemResolver)
import StoreService.Names.Turns (Turns, newTurns)

data NamesEnv = NamesEnv
  { neChat :: ChatController,
    neUserId :: UserId,
    neConfig :: NamesConfig,
    neStore :: DBStore,
    nePhases :: Phases,
    neTurns :: Turns,
    neProfiles :: Profiles
  }

newNamesEnv :: ChatController -> UserId -> NamesConfig -> DBStore -> IO NamesEnv
newNamesEnv neChat neUserId neConfig neStore = do
  nePhases <- newPhases
  neTurns <- newTurns
  neProfiles <- newProfiles defaultProfileLimits monotonicSeconds (chatProfile neChat neUserId) systemResolver
  pure NamesEnv {neChat, neUserId, neConfig, neStore, nePhases, neTurns, neProfiles}

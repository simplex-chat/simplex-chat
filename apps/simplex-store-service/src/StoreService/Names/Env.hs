{-# LANGUAGE NamedFieldPuns #-}

module StoreService.Names.Env (NamesEnv (..), newNamesEnv) where

import Simplex.Chat.Controller (ChatController (..))
import Simplex.Messaging.Agent.Protocol (UserId)
import StoreService.Config (NamesConfig (..))
import StoreService.Names.Chain (newChainClient)
import StoreService.Names.Contracts (Contracts, newContracts)
import StoreService.Names.Profiles (Profiles, chatProfile, defaultProfileLimits, monotonicSeconds, newProfiles, systemResolver)

data NamesEnv = NamesEnv
  { neConfig :: NamesConfig,
    neContracts :: Contracts,
    neProfiles :: Profiles
  }

newNamesEnv :: ChatController -> UserId -> NamesConfig -> IO NamesEnv
newNamesEnv neChat@ChatController {random} neUserId neConfig@NamesConfig {nTLD, nChain} = do
  neChain <- newChainClient random nChain
  neContracts <- newContracts nTLD neChain
  neProfiles <- newProfiles defaultProfileLimits monotonicSeconds (chatProfile neChat neUserId) systemResolver
  pure NamesEnv {neConfig, neContracts, neProfiles}

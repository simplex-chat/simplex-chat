{-# LANGUAGE NamedFieldPuns #-}

module StoreService.Names.Env (NamesEnv (..), newNamesEnv) where

import Data.Time.Clock (getCurrentTime)
import Simplex.Chat.Controller (ChatController (..))
import Simplex.Messaging.Agent.Protocol (UserId)
import Simplex.Messaging.Agent.Store.Common (DBStore)
import StoreService.Config (NamesConfig (..))
import StoreService.Names.Chain (ChainClient, newChainClient)
import StoreService.Names.Contracts (Contracts, newContracts)
import StoreService.Names.Phases (Phases, newPhases)
import StoreService.Names.Profiles (Profiles, chatProfile, defaultProfileLimits, monotonicSeconds, newProfiles, systemResolver)
import StoreService.Names.Records (Records, newRecords)
import StoreService.Names.Sender (Sender, newSender)
import StoreService.Names.Turns (Turns)
import StoreService.Providers (Provider)
import StoreService.Waiters (Waiters)

data NamesEnv = NamesEnv
  { neChat :: ChatController,
    neUserId :: UserId,
    neConfig :: NamesConfig,
    neStore :: DBStore,
    nePhases :: Phases,
    neTurns :: Turns,
    neChain :: ChainClient,
    neContracts :: Contracts,
    neProfiles :: Profiles,
    neSender :: Sender,
    neRecords :: Records,
    neWaiters :: Waiters,
    neProviders :: [Provider]
  }

newNamesEnv :: ChatController -> UserId -> NamesConfig -> DBStore -> Turns -> Waiters -> [Provider] -> IO NamesEnv
newNamesEnv neChat@ChatController {random} neUserId neConfig@NamesConfig {nTLD, nChain, nRecordEditsPerYear} neStore neTurns neWaiters neProviders = do
  nePhases <- newPhases
  neChain <- newChainClient random nChain
  neContracts <- newContracts nTLD neChain
  neProfiles <- newProfiles defaultProfileLimits monotonicSeconds (chatProfile neChat neUserId) systemResolver
  neSender <- newSender random neChain neStore getCurrentTime
  neRecords <- newRecords neStore neTurns neContracts neChain neSender nTLD nRecordEditsPerYear getCurrentTime
  pure NamesEnv {neChat, neUserId, neConfig, neStore, nePhases, neTurns, neChain, neContracts, neProfiles, neSender, neRecords, neWaiters, neProviders}

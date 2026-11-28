{-# LANGUAGE NamedFieldPuns #-}

module StoreService.Names.Env (NamesEnv (..), newNamesEnv) where

import Data.Text (Text)
import Simplex.Chat.Controller (ChatController (..))
import Simplex.Chat.Types (User)
import Simplex.Messaging.Agent.Store.Common (DBStore)
import StoreService.Config (NamesConfig (..))
import StoreService.Names.Chain (newChainClient)
import StoreService.Names.Contracts (Contracts, newContracts)
import StoreService.Names.Phases (Phases, newPhases)
import StoreService.Names.Profiles (Profiles, chatProfile, defaultProfileLimits, monotonicSeconds, newProfiles, systemResolver)
import StoreService.Names.Records (Records, newRecords)
import StoreService.Names.Sender (Sender, newSender)
import StoreService.Names.Turns (Turns)
import StoreService.Names.Unavailable (Unavailable)
import StoreService.Providers (Provider)
import StoreService.Store.Invoices (currentSecond)
import StoreService.Waiters (Waiters)

data NamesEnv = NamesEnv
  { neChat :: ChatController,
    neUser :: User,
    neConfig :: NamesConfig,
    neStore :: DBStore,
    nePhases :: Phases,
    neTurns :: Turns,
    neContracts :: Contracts,
    neProfiles :: Profiles,
    neSender :: Sender,
    neRecords :: Records,
    neWaiters :: Waiters,
    neProviders :: [Provider],
    neReport :: Text -> IO (),
    neUnavailable :: Unavailable
  }

newNamesEnv :: ChatController -> User -> NamesConfig -> DBStore -> Turns -> Waiters -> [Provider] -> (Text -> IO ()) -> Unavailable -> IO NamesEnv
newNamesEnv neChat@ChatController {random} neUser neConfig@NamesConfig {nTLD, nChain, nRecordEditsPerYear, nMinPriceCentsPerYear} neStore neTurns neWaiters neProviders neReport neUnavailable = do
  nePhases <- newPhases
  chain <- newChainClient random nChain
  neContracts <- newContracts neStore nTLD nMinPriceCentsPerYear chain
  neProfiles <- newProfiles defaultProfileLimits monotonicSeconds (chatProfile neChat neUser) systemResolver
  neSender <- newSender random chain neStore currentSecond
  neRecords <- newRecords neStore neTurns neContracts neSender nTLD nRecordEditsPerYear currentSecond
  pure NamesEnv {neChat, neUser, neConfig, neStore, nePhases, neTurns, neContracts, neProfiles, neSender, neRecords, neWaiters, neProviders, neReport, neUnavailable}

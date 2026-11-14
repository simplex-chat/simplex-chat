{-# LANGUAGE NamedFieldPuns #-}

module StoreService.Names.Phases (Phases, RegistrationId (..), newPhases, publish, awaitChange, phasesCount, phasesWaiting) where

import Control.Concurrent.STM
import Control.Exception (bracket)
import Data.Int (Int64)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)

newtype RegistrationId = RegistrationId Text
  deriving (Eq, Ord, Show)

-- | wVersion only grows, so a publish that arrives after a newer one cannot wake anyone.
data Watch = Watch {wVersion :: TVar Int64, wRefs :: TVar Int}

newtype Phases = Phases (TVar (Map RegistrationId Watch))

newPhases :: IO Phases
newPhases = Phases <$> newTVarIO Map.empty

-- | Call after the transaction that wrote this version commits.
publish :: Phases -> RegistrationId -> Int64 -> STM ()
publish (Phases watches) rid version =
  readTVar watches >>= mapM_ (\Watch {wVersion} -> modifyTVar' wVersion (max version)) . Map.lookup rid

-- | The registrations with a waiter.
phasesCount :: Phases -> IO Int
phasesCount (Phases watches) = Map.size <$> readTVarIO watches

-- | The waiters across all registrations.
phasesWaiting :: Phases -> STM Int
phasesWaiting (Phases watches) = readTVar watches >>= fmap sum . mapM (\Watch {wRefs} -> readTVar wRefs) . Map.elems

subscribe :: Phases -> RegistrationId -> STM Watch
subscribe (Phases watches) rid =
  readTVar watches >>= \ws -> case Map.lookup rid ws of
    Just watch@Watch {wRefs} -> watch <$ modifyTVar' wRefs (+ 1)
    Nothing -> do
      watch <- Watch <$> newTVar minBound <*> newTVar 1
      watch <$ writeTVar watches (Map.insert rid watch ws)

release :: Phases -> RegistrationId -> STM ()
release (Phases watches) rid =
  readTVar watches >>= \ws -> mapM_ (drop' ws) (Map.lookup rid ws)
  where
    drop' ws Watch {wRefs} = do
      n <- pred <$> readTVar wRefs
      if n <= 0 then writeTVar watches (Map.delete rid ws) else writeTVar wRefs n

-- | True once the version differs from @seen@, False after @micros@ without a change.
-- Subscribes before reading the version, so a publish between the read and the wait is not missed.
awaitChange :: Phases -> RegistrationId -> IO Int64 -> Int64 -> Int -> IO Bool
awaitChange phases rid readVersion seen micros =
  bracket (atomically $ subscribe phases rid) (const . atomically $ release phases rid) $ \Watch {wVersion} -> do
    current <- readVersion
    atomically $ modifyTVar' wVersion (max current)
    if current /= seen
      then pure True
      else do
        timer <- registerDelay micros
        atomically $
          (readTVar wVersion >>= \v -> if v > seen then pure True else retry)
            `orElse` (False <$ (readTVar timer >>= check))

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module StoreService.Names.Phases (Phases, RegistrationId (..), newPhases, publish, awaitChange, awaitPublish, phasesCount, phasesWaiting) where

import Control.Concurrent.STM
import Control.Exception (bracket)
import Data.Int (Int64)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Simplex.Messaging.TMap (TMap)
import qualified Simplex.Messaging.TMap as TM

newtype RegistrationId = RegistrationId Text
  deriving (Eq, Ord, Show)

-- | wVersion only grows, so a publish that arrives after a newer one cannot wake anyone.
data Watch = Watch {wVersion :: TVar Int64, wRefs :: TVar Int}

newtype Phases = Phases (TMap RegistrationId Watch)

newPhases :: IO Phases
newPhases = Phases <$> TM.emptyIO

-- | Call after the transaction that wrote this version commits.
publish :: Phases -> RegistrationId -> Int64 -> STM ()
publish (Phases watches) rid version =
  TM.lookup rid watches >>= mapM_ (\Watch {wVersion} -> modifyTVar' wVersion (max version))

-- | The registrations with a waiter.
phasesCount :: Phases -> IO Int
phasesCount (Phases watches) = Map.size <$> readTVarIO watches

-- | The waiters across all registrations.
phasesWaiting :: Phases -> STM Int
phasesWaiting (Phases watches) = readTVar watches >>= fmap sum . mapM (\Watch {wRefs} -> readTVar wRefs) . Map.elems

subscribe :: Phases -> RegistrationId -> STM Watch
subscribe (Phases watches) rid =
  TM.lookup rid watches >>= \case
    Just watch@Watch {wRefs} -> watch <$ modifyTVar' wRefs (+ 1)
    Nothing -> do
      watch <- Watch <$> newTVar minBound <*> newTVar 1
      watch <$ TM.insert rid watch watches

release :: Phases -> RegistrationId -> STM ()
release (Phases watches) rid =
  TM.lookup rid watches >>= mapM_ drop'
  where
    drop' Watch {wRefs} = do
      n <- pred <$> readTVar wRefs
      if n <= 0 then TM.delete rid watches else writeTVar wRefs n

-- | True once the version differs from @seen@, False after @micros@ without a change.
-- Subscribes before reading the version, so a publish between the read and the wait is not missed.
awaitChange :: Phases -> RegistrationId -> IO Int64 -> Int64 -> Int -> IO Bool
awaitChange phases rid readVersion seen micros =
  bracket (atomically $ subscribe phases rid) (const . atomically $ release phases rid) $ \Watch {wVersion} -> do
    current <- readVersion
    atomically $ modifyTVar' wVersion (max current)
    if current /= seen
      then pure True
      else awaitAbove wVersion seen micros

awaitPublish :: Phases -> RegistrationId -> Int -> IO Bool
awaitPublish phases rid micros =
  bracket (atomically $ subscribe phases rid >>= \watch@Watch {wVersion} -> (,) watch <$> readTVar wVersion) (const . atomically $ release phases rid) $ \(Watch {wVersion}, seen) ->
    awaitAbove wVersion seen micros

awaitAbove :: TVar Int64 -> Int64 -> Int -> IO Bool
awaitAbove wVersion seen micros = do
  timer <- registerDelay micros
  atomically $
    (readTVar wVersion >>= \v -> if v > seen then pure True else retry)
      `orElse` (False <$ (readTVar timer >>= check))

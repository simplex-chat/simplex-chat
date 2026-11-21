{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | A thread that takes a turn it already holds waits forever. Label turns are taken before a code turn or a balance turn,
-- and a balance turn is never taken with a code turn.
module StoreService.Names.Turns (Turns, Moved, newTurns, withLabelTurn, withLabelTurns, withRegistrationTurn, withRegistrationTurns, withCodeTurn, withBalanceTurn, turnsCount, labelTurnCount, codeTurnCount, balanceTurnCount) where

import Control.Concurrent.STM
import Control.Exception (bracket)
import qualified Data.Map.Strict as Map
import Simplex.Messaging.Agent.Lock (Lock, createLock, withLock')
import Simplex.Messaging.Agent.Store.Common (DBStore, withTransaction)
import Simplex.Messaging.SimplexName (LabelHash)
import Simplex.Messaging.TMap (TMap)
import qualified Simplex.Messaging.TMap as TM
import StoreService.Names.Phases (RegistrationId (..))
import StoreService.Names.Store (CodeHash, NameRegistration (..), getRegistration)

-- | Each lock counts the requests holding or waiting for it, and its entry goes when none are left.
data Turns = Turns {labelTurns :: TMap LabelHash (Lock, Int), codeTurns :: TMap CodeHash (Lock, Int), balanceTurns :: TMap RegistrationId (Lock, Int)}

newTurns :: IO Turns
newTurns = Turns <$> TM.emptyIO <*> TM.emptyIO <*> TM.emptyIO

withLabelTurn :: Turns -> LabelHash -> IO a -> IO a
withLabelTurn Turns {labelTurns} = withTurn labelTurns

-- | Taken in hash order, so two requests moving between the same names cannot each hold one and wait for the other.
withLabelTurns :: Turns -> LabelHash -> LabelHash -> IO a -> IO a
withLabelTurns turns a b action
  | a == b = withLabelTurn turns a action
  | a < b = withLabelTurn turns a $ withLabelTurn turns b action
  | otherwise = withLabelTurn turns b $ withLabelTurn turns a action

type Moved = ()

withRegistrationTurn :: DBStore -> Turns -> NameRegistration -> (NameRegistration -> IO a) -> IO (Maybe (Either Moved a))
withRegistrationTurn st turns r = withRegistrationTurns st turns r $ nrLabelHash r

withRegistrationTurns :: DBStore -> Turns -> NameRegistration -> LabelHash -> (NameRegistration -> IO a) -> IO (Maybe (Either Moved a))
withRegistrationTurns st turns NameRegistration {nrId, nrLabelHash = held} other action =
  withLabelTurns turns held other $
    withTransaction st (`getRegistration` nrId) >>= traverse inTurns
  where
    inTurns current
      | nrLabelHash current `elem` [held, other] = Right <$> action current
      | otherwise = pure $ Left ()

-- | The label hashes, code hashes and balances with a holder or a waiter.
turnsCount :: Turns -> IO Int
turnsCount Turns {labelTurns, codeTurns, balanceTurns} = atomically $ (\l c b -> l + c + b) <$> size labelTurns <*> size codeTurns <*> size balanceTurns
  where
    size locks = Map.size <$> readTVar locks

-- | The requests holding or waiting for the label hash's turn.
labelTurnCount :: Turns -> LabelHash -> STM Int
labelTurnCount Turns {labelTurns} = turnRequests labelTurns

-- | Keyed by the code's SHA-256 hash.
withCodeTurn :: Turns -> CodeHash -> IO a -> IO a
withCodeTurn Turns {codeTurns} = withTurn codeTurns

codeTurnCount :: Turns -> CodeHash -> STM Int
codeTurnCount Turns {codeTurns} = turnRequests codeTurns

-- | Keyed by the order the balance belongs to.
withBalanceTurn :: Turns -> RegistrationId -> IO a -> IO a
withBalanceTurn Turns {balanceTurns} = withTurn balanceTurns

-- | The requests holding or waiting for the balance's turn.
balanceTurnCount :: Turns -> RegistrationId -> STM Int
balanceTurnCount Turns {balanceTurns} = turnRequests balanceTurns

turnRequests :: Ord k => TMap k (Lock, Int) -> k -> STM Int
turnRequests locks key = maybe 0 snd <$> TM.lookup key locks

withTurn :: Ord k => TMap k (Lock, Int) -> k -> IO a -> IO a
withTurn locks key action =
  bracket (atomically enter) (const $ atomically leave) $ \lock -> withLock' lock "turn" action
  where
    enter =
      TM.lookup key locks >>= \case
        Just (lock, n) -> lock <$ TM.insert key (lock, n + 1) locks
        Nothing -> do
          lock <- createLock
          lock <$ TM.insert key (lock, 1) locks
    leave = TM.update (\(lock, n) -> if n <= 1 then Nothing else Just (lock, n - 1)) key locks

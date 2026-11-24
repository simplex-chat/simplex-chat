{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | A thread that takes a turn it already holds waits forever. Label turns are taken before a code turn or a balance turn,
-- and a balance turn is never taken with a code turn.
module StoreService.Names.Turns (Turns, NameSelected, newTurns, withLabelTurn, withLabelTurns, withRegistrationTurn, withRegistrationTurns, withCodeTurn, withBalanceTurn) where

import qualified Data.Set as S
import Simplex.Messaging.Agent.Client (withLockMap, withLocksMap)
import Simplex.Messaging.Agent.Lock (Lock)
import Simplex.Messaging.Agent.Store.Common (DBStore, withTransaction)
import Simplex.Messaging.SimplexName (LabelHash)
import Simplex.Messaging.TMap (TMap)
import qualified Simplex.Messaging.TMap as TM
import StoreService.Names.Phases (RegistrationId (..))
import StoreService.Names.Store (CodeHash, NameRegistrationOrder (..), getRegistration)

data Turns = Turns {labelTurns :: TMap LabelHash Lock, codeTurns :: TMap CodeHash Lock, balanceTurns :: TMap RegistrationId Lock}

newTurns :: IO Turns
newTurns = Turns <$> TM.emptyIO <*> TM.emptyIO <*> TM.emptyIO

withLabelTurn :: Turns -> LabelHash -> IO a -> IO a
withLabelTurn Turns {labelTurns} h = withLockMap labelTurns h "turn"

-- | Taken in hash order, so two requests for the same two names cannot each hold one and wait for the other.
withLabelTurns :: Turns -> LabelHash -> LabelHash -> IO a -> IO a
withLabelTurns Turns {labelTurns} a b = withLocksMap labelTurns (S.fromList [a, b]) "turn"

type NameSelected = ()

withRegistrationTurn :: DBStore -> Turns -> NameRegistrationOrder -> (NameRegistrationOrder -> IO a) -> IO (Maybe (Either NameSelected a))
withRegistrationTurn st turns r = withRegistrationTurns st turns r $ nrLabelHash r

withRegistrationTurns :: DBStore -> Turns -> NameRegistrationOrder -> LabelHash -> (NameRegistrationOrder -> IO a) -> IO (Maybe (Either NameSelected a))
withRegistrationTurns st turns NameRegistrationOrder {nrId, nrLabelHash = held} other action =
  withLabelTurns turns held other $
    withTransaction st (`getRegistration` nrId) >>= traverse inTurns
  where
    inTurns current
      | nrLabelHash current `elem` [held, other] = Right <$> action current
      | otherwise = pure $ Left ()

-- | Keyed by the code's SHA-256 hash.
withCodeTurn :: Turns -> CodeHash -> IO a -> IO a
withCodeTurn Turns {codeTurns} h = withLockMap codeTurns h "turn"

-- | Keyed by the order the balance belongs to.
withBalanceTurn :: Turns -> RegistrationId -> IO a -> IO a
withBalanceTurn Turns {balanceTurns} rid = withLockMap balanceTurns rid "turn"

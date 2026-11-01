{-# LANGUAGE NamedFieldPuns #-}

-- | A thread that takes a turn it already holds waits forever.
module StoreService.Names.Turns (Turns, newTurns, withCodeTurn) where

import Control.Concurrent.STM
import Control.Exception (bracket, bracket_)
import Data.ByteString (ByteString)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

-- | Each lock counts the requests holding or waiting for it, and its entry goes when none are left.
type TurnLocks = TVar (Map ByteString (TMVar (), Int))

data Turns = Turns {codeTurns :: TurnLocks}

newTurns :: IO Turns
newTurns = Turns <$> newTVarIO Map.empty

-- | Keyed by the code's SHA-256 hash.
withCodeTurn :: Turns -> ByteString -> IO a -> IO a
withCodeTurn Turns {codeTurns} = withTurn codeTurns

withTurn :: TurnLocks -> ByteString -> IO a -> IO a
withTurn locks key action =
  bracket (atomically enter) (const $ atomically leave) $ \lock ->
    bracket_ (atomically $ takeTMVar lock) (atomically $ putTMVar lock ()) action
  where
    enter = do
      held <- readTVar locks
      case Map.lookup key held of
        Just (lock, n) -> lock <$ writeTVar locks (Map.insert key (lock, n + 1) held)
        Nothing -> do
          lock <- newTMVar ()
          lock <$ writeTVar locks (Map.insert key (lock, 1) held)
    leave = modifyTVar' locks (Map.update (\(lock, n) -> if n <= 1 then Nothing else Just (lock, n - 1)) key)

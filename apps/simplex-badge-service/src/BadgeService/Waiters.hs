{-# LANGUAGE NamedFieldPuns #-}

module BadgeService.Waiters (Waiters, Seen, newWaiters, publish, publishPayment, awaitStatus, waitingCount, waitingCountSTM) where

import Control.Concurrent.STM
import Control.Exception (bracket)
import Control.Monad (forM_, when)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isNothing)
import Data.Text (Text)
import Simplex.Chat.PaymentService.Types (InvoiceId (..), InvoiceStatus)

-- | wStatus stays Nothing until a publish or a read fills it, so a publish mid-read is not overwritten by the older answer.
data Watch = Watch {wStatus :: TVar (Maybe InvoiceStatus), wPayments :: TVar Int, wRefs :: TVar Int}

newtype Waiters = Waiters (TVar (Map Text Watch))

type Seen = (InvoiceStatus, (Text, Bool))

newWaiters :: IO Waiters
newWaiters = Waiters <$> newTVarIO Map.empty

-- | Call after the settling transaction commits. The reader re-reads the row, so an overwriting publish costs nothing.
publish :: Waiters -> InvoiceId -> InvoiceStatus -> STM ()
publish (Waiters wv) (InvoiceId iid) status = do
  watches <- readTVar wv
  forM_ (Map.lookup iid watches) $ \Watch {wStatus} -> writeTVar wStatus (Just status)

-- | Call after the commit, for a payment that did not move the status.
publishPayment :: Waiters -> InvoiceId -> STM ()
publishPayment (Waiters wv) (InvoiceId iid) = do
  watches <- readTVar wv
  forM_ (Map.lookup iid watches) $ \Watch {wPayments} -> modifyTVar' wPayments (+ 1)

subscribe :: Waiters -> InvoiceId -> STM Watch
subscribe (Waiters wv) (InvoiceId iid) = do
  watches <- readTVar wv
  case Map.lookup iid watches of
    Just watch@Watch {wRefs} -> do
      modifyTVar' wRefs (+ 1)
      pure watch
    Nothing -> do
      status <- newTVar Nothing
      payments <- newTVar 0
      refs <- newTVar 1
      let watch = Watch {wStatus = status, wPayments = payments, wRefs = refs}
      writeTVar wv (Map.insert iid watch watches)
      pure watch

release :: Waiters -> InvoiceId -> STM ()
release (Waiters wv) (InvoiceId iid) = do
  watches <- readTVar wv
  forM_ (Map.lookup iid watches) $ \Watch {wRefs} -> do
    n <- pred <$> readTVar wRefs
    if n <= 0
      then writeTVar wv (Map.delete iid watches)
      else writeTVar wRefs n

-- | Subscribe, then read, then block; reading first would miss a settlement landing in between.
awaitStatus :: Waiters -> InvoiceId -> IO Seen -> Seen -> Int -> IO InvoiceStatus
awaitStatus w iid readSeen seen@(seenStatus, _) usec =
  bracket (atomically $ subscribe w iid) (const . atomically $ release w iid) $ \Watch {wStatus, wPayments} -> do
    paidAt <- readTVarIO wPayments
    current@(currentStatus, _) <- readSeen -- after subscribing, never before
    -- a publish that landed between subscribe and this read is the fresher answer, so keep it
    atomically $ readTVar wStatus >>= \published -> when (isNothing published) (writeTVar wStatus (Just currentStatus))
    if current /= seen
      then pure currentStatus
      else do
        timer <- registerDelay usec
        atomically $
          ( do
              published <- readTVar wStatus
              payments <- readTVar wPayments
              case published of
                -- seeded above and only republished, so this stays `Just` while the bracket holds its reference
                Just s | s /= seenStatus || payments /= paidAt -> pure s
                _ -> retry
          )
            `orElse` ( do
                         readTVar timer >>= check
                         fromMaybe seenStatus <$> readTVar wStatus
                     )

waitingCount :: Waiters -> IO Int
waitingCount = atomically . waitingCountSTM

-- | In STM so the poller can block on it changing, letting an arriving browser shorten the sleep it lands in.
waitingCountSTM :: Waiters -> STM Int
waitingCountSTM (Waiters wv) = Map.size <$> readTVar wv

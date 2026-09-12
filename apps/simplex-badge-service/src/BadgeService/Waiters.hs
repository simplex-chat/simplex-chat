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

-- | 'wStatus' stays 'Nothing' until a publish or the caller's own read fills it in, so a
-- publish arriving mid-read is not overwritten by the older answer. 'wPayments' counts
-- payments seen: a payment the provider has not confirmed leaves the status alone, so
-- without it a held request could only be released by a timeout.
data Watch = Watch {wStatus :: TVar (Maybe InvoiceStatus), wPayments :: TVar Int, wRefs :: TVar Int}

newtype Waiters = Waiters (TVar (Map Text Watch))

-- | What a held request is waiting out: the status it last saw, and the payment figure and
-- provider verdict it drew under that status.
type Seen = (InvoiceStatus, (Text, Bool))

newWaiters :: IO Waiters
newWaiters = Waiters <$> newTVarIO Map.empty

-- | Call after the settling transaction commits. What is published is a wake-up hint, never
-- an answer: the reader re-reads the row, so a later publish overwriting an earlier one costs
-- nothing.
publish :: Waiters -> InvoiceId -> InvoiceStatus -> STM ()
publish (Waiters wv) (InvoiceId iid) status = do
  watches <- readTVar wv
  forM_ (Map.lookup iid watches) $ \Watch {wStatus} -> writeTVar wStatus (Just status)

-- | For a payment that did not move the status. Same rule: call after the commit.
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

-- | Subscribe, then read, then block: reading first misses a settlement landing in between and
-- waits out the whole timeout with the answer already committed. Released by a status that differs
-- from 'seen', by a payment arriving even if it leaves the status alone, or by the timer.
awaitStatus :: Waiters -> InvoiceId -> IO Seen -> Seen -> Int -> IO InvoiceStatus
awaitStatus w iid readSeen seen@(seenStatus, _) usec =
  bracket (atomically $ subscribe w iid) (const . atomically $ release w iid) $ \Watch {wStatus, wPayments} -> do
    paidAt <- readTVarIO wPayments
    current@(currentStatus, _) <- readSeen -- after subscribing, never before
    -- a publish that landed between subscribe and this read is the fresher answer, so keep it
    atomically $ readTVar wStatus >>= \published -> when (isNothing published) (writeTVar wStatus (Just currentStatus))
    -- the counter only counts what arrives after the baseline above, so a payment that landed
    -- before it shows up here or nowhere
    if current /= seen
      then pure currentStatus
      else do
        timer <- registerDelay usec
        atomically $
          ( do
              published <- readTVar wStatus
              payments <- readTVar wPayments
              case published of
                -- seeded above and only ever republished, so this is `Just` for as long as the
                -- bracket holds its reference and nothing here has to answer for `Nothing`
                Just s | s /= seenStatus || payments /= paidAt -> pure s
                _ -> retry
          )
            `orElse` ( do
                         readTVar timer >>= check
                         fromMaybe seenStatus <$> readTVar wStatus
                     )

waitingCount :: Waiters -> IO Int
waitingCount = atomically . waitingCountSTM

-- | In STM so the poller can block on it changing: an arriving browser has to shorten
-- the sleep it lands in, not the one after.
waitingCountSTM :: Waiters -> STM Int
waitingCountSTM (Waiters wv) = Map.size <$> readTVar wv

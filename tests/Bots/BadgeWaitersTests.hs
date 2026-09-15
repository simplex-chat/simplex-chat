{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Bots.BadgeWaitersTests (badgeWaitersTests) where

import BadgeService.Waiters
import Control.Concurrent.Async (wait, withAsync)
import Control.Concurrent.STM
import Control.Exception (Exception, SomeException, throwIO, try)
import Data.IORef (newIORef, readIORef)
import Data.Text (Text)
import Simplex.Chat.PaymentService.Types (InvoiceId (..), InvoiceStatus (..))
import System.Timeout (timeout)
import Test.Hspec

badgeWaitersTests :: Spec
badgeWaitersTests = describe "badge service waiters" $ do
  it "wakes on a status published between subscribe and the database read" testSubscribeBeforeRead
  it "answers a payment whose publish landed before this request subscribed" testPaymentPublishedBeforeSubscribe
  it "is not woken by a publish for a different invoice" testDifferentInvoiceNoWake
  it "returns the current status, not an error, when the wait times out" testTimeoutReturnsCurrentStatus
  it "empties the map when the waiting action throws" testMapEmptiesOnException
  it "empties the map after a normal return" testMapEmptiesOnNormalReturn
  it "keeps one map entry for two waiters on the same invoice" testRefcountSharedInvoice
  it "counts one entry per invoice, not per waiter" testWaitingCountPerInvoice

iid, iid2 :: InvoiceId
iid = InvoiceId "inv-1"
iid2 = InvoiceId "inv-2"

-- | An invoice nobody has paid into: no figure, and no verdict from the provider.
unpaid :: (Text, Bool)
unpaid = ("", False)

openUnpaid :: Seen
openUnpaid = (ISOpen, unpaid)

data Boom = Boom deriving (Show)

instance Exception Boom

-- | This suite has hung rather than failed before. `us` must exceed every awaitStatus
-- timeout the test uses.
bounded :: String -> Int -> IO a -> IO a
bounded what us act =
  timeout us act >>= \case
    Just a -> pure a
    Nothing -> do
      expectationFailure (what <> " did not finish within " <> show us <> "us")
      error "unreachable: expectationFailure always throws"

testSubscribeBeforeRead :: IO ()
testSubscribeBeforeRead = bounded "the ordering wait" 2_000_000 $ do
  w <- newWaiters
  ref <- newIORef ISOpen
  let readStatus = do
        atomically $ publish w iid ISPaid
        (\s -> (s, unpaid)) <$> readIORef ref
  s <- awaitStatus w iid readStatus openUnpaid 200_000
  s `shouldBe` ISPaid

testDifferentInvoiceNoWake :: IO ()
testDifferentInvoiceNoWake = bounded "the isolation wait" 2_000_000 $ do
  w <- newWaiters
  otherSubscribed <- newEmptyTMVarIO
  let otherRead = atomically (putTMVar otherSubscribed ()) >> pure openUnpaid
  withAsync (awaitStatus w iid2 otherRead openUnpaid 200_000) $ \other ->
    withAsync (awaitStatus w iid (pure openUnpaid) openUnpaid 200_000) $ \watched -> do
      atomically $ takeTMVar otherSubscribed
      atomically $ publish w iid2 ISPaid
      otherResult <- wait other
      otherResult `shouldBe` ISPaid
      watchedResult <- wait watched
      watchedResult `shouldBe` ISOpen

testTimeoutReturnsCurrentStatus :: IO ()
testTimeoutReturnsCurrentStatus = bounded "the timeout wait" 1_000_000 $ do
  w <- newWaiters
  s <- awaitStatus w iid (pure openUnpaid) openUnpaid 50_000
  s `shouldBe` ISOpen

testMapEmptiesOnException :: IO ()
testMapEmptiesOnException = bounded "the exception-cleanup wait" 1_000_000 $ do
  w <- newWaiters
  _ <- try @SomeException $ awaitStatus w iid (throwIO Boom) openUnpaid 1_000_000
  waitingCount w >>= (`shouldBe` 0)

testMapEmptiesOnNormalReturn :: IO ()
testMapEmptiesOnNormalReturn = bounded "the normal-return cleanup wait" 1_000_000 $ do
  w <- newWaiters
  _ <- awaitStatus w iid (pure openUnpaid) openUnpaid 50_000
  waitingCount w >>= (`shouldBe` 0)

testRefcountSharedInvoice :: IO ()
testRefcountSharedInvoice = bounded "the shared-invoice refcount wait" 2_000_000 $ do
  w <- newWaiters
  entered <- newTVarIO (0 :: Int)
  go <- newTVarIO False
  let readStatus = do
        atomically $ modifyTVar' entered (+ 1)
        atomically $ readTVar go >>= check
        pure openUnpaid
  withAsync (awaitStatus w iid readStatus openUnpaid 500_000) $ \a1 ->
    withAsync (awaitStatus w iid readStatus openUnpaid 500_000) $ \a2 -> do
      atomically $ readTVar entered >>= check . (== 2)
      waitingCount w >>= (`shouldBe` 1)
      atomically $ writeTVar go True
      atomically $ publish w iid ISPaid
      r1 <- wait a1
      r2 <- wait a2
      r1 `shouldBe` ISPaid
      r2 `shouldBe` ISPaid
      waitingCount w >>= (`shouldBe` 0)

testWaitingCountPerInvoice :: IO ()
testWaitingCountPerInvoice = bounded "the per-invoice waiting-count wait" 2_000_000 $ do
  w <- newWaiters
  entered <- newTVarIO (0 :: Int)
  go <- newTVarIO False
  let readStatus = do
        atomically $ modifyTVar' entered (+ 1)
        atomically $ readTVar go >>= check
        pure openUnpaid
  withAsync (awaitStatus w iid readStatus openUnpaid 500_000) $ \a1 ->
    withAsync (awaitStatus w iid2 readStatus openUnpaid 500_000) $ \a2 -> do
      atomically $ readTVar entered >>= check . (== 2)
      waitingCount w >>= (`shouldBe` 2)
      atomically $ writeTVar go True
      atomically $ publish w iid ISPaid
      atomically $ publish w iid2 ISPaid
      r1 <- wait a1
      r2 <- wait a2
      r1 `shouldBe` ISPaid
      r2 `shouldBe` ISPaid
      waitingCount w >>= (`shouldBe` 0)

-- | The payment committed before this request subscribed, so its publish found no watch and
-- the counter cannot report it. The read after subscribing is the only thing that can, and a
-- request that waits for the counter alone parks for its whole timeout with the money already in.
testPaymentPublishedBeforeSubscribe :: IO ()
testPaymentPublishedBeforeSubscribe = do
  w <- newWaiters
  s <- bounded "the lost-publish wait" 1_000_000 $ awaitStatus w iid (pure (ISOpen, ("0.005", True))) openUnpaid 30_000_000
  s `shouldBe` ISOpen

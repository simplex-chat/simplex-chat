{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Bots.StoreService.NamesMetricsTests (namesMetricsTests) where

import Bots.StoreService.FakeEthNode
import Bots.StoreService.NamesContractsTests (minPriceCentsPerYear, withContracts)
import Bots.StoreService.NamesStoreTests (at, btc, insertInvoiceRow, newReg, order, ownerA, paidOrder, price, reveal, t0)
import Bots.StoreService.WebTests (digestFixture, statusOf, webGet, webRequestBody, withApp, withServiceStore)
import Control.Concurrent.Async (cancel)
import Control.Concurrent.STM (TVar, newTVarIO, readTVarIO)
import Control.Exception (finally)
import Control.Monad (forM_)
import Crypto.Random (ChaChaDRG)
import qualified Data.Aeson as J
import qualified Data.ByteString.Lazy as LB
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Network.HTTP.Client (responseBody, responseHeaders)
import Network.HTTP.Types (hContentType)
import Simplex.Messaging.Agent.Store.Common (withTransaction)
import qualified Simplex.Messaging.Crypto as C
import Simplex.Messaging.Eth.Address (addressFromPrivateKey)
import Simplex.Messaging.SimplexName (SimplexTLD (..))
import Simplex.Messaging.Util (tshow)
import StoreService.Names.Abi (addressWord, selector, uint256)
import StoreService.Names.Chain (TxHash (..))
import StoreService.Names.Contracts (newContracts)
import StoreService.Names.Metrics
import StoreService.Names.Phases (RegistrationId (..))
import StoreService.Names.Store
import System.Timeout (timeout)
import Test.Hspec

namesMetricsTests :: Spec
namesMetricsTests = describe "StoreService names: metrics" $ do
  g <- runIO C.newRandom
  it "counts orders by phase, the oldest change among the orders the service moves on, pending transactions by nonce, open name invoices and held balances" testCounts
  it "reads the registrar's ETH balance above 64 bits and its sponsor allowance in cents, and keeps the last read while a read fails" $ testFunds g
  it "answers GET /metrics while the node holds the funds read, and nothing else" $ testScrape g

testCounts :: IO ()
testCounts = withServiceStore $ \st -> do
  let placedAs status rid n terms = withTransaction st $ \db -> do
        insertInvoiceRow db ("inv-" <> rid) (at 60) status
        createPaidRegistration db (newReg rid ownerA $ order (rid <> "bakery") n) terms (paidOrder $ "inv-" <> rid) t0 `shouldReturn` Right ()
      moved rid minute change = withTransaction st $ \db -> do
        Just r <- getRegistration db (RegistrationId rid)
        updateRegistration db (change r) (at minute) `shouldReturn` Written 2
  placedAs "open" "u" 1 (OTInvoice btc price)
  mapM_ (\(rid, n) -> placedAs "paid" rid n (OTInvoice btc price)) [("src", 2), ("q", 3), ("c1", 4), ("c2", 5), ("rg", 6), ("f", 7)]
  moved "src" 1 $ \r -> r {nrPhase = PTaken}
  moved "f" 1 $ \r -> r {nrPhase = PFailed}
  moved "c1" 2 $ \r -> r {nrPhase = PCommitted}
  moved "c2" 3 $ \r -> r {nrPhase = PCommitted, nrReveal = Just reveal}
  moved "rg" 4 $ \r -> r {nrPhase = PRegistering}
  moved "q" 5 $ \r -> r {nrPhase = PQueued}
  withTransaction st $ \db -> do
    creditBalance db (RegistrationId "src") 6000 t0
    creditBalance db (RegistrationId "f") 2500 t0
    insertInvoiceRow db "inv-badge" (at 60) "open"
  placedAs "open" "h" 8 (OTBalance (RegistrationId "src") price 6000 (Just btc))
  withTransaction st $ \db -> do
    forM_ [(40, 0, 6), (41, 1, 7), (42, 1, 8), (43, 2, 9)] $ \(n, nonce, minute) ->
      insertTransaction db (TxCommit $ RegistrationId "q") NewTransaction {ntxRaw = "raw", ntxHash = TxHash $ digestFixture n, ntxNonce = nonce} (at minute)
    recordTransactionOutcome db (TxHash $ digestFixture 40) TSPending TSMined (Just 120) `shouldReturn` True
  counts <- withTransaction st namesCounts
  typedSamples (metricsText counts noFundsRead)
    `shouldBe` [ "simplex_store_names_orders{phase=\"unpaid\"} 2",
                 "simplex_store_names_orders{phase=\"queued\"} 1",
                 "simplex_store_names_orders{phase=\"committing\"} 0",
                 "simplex_store_names_orders{phase=\"committed\"} 2",
                 "simplex_store_names_orders{phase=\"registering\"} 1",
                 "simplex_store_names_orders{phase=\"registered\"} 0",
                 "simplex_store_names_orders{phase=\"taken\"} 1",
                 "simplex_store_names_orders{phase=\"failed\"} 1",
                 "simplex_store_names_queue_oldest_change_timestamp_seconds " <> seconds (at 3),
                 "simplex_store_names_transactions_pending 2",
                 "simplex_store_names_transactions_oldest_pending_timestamp_seconds " <> seconds (at 7),
                 "simplex_store_names_invoices_open 2",
                 "simplex_store_names_balances_held 1",
                 "simplex_store_names_balances_held_cents 6000",
                 "simplex_store_names_funds_read_ok 0"
               ]

testFunds :: TVar ChaChaDRG -> IO ()
testFunds g = withContracts g $ \node ct -> do
  registrar <- addressFromPrivateKey g fakeRegistrarKey
  answerCall node fakeSponsor (selector "registrarAllowance(address)" <> addressWord registrar) $ Returns $ uint256 (123456 * 10 ^ (16 :: Int) + 5 :: Integer)
  setFault node "eth_getBalance" $ Answers $ J.String "0x15af1d78b58c40000"
  v <- newTVarIO noFundsRead
  started <- getCurrentTime
  readFunds ct v
  ended <- getCurrentTime
  FundsRead {frFunds} <- readTVarIO v
  readAt <- maybe (fail "no funds were read") (pure . snd) frFunds
  (started <= readAt && readAt <= ended) `shouldBe` True
  let read' = ["simplex_store_names_registrar_balance_wei 25000000000000000000", "simplex_store_names_sponsor_allowance_cents 123456", "simplex_store_names_funds_read_timestamp_seconds " <> seconds readAt]
  fundsSamples <$> readTVarIO v `shouldReturn` read' <> ["simplex_store_names_funds_read_ok 1"]
  setFault node "eth_getBalance" HttpFailure
  readFunds ct v
  fundsSamples <$> readTVarIO v `shouldReturn` read' <> ["simplex_store_names_funds_read_ok 0"]

testScrape :: TVar ChaChaDRG -> IO ()
testScrape g = withServiceStore $ \st -> withFakeEthNode $ \node -> do
  ct <- newContracts st TLDSimplex minPriceCentsPerYear =<< fakeChainClient g node
  v <- newTVarIO noFundsRead
  (reads', _) <- pausedAt node "eth_getBalance" (runFundsReads ct v)
  (`finally` cancel reads') $ withApp (metricsApp st v) $ \client -> do
    r <- timeout scrapeMicros (webGet client "/metrics") >>= maybe (fail "the scrape waited for the node") pure
    (statusOf r, lookup hContentType (responseHeaders r)) `shouldBe` (200, Just "text/plain; version=0.0.4; charset=utf-8")
    filter ("simplex_store_names_funds" `T.isPrefixOf`) (typedSamples $ decodeUtf8 $ LB.toStrict $ responseBody r) `shouldBe` ["simplex_store_names_funds_read_ok 0"]
    statusOf <$> webGet client "/" `shouldReturn` 404
    statusOf <$> webRequestBody client "POST" "/metrics" [] "" `shouldReturn` 405

scrapeMicros :: Int
scrapeMicros = 2000000

typedSamples :: Text -> [Text]
typedSamples text = [l | l <- ls, not (T.null l || "#" `T.isPrefixOf` l), described (T.takeWhile (`notElem` [' ', '{']) l)]
  where
    ls = T.lines text
    described name = ("# TYPE " <> name <> " gauge") `elem` ls && any (("# HELP " <> name <> " ") `T.isPrefixOf`) ls

fundsSamples :: FundsRead -> [Text]
fundsSamples = filter (\l -> any (`T.isPrefixOf` l) ["simplex_store_names_registrar_", "simplex_store_names_sponsor_", "simplex_store_names_funds_"]) . typedSamples . metricsText noCounts
  where
    noCounts = NamesCounts {ncPhases = [], ncQueueOldestChange = Nothing, ncPendingTransactions = 0, ncOldestPending = Nothing, ncOpenInvoices = 0, ncHeldBalances = 0, ncHeldCents = 0}

seconds :: UTCTime -> Text
seconds t = tshow (floor $ utcTimeToPOSIXSeconds t :: Integer)

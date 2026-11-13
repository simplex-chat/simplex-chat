{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Bots.StoreService.MetricsTests (storeMetricsTests) where

import Bots.StoreService.FakeEthNode
import Bots.StoreService.NamesContractsTests (minPriceCentsPerYear)
import Bots.StoreService.NamesStoreTests (at, btc, insertInvoiceRow, insertProviderInvoice, newEdit, newReg, order, ownerA, ownerB, paidOrder, price, reveal, setChannel, t0)
import Bots.StoreService.WebTests (digestFixture, statusOf, webGet, webRequestBody, withApp, withServiceStore)
import Control.Concurrent.Async (cancel)
import Control.Concurrent.STM (TVar, newTVarIO, readTVarIO)
import Control.Exception (finally)
import Control.Monad (forM_)
import Crypto.Random (ChaChaDRG)
import qualified Data.Aeson as J
import qualified Data.ByteString.Lazy as LB
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust, isJust)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.Word (Word8)
import Network.HTTP.Client (Response, responseBody, responseHeaders)
import Network.HTTP.Types (hContentType)
import Simplex.Chat.Badges.Types (SubscriptionState (..))
import Simplex.Messaging.Agent.Store.Common (withTransaction)
import Simplex.Messaging.Agent.Store.DB (Binary (..))
import qualified Simplex.Messaging.Agent.Store.DB as DB
import qualified Simplex.Messaging.Crypto as C
import Simplex.Messaging.Encoding.String (textEncode)
import Simplex.Messaging.Eth.Address (addressFromPrivateKey)
import Simplex.Messaging.SimplexName (SimplexTLD (..))
import Simplex.Messaging.Util (tshow)
import StoreService.Metrics
import StoreService.Names.Abi (addressWord, selector, uint256)
import StoreService.Names.Chain (TxHash (..))
import StoreService.Names.Contracts (newContracts, termsOf, termsText)
import StoreService.Names.Phases (RegistrationId (..))
import StoreService.Names.Store
import StoreService.Store (badgeCounts)
import StoreService.Store.Invoices (openInvoiceCounts)
import System.Timeout (timeout)
import Test.Hspec

storeMetricsTests :: Spec
storeMetricsTests = describe "StoreService: metrics" $ do
  g <- runIO C.newRandom
  it "counts open invoices by provider with those the sweep would expire, badge purchases by status, subscriptions by state and open badge codes, with the providers' last list and webhook" testServiceCounts
  it "counts name orders by phase, the oldest change among the orders the service moves on, pending transactions by nonce, open name invoices, held balances and pending record edits" testNamesCounts
  it "reads the registrar's funds, the latest block and which terms texts are held, and keeps each part while a read fails" $ testChainRead g
  it "answers GET /metrics with the service metrics alone without names, and with the names metrics while the node holds the chain read" $ testScrape g

testServiceCounts :: IO ()
testServiceCounts = withServiceStore $ \st -> do
  withTransaction st $ \db -> do
    insertProviderInvoice db "crypto" "c-overdue" (at 1) "open"
    insertProviderInvoice db "crypto" "c-open" (at 60) "open"
    insertProviderInvoice db "crypto" "c-paid" (at 1) "paid"
    insertProviderInvoice db "stripe" "s-funded" (at 1) "open"
    DB.execute
      db
      "INSERT INTO sx_badge_service_payments (payment_id, invoice_id, provider, amount, currency, status, created_at, updated_at) VALUES (?,?,?,?,?,?,?,?)"
      ("pay-s" :: Text, "s-funded" :: Text, "stripe" :: Text, price, "usd" :: Text, "pending" :: Text, t0, t0)
    forM_ [(1, "active"), (2, "active"), (3, "expired")] $ \(n, status) ->
      DB.execute
        db
        "INSERT INTO sx_badge_service_badge_purchases (purchase_key, master_key, initial_badge_type, current_badge_type, status, created_at, updated_at) VALUES (?,?,?,?,?,?,?)"
        (Binary $ digestFixture n, Binary $ digestFixture (n + 100), "supporter" :: Text, "supporter" :: Text, status :: Text, t0, t0)
    forM_ [("sub-1", SSRenewing), ("sub-2", SSRenewing), ("sub-3", SSBillingRetry)] $ \(paymentId, state) ->
      DB.execute
        db
        "INSERT INTO sx_badge_service_payments (payment_id, provider, status, subscription_state, created_at, updated_at) VALUES (?,?,?,?,?,?)"
        (paymentId :: Text, "google" :: Text, "paid" :: Text, textEncode state, t0, t0)
    forM_ codes $ \(n, paymentStatus, (redeemLimit, redeemCount), revokedAt, expiresAt) ->
      DB.execute
        db
        "INSERT INTO sx_badge_service_badge_codes (code_hash, badge_type, months, code_payment_status, redeem_limit, redeem_count, revoked_at, expires_at, created_at) VALUES (?,?,?,?,?,?,?,?,?)"
        (Binary $ digestFixture n, "supporter" :: Text, 1 :: Int, paymentStatus, redeemLimit, redeemCount, revokedAt, expiresAt, t0)
  (invoices, badges) <- withTransaction st $ \db -> (,) <$> openInvoiceCounts db (at 10) <*> badgeCounts db (at 10)
  invoices `shouldBe` [("crypto", 2, 1), ("stripe", 1, 0)]
  let times = ProviderTimes {ptListedOk = M.fromList [("crypto", at 11), ("stripe", at 12)], ptWebhookAt = M.fromList [("stripe", at 13)]}
  typedSamples (serviceText invoices badges times)
    `shouldBe` [ "simplex_store_invoices_open{provider=\"crypto\"} 2",
                 "simplex_store_invoices_open{provider=\"stripe\"} 1",
                 "simplex_store_invoices_overdue{provider=\"crypto\"} 1",
                 "simplex_store_invoices_overdue{provider=\"stripe\"} 0",
                 "simplex_store_provider_list_ok_timestamp_seconds{provider=\"crypto\"} " <> seconds (at 11),
                 "simplex_store_provider_list_ok_timestamp_seconds{provider=\"stripe\"} " <> seconds (at 12),
                 "simplex_store_provider_webhook_timestamp_seconds{provider=\"stripe\"} " <> seconds (at 13),
                 "simplex_store_badges_purchases{status=\"active\"} 2",
                 "simplex_store_badges_purchases{status=\"expired\"} 1",
                 "simplex_store_badges_subscriptions{state=\"billingRetry\"} 1",
                 "simplex_store_badges_subscriptions{state=\"renewing\"} 2",
                 "simplex_store_badges_codes_open 2"
               ]
  where
    -- open: the first two; then unpaid, used up, revoked, expired
    codes :: [(Word8, Text, (Int, Int), Maybe UTCTime, Maybe UTCTime)]
    codes =
      [ (1, "paid", (1, 0), Nothing, Nothing),
        (2, "free", (3, 1), Nothing, Just (at 120)),
        (3, "unpaid", (1, 0), Nothing, Nothing),
        (4, "paid", (2, 2), Nothing, Nothing),
        (5, "paid", (1, 0), Just (at 5), Nothing),
        (6, "paid", (1, 0), Nothing, Just (at 5))
      ]

testNamesCounts :: IO ()
testNamesCounts = withServiceStore $ \st -> do
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
    Just mined <- insertRecordEdit db (newEdit ownerA 1 setChannel) (at 8)
    insertRecordEdit db (newEdit ownerB 3 setChannel) (at 9) >>= (`shouldSatisfy` isJust)
    settleRecordEdit db mined (ESMined Nothing) `shouldReturn` True
  counts <- withTransaction st namesCounts
  typedSamples (namesText counts noChainRead)
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
                 "simplex_store_names_record_edits_pending 1",
                 "simplex_store_names_record_edits_oldest_pending_timestamp_seconds " <> seconds (at 9),
                 "simplex_store_names_chain_read_ok 0"
               ]

testChainRead :: TVar ChaChaDRG -> IO ()
testChainRead g = withServiceStore $ \st -> withFakeEthNode $ \node -> do
  c <- fakeChainClient g node
  ct <- newContracts st TLDSimplex minPriceCentsPerYear c
  let ns = NamesSource {nsChain = c, nsContracts = ct}
  registrar <- addressFromPrivateKey g fakeRegistrarKey
  answerCall node fakeSponsor (selector "registrarAllowance(address)" <> addressWord registrar) $ Returns $ uint256 (123456 * 10 ^ (16 :: Int) + 5 :: Integer)
  setFault node "eth_getBalance" $ Answers $ J.String "0x15af1d78b58c40000"
  v <- newTVarIO noChainRead
  started <- getCurrentTime
  readChain ns v
  ended <- getCurrentTime
  ChainRead {crReadAt} <- readTVarIO v
  readAt <- maybe (fail "no chain read succeeded") pure crReadAt
  (started <= readAt && readAt <= ended) `shouldBe` True
  let held general = ["simplex_store_names_terms_held{kind=\"general\"} " <> general, "simplex_store_names_terms_held{kind=\"community\"} 0"]
  chainSamples <$> readTVarIO v
    `shouldReturn` [ "simplex_store_names_registrar_balance_wei 25000000000000000000",
                     "simplex_store_names_sponsor_allowance_cents 123456",
                     "simplex_store_names_chain_head_block 1",
                     "simplex_store_names_chain_head_timestamp_seconds " <> tshow genesisTime,
                     "simplex_store_names_chain_base_fee_wei " <> tshow (fakeBaseFee :: Integer)
                   ]
      <> held "0"
      <> ["simplex_store_names_chain_read_timestamp_seconds " <> seconds readAt, "simplex_store_names_chain_read_ok 1"]
  termsText ct (fromJust $ termsOf $ fakeTermsLink fakeGeneralTermsText) `shouldReturn` Right "SimpleX general terms v1"
  readChain ns v
  fetched <- chainSamples <$> readTVarIO v
  filter ("simplex_store_names_terms_held" `T.isPrefixOf`) fetched `shouldBe` held "1"
  setFault node "eth_getBalance" HttpFailure
  readChain ns v
  chainSamples <$> readTVarIO v `shouldReturn` init fetched <> ["simplex_store_names_chain_read_ok 0"]

testScrape :: TVar ChaChaDRG -> IO ()
testScrape g = withServiceStore $ \st -> withFakeEthNode $ \node -> do
  c <- fakeChainClient g node
  ct <- newContracts st TLDSimplex minPriceCentsPerYear c
  listedOk <- newTVarIO M.empty
  webhookAt <- newTVarIO M.empty
  v <- newTVarIO noChainRead
  let badgesOnly = MetricsEnv {meStore = st, meListedOk = listedOk, meWebhookAt = webhookAt, meNames = Nothing}
      ns = NamesSource {nsChain = c, nsContracts = ct}
  withApp (metricsApp badgesOnly v) $ \client -> do
    r <- webGet client "/metrics"
    (statusOf r, lookup hContentType (responseHeaders r)) `shouldBe` (200, Just "text/plain; version=0.0.4; charset=utf-8")
    let samples = typedSamples $ bodyText r
    samples `shouldContain` ["simplex_store_badges_codes_open 0"]
    filter ("simplex_store_names_" `T.isPrefixOf`) samples `shouldBe` []
    statusOf <$> webGet client "/" `shouldReturn` 404
    statusOf <$> webRequestBody client "POST" "/metrics" [] "" `shouldReturn` 405
  (reads', _) <- pausedAt node "eth_getBalance" (runChainReads ns v)
  (`finally` cancel reads') $ withApp (metricsApp badgesOnly {meNames = Just ns} v) $ \client -> do
    r <- timeout scrapeMicros (webGet client "/metrics") >>= maybe (fail "the scrape waited for the node") pure
    let samples = typedSamples $ bodyText r
    samples `shouldContain` ["simplex_store_badges_codes_open 0", "simplex_store_names_orders{phase=\"unpaid\"} 0"]
    filter ("simplex_store_names_chain_" `T.isPrefixOf`) samples `shouldBe` ["simplex_store_names_chain_read_ok 0"]

scrapeMicros :: Int
scrapeMicros = 2000000

bodyText :: Response LB.ByteString -> Text
bodyText = decodeUtf8 . LB.toStrict . responseBody

typedSamples :: Text -> [Text]
typedSamples text = [l | l <- ls, not (T.null l || "#" `T.isPrefixOf` l), described (T.takeWhile (`notElem` [' ', '{']) l)]
  where
    ls = T.lines text
    described name = ("# TYPE " <> name <> " gauge") `elem` ls && any (("# HELP " <> name <> " ") `T.isPrefixOf`) ls

chainSamples :: ChainRead -> [Text]
chainSamples = filter (\l -> any (`T.isPrefixOf` l) ["simplex_store_names_registrar_", "simplex_store_names_sponsor_", "simplex_store_names_chain_", "simplex_store_names_terms_"]) . typedSamples . namesText noCounts
  where
    noCounts = NamesCounts {ncPhases = [], ncQueueOldestChange = Nothing, ncPendingTransactions = 0, ncOldestPending = Nothing, ncOpenInvoices = 0, ncHeldBalances = 0, ncHeldCents = 0, ncPendingEdits = 0, ncOldestEdit = Nothing}

seconds :: UTCTime -> Text
seconds t = tshow (floor $ utcTimeToPOSIXSeconds t :: Integer)

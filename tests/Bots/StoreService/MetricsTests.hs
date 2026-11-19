{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Bots.StoreService.MetricsTests (storeMetricsTests) where

import Bots.StoreService.FakeEthNode
import Bots.StoreService.NamesContractsTests (minPriceCentsPerYear)
import Bots.StoreService.NamesStoreTests (at, commitFixture, hashFixture, insertInvoiceRow, insertProviderInvoice, newEdit, newReg, order, ownerA, ownerB, paidOrder, price, reveal, setChannel, t0)
import Bots.StoreService.WebTests (digestFixture, statusOf, webGet, webRequestBody, withApp, withServiceStore)
import Control.Concurrent.Async (cancel)
import Control.Concurrent.STM (TVar, atomically, modifyTVar', newTVarIO, readTVarIO)
import Control.Exception (finally)
import Control.Monad (forM_)
import Crypto.Random (ChaChaDRG)
import qualified Data.Aeson as J
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import qualified Data.Map.Strict as M
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.Word (Word64, Word8)
import Network.HTTP.Client (Response, responseBody, responseHeaders)
import Network.HTTP.Types (hContentType, status404)
import Network.Wai (responseLBS)
import qualified Network.Wai.Handler.Warp as Warp
import Simplex.Chat.Badges.Types (BadgePurchaseStatus (..), SubscriptionState (..))
import Simplex.Chat.PaymentService.Types (PaymentProvider (..))
import Simplex.Messaging.Agent.Store.Common (withTransaction)
import Simplex.Messaging.Agent.Store.DB (Binary (..))
import qualified Simplex.Messaging.Agent.Store.DB as DB
import Simplex.Messaging.Crypto (keccak256)
import qualified Simplex.Messaging.Crypto as C
import Simplex.Messaging.Encoding.String (textEncode)
import Simplex.Messaging.Encoding (Encoding (..))
import Simplex.Messaging.Eth.Address (Address, addressFromPrivateKey)
import Simplex.Messaging.SimplexName (SimplexTLD (..))
import qualified Simplex.Messaging.TMap as TM
import Simplex.Messaging.Util (tshow)
import StoreService.Config (ListenerConfig (..))
import StoreService.Metrics
import StoreService.Names.Chain (AbiValue (..), TxHash (..), abiArray, abiBytes, abiTuple, addressWord, selector, uint256)
import StoreService.Names.Contracts (RegisteredNames (..), newContracts, termsOf, termsText)
import StoreService.Names.Registration (RegistrationId (..))
import StoreService.Names.Store
import StoreService.Store (badgeCounts)
import StoreService.Store.Invoices (openInvoiceCounts)
import System.Timeout (timeout)
import Test.Hspec

storeMetricsTests :: Spec
storeMetricsTests = describe "StoreService: metrics" $ do
  g <- runIO C.newRandom
  it "counts open invoices by provider with those the sweep would expire, badge purchases by status, subscriptions by state and open badge codes, with the providers' last list and webhook, and 0 for a provider, status or state without rows" testServiceCounts
  it "counts name orders by phase, the oldest change among the orders the service moves on, pending transactions by nonce, open name invoices, held balances and pending record edits" testNamesCounts
  it "reads the registrar's funds, the latest block and which terms texts are held, and keeps each part while a read fails" $ testChainRead g
  it "answers GET /metrics with the service metrics alone without names, and with the names metrics while the node holds the chain read" $ testScrape g
  it "counts live names and owned subnames by level and records, with their label lengths, and keeps the last read while one fails" $ testRegisteredNames g
  it "runs on when its port is taken, and reports the failure once" testPortTaken

testPortTaken :: IO ()
testPortTaken = withServiceStore $ \st -> Warp.testWithApplication (pure $ \_ respond -> respond $ responseLBS status404 [] "") $ \port -> do
  listedOk <- TM.emptyIO
  webhookAt <- TM.emptyIO
  reports <- newTVarIO []
  let env = MetricsEnv {meStore = st, meProviders = [], meListedOk = listedOk, meWebhookAt = webhookAt, meNames = Nothing, meReport = \r -> atomically $ modifyTVar' reports (r :)}
      lc = ListenerConfig {lHost = "127.0.0.1", lBadgesPort = 0, lBadgesSite = Nothing, lTrustForwardedFor = False, lNamesSite = Nothing, lMetricsPort = Just port}
  ended <- timeout 2000000 (runMetrics lc port env)
  isJust ended `shouldBe` False
  map (T.isPrefixOf $ "store metrics: the listener on port " <> tshow port <> " failed") <$> readTVarIO reports `shouldReturn` [True]

testRegisteredNames :: TVar ChaChaDRG -> IO ()
testRegisteredNames g = withServiceStore $ \st -> withFakeEthNode $ \node -> do
  ct <- newContracts st TLDSimplex minPriceCentsPerYear =<< fakeChainClient g node
  let answer to signature args = answerCall node to (selector signature <> args) . Returns
      str t = abiTuple [Dynamic (abiBytes $ encodeUtf8 t)]
      texts n (contact, channel) =
        forM_ [("simplex.contact", contact), ("simplex.channel", channel)] $ \(key, text) ->
          answer fakeResolver "text(bytes32,string)" (abiTuple [Static n, Dynamic (abiBytes key)]) (str text)
      live = genesisTime + 1000
      names :: [(Text, Word64, Maybe (Text, Text))]
      names =
        [ ("alicebakery", live, Just ("", "")),
          ("bobsbakery", live, Just ("https://smp8.simplex.im/a#bob", "")),
          ("carolshop", live, Nothing),
          ("expiredname", genesisTime, Nothing)
        ]
      aliceNode = keccak256 (censusBase <> keccak256 "alicebakery")
      shopNode = keccak256 (aliceNode <> keccak256 "shop")
      oldNode = keccak256 (aliceNode <> keccak256 "old")
  answer fakeBaseRegistrar "ens()" "" $ addressWord censusRegistry
  answer fakeBaseRegistrar "subnameHook()" "" $ addressWord censusSubnames
  answer fakeBaseRegistrar "baseNode()" "" censusBase
  answer fakeBaseRegistrar "totalSupply()" "" $ uint256 (length names)
  forM_ (zip [0 :: Int ..] names) $ \(i, (label, expiry, records)) -> do
    let labelHash = keccak256 (encodeUtf8 label)
        n = keccak256 (censusBase <> labelHash)
    answer fakeBaseRegistrar "tokenByIndex(uint256)" (uint256 i) labelHash
    answer fakeBaseRegistrar "nameExpires(uint256)" labelHash (uint256 expiry)
    answer fakeBaseRegistrar "labelOf(uint256)" labelHash (str label)
    answer censusRegistry "resolver(bytes32)" n $ addressWord (maybe censusOtherResolver (const fakeResolver) records)
    mapM_ (texts n) records
    answer censusSubnames "childrenLength(bytes32)" n $ uint256 (if n == aliceNode then 2 else 0 :: Int)
  answer censusSubnames "getChildren(bytes32,uint256,uint256)" (aliceNode <> uint256 (0 :: Int) <> uint256 (100 :: Int)) $
    abiTuple [Dynamic (uint256 (2 :: Int) <> keccak256 "shop" <> keccak256 "old"), Dynamic (abiArray [abiBytes "shop", abiBytes "old"])]
  answer censusSubnames "ownerOf(uint256)" shopNode $ addressWord censusOwner
  answer censusSubnames "ownerOf(uint256)" oldNode $ B.replicate 32 0
  answer censusRegistry "resolver(bytes32)" shopNode $ addressWord fakeResolver
  texts shopNode ("https://smp8.simplex.im/a#shop", "https://smp8.simplex.im/c#shop")
  answer censusSubnames "childrenLength(bytes32)" shopNode $ uint256 (0 :: Int)
  v <- newTVarIO noRegisteredNamesRead
  readRegisteredNames ct v
  fmap (rgBlock . fst) . rrNames <$> readTVarIO v `shouldReturn` Just 1
  let counted =
        [ "simplex_names{level=\"2\",records=\"none\"} 1",
          "simplex_names{level=\"2\",records=\"contact\"} 1",
          "simplex_names{level=\"2\",records=\"channel\"} 0",
          "simplex_names{level=\"2\",records=\"both\"} 0",
          "simplex_names{level=\"2\",records=\"other_resolver\"} 1",
          "simplex_names{level=\"3\",records=\"none\"} 0",
          "simplex_names{level=\"3\",records=\"contact\"} 0",
          "simplex_names{level=\"3\",records=\"channel\"} 0",
          "simplex_names{level=\"3\",records=\"both\"} 1",
          "simplex_names{level=\"3\",records=\"other_resolver\"} 0"
        ]
      lengths =
        [ "simplex_name_length_bucket{level=\"2\",le=\"8.0\"} 0",
          "simplex_name_length_bucket{level=\"2\",le=\"9.0\"} 1",
          "simplex_name_length_bucket{level=\"2\",le=\"10.0\"} 2",
          "simplex_name_length_bucket{level=\"2\",le=\"11.0\"} 3",
          "simplex_name_length_bucket{level=\"2\",le=\"63.0\"} 3",
          "simplex_name_length_bucket{level=\"2\",le=\"+Inf\"} 3",
          "simplex_name_length_sum{level=\"2\"} 30",
          "simplex_name_length_count{level=\"2\"} 3",
          "simplex_name_length_bucket{level=\"3\",le=\"3.0\"} 0",
          "simplex_name_length_bucket{level=\"3\",le=\"4.0\"} 1",
          "simplex_name_length_sum{level=\"3\"} 4",
          "simplex_name_length_count{level=\"3\"} 1",
          "simplex_names_block 1",
          "simplex_names_refresh_failures_total 0"
        ]
      censusLines = T.lines . registeredNamesText <$> readTVarIO v
  filter ("simplex_names{" `T.isPrefixOf`) <$> censusLines `shouldReturn` counted
  censusLines >>= \ls -> forM_ lengths $ \l -> (l, l `elem` ls) `shouldBe` (l, True)
  setFault node "eth_getBlockByNumber" HttpFailure
  readRegisteredNames ct v
  rrFailures <$> readTVarIO v `shouldReturn` 1
  filter ("simplex_names{" `T.isPrefixOf`) <$> censusLines `shouldReturn` counted

censusRegistry, censusSubnames, censusOtherResolver, censusOwner :: Address
censusRegistry = censusAddress 0x21
censusSubnames = censusAddress 0x22
censusOtherResolver = censusAddress 0x23
censusOwner = censusAddress 0x24

censusAddress :: Word8 -> Address
censusAddress = either error id . smpDecode . B.replicate 20

censusBase :: ByteString
censusBase = B.replicate 32 0x11

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
    forM_ [(1, PSIssued), (2, PSIssued), (3, PSSuperseded)] $ \(n, status) ->
      DB.execute
        db
        "INSERT INTO sx_badge_service_badge_purchases (purchase_key, master_key, initial_badge_type, current_badge_type, status, created_at, updated_at) VALUES (?,?,?,?,?,?,?)"
        (Binary $ digestFixture n, Binary $ digestFixture (n + 100), "supporter" :: Text, "supporter" :: Text, textEncode status, t0, t0)
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
  invoices `shouldBe` [(PPCrypto, 2, 1), (PPStripe, 1, 0)]
  let times = ProviderTimes {ptListedOk = M.fromList [(PPCrypto, at 11), (PPStripe, at 12)], ptWebhookAt = M.fromList [(PPStripe, at 13)]}
  typedSamples (serviceText [PPCrypto, PPStripe] invoices badges times)
    `shouldBe` [ "simplex_store_invoices_open{provider=\"stripe\"} 1",
                 "simplex_store_invoices_open{provider=\"crypto\"} 2",
                 "simplex_store_invoices_overdue{provider=\"stripe\"} 0",
                 "simplex_store_invoices_overdue{provider=\"crypto\"} 1",
                 "simplex_store_provider_list_ok_timestamp_seconds{provider=\"stripe\"} " <> seconds (at 12),
                 "simplex_store_provider_list_ok_timestamp_seconds{provider=\"crypto\"} " <> seconds (at 11),
                 "simplex_store_provider_webhook_timestamp_seconds{provider=\"stripe\"} " <> seconds (at 13),
                 "simplex_store_badges_purchases{status=\"acquiring\"} 0",
                 "simplex_store_badges_purchases{status=\"issued\"} 2",
                 "simplex_store_badges_purchases{status=\"superseded\"} 1",
                 "simplex_store_badges_purchases{status=\"failed\"} 0",
                 "simplex_store_badges_subscriptions{state=\"renewing\"} 2",
                 "simplex_store_badges_subscriptions{state=\"cancelled\"} 0",
                 "simplex_store_badges_subscriptions{state=\"billingRetry\"} 1",
                 "simplex_store_badges_subscriptions{state=\"ended\"} 0",
                 "simplex_store_badges_codes_open 2"
               ]
  closed <- withTransaction st $ \db -> do
    DB.execute_ db "UPDATE sx_badge_service_invoices SET status = 'expired' WHERE status = 'open'"
    openInvoiceCounts db (at 10)
  closed `shouldBe` []
  filter ("simplex_store_invoices_" `T.isPrefixOf`) (typedSamples (serviceText [PPCrypto, PPStripe] closed badges times))
    `shouldBe` [ "simplex_store_invoices_open{provider=\"stripe\"} 0",
                 "simplex_store_invoices_open{provider=\"crypto\"} 0",
                 "simplex_store_invoices_overdue{provider=\"stripe\"} 0",
                 "simplex_store_invoices_overdue{provider=\"crypto\"} 0"
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
  let placedAs status rid n held = withTransaction st $ \db -> do
        insertInvoiceRow db ("inv-" <> rid) (at 60) status
        createPaidRegistration db (newReg rid ownerA $ order (rid <> "bakery") n) held (paidOrder $ "inv-" <> rid) t0 `shouldReturn` Right ()
      moved rid minute change = withTransaction st $ \db -> do
        Just r <- getRegistration db (RegistrationId rid)
        updateRegistration db (change r) (at minute) `shouldReturn` Written 2
  placedAs "open" "u" 1 Nothing
  mapM_ (\(rid, n) -> placedAs "paid" rid n Nothing) [("src", 2), ("q", 3), ("c1", 4), ("c2", 5), ("rg", 6), ("f", 7)]
  moved "src" 1 $ \r -> r {nrPhase = OTaken}
  moved "f" 1 $ \r -> r {nrPhase = OFailed (at 1) FRRegistryRefused}
  moved "c1" 2 $ \r -> r {nrPhase = OCommitted commitFixture, nrReveal = Nothing}
  moved "c2" 3 $ \r -> r {nrPhase = OCommitted commitFixture, nrReveal = Just reveal}
  moved "rg" 4 $ \r -> r {nrPhase = ORegistering commitFixture $ TxHash $ hashFixture 9}
  moved "q" 5 $ \r -> r {nrPhase = OQueued}
  withTransaction st $ \db -> do
    creditBalance db (RegistrationId "src") 6000 t0
    creditBalance db (RegistrationId "f") 2500 t0
    insertInvoiceRow db "inv-badge" (at 60) "open"
  placedAs "open" "h" 8 (Just BalanceUse {buSource = RegistrationId "src", buUsed = 6000})
  withTransaction st $ \db -> do
    forM_ [(40, 0, 6), (41, 1, 7), (42, 1, 8), (43, 2, 9)] $ \(n, nonce, minute) ->
      insertTransaction db (TxRegistration RTCommit $ RegistrationId "q") NewTransaction {ntxRaw = "raw", ntxHash = TxHash $ hashFixture n, ntxNonce = nonce} (at minute)
    recordTransactionOutcome db (TxHash $ hashFixture 40) TSPending (TSMinedAt 120) `shouldReturn` True
    Just mined <- insertRecordEdit db (newEdit ownerA 1 setChannel) (at 8)
    insertRecordEdit db (newEdit ownerB 3 setChannel) (at 9) >>= (`shouldSatisfy` isJust)
    settleRecordEdit db mined (EOApplied Nothing) `shouldReturn` True
  withTransaction st $ \db -> forM_ [("c1", 2), ("rg", 1), ("f", 0)] $ \(rid, minute) -> setPaidAt db (RegistrationId rid) (at minute)
  counts <- withTransaction st namesCounts
  typedSamples (namesText fakeMaxFeePerGas counts noChainRead)
    `shouldBe` [ "simplex_store_names_orders{phase=\"unpaid\"} 2",
                 "simplex_store_names_orders{phase=\"closed\"} 0",
                 "simplex_store_names_orders{phase=\"queued\"} 1",
                 "simplex_store_names_orders{phase=\"committing\"} 0",
                 "simplex_store_names_orders{phase=\"committed\"} 2",
                 "simplex_store_names_orders{phase=\"registering\"} 1",
                 "simplex_store_names_orders{phase=\"registered\"} 0",
                 "simplex_store_names_orders{phase=\"taken\"} 1",
                 "simplex_store_names_orders{phase=\"failed\",reason=\"sends_used_up\"} 0",
                 "simplex_store_names_orders{phase=\"failed\",reason=\"registry_refused\"} 1",
                 "simplex_store_names_orders{phase=\"failed\",reason=\"signature_refused\"} 0",
                 "simplex_store_names_orders{phase=\"failed\",reason=\"month_ended\"} 0",
                 "simplex_store_names_orders{phase=\"failed\",reason=\"unread\"} 0",
                 "simplex_store_names_queue_oldest_change_timestamp_seconds " <> seconds (at 3),
                 "simplex_store_names_unregistered_oldest_paid_timestamp_seconds{phase=\"committed\"} " <> seconds (at 2),
                 "simplex_store_names_unregistered_oldest_paid_timestamp_seconds{phase=\"registering\"} " <> seconds (at 1),
                 "simplex_store_names_unregistered_oldest_paid_timestamp_seconds{phase=\"failed\"} " <> seconds (at 0),
                 "simplex_store_names_transactions_pending 2",
                 "simplex_store_names_transactions_oldest_pending_timestamp_seconds " <> seconds (at 7),
                 "simplex_store_names_invoices_open 2",
                 "simplex_store_names_balances_held 1",
                 "simplex_store_names_balances_held_cents 6000",
                 "simplex_store_names_record_edits_pending 1",
                 "simplex_store_names_record_edits_oldest_pending_timestamp_seconds " <> seconds (at 9),
                 "simplex_store_names_max_fee_per_gas_wei 100000000000",
                 "simplex_store_names_chain_read_ok 0"
               ]

testChainRead :: TVar ChaChaDRG -> IO ()
testChainRead g = withServiceStore $ \st -> withFakeEthNode $ \node -> do
  ct <- newContracts st TLDSimplex minPriceCentsPerYear =<< fakeChainClient g node
  registrar <- addressFromPrivateKey g fakeRegistrarKey
  answerCall node fakeSponsor (selector "registrarAllowance(address)" <> addressWord registrar) $ Returns $ uint256 (123456 * 10 ^ (16 :: Int) + 5 :: Integer)
  setFault node "eth_getBalance" $ Answers $ J.String "0x15af1d78b58c40000"
  v <- newTVarIO noChainRead
  started <- getCurrentTime
  readChain ct v
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
  Right (Just general) <- termsOf ct $ fakeTermsLink fakeGeneralTermsText
  termsText ct general `shouldReturn` Right "SimpleX general terms v1"
  readChain ct v
  fetched <- chainSamples <$> readTVarIO v
  filter ("simplex_store_names_terms_held" `T.isPrefixOf`) fetched `shouldBe` held "1"
  setFault node "eth_getBalance" HttpFailure
  readChain ct v
  chainSamples <$> readTVarIO v `shouldReturn` init fetched <> ["simplex_store_names_chain_read_ok 0"]

testScrape :: TVar ChaChaDRG -> IO ()
testScrape g = withServiceStore $ \st -> withFakeEthNode $ \node -> do
  ct <- newContracts st TLDSimplex minPriceCentsPerYear =<< fakeChainClient g node
  listedOk <- TM.emptyIO
  webhookAt <- TM.emptyIO
  v <- newTVarIO noChainRead
  census <- newTVarIO noRegisteredNamesRead
  let badgesOnly = MetricsEnv {meStore = st, meProviders = [PPCrypto], meListedOk = listedOk, meWebhookAt = webhookAt, meNames = Nothing, meReport = \_ -> pure ()}
  withApp (metricsApp badgesOnly v census) $ \client -> do
    r <- webGet client "/metrics"
    (statusOf r, lookup hContentType (responseHeaders r)) `shouldBe` (200, Just "text/plain; version=0.0.4; charset=utf-8")
    let samples = typedSamples $ bodyText r
    samples `shouldContain` ["simplex_store_badges_codes_open 0"]
    samples `shouldContain` ["simplex_store_invoices_open{provider=\"crypto\"} 0"]
    filter (\l -> any (`T.isPrefixOf` l) ["simplex_store_names_", "simplex_names", "simplex_name_"]) samples `shouldBe` []
    statusOf <$> webGet client "/" `shouldReturn` 404
    statusOf <$> webRequestBody client "POST" "/metrics" [] "" `shouldReturn` 405
  (reads', _) <- pausedAt node "eth_getBalance" (runChainReads ct v)
  (`finally` cancel reads') $ withApp (metricsApp badgesOnly {meNames = Just ct} v census) $ \client -> do
    r <- timeout scrapeMicros (webGet client "/metrics") >>= maybe (fail "the scrape waited for the node") pure
    let samples = typedSamples $ bodyText r
    forM_ ["simplex_store_badges_codes_open 0", "simplex_store_names_orders{phase=\"unpaid\"} 0", "simplex_names_refresh_failures_total 0"] $ \s ->
      samples `shouldContain` [s]
    filter ("simplex_store_names_chain_" `T.isPrefixOf`) samples `shouldBe` ["simplex_store_names_chain_read_ok 0"]
    filter ("simplex_names{" `T.isPrefixOf`) samples `shouldBe` []

scrapeMicros :: Int
scrapeMicros = 2000000

bodyText :: Response LB.ByteString -> Text
bodyText = decodeUtf8 . LB.toStrict . responseBody

typedSamples :: Text -> [Text]
typedSamples text = [l | l <- ls, not (T.null l || "#" `T.isPrefixOf` l), described (T.takeWhile (`notElem` [' ', '{']) l)]
  where
    ls = T.lines text
    described name = any (("# TYPE " <> name <> " ") `T.isPrefixOf`) ls && any (("# HELP " <> name <> " ") `T.isPrefixOf`) ls

chainSamples :: ChainRead -> [Text]
chainSamples = filter (\l -> any (`T.isPrefixOf` l) ["simplex_store_names_registrar_", "simplex_store_names_sponsor_", "simplex_store_names_chain_", "simplex_store_names_terms_"]) . typedSamples . namesText fakeMaxFeePerGas noCounts
  where
    noCounts = NamesCounts {ncPhases = [], ncQueueOldestChange = Nothing, ncPendingTransactions = 0, ncOldestPending = Nothing, ncOpenInvoices = 0, ncHeldBalances = 0, ncHeldCents = 0, ncPendingEdits = 0, ncOldestEdit = Nothing, ncOldestUnregisteredPaid = []}

seconds :: UTCTime -> Text
seconds t = tshow (floor $ utcTimeToPOSIXSeconds t :: Integer)

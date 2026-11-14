{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module StoreService.Metrics
  ( MetricsEnv (..),
    NamesSource (..),
    ChainRead (..),
    ProviderTimes (..),
    CensusRead (..),
    noChainRead,
    noCensusRead,
    readChain,
    runChainReads,
    readCensus,
    serviceText,
    namesText,
    censusText,
    metricsApp,
    runMetrics,
  )
where

import Control.Concurrent (threadDelay)
import Control.Concurrent.STM
import Control.Logger.Simple (logWarn)
import Control.Monad (forever, unless, void)
import qualified Data.ByteString.Lazy as LB
import Data.Either (isRight)
import Data.List (nub, sort)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Time.Clock (UTCTime, addUTCTime, getCurrentTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Network.HTTP.Types (hContentType, status200)
import Network.Wai (Application, pathInfo, responseLBS)
import qualified Network.Wai.Handler.Warp as Warp
import Simplex.Chat.Badges.Code (maxNameLength)
import Simplex.Chat.Badges.Types (BadgePurchaseStatus (..), SubscriptionState (..))
import Simplex.Messaging.Agent.Store.Common (DBStore, withTransaction)
import Simplex.Messaging.Encoding.String (textEncode)
import Simplex.Messaging.Util (raceAny_, tshow)
import StoreService.Config (ListenerConfig)
import StoreService.Names.Census (Census (..), CensusName (..), nameCensus, recordsLabel)
import StoreService.Names.Chain (Block (..), ChainClient, latestBlock)
import StoreService.Names.Contracts (Contracts, RegistrarFunds (..), registrarFunds, termsHeld)
import StoreService.Names.Store (NamesCounts (..), namesCounts)
import StoreService.Poller (expiryGrace)
import StoreService.Safely (safely)
import StoreService.Store (BadgeCounts (..), badgeCounts)
import StoreService.Store.Invoices (openInvoiceCounts)
import StoreService.Web.Http (notFound, only)
import StoreService.Web.Server (webSettings)

data MetricsEnv = MetricsEnv
  { meStore :: DBStore,
    meProviders :: [Text],
    meListedOk :: TVar (Map Text UTCTime),
    meWebhookAt :: TVar (Map Text UTCTime),
    -- | @Nothing@ when the ini has no @[names]@ section.
    meNames :: Maybe NamesSource
  }

data NamesSource = NamesSource {nsChain :: ChainClient, nsContracts :: Contracts}

-- | Each part keeps its last value while its read fails.
data ChainRead = ChainRead
  { crFunds :: Maybe RegistrarFunds,
    crHead :: Maybe Block,
    crTerms :: [(Text, Bool)],
    -- | the last read in which every part succeeded
    crReadAt :: Maybe UTCTime,
    crOk :: Bool
  }

noChainRead :: ChainRead
noChainRead = ChainRead {crFunds = Nothing, crHead = Nothing, crTerms = [], crReadAt = Nothing, crOk = False}

-- | By each provider's text encoding.
data ProviderTimes = ProviderTimes {ptListedOk :: Map Text UTCTime, ptWebhookAt :: Map Text UTCTime}

chainReadInterval :: Int
chainReadInterval = 60 * 1000000

metricsHost :: String
metricsHost = "127.0.0.1"

readChain :: NamesSource -> TVar ChainRead -> IO ()
readChain NamesSource {nsChain, nsContracts} v = do
  funds <- registrarFunds nsContracts
  head' <- latestBlock nsChain
  terms <- termsHeld nsContracts
  now <- getCurrentTime
  let ok = isRight funds && isRight head' && isRight terms
  atomically $ modifyTVar' v $ \ChainRead {crFunds, crHead, crTerms, crReadAt} ->
    ChainRead
      { crFunds = either (const crFunds) Just funds,
        crHead = either (const crHead) Just head',
        crTerms = either (const crTerms) id terms,
        crReadAt = if ok then Just now else crReadAt,
        crOk = ok
      }
  warnIfFailed "the registrar's funds" funds
  warnIfFailed "the latest block" head'
  warnIfFailed "the terms links" terms
  where
    warnIfFailed :: Show e => Text -> Either e a -> IO ()
    warnIfFailed what = either (\e -> logWarn $ "store metrics: " <> what <> " could not be read: " <> tshow e) (const $ pure ())

runChainReads :: NamesSource -> TVar ChainRead -> IO ()
runChainReads ns v = forever $ do
  done <- safely "store metrics: the chain read" (readChain ns v)
  unless done $ atomically $ modifyTVar' v $ \cr -> cr {crOk = False}
  threadDelay chainReadInterval

-- | A failed census keeps the last one and is counted.
data CensusRead = CensusRead {crCensus :: Maybe (Census, UTCTime), crFailures :: Int}

noCensusRead :: CensusRead
noCensusRead = CensusRead {crCensus = Nothing, crFailures = 0}

censusInterval :: Int
censusInterval = 300 * 1000000

readCensus :: NamesSource -> TVar CensusRead -> IO ()
readCensus NamesSource {nsChain, nsContracts} v = do
  counted <- nameCensus nsChain nsContracts
  case counted of
    Right census -> do
      now <- getCurrentTime
      atomically $ modifyTVar' v $ \cr -> cr {crCensus = Just (census, now)}
    Left e -> do
      atomically $ modifyTVar' v $ \cr -> cr {crFailures = crFailures cr + 1}
      logWarn $ "store metrics: the names census failed: " <> tshow e

runCensus :: NamesSource -> TVar CensusRead -> IO ()
runCensus ns v = forever $ do
  done <- safely "store metrics: the names census" (readCensus ns v)
  unless done $ atomically $ modifyTVar' v $ \cr -> cr {crFailures = crFailures cr + 1}
  threadDelay censusInterval

-- | Open invoices come as (provider, open, overdue).
serviceText :: [Text] -> [(Text, Int, Int)] -> BadgeCounts -> ProviderTimes -> Text
serviceText providers invoices BadgeCounts {bcPurchases, bcSubscriptions, bcOpenCodes} ProviderTimes {ptListedOk, ptWebhookAt} =
  T.concat
    [ gauge "simplex_store_invoices_open" "Open invoices by provider" [labelled "provider" p (value n) | (p, (n, _)) <- invoiceCounts],
      gauge "simplex_store_invoices_overdue" "Open invoices past their expiry and its grace with no money received, which the poller expires once the provider answers" [labelled "provider" p (value n) | (p, (_, n)) <- invoiceCounts],
      gauge "simplex_store_provider_list_ok_timestamp_seconds" "When the poller last listed the provider's open invoices successfully" [labelled "provider" p (timestamp t) | (p, t) <- M.toList ptListedOk],
      gauge "simplex_store_provider_webhook_timestamp_seconds" "When the provider last delivered a webhook that verified" [labelled "provider" p (timestamp t) | (p, t) <- M.toList ptWebhookAt],
      gauge "simplex_store_badges_purchases" "Badge purchases by status" [labelled "status" s (value n) | (s, n) <- withZeros purchaseStatuses bcPurchases],
      gauge "simplex_store_badges_subscriptions" "App store subscriptions paying for badges, by state" [labelled "state" s (value n) | (s, n) <- withZeros subscriptionStates bcSubscriptions],
      gauge "simplex_store_badges_codes_open" "Badge codes paid or free, not revoked or expired, with redemptions left" (Just $ value bcOpenCodes)
    ]
  where
    invoiceCounts = M.toList $ M.union (M.fromList [(p, (n, overdue)) | (p, n, overdue) <- invoices]) (M.fromList [(p, (0, 0)) | p <- providers])

withZeros :: [Text] -> [(Text, Int)] -> [(Text, Int)]
withZeros known counts = M.toList $ M.union (M.fromList counts) (M.fromList [(k, 0) | k <- known])

purchaseStatuses :: [Text]
purchaseStatuses = map textEncode [PSAcquiring, PSIssued, PSSuperseded, PSFailed]

subscriptionStates :: [Text]
subscriptionStates = map textEncode [SSRenewing, SSCancelled, SSBillingRetry, SSEnded]

namesText :: NamesCounts -> ChainRead -> Text
namesText NamesCounts {ncPhases, ncQueueOldestChange, ncPendingTransactions, ncOldestPending, ncOpenInvoices, ncHeldBalances, ncHeldCents, ncPendingEdits, ncOldestEdit} ChainRead {crFunds, crHead, crTerms, crReadAt, crOk} =
  T.concat
    [ gauge "simplex_store_names_orders" "Name orders by phase" [labelled "phase" (textEncode p) (value $ fromMaybe 0 $ lookup p ncPhases) | p <- [minBound .. maxBound]],
      gauge "simplex_store_names_queue_oldest_change_timestamp_seconds" "Last change of the order longest unmoved among those the service moves on: queued, committing, registering, or committed with its reveal" (timestamp <$> ncQueueOldestChange),
      gauge "simplex_store_names_transactions_pending" "Registrar transactions sent and not settled, one per nonce" (Just $ value ncPendingTransactions),
      gauge "simplex_store_names_transactions_oldest_pending_timestamp_seconds" "When the oldest pending registrar transaction was first signed" (timestamp <$> ncOldestPending),
      gauge "simplex_store_names_invoices_open" "Open invoices of name orders" (Just $ value ncOpenInvoices),
      gauge "simplex_store_names_balances_held" "Balances reserved by an order paying the difference, until that order is paid or closed" (Just $ value ncHeldBalances),
      gauge "simplex_store_names_balances_held_cents" "Cents of the balances orders hold" (Just $ value ncHeldCents),
      gauge "simplex_store_names_record_edits_pending" "Record edits relayed and not settled" (Just $ value ncPendingEdits),
      gauge "simplex_store_names_record_edits_oldest_pending_timestamp_seconds" "When the oldest pending record edit was made" (timestamp <$> ncOldestEdit),
      gauge "simplex_store_names_registrar_balance_wei" "The registrar key's ETH balance, as last read" (value . rfBalanceWei <$> crFunds),
      gauge "simplex_store_names_sponsor_allowance_cents" "The registrar key's allowance left on the sponsor, as last read" (value . rfAllowanceCents <$> crFunds),
      gauge "simplex_store_names_chain_head_block" "The node's latest block number, as last read" (value . blockNumber <$> crHead),
      gauge "simplex_store_names_chain_head_timestamp_seconds" "The node's latest block time, as last read" (value . blockTimestamp <$> crHead),
      gauge "simplex_store_names_chain_base_fee_wei" "The node's latest block base fee per gas, as last read" (value . blockBaseFee <$> crHead),
      gauge "simplex_store_names_terms_held" "1 when the service holds the text of the terms the name policy links, by kind, as last read" [labelled "kind" k (value $ fromEnum held) | (k, held) <- crTerms],
      gauge "simplex_store_names_chain_read_timestamp_seconds" "When every part of the chain read last succeeded" (timestamp <$> crReadAt),
      gauge "simplex_store_names_chain_read_ok" "1 when the last chain read succeeded in every part, 0 when a part failed or none was made" (Just $ value $ fromEnum crOk)
    ]

value :: Show a => a -> (Text, Text)
value n = ("", tshow n)

timestamp :: UTCTime -> (Text, Text)
timestamp t = value (floor $ utcTimeToPOSIXSeconds t :: Integer)

labelled :: Text -> Text -> (Text, Text) -> (Text, Text)
labelled name v = withLabels [(name, v)]

withLabels :: [(Text, Text)] -> (Text, Text) -> (Text, Text)
withLabels ls (_, sample) = (labelSet ls, sample)

labelSet :: [(Text, Text)] -> Text
labelSet ls = "{" <> T.intercalate "," [name <> "=\"" <> escape v <> "\"" | (name, v) <- ls] <> "}"
  where
    escape = T.replace "\n" "\\n" . T.replace "\"" "\\\"" . T.replace "\\" "\\\\"

gauge :: Foldable f => Text -> Text -> f (Text, Text) -> Text
gauge = family "gauge"

family :: Foldable f => Text -> Text -> Text -> f (Text, Text) -> Text
family kind name help samples
  | null samples = ""
  | otherwise = "# HELP " <> name <> " " <> help <> "\n# TYPE " <> name <> " " <> kind <> "\n" <> foldMap sample samples <> "\n"
  where
    sample (labels, v) = name <> labels <> " " <> v <> "\n"

-- | The census of #8114's exporter, under its metric names: names and subnames by level and records set, and their
-- label lengths. Before the first census only the failures are answered.
censusText :: CensusRead -> Text
censusText CensusRead {crCensus, crFailures} =
  family "counter" "simplex_names_refresh_failures_total" "Refreshes that failed" (Just $ value crFailures)
    <> foldMap counted crCensus
  where
    counted (Census {cBlock, cNames}, refreshedAt) =
      T.concat
        [ gauge "simplex_names" "Registered names by level, and by simplex.contact and simplex.channel set" [withLabels [("level", tshow l), ("records", recordsLabel r)] (value $ count l r) | l <- levelsSeen, r <- [minBound .. maxBound]],
          lengthHistogram,
          gauge "simplex_names_block" "Block the names were read at" (Just $ value cBlock),
          gauge "simplex_names_refresh_timestamp_seconds" "Time of the last refresh" (Just $ timestamp refreshedAt)
        ]
      where
        levelsSeen = sort $ nub $ 2 : map cnLevel cNames
        count l r = length [() | CensusName {cnLevel, cnRecords} <- cNames, cnLevel == l, cnRecords == r]
        lengthHistogram =
          "# HELP simplex_name_length Label lengths in characters, by level\n# TYPE simplex_name_length histogram\n"
            <> foldMap lengthsAt levelsSeen
            <> "\n"
        lengthsAt l =
          let lengths = [cnLength | CensusName {cnLevel, cnLength} <- cNames, cnLevel == l]
              level = [("level", tshow l)]
              line suffix ls n = "simplex_name_length" <> suffix <> labelSet ls <> " " <> tshow n <> "\n"
           in foldMap (\b -> line "_bucket" (level <> [("le", tshow b <> ".0")]) (length $ filter (<= b) lengths)) [1 .. maxNameLength]
                <> line "_bucket" (level <> [("le", "+Inf")]) (length lengths)
                <> line "_sum" level (sum lengths)
                <> line "_count" level (length lengths)

metricsApp :: MetricsEnv -> TVar ChainRead -> TVar CensusRead -> Application
metricsApp MetricsEnv {meStore, meProviders, meListedOk, meWebhookAt, meNames} v census req respond = case pathInfo req of
  ["metrics"] -> only "GET" req respond $ do
    now <- getCurrentTime
    (invoices, badges, names) <- withTransaction meStore $ \db ->
      (,,) <$> openInvoiceCounts db (addUTCTime (negate expiryGrace) now) <*> badgeCounts db now <*> mapM (const $ namesCounts db) meNames
    times <- ProviderTimes <$> readTVarIO meListedOk <*> readTVarIO meWebhookAt
    chain <- readTVarIO v
    counted <- readTVarIO census
    let text = serviceText meProviders invoices badges times <> foldMap (\n -> namesText n chain <> censusText counted) names
    respond $ responseLBS status200 [(hContentType, "text/plain; version=0.0.4; charset=utf-8")] $ LB.fromStrict $ encodeUtf8 text
  _ -> respond notFound

runMetrics :: ListenerConfig -> Int -> MetricsEnv -> IO ()
runMetrics lc port env@MetricsEnv {meNames} = do
  v <- newTVarIO noChainRead
  census <- newTVarIO noCensusRead
  let serve = safely ("store metrics: the endpoint on port " <> tshow port) $ Warp.runSettings (Warp.setPort port . Warp.setHost (fromString metricsHost) $ webSettings lc) (metricsApp env v census)
  raceAny_ $ (void serve >> forever (threadDelay maxBound)) : concat [[runChainReads ns v, runCensus ns census] | Just ns <- [meNames]]

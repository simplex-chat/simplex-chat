{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module StoreService.Names.Metrics
  ( FundsRead (..),
    noFundsRead,
    readFunds,
    runFundsReads,
    metricsText,
    metricsApp,
    runMetrics,
  )
where

import Control.Concurrent (threadDelay)
import Control.Concurrent.STM
import Control.Logger.Simple (logWarn)
import Control.Monad (forever, unless, void)
import qualified Data.ByteString.Lazy as LB
import Data.Maybe (fromMaybe)
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Network.HTTP.Types (hContentType, status200)
import Network.Wai (Application, pathInfo, responseLBS)
import qualified Network.Wai.Handler.Warp as Warp
import Simplex.Messaging.Agent.Store.Common (DBStore, withTransaction)
import Simplex.Messaging.Encoding.String (textEncode)
import Simplex.Messaging.Util (raceAny_, tshow)
import StoreService.Config (ListenerConfig)
import StoreService.Names.Contracts (Contracts, RegistrarFunds (..), registrarFunds)
import StoreService.Names.Store (NamesCounts (..), namesCounts)
import StoreService.Safely (safely)
import StoreService.Web.Http (notFound, only)
import StoreService.Web.Server (webSettings)

data FundsRead = FundsRead
  { frFunds :: Maybe (RegistrarFunds, UTCTime),
    frOk :: Bool
  }

noFundsRead :: FundsRead
noFundsRead = FundsRead {frFunds = Nothing, frOk = False}

fundsReadInterval :: Int
fundsReadInterval = 60 * 1000000

metricsHost :: String
metricsHost = "127.0.0.1"

readFunds :: Contracts -> TVar FundsRead -> IO ()
readFunds ct v =
  registrarFunds ct >>= \case
    Right funds -> do
      now <- getCurrentTime
      atomically $ writeTVar v FundsRead {frFunds = Just (funds, now), frOk = True}
    Left e -> do
      atomically $ modifyTVar' v $ \fr -> fr {frOk = False}
      logWarn $ "names metrics: the registrar's funds could not be read: " <> tshow e

runFundsReads :: Contracts -> TVar FundsRead -> IO ()
runFundsReads ct v = forever $ do
  done <- safely "names metrics: the funds read" (readFunds ct v)
  unless done $ atomically $ modifyTVar' v $ \fr -> fr {frOk = False}
  threadDelay fundsReadInterval

metricsText :: NamesCounts -> FundsRead -> Text
metricsText NamesCounts {ncPhases, ncQueueOldestChange, ncPendingTransactions, ncOldestPending, ncOpenInvoices, ncHeldBalances, ncHeldCents} FundsRead {frFunds, frOk} =
  T.concat
    [ gauge "simplex_store_names_orders" "Name orders by phase" [("{phase=\"" <> textEncode p <> "\"}", tshow $ fromMaybe 0 $ lookup p ncPhases) | p <- [minBound .. maxBound]],
      gauge "simplex_store_names_queue_oldest_change_timestamp_seconds" "Last change of the order longest unmoved among those the service moves on: queued, committing, registering, or committed with its reveal" (timestamp <$> ncQueueOldestChange),
      gauge "simplex_store_names_transactions_pending" "Registrar transactions sent and not settled, one per nonce" (Just $ value ncPendingTransactions),
      gauge "simplex_store_names_transactions_oldest_pending_timestamp_seconds" "When the oldest pending registrar transaction was first signed" (timestamp <$> ncOldestPending),
      gauge "simplex_store_names_invoices_open" "Open invoices of name orders" (Just $ value ncOpenInvoices),
      gauge "simplex_store_names_balances_held" "Balances an order holds while its invoice for the difference is open" (Just $ value ncHeldBalances),
      gauge "simplex_store_names_balances_held_cents" "Cents of the balances orders hold" (Just $ value ncHeldCents),
      gauge "simplex_store_names_registrar_balance_wei" "The registrar key's ETH balance, as last read" (value . rfBalanceWei . fst <$> frFunds),
      gauge "simplex_store_names_sponsor_allowance_cents" "The registrar key's allowance left on the sponsor, as last read" (value . rfAllowanceCents . fst <$> frFunds),
      gauge "simplex_store_names_funds_read_timestamp_seconds" "When the registrar's funds were last read" (timestamp . snd <$> frFunds),
      gauge "simplex_store_names_funds_read_ok" "1 when the last read of the registrar's funds succeeded, 0 when it failed or none was made" (Just $ value $ fromEnum frOk)
    ]
  where
    value :: Show a => a -> (Text, Text)
    value n = ("", tshow n)
    timestamp :: UTCTime -> (Text, Text)
    timestamp t = value (floor $ utcTimeToPOSIXSeconds t :: Integer)

gauge :: Foldable f => Text -> Text -> f (Text, Text) -> Text
gauge name help samples
  | null samples = ""
  | otherwise = "# HELP " <> name <> " " <> help <> "\n# TYPE " <> name <> " gauge\n" <> foldMap sample samples <> "\n"
  where
    sample (labels, v) = name <> labels <> " " <> v <> "\n"

metricsApp :: DBStore -> TVar FundsRead -> Application
metricsApp st v req respond = case pathInfo req of
  ["metrics"] -> only "GET" req respond $ do
    text <- metricsText <$> withTransaction st namesCounts <*> readTVarIO v
    respond $ responseLBS status200 [(hContentType, "text/plain; version=0.0.4; charset=utf-8")] $ LB.fromStrict $ encodeUtf8 text
  _ -> respond notFound

runMetrics :: ListenerConfig -> Int -> DBStore -> Contracts -> IO ()
runMetrics lc port st ct = do
  v <- newTVarIO noFundsRead
  let serve = safely ("names metrics: the endpoint on port " <> tshow port) $ Warp.runSettings (Warp.setPort port . Warp.setHost (fromString metricsHost) $ webSettings lc) (metricsApp st v)
  raceAny_ [runFundsReads ct v, void serve >> forever (threadDelay maxBound)]

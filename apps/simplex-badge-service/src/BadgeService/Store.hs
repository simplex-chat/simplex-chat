{-# LANGUAGE CPP #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeOperators #-}

-- | Queries over the badge service's own tables: web orders, invoices, provider events,
-- purchases and payments, the ledger, issuances, redemption codes and the price catalog.
-- Structured after "Directory.Store"\/"Directory.Store.Migrate": every function here takes a
-- 'DB.Connection' and opens no transaction of its own; 'withServiceTransaction' is the only
-- place a transaction is opened, which is what lets a caller compose a purchase row, a payment
-- row, several ledger entries, an issuance and a code redemption into one atomic transaction --
-- or, on the web side, an order, its invoice, a provider event and a code row.
--
-- The RPC path (B1) and the web-checkout path (D0) share this module and share nothing else:
-- there is no store function that resolves an order to a code or a purchase -- that join does
-- not exist in the schema (docs\/protocol\/badges-web.md §3 Linkage); a caller that needs it
-- derives the code from the order id and looks it up by hash with 'getCodeByHash'.
module BadgeService.Store
  ( ServiceError (..),
    withServiceTransaction,

    -- * Web orders and their invoices
    OrderMethod (..),
    WebOrderStatus (..),
    NewWebOrder (..),
    WebOrder (..),
    createOrder,
    getOrder,
    getOrderByProviderRef,
    getOrderByShortRef,
    getStuckOrders,
    updateOrderStatus,
    setOrderProviderRef,
    setOrderSettled,

    -- * Provider events
    recordProviderEvent,
    markProviderEventProcessed,

    -- * Purchases and payments
    BadgePurchaseRow (..),
    getPurchaseByKey,
    createPurchase,
    createCodePayment,
    attachPurchasePayment,

    -- * Ledger
    getLastLedgerEntry,
    appendLedgerEntry,
    getLedgerSince,
    getLedgerEntryIdByUuid,

    -- * Issuances
    NewIssuance (..),
    getIssuanceForPeriod,
    getIssuanceForRedeemedCode,
    createIssuance,

    -- * Codes
    BadgeCode (..),
    NewBadgeCode (..),
    getCodeByHash,
    markCodeRedeemed,
    unredeemCode,
    insertCodes,
    revokeCode,
    revokeBatch,

    -- * Catalog
    getActiveCatalog,
    getPriceById,
    getOfferById,
    setPriceStatus,
    setOfferStatus,
  )
where

import qualified Control.Exception as E
import Control.Monad (when)
import Control.Monad.Except (ExceptT, liftEither, runExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as J
import qualified Data.ByteString.Lazy as BL
import Data.ByteString (ByteString)
import Data.Int (Int64)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (UTCTime)
import Data.Word (Word32, Word8)
import Simplex.Chat.Badges (BadgeCredential (..), BadgeMasterKey (..), BadgeType (..))
import Simplex.Chat.Badges.Service (BadgeCatalog (..), BadgeOffer (..), BadgePrice (..))
import Simplex.Chat.Badges.Types
  ( BadgeIssuance (..),
    BadgeItemStatus (..),
    BadgeLedgerEntry (..),
    BadgeOfferId (..),
    BadgePriceId (..),
    BadgePurchaseStatus (..),
    LedgerCreditType (..),
    LedgerDebitType (..),
    LedgerEntryType (..),
    OfferDiscount (..),
  )
import Simplex.Chat.PaymentService.Types (CurrencyAmount (..), InvoiceStatus (..), PaymentProvider (..), PaymentStatus (..))
import Simplex.Messaging.Agent.Store.Common (DBStore, withTransaction)
import Simplex.Messaging.Agent.Store.DB (Binary (..), fromTextField_)
import qualified Simplex.Messaging.Agent.Store.DB as DB
import qualified Simplex.Messaging.Crypto as C
import Simplex.Messaging.Encoding.String (TextEncoding (..))
import Simplex.Messaging.Util (tshow)
#if defined(dbPostgres)
import Database.PostgreSQL.Simple (Only (..), Query, ToRow, (:.) (..))
import Database.PostgreSQL.Simple.FromField (FromField (..))
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Simple.ToField (ToField (..))
#else
import Database.SQLite.Simple (Only (..), Query, ToRow, (:.) (..))
import Database.SQLite.Simple.FromField (FromField (..))
import Database.SQLite.Simple.QQ (sql)
import Database.SQLite.Simple.ToField (ToField (..))
#endif

-- | The error type of every store function: not-found (a lookup targeted by a mutation
-- doesn't exist), conflict (a write would clobber state a caller didn't expect to overwrite)
-- and decode failures (a row can't be reconstructed into its Haskell shape).
data ServiceError
  = SEPurchaseNotFound
  | SECodeNotFound
  | SEPriceNotFound
  | SEOfferNotFound
  | SEOrderNotFound
  | -- | 'markProviderEventProcessed' only: no @provider_events@ row for that
    -- @(provider, event_id)@. Its caller reaches it through a 'recordProviderEvent' that
    -- returned 'True' in the same transaction, so the row is always there; a 'Left' here means
    -- the two calls disagreed about the key, which must not pass silently.
    SEProviderEventNotFound
  | -- | 'attachPurchasePayment' only: the purchase already has a payment attached.
    SEPaymentConflict
  | -- | 'markCodeRedeemed' only: the code was already claimed by another redemption, so this one
    -- must not overwrite it. The caller re-classifies from the code row.
    SECodeConflict
  | SEDecodeError Text
  deriving (Eq, Show)

-- | Thrown internally to force a real rollback out of 'withTransaction': postgresql-simple\/
-- sqlite-simple commit on a normal return and only roll back on an exception, so a plain
-- @'Left' e@ returned from the wrapped action would otherwise be committed along with
-- whatever it already wrote. Never leaves this module.
newtype ServiceRollback = ServiceRollback ServiceError

instance Show ServiceRollback where
  show (ServiceRollback e) = "ServiceRollback " <> show e

instance E.Exception ServiceRollback

-- | The only place a transaction is opened. Every store function above takes a
-- 'DB.Connection' and opens none of its own, so a command handler can sequence several of
-- them here and get one transaction: a 'Left' rolls back everything the action already wrote.
withServiceTransaction :: DBStore -> (DB.Connection -> ExceptT ServiceError IO a) -> IO (Either ServiceError a)
withServiceTransaction st action =
  (Right <$> withTransaction st runInTransaction) `E.catch` \(ServiceRollback e) -> pure (Left e)
  where
    runInTransaction db =
      runExceptT (action db) >>= \case
        Left e -> E.throwIO (ServiceRollback e)
        Right a -> pure a

-- Web orders and their invoices ----------------------------------------------

-- | @web_orders.method@ (A3: @CHECK (method IN ('card','btc','xmr'))@). It is the single source
-- for everything the payment method decides: the invoice's provider below, and E4's
-- @cryptoCurrency@, which is derived from it rather than read from
-- @invoices.payment_crypto_currency@ -- that column exists and is deliberately left NULL (A3).
--
-- It lives here rather than in D6's @Orders.hs@, which the plan first named as its home, only
-- because D0 has to persist a method before that module exists; D6 imports it (plan §9).
data OrderMethod = OMCard | OMBtc | OMXmr
  deriving (Eq, Show)

instance TextEncoding OrderMethod where
  textEncode = \case
    OMCard -> "card"
    OMBtc -> "btc"
    OMXmr -> "xmr"
  textDecode = \case
    "card" -> Just OMCard
    "btc" -> Just OMBtc
    "xmr" -> Just OMXmr
    _ -> Nothing

instance ToField OrderMethod where toField = toField . textEncode

instance FromField OrderMethod where fromField = fromTextField_ textDecode

-- | @web_orders.status@ (A3: @CHECK (status IN ('invoiced','pending','paid','expired','failed'))@),
-- authoritative for the order lifecycle and what E3, E4 and H3 read. @invoices.status@ is
-- maintained in step with it, in the same transaction, by 'updateOrderStatus' and
-- 'setOrderSettled', and is read by nothing in this plan.
--
-- Distinct from 'InvoiceStatus', the shared @invoices@ vocabulary: an order tracks five states
-- because E3 and E5 have to tell an underpaid expiry from a provider failure, where an invoice
-- has only the three the payment model defines. 'orderInvoiceStatus' is the one place the two
-- meet, and it projects the five onto the three.
data WebOrderStatus = WOSInvoiced | WOSPending | WOSPaid | WOSExpired | WOSFailed
  deriving (Eq, Show)

instance TextEncoding WebOrderStatus where
  textEncode = \case
    WOSInvoiced -> "invoiced"
    WOSPending -> "pending"
    WOSPaid -> "paid"
    WOSExpired -> "expired"
    WOSFailed -> "failed"
  textDecode = \case
    "invoiced" -> Just WOSInvoiced
    "pending" -> Just WOSPending
    "paid" -> Just WOSPaid
    "expired" -> Just WOSExpired
    "failed" -> Just WOSFailed
    _ -> Nothing

instance ToField WebOrderStatus where toField = toField . textEncode

instance FromField WebOrderStatus where fromField = fromTextField_ textDecode

-- | The @invoices.status@ that goes with each order status, so the two columns cannot drift
-- (A3's invariant). A projection, not a bijection: an invoice is 'ISOpen' while the order is
-- either invoiced or partly paid, and 'ISExpired' whether the order ran out of time or the
-- provider failed it. Nothing reads it back (A3), and @web_orders.status@ keeps the distinctions
-- this drops.
orderInvoiceStatus :: WebOrderStatus -> InvoiceStatus
orderInvoiceStatus = \case
  WOSInvoiced -> ISOpen
  WOSPending -> ISOpen
  WOSPaid -> ISPaid
  WOSExpired -> ISExpired
  WOSFailed -> ISExpired

-- | The @invoices.provider@ of an order, derived from its method rather than passed in: the
-- card methods go to Stripe (F1) and both crypto methods to the same BTCPay instance (E2), so
-- a caller could only ever get this wrong.
orderInvoiceProvider :: OrderMethod -> PaymentProvider
orderInvoiceProvider = \case
  OMCard -> PPStripe
  OMBtc -> PPCrypto
  OMXmr -> PPCrypto

-- | Both rows 'createOrder' writes, as one record: D6 fills a named structure rather than a
-- fifteen-argument call, and the field names read as the columns do. The order's own columns
-- come first, then the @invoices@ ones.
--
-- @status@ is not a field: a new order is always @invoiced@ and its invoice always @invoiced@
-- with it, so there is no representable state where a row is created already paid or expired.
-- Nor are @amount_paid@ and @settled_at@, which only 'updateOrderStatus' and 'setOrderSettled'
-- ever write.
data NewWebOrder = NewWebOrder
  { -- | 128 random bits, base64url (D6). A bearer capability for the code (§3 Linkage,
    -- decision 9), so it must not be sequential or derived from anything guessable.
    orderId :: Text,
    -- | The @invoices@ row this call writes; minted by D6, not here, because the store opens no
    -- transaction and mints no identifiers.
    invoiceId :: Text,
    -- | The provider's invoice \/ session \/ payment-intent id. 'Nothing' only for a caller that
    -- has not made the provider call yet; D6 always has it by the time it writes, and
    -- 'setOrderProviderRef' can replace it later.
    providerRef :: Maybe Text,
    method :: OrderMethod,
    -- | 5 Crockford characters (D6), unique per order: the reference support resolves by (H2),
    -- shown on card statements (F1) and on the crypto payment and result screens (E5, E6).
    shortRef :: Text,
    badgeType :: BadgeType,
    priceId :: Maybe BadgePriceId,
    offerId :: Maybe BadgeOfferId,
    months :: Word8,
    -- | The charged total from A4's 'BadgeService.Catalog.offerTotal', in minor units of
    -- @currency@. Written to @invoices.price@ and @invoices.amount@ alike, with
    -- @discount_amount@ and @credit_amount@ left NULL: an offer's discount is expressed as free
    -- months, so the total IS the price, and a web order has no credit to apply.
    amount :: CurrencyAmount,
    currency :: Text,
    -- | Card only: the provider's hosted checkout URL (F1).
    payUrl :: Maybe Text,
    -- | Crypto only: the address and the amount in the crypto currency, both from E2's
    -- @getPaymentMethods@, so E4 serves them from the database and never re-reads the provider.
    paymentAddress :: Maybe Text,
    cryptoAmount :: Maybe Text,
    expiresAt :: UTCTime
  }

-- | An order joined to its @invoices@ row, which is how every read here returns one: a single
-- call yields the amount, the currency, the address, the crypto amount, the payment URL and the
-- expiry that E4's response needs. E4's @code@ and @disclosureExpiresAt@ do NOT come from here
-- -- they come from 'getCodeByHash' on the code derived from 'orderId' (§3 Linkage).
data WebOrder = WebOrder
  { orderId :: Text,
    invoiceId :: Text,
    providerRef :: Maybe Text,
    method :: OrderMethod,
    shortRef :: Text,
    badgeType :: BadgeType,
    priceId :: Maybe BadgePriceId,
    offerId :: Maybe BadgeOfferId,
    months :: Word8,
    status :: WebOrderStatus,
    -- | Amount received so far, in minor units of the invoice currency at the rate the provider
    -- locked -- never in crypto. A partial payment records it while the order stays @pending@.
    amountPaid :: Maybe CurrencyAmount,
    settledAt :: Maybe UTCTime,
    createdAt :: UTCTime,
    updatedAt :: UTCTime,
    -- from the @invoices@ row
    amount :: CurrencyAmount,
    currency :: Text,
    payUrl :: Maybe Text,
    paymentAddress :: Maybe Text,
    cryptoAmount :: Maybe Text,
    expiresAt :: UTCTime
  }
  deriving (Show)

type OrderRow = (Text, Text, Maybe Text, OrderMethod, Text, BadgeType, Maybe Text, Maybe Text, Int, WebOrderStatus)

type OrderInvoiceRow = (Maybe Int64, Maybe UTCTime, UTCTime, UTCTime, Int64, Text, Maybe Text, Maybe Text, Maybe Text, UTCTime)

-- | @months@ and both amounts are read as signed integers and converted: see 'word8FromInt' and
-- 'word32FromInt64'.
rowToOrder :: (OrderRow :. OrderInvoiceRow) -> Either ServiceError WebOrder
rowToOrder
  ( (orderId, invoiceId, providerRef, method, shortRef, badgeType, priceId, offerId, monthsInt, status)
      :. (amountPaidInt, settledAt, createdAt, updatedAt, amountInt, currency, payUrl, paymentAddress, cryptoAmount, expiresAt)
    ) = do
    months <- word8FromInt ("order " <> orderId <> " months") monthsInt
    amount <- CurrencyAmount <$> word32FromInt64 ("order " <> orderId <> " amount") amountInt
    amountPaid <- mapM (fmap CurrencyAmount . word32FromInt64 ("order " <> orderId <> " amount_paid")) amountPaidInt
    Right
      WebOrder
        { orderId,
          invoiceId,
          providerRef,
          method,
          shortRef,
          badgeType,
          priceId = BadgePriceId <$> priceId,
          offerId = BadgeOfferId <$> offerId,
          months,
          status,
          amountPaid,
          settledAt,
          createdAt,
          updatedAt,
          amount,
          currency,
          payUrl,
          paymentAddress,
          cryptoAmount,
          expiresAt
        }

-- | The join every order read uses. It is an INNER join: 'createOrder' is the only writer of a
-- @web_orders@ row and always writes the invoice with it, so an order without one does not
-- exist, and 'WebOrder' can hold the invoice's NOT NULL columns unwrapped.
orderSelect :: Query
orderSelect =
  [sql|
    SELECT o.order_id, o.invoice_id, o.provider_ref, o.method, o.short_ref, o.badge_type, o.price_id, o.offer_id, o.months, o.status,
           o.amount_paid, o.settled_at, o.created_at, o.updated_at,
           i.amount, i.currency, i.payment_url, i.payment_address, i.payment_crypto_amount, i.expires_at
    FROM sx_badge_service_web_orders o
    JOIN sx_badge_service_invoices i ON i.invoice_id = o.invoice_id
  |]

-- | Writes the @invoices@ row and the @web_orders@ row that references it, in that order (the
-- foreign key points that way). Both start @invoiced@. The caller supplies the transaction, as
-- everywhere else here, so a failed provider call after a partial write leaves neither row.
--
-- @invoices.payment_crypto_currency@ is deliberately not written: 'method' is the single source
-- and E4 derives the currency from it (A3).
createOrder :: DB.Connection -> NewWebOrder -> UTCTime -> ExceptT ServiceError IO ()
createOrder db newOrder now = do
  let NewWebOrder {orderId, invoiceId, providerRef, method, shortRef, badgeType, priceId, offerId, months} = newOrder
      NewWebOrder {amount = CurrencyAmount amount, currency, payUrl, paymentAddress, cryptoAmount, expiresAt} = newOrder
  liftIO $
    DB.execute
      db
      [sql|
        INSERT INTO sx_badge_service_invoices
          (invoice_id, provider, price, amount, currency, payment_url, payment_address, payment_crypto_amount, expires_at, status, created_at, updated_at)
        VALUES (?,?,?,?,?,?,?,?,?,?,?,?)
      |]
      ( (invoiceId, orderInvoiceProvider method, amount, amount, currency, payUrl)
          :. (paymentAddress, cryptoAmount, expiresAt, orderInvoiceStatus WOSInvoiced, now, now)
      )
  liftIO $
    DB.execute
      db
      [sql|
        INSERT INTO sx_badge_service_web_orders
          (order_id, invoice_id, provider_ref, method, short_ref, badge_type, price_id, offer_id, months, status, created_at, updated_at)
        VALUES (?,?,?,?,?,?,?,?,?,?,?,?)
      |]
      ( (orderId, invoiceId, providerRef, method, shortRef, badgeType)
          :. (unPriceId <$> priceId, unOfferId <$> offerId, months, WOSInvoiced, now, now)
      )
  where
    unPriceId (BadgePriceId pid) = pid
    unOfferId (BadgeOfferId oid) = oid

getOrder :: DB.Connection -> Text -> ExceptT ServiceError IO (Maybe WebOrder)
getOrder db orderId = queryOneOrder db (orderSelect <> " WHERE o.order_id = ?") (Only orderId)

-- | At most one row: A3's @idx_web_orders_provider_ref@ is UNIQUE, so a provider reference that
-- resolved to two orders could not have been stored. F2 resolves a Stripe charge this way, and
-- H3 re-reads provider state through it.
getOrderByProviderRef :: DB.Connection -> Text -> ExceptT ServiceError IO (Maybe WebOrder)
getOrderByProviderRef db providerRef = queryOneOrder db (orderSelect <> " WHERE o.provider_ref = ?") (Only providerRef)

-- | At most one row, on A3's UNIQUE @idx_web_orders_short_ref@. This is what support resolves a
-- bank-statement reference with (H2's @--ref@ subcommands).
getOrderByShortRef :: DB.Connection -> Text -> ExceptT ServiceError IO (Maybe WebOrder)
getOrderByShortRef db shortRef = queryOneOrder db (orderSelect <> " WHERE o.short_ref = ?") (Only shortRef)

queryOneOrder :: ToRow q => DB.Connection -> Query -> q -> ExceptT ServiceError IO (Maybe WebOrder)
queryOneOrder db q params = do
  rows <- liftIO $ DB.query db q params
  case rows of
    [] -> pure Nothing
    (row : _) -> Just <$> liftEither (rowToOrder row)

-- | Orders still open (@invoiced@ or @pending@) whose invoice expiry has passed, oldest expiry
-- first. H3's reconciliation pass reads it and re-reads each one's provider state: a missed
-- webhook is normal, so a stuck order is a routine outcome rather than an error.
--
-- The status filter is exactly the plan's two: a @paid@ order is never stuck however long ago
-- its invoice expired, and @expired@ and @failed@ are left out too, even though E3 can still
-- move either to @paid@ on a late webhook. So an order that a webhook already marked @expired@
-- and that then settles on chain with THAT webhook missed is not recovered by H3's pass; it is
-- recovered by support (H2). Widening the filter to all four is a change to H3's contract, not
-- to this query.
getStuckOrders :: DB.Connection -> UTCTime -> ExceptT ServiceError IO [WebOrder]
getStuckOrders db now = do
  rows <-
    liftIO $
      DB.query
        db
        (orderSelect <> " WHERE o.status IN (?,?) AND i.expires_at < ? ORDER BY i.expires_at ASC, o.order_id ASC")
        (WOSInvoiced, WOSPending, now)
  liftEither $ mapM rowToOrder rows

-- | The new order status and, optionally, the amount received so far -- so a partial payment or
-- an underpaid expiry records what arrived without settling the order (E3). @settled_at@ is not
-- touched: only 'setOrderSettled' writes it, and only together with @paid@.
--
-- @Nothing@ leaves a previously recorded @amount_paid@ alone rather than clearing it: an order
-- moving from @pending@ to @expired@ underpaid must keep the amount E5 renders. The matching
-- @invoices.status@ is written in the same statement pair, keeping A3's invariant.
updateOrderStatus :: DB.Connection -> Text -> WebOrderStatus -> Maybe CurrencyAmount -> UTCTime -> ExceptT ServiceError IO ()
updateOrderStatus db orderId status amountPaid now = do
  updated <- case amountPaid of
    Nothing ->
      liftIO $
        DB.query
          db
          "UPDATE sx_badge_service_web_orders SET status = ?, updated_at = ? WHERE order_id = ? RETURNING order_id"
          (status, now, orderId)
    Just (CurrencyAmount paid) ->
      liftIO $
        DB.query
          db
          "UPDATE sx_badge_service_web_orders SET status = ?, amount_paid = ?, updated_at = ? WHERE order_id = ? RETURNING order_id"
          (status, paid, now, orderId)
  when (null (updated :: [Only Text])) $ throwError SEOrderNotFound
  setOrderInvoiceStatus db orderId status now

-- | Points the order at a provider invoice \/ session \/ payment-intent id, replacing whatever
-- was there: F3 learns a Stripe payment intent only after the checkout session completes, so
-- the reference an order was created with is not always its final one. A3's UNIQUE index makes
-- a value that already belongs to another order fail loudly rather than resolve a charge to the
-- wrong order.
setOrderProviderRef :: DB.Connection -> Text -> Text -> UTCTime -> ExceptT ServiceError IO ()
setOrderProviderRef db orderId providerRef now = do
  updated <-
    liftIO $
      DB.query
        db
        "UPDATE sx_badge_service_web_orders SET provider_ref = ?, updated_at = ? WHERE order_id = ? RETURNING order_id"
        (providerRef, now, orderId)
  when (null (updated :: [Only Text])) $ throwError SEOrderNotFound

-- | Settlement's single writer: @settled_at@, @amount_paid@ and @status = 'paid'@ go in one
-- statement, so no reader can ever see a paid order without its amount or its time, and
-- @invoices.status@ moves to @settled@ with them. E3, H3 and F2 all settle through it.
--
-- It does not guard on the current status. Settlement is idempotent and monotonic toward @paid@
-- (E3), but that is the caller's rule to apply -- it decides whether a second @InvoiceSettled@
-- is a replay before it gets here, because it must also decide whether to write a code row.
setOrderSettled :: DB.Connection -> Text -> CurrencyAmount -> UTCTime -> ExceptT ServiceError IO ()
setOrderSettled db orderId (CurrencyAmount amountPaid) settledAt = do
  updated <-
    liftIO $
      DB.query
        db
        [sql|
          UPDATE sx_badge_service_web_orders
          SET status = ?, amount_paid = ?, settled_at = ?, updated_at = ?
          WHERE order_id = ?
          RETURNING order_id
        |]
        (WOSPaid, amountPaid, settledAt, settledAt, orderId)
  when (null (updated :: [Only Text])) $ throwError SEOrderNotFound
  setOrderInvoiceStatus db orderId WOSPaid settledAt

-- | The @invoices.status@ half of A3's invariant, written through the order's own @invoice_id@
-- so no caller has to carry it.
setOrderInvoiceStatus :: DB.Connection -> Text -> WebOrderStatus -> UTCTime -> ExceptT ServiceError IO ()
setOrderInvoiceStatus db orderId status now =
  liftIO $
    DB.execute
      db
      [sql|
        UPDATE sx_badge_service_invoices
        SET status = ?, updated_at = ?
        WHERE invoice_id IN (SELECT invoice_id FROM sx_badge_service_web_orders WHERE order_id = ?)
      |]
      (orderInvoiceStatus status, now, orderId)

-- Provider events ---------------------------------------------------------------

-- | Records the arrival of a provider webhook event and answers whether it should be processed.
--
-- 'False' means, and only means, that this event has already been processed: a row exists AND
-- its @processed_at@ is set. A row whose @processed_at@ is NULL is one whose previous attempt
-- did not complete -- the process died between recording the event and finishing the settlement
-- transaction -- so this returns 'True' and the event is processed again (E3). Treating that row
-- as a duplicate would strand a paid order forever, which is the failure this whole table
-- exists to prevent.
--
-- The insert is @ON CONFLICT DO NOTHING@ against A3's @(provider, event_id)@ primary key rather
-- than a read followed by a write, so two deliveries of the same event racing in separate
-- transactions cannot both insert.
recordProviderEvent :: DB.Connection -> PaymentProvider -> Text -> UTCTime -> ExceptT ServiceError IO Bool
recordProviderEvent db provider eventId now = do
  inserted <-
    liftIO $
      DB.query
        db
        [sql|
          INSERT INTO sx_badge_service_provider_events (provider, event_id, received_at)
          VALUES (?,?,?)
          ON CONFLICT (provider, event_id) DO NOTHING
          RETURNING event_id
        |]
        (provider, eventId, now)
  case (inserted :: [Only Text]) of
    (_ : _) -> pure True -- first delivery
    [] -> do
      -- the row was already there; @received_at@ keeps the first delivery's time
      rows <-
        liftIO $
          DB.query
            db
            "SELECT processed_at FROM sx_badge_service_provider_events WHERE provider = ? AND event_id = ?"
            (provider, eventId)
      pure $ case rows :: [Only (Maybe UTCTime)] of
        (Only (Just _) : _) -> False -- processed already: a replay
        _ -> True -- recorded but never processed: the previous attempt did not complete

-- | Closes the event out, inside the settlement transaction that processed it -- so a crash
-- anywhere before the commit leaves @processed_at@ NULL and 'recordProviderEvent' hands the
-- event back on the provider's next delivery.
markProviderEventProcessed :: DB.Connection -> PaymentProvider -> Text -> UTCTime -> ExceptT ServiceError IO ()
markProviderEventProcessed db provider eventId now = do
  updated <-
    liftIO $
      DB.query
        db
        "UPDATE sx_badge_service_provider_events SET processed_at = ? WHERE provider = ? AND event_id = ? RETURNING event_id"
        (now, provider, eventId)
  when (null (updated :: [Only Text])) $ throwError SEProviderEventNotFound

-- Purchases and payments -----------------------------------------------------

-- | The service's own projection of a @badge_purchases@ row. The single shared purchase record
-- core §3 drafted as @Badges.Types.BadgePurchase@ carried client-only columns (@user_id@,
-- @purchase_priv_key@, alert bookkeeping) that only exist on the client's own table (added by
-- the client-only @20260731_user_badges@ migration); the service's table has just the columns
-- 'badgeSchema' creates. That draft is gone: the client's half is
-- 'Simplex.Chat.Store.Badges.UserBadgePurchase' and this is the service's.
data BadgePurchaseRow = BadgePurchaseRow
  { badgePurchaseId :: Int64,
    purchaseKey :: C.PublicKeyEd25519,
    masterKey :: BadgeMasterKey,
    initialBadgeType :: BadgeType,
    currentBadgeType :: BadgeType,
    paymentId :: Maybe Text,
    status :: BadgePurchaseStatus,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving (Show)

type PurchaseRow = (Int64, C.PublicKeyEd25519, Binary ByteString, BadgeType, BadgeType, Maybe Text, BadgePurchaseStatus, UTCTime, UTCTime)

rowToPurchase :: PurchaseRow -> BadgePurchaseRow
rowToPurchase (badgePurchaseId, purchaseKey, Binary mk, initialBadgeType, currentBadgeType, paymentId, status, createdAt, updatedAt) =
  BadgePurchaseRow {badgePurchaseId, purchaseKey, masterKey = BadgeMasterKey mk, initialBadgeType, currentBadgeType, paymentId, status, createdAt, updatedAt}

purchaseSelectColumns :: Query
purchaseSelectColumns =
  "badge_purchase_id, purchase_key, master_key, initial_badge_type, current_badge_type, payment_id, status, created_at, updated_at"

getPurchaseByKey :: DB.Connection -> C.PublicKeyEd25519 -> ExceptT ServiceError IO (Maybe BadgePurchaseRow)
getPurchaseByKey db purchaseKey = do
  rows <-
    liftIO $
      DB.query
        db
        ("SELECT " <> purchaseSelectColumns <> " FROM sx_badge_service_badge_purchases WHERE purchase_key = ?")
        (Only purchaseKey)
  pure $ rowToPurchase <$> listToMaybe rows

-- | Writes the row with status 'PSIssued' and both badge-type columns set to the same value:
-- the service has no pre-response row, so this is the first and only write that establishes
-- a purchase for a given key. @payment_id@ starts NULL; 'createCodePayment' attaches one
-- later.
createPurchase :: DB.Connection -> C.PublicKeyEd25519 -> BadgeMasterKey -> BadgeType -> UTCTime -> ExceptT ServiceError IO BadgePurchaseRow
createPurchase db purchaseKey masterKey@(BadgeMasterKey mk) badgeType now = do
  [Only badgePurchaseId] <-
    liftIO $
      DB.query
        db
        [sql|
          INSERT INTO sx_badge_service_badge_purchases
            (purchase_key, master_key, initial_badge_type, current_badge_type, status, created_at, updated_at)
          VALUES (?,?,?,?,?,?,?)
          RETURNING badge_purchase_id
        |]
        (purchaseKey, Binary mk, badgeType, badgeType, PSIssued, now, now)
  pure
    BadgePurchaseRow
      { badgePurchaseId,
        purchaseKey,
        masterKey,
        initialBadgeType = badgeType,
        currentBadgeType = badgeType,
        paymentId = Nothing,
        status = PSIssued,
        createdAt = now,
        updatedAt = now
      }

-- | Writes the @payments@ row alone (caller-minted UUID as @payment_id@, @provider = 'code'@,
-- @invoice_id@ NULL, @status = 'settled'@ via 'PSSettled'\'s 'ToField'). Attaching it
-- to the purchase is 'attachPurchasePayment', a separate call because the two are not always
-- paired: @badge_purchases.payment_id@ is @UNIQUE@ and holds at most one payment, so a second
-- code redeemed under a purchase key that already has one still needs its @payments@ row (the
-- @credit(payment)@ ledger entry references it) but must not repoint the purchase.
createCodePayment :: DB.Connection -> Text -> UTCTime -> ExceptT ServiceError IO ()
createCodePayment db paymentId now =
  liftIO $
    DB.execute
      db
      [sql|
        INSERT INTO sx_badge_service_payments (payment_id, invoice_id, provider, status, created_at, updated_at)
        VALUES (?,?,?,?,?,?)
      |]
      (paymentId, Nothing :: Maybe Text, PPCode, PSSettled, now, now)

-- | Points the purchase's @payment_id@ at an existing payment. Guarded by @payment_id IS NULL@
-- so a purchase that already has a payment is never silently repointed; on no rows affected, a
-- follow-up existence check distinguishes an unknown purchase ('SEPurchaseNotFound') from one
-- that already has a payment ('SEPaymentConflict').
attachPurchasePayment :: DB.Connection -> Int64 -> Text -> UTCTime -> ExceptT ServiceError IO ()
attachPurchasePayment db badgePurchaseId paymentId now = do
  attached <-
    liftIO $
      DB.query
        db
        [sql|
          UPDATE sx_badge_service_badge_purchases
          SET payment_id = ?, updated_at = ?
          WHERE badge_purchase_id = ? AND payment_id IS NULL
          RETURNING badge_purchase_id
        |]
        (paymentId, now, badgePurchaseId)
  when (null (attached :: [Only Int64])) $ do
    exists <-
      liftIO $
        DB.query
          db
          "SELECT 1 FROM sx_badge_service_badge_purchases WHERE badge_purchase_id = ?"
          (Only badgePurchaseId)
    throwError $ if null (exists :: [Only Int]) then SEPurchaseNotFound else SEPaymentConflict

-- Ledger ----------------------------------------------------------------------

-- The service's own @badge_ledger@ table has just the columns 'badgeSchema' creates: the
-- @entry_type_unknown@\/@entry_type_value@ fallback columns that would let 'CTUnknown'\/
-- 'DTUnknown' round-trip only exist on the client's table (again added by the client-only
-- @20260731_user_badges@ ALTERs). Those variants exist for a client decoding an entry type
-- from a service ahead of it; the service, originating every entry it writes, never needs to
-- persist one, so 'encodeLedgerEntryType' rejects them.

type LedgerCoreRow = (Int64, Text, Int64, Int, Int, UTCTime, BadgeType, Maybe UTCTime, UTCTime, UTCTime)

type LedgerTypeRow = (Text, Maybe Text, Maybe Text, Maybe Text, Maybe Text, Maybe Int64, Maybe Int64)

ledgerSelectColumns :: Query
ledgerSelectColumns =
  "entry_id, entry_uuid, badge_purchase_id, change_months, balance_months, balance_start_ts, balance_badge_type, was_paused_since, service_created_at, created_at, "
    <> "entry_type, entry_credit_type, entry_debit_type, payment_id, charge_id, from_purchase_id, to_purchase_id"

-- | @'CTCharge' {chargeId}@ is typed 'Int64' in "Simplex.Chat.Badges.Types", but the column it
-- would persist through (@charge_id@) is @subscription_charges@\' TEXT primary key. Both
-- directions reject that constructor explicitly rather than inventing a silent, possibly
-- wrong, numeric<->text coercion; subscriptions are out of scope (plan \'6), so nothing writes
-- one. @'CTPayment' {paymentId}@ had the same defect and is now @'Maybe' 'Text'@, matching
-- @badge_ledger.payment_id TEXT REFERENCES payments@, which is NULLABLE (B7, widened in B10,
-- plan \'9). The service always names its payment row; the CLIENT, copying the same entries into
-- its own ledger, has no payments row for a code redemption and writes NULL -- so both directions
-- here carry the 'Maybe' through rather than requiring an id the client cannot have.
encodeLedgerEntryType :: LedgerEntryType -> ExceptT ServiceError IO LedgerTypeRow
encodeLedgerEntryType = \case
  LECredit creditType -> case creditType of
    CTPayment {paymentId} -> pure ("credit", Just "payment", Nothing, paymentId, Nothing, Nothing, Nothing)
    CTCharge {} -> throwError $ SEDecodeError "CTCharge.chargeId (Int64) does not fit the charge_id TEXT column; unresolved type mismatch, see SDD progress log"
    CTSupport -> pure ("credit", Just "support", Nothing, Nothing, Nothing, Nothing, Nothing)
    CTTransferIn {fromPurchaseId} -> pure ("credit", Just "transfer_in", Nothing, Nothing, Nothing, fromPurchaseId, Nothing)
    CTOpening -> pure ("credit", Just "opening", Nothing, Nothing, Nothing, Nothing, Nothing)
    CTUnknown {tag} -> throwError $ SEDecodeError ("cannot persist unknown ledger credit type: " <> tag)
  LEDebit debitType -> case debitType of
    DTRefund -> pure ("debit", Nothing, Just "refund", Nothing, Nothing, Nothing, Nothing)
    DTUpgrade {toPurchaseId} -> pure ("debit", Nothing, Just "upgrade", Nothing, Nothing, Nothing, Just toPurchaseId)
    DTTransferOut {toPurchaseId} -> pure ("debit", Nothing, Just "transfer_out", Nothing, Nothing, Nothing, Just toPurchaseId)
    DTSupport -> pure ("debit", Nothing, Just "support", Nothing, Nothing, Nothing, Nothing)
    DTBadge -> pure ("debit", Nothing, Just "badge", Nothing, Nothing, Nothing, Nothing)
    DTLapse -> pure ("debit", Nothing, Just "lapse", Nothing, Nothing, Nothing, Nothing)
    DTUnknown {tag} -> throwError $ SEDecodeError ("cannot persist unknown ledger debit type: " <> tag)

decodeLedgerEntryType :: LedgerTypeRow -> Either ServiceError LedgerEntryType
decodeLedgerEntryType row = case row of
  -- payment_id is read whether or not it is there: a NULL is what a client-written entry holds.
  ("credit", Just "payment", _, paymentId, _, _, _) -> Right $ LECredit (CTPayment paymentId)
  ("credit", Just "support", _, _, _, _, _) -> Right $ LECredit CTSupport
  ("credit", Just "transfer_in", _, _, _, fromPurchaseId, _) -> Right $ LECredit (CTTransferIn fromPurchaseId)
  ("credit", Just "opening", _, _, _, _, _) -> Right $ LECredit CTOpening
  ("debit", _, Just "refund", _, _, _, _) -> Right $ LEDebit DTRefund
  ("debit", _, Just "upgrade", _, _, _, Just toPurchaseId) -> Right $ LEDebit (DTUpgrade toPurchaseId)
  ("debit", _, Just "transfer_out", _, _, _, Just toPurchaseId) -> Right $ LEDebit (DTTransferOut toPurchaseId)
  ("debit", _, Just "support", _, _, _, _) -> Right $ LEDebit DTSupport
  ("debit", _, Just "badge", _, _, _, _) -> Right $ LEDebit DTBadge
  ("debit", _, Just "lapse", _, _, _, _) -> Right $ LEDebit DTLapse
  _ -> Left $ SEDecodeError ("malformed or unsupported ledger entry type row: " <> tshow row)

rowToLedgerEntry :: (LedgerCoreRow :. LedgerTypeRow) -> Either ServiceError BadgeLedgerEntry
rowToLedgerEntry ((entryId, entryUuid, badgePurchaseId, changeMonths, balanceMonths, balanceStartTs, balanceBadgeType, wasPausedSince, serviceCreatedAt, createdAt) :. typeRow) = do
  entryType <- decodeLedgerEntryType typeRow
  Right BadgeLedgerEntry {entryId, entryUuid, badgePurchaseId, changeMonths, balanceMonths, balanceStartTs, balanceBadgeType, wasPausedSince, serviceCreatedAt, createdAt, entryType}

-- | Ignores the input's 'entryId' (DB-assigned via IDENTITY) and returns the entry with the
-- assigned id filled in.
appendLedgerEntry :: DB.Connection -> BadgeLedgerEntry -> ExceptT ServiceError IO BadgeLedgerEntry
appendLedgerEntry db entry = do
  let BadgeLedgerEntry {entryUuid, badgePurchaseId, changeMonths, balanceMonths, balanceStartTs, balanceBadgeType, wasPausedSince, serviceCreatedAt, createdAt, entryType} = entry
  typeRow <- encodeLedgerEntryType entryType
  [Only newEntryId] <-
    liftIO $
      DB.query
        db
        [sql|
          INSERT INTO sx_badge_service_badge_ledger
            (entry_uuid, badge_purchase_id, change_months, balance_months, balance_start_ts, balance_badge_type, was_paused_since, service_created_at, created_at,
             entry_type, entry_credit_type, entry_debit_type, payment_id, charge_id, from_purchase_id, to_purchase_id)
          VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)
          RETURNING entry_id
        |]
        ((entryUuid, badgePurchaseId, changeMonths, balanceMonths, balanceStartTs, balanceBadgeType, wasPausedSince, serviceCreatedAt, createdAt) :. typeRow)
  pure BadgeLedgerEntry {entryId = newEntryId, entryUuid, badgePurchaseId, changeMonths, balanceMonths, balanceStartTs, balanceBadgeType, wasPausedSince, serviceCreatedAt, createdAt, entryType}

getLastLedgerEntry :: DB.Connection -> Int64 -> ExceptT ServiceError IO (Maybe BadgeLedgerEntry)
getLastLedgerEntry db badgePurchaseId = do
  rows <-
    liftIO $
      DB.query
        db
        ("SELECT " <> ledgerSelectColumns <> " FROM sx_badge_service_badge_ledger WHERE badge_purchase_id = ? ORDER BY entry_id DESC LIMIT 1")
        (Only badgePurchaseId)
  case rows of
    [] -> pure Nothing
    (row : _) -> Just <$> liftEither (rowToLedgerEntry row)

-- | Entries for a purchase, oldest first. @Nothing@ returns the full ledger; @Just entryId@
-- returns only entries strictly after it, matching 'BadgeStatement.previousEntryId'\'s
-- semantics (absent for the full ledger).
getLedgerSince :: DB.Connection -> Int64 -> Maybe Int64 -> ExceptT ServiceError IO [BadgeLedgerEntry]
getLedgerSince db badgePurchaseId sinceEntryId = do
  rows <- case sinceEntryId of
    Nothing ->
      liftIO $
        DB.query
          db
          ("SELECT " <> ledgerSelectColumns <> " FROM sx_badge_service_badge_ledger WHERE badge_purchase_id = ? ORDER BY entry_id ASC")
          (Only badgePurchaseId)
    Just sinceId ->
      liftIO $
        DB.query
          db
          ("SELECT " <> ledgerSelectColumns <> " FROM sx_badge_service_badge_ledger WHERE badge_purchase_id = ? AND entry_id > ? ORDER BY entry_id ASC")
          (badgePurchaseId, sinceId)
  liftEither $ mapM rowToLedgerEntry rows

-- | Resolves the wire @entryId@ (the row's @entry_uuid@, which is what a client asserts) to the
-- local @entry_id@ 'getLedgerSince' queries on, scoped to one purchase: an entry belonging to a
-- different purchase resolves to 'Nothing', so an asserted uuid cannot be used to probe another
-- purchase's ledger. 'Nothing' also covers a uuid the service simply does not hold, which the
-- RPC treats as an assertion that names nothing and answers with the complete history.
getLedgerEntryIdByUuid :: DB.Connection -> Int64 -> Text -> ExceptT ServiceError IO (Maybe Int64)
getLedgerEntryIdByUuid db badgePurchaseId entryUuid = do
  rows <-
    liftIO $
      DB.query
        db
        "SELECT entry_id FROM sx_badge_service_badge_ledger WHERE badge_purchase_id = ? AND entry_uuid = ?"
        (badgePurchaseId, entryUuid)
  pure $ fromOnly <$> listToMaybe rows

-- Issuances ---------------------------------------------------------------------

-- | Fields needed to create one @badge_issuances@ row. Unlike the shared 'BadgeIssuance',
-- @period_start@\/@period_end@\/@expiry@ are definite here, matching the NOT NULL columns;
-- 'BadgeIssuance' declares them 'Maybe' only because that record is also the wire shape,
-- which has no reason to require them from a client.
data NewIssuance = NewIssuance
  { issuanceId :: Text,
    badgePurchaseId :: Int64,
    badgeType :: BadgeType,
    periodStart :: UTCTime,
    periodEnd :: UTCTime,
    expiry :: UTCTime,
    -- | Named apart from 'BadgeIssuance'\'s 'entryId' (the same @entry_id@ column) so the two
    -- don't collide as a bare selector under 'DuplicateRecordFields' at call sites that have
    -- both types in scope.
    ledgerEntryId :: Maybe Int64,
    credential :: BadgeCredential
  }

type IssuanceRow = (Text, Int64, Maybe Int64, BadgeType, UTCTime, UTCTime, UTCTime, Binary ByteString, UTCTime)

issuanceSelectColumns :: Query
issuanceSelectColumns = "issuance_id, badge_purchase_id, entry_id, badge_type, period_start, period_end, expiry, credential, created_at"

-- | 'BadgeCredential' already has 'J.ToJSON'\/'J.FromJSON' (it crosses the wire in
-- 'BSPBadgeCredential'); the @credential@ BYTEA\/BLOB column stores that same JSON encoding,
-- so a malformed row is a genuine 'SEDecodeError' rather than a second, DB-only codec.
rowToIssuance :: IssuanceRow -> Either ServiceError BadgeIssuance
rowToIssuance (issuanceId, badgePurchaseId, entryId, badgeType, periodStart, periodEnd, expiry, Binary credBytes, createdAt) =
  case J.eitherDecodeStrict credBytes of
    Left err -> Left $ SEDecodeError ("issuance " <> issuanceId <> " credential: " <> T.pack err)
    Right credential ->
      Right
        BadgeIssuance
          { issuanceId,
            badgePurchaseId,
            badgeType,
            periodStart = Just periodStart,
            periodEnd = Just periodEnd,
            expiry = Just expiry,
            entryId,
            credential,
            createdAt
          }

createIssuance :: DB.Connection -> NewIssuance -> UTCTime -> ExceptT ServiceError IO BadgeIssuance
createIssuance db NewIssuance {issuanceId, badgePurchaseId, badgeType, periodStart, periodEnd, expiry, ledgerEntryId, credential} now = do
  liftIO $
    DB.execute
      db
      [sql|
        INSERT INTO sx_badge_service_badge_issuances
          (issuance_id, badge_purchase_id, entry_id, badge_type, period_start, period_end, expiry, credential, created_at)
        VALUES (?,?,?,?,?,?,?,?,?)
      |]
      (issuanceId, badgePurchaseId, ledgerEntryId, badgeType, periodStart, periodEnd, expiry, Binary (BL.toStrict (J.encode credential)), now)
  pure
    BadgeIssuance
      { issuanceId,
        badgePurchaseId,
        badgeType,
        periodStart = Just periodStart,
        periodEnd = Just periodEnd,
        expiry = Just expiry,
        entryId = ledgerEntryId,
        credential,
        createdAt = now
      }

-- | The issuance for a purchase whose period covers a given instant (half-open:
-- @period_start <= t < period_end@).
getIssuanceForPeriod :: DB.Connection -> Int64 -> UTCTime -> ExceptT ServiceError IO (Maybe BadgeIssuance)
getIssuanceForPeriod db badgePurchaseId asOf = do
  rows <-
    liftIO $
      DB.query
        db
        ("SELECT " <> issuanceSelectColumns <> " FROM sx_badge_service_badge_issuances WHERE badge_purchase_id = ? AND period_start <= ? AND period_end > ?")
        (badgePurchaseId, asOf, asOf)
  case rows of
    [] -> pure Nothing
    (row : _) -> Just <$> liftEither (rowToIssuance row)

-- | code hash -> @redeemed_purchase_id@ -> the issuance whose period contains @redeemed_at@.
-- A purchase may have several issuances by the time B7's replay path runs this, so the
-- redemption time (not just the purchase) picks out exactly one.
getIssuanceForRedeemedCode :: DB.Connection -> ByteString -> ExceptT ServiceError IO (Maybe BadgeIssuance)
getIssuanceForRedeemedCode db codeHash = do
  rows <-
    liftIO $
      DB.query
        db
        [sql|
          SELECT i.issuance_id, i.badge_purchase_id, i.entry_id, i.badge_type, i.period_start, i.period_end, i.expiry, i.credential, i.created_at
          FROM sx_badge_service_codes c
          JOIN sx_badge_service_badge_issuances i ON i.badge_purchase_id = c.redeemed_purchase_id
          WHERE c.code_hash = ? AND c.redeemed_at IS NOT NULL AND i.period_start <= c.redeemed_at AND i.period_end > c.redeemed_at
        |]
        (Only (Binary codeHash))
  case rows of
    [] -> pure Nothing
    (row : _) -> Just <$> liftEither (rowToIssuance row)

-- Codes -----------------------------------------------------------------------

data BadgeCode = BadgeCode
  { codeHash :: ByteString,
    badgeType :: BadgeType,
    months :: Word8,
    batch :: Text,
    expiresAt :: UTCTime,
    redeemedPurchaseId :: Maybe Int64,
    redeemedAt :: Maybe UTCTime,
    unredeemedAt :: Maybe UTCTime,
    revokedAt :: Maybe UTCTime,
    createdAt :: UTCTime
  }
  deriving (Show)

-- | Fields needed to create a new, unredeemed code; 'insertCodes' is the only writer of a
-- fresh code, so there is no representable state where a new row starts out redeemed,
-- unredeemed-again or revoked.
data NewBadgeCode = NewBadgeCode
  { codeHash :: ByteString,
    badgeType :: BadgeType,
    months :: Word8,
    batch :: Text,
    expiresAt :: UTCTime
  }

type CodeRow = (Binary ByteString, BadgeType, Int, Text, UTCTime, Maybe Int64, Maybe UTCTime, Maybe UTCTime, Maybe UTCTime, UTCTime)

-- | @months@ is read as a signed 'Int' and converted: see 'word8FromInt'.
rowToCode :: CodeRow -> Either ServiceError BadgeCode
rowToCode (Binary codeHash, badgeType, monthsInt, batch, expiresAt, redeemedPurchaseId, redeemedAt, unredeemedAt, revokedAt, createdAt) = do
  months <- word8FromInt "code months" monthsInt
  Right BadgeCode {codeHash, badgeType, months, batch, expiresAt, redeemedPurchaseId, redeemedAt, unredeemedAt, revokedAt, createdAt}

-- | Joined to @badge_purchases@ so the caller sees the @purchase_key@ behind
-- @redeemed_purchase_id@: a redeeming request whose own key matches is a replay of its own
-- redemption, one whose key differs is another key trying to use an already-used code -- the
-- two get different responses later. @Nothing@ purchase key means the code has never been
-- redeemed.
getCodeByHash :: DB.Connection -> ByteString -> ExceptT ServiceError IO (Maybe (BadgeCode, Maybe C.PublicKeyEd25519))
getCodeByHash db codeHash = do
  rows <-
    liftIO $
      DB.query
        db
        [sql|
          SELECT c.code_hash, c.badge_type, c.months, c.batch, c.expires_at, c.redeemed_purchase_id, c.redeemed_at, c.unredeemed_at, c.revoked_at, c.created_at,
                 p.purchase_key
          FROM sx_badge_service_codes c
          LEFT JOIN sx_badge_service_badge_purchases p ON p.badge_purchase_id = c.redeemed_purchase_id
          WHERE c.code_hash = ?
        |]
        (Only (Binary codeHash))
  case rows of
    [] -> pure Nothing
    ((codeRow :. Only redeemerKey) : _) -> do
      code <- liftEither $ rowToCode codeRow
      pure $ Just (code, redeemerKey)

-- | Claims an unredeemed, unrevoked code for a purchase. Guarded by
-- @redeemed_purchase_id IS NULL AND revoked_at IS NULL@ so neither an existing redemption nor a
-- revocation is overwritten: on no rows affected, a follow-up existence check distinguishes an
-- unknown code ('SECodeNotFound') from one already claimed or revoked ('SECodeConflict'), which
-- the caller answers by re-classifying — a revoked code then classifies as 'RedeemRevoked' and is
-- answered @code_invalid@, which is the right answer.
--
-- __The revocation half of the guard is load-bearing, unlike the redemption half.__ B8's
-- @codes revoke@ runs in a SECOND PROCESS against the same database, and this service separates
-- classification from this write by the signing IO — so a revocation issued in that window would
-- otherwise be silently overwritten and the code redeemed anyway. That is the whole point of
-- being able to revoke a code that is being abused. The redemption half cannot fire today (the
-- request loop is single-threaded, see @BadgeService.Service.redemptionRetries@, which also
-- records the ledger's lack of an equivalent guard); it is kept because it costs nothing and
-- because 'unredeemCode' also writes this row out of band.
markCodeRedeemed :: DB.Connection -> ByteString -> Int64 -> UTCTime -> ExceptT ServiceError IO ()
markCodeRedeemed db codeHash badgePurchaseId now = do
  rows <-
    liftIO $
      DB.query
        db
        [sql|
          UPDATE sx_badge_service_codes
          SET redeemed_purchase_id = ?, redeemed_at = ?
          WHERE code_hash = ? AND redeemed_purchase_id IS NULL AND revoked_at IS NULL
          RETURNING code_hash
        |]
        (badgePurchaseId, now, Binary codeHash)
  when (null (rows :: [Only (Binary ByteString)])) $ do
    exists <-
      liftIO $
        DB.query
          db
          "SELECT 1 FROM sx_badge_service_codes WHERE code_hash = ?"
          (Only (Binary codeHash))
    throwError $ if null (exists :: [Only Int]) then SECodeNotFound else SECodeConflict

-- | Clears both redemption columns and sets @unredeemed_at@, which both re-enables
-- redemption and reopens E4's disclosure window.
unredeemCode :: DB.Connection -> ByteString -> UTCTime -> ExceptT ServiceError IO ()
unredeemCode db codeHash now = do
  rows <-
    liftIO $
      DB.query
        db
        [sql|
          UPDATE sx_badge_service_codes
          SET redeemed_purchase_id = NULL, redeemed_at = NULL, unredeemed_at = ?
          WHERE code_hash = ?
          RETURNING code_hash
        |]
        (now, Binary codeHash)
  when (null (rows :: [Only (Binary ByteString)])) $ throwError SECodeNotFound

insertCodes :: DB.Connection -> [NewBadgeCode] -> UTCTime -> ExceptT ServiceError IO ()
insertCodes db codes now =
  liftIO $
    DB.executeMany
      db
      [sql|
        INSERT INTO sx_badge_service_codes (code_hash, badge_type, months, batch, expires_at, created_at)
        VALUES (?,?,?,?,?,?)
      |]
      (map toRow codes)
  where
    toRow NewBadgeCode {codeHash, badgeType, months, batch, expiresAt} = (Binary codeHash, badgeType, months, batch, expiresAt, now)

revokeCode :: DB.Connection -> ByteString -> UTCTime -> ExceptT ServiceError IO ()
revokeCode db codeHash now = do
  rows <-
    liftIO $
      DB.query
        db
        [sql|
          UPDATE sx_badge_service_codes
          SET revoked_at = ?
          WHERE code_hash = ? AND revoked_at IS NULL
          RETURNING code_hash
        |]
        (now, Binary codeHash)
  when (null (rows :: [Only (Binary ByteString)])) $ throwError SECodeNotFound

-- | Sets @revoked_at@ on every unrevoked code of a batch through @idx_codes_batch@, which is
-- what B8's @codes revoke --batch@ calls. Returns the number of codes revoked; a batch name
-- that matches nothing (typo, already fully revoked) is not an error, just zero.
revokeBatch :: DB.Connection -> Text -> UTCTime -> ExceptT ServiceError IO Int
revokeBatch db batch now = do
  rows <-
    liftIO $
      DB.query
        db
        [sql|
          UPDATE sx_badge_service_codes
          SET revoked_at = ?
          WHERE batch = ? AND revoked_at IS NULL
          RETURNING code_hash
        |]
        (now, batch)
  pure $ length (rows :: [Only (Binary ByteString)])

-- Catalog -----------------------------------------------------------------------

-- | postgresql-simple has no 'DB.FromField' instance for any @Word*@ type (Postgres has no
-- unsigned integer type); sqlite-simple does. Every @Word8@\/@Word32@ column here is read as
-- a signed integer and converted, so both backends decode the same way; the range check turns
-- a corrupt or out-of-range value into a named 'SEDecodeError' rather than a silent
-- 'fromIntegral' wrap (the same hazard 'BadgeService.Catalog.chargeableMonths' guards against).
word8FromInt :: Text -> Int -> Either ServiceError Word8
word8FromInt label n
  | n >= 0 && n <= fromIntegral (maxBound :: Word8) = Right (fromIntegral n)
  | otherwise = Left $ SEDecodeError (label <> ": " <> tshow n <> " does not fit a Word8")

word32FromInt64 :: Text -> Int64 -> Either ServiceError Word32
word32FromInt64 label n
  | n >= 0 && n <= fromIntegral (maxBound :: Word32) = Right (fromIntegral n)
  | otherwise = Left $ SEDecodeError (label <> ": " <> tshow n <> " does not fit a Word32")

type PriceRow = (Text, BadgeType, Int64, Text, BadgeItemStatus, UTCTime)

rowToPrice :: PriceRow -> Either ServiceError BadgePrice
rowToPrice (priceId, badgeType, monthPriceMinor, currency, status, createdAt) = do
  monthPriceAmount <- word32FromInt64 ("price " <> priceId <> " month_price") monthPriceMinor
  Right BadgePrice {priceId = BadgePriceId priceId, badgeType, monthPrice = CurrencyAmount monthPriceAmount, currency, status, createdAt}

type OfferRow = (Text, Maybe Text, Int, Maybe Int, Maybe Int, BadgeItemStatus, UTCTime)

decodeDiscount :: Text -> Maybe Word8 -> Maybe Word8 -> Either ServiceError OfferDiscount
decodeDiscount _ (Just freeMonths) Nothing = Right (ODFreeMonths freeMonths)
decodeDiscount _ Nothing (Just discount) = Right (ODDiscount discount)
decodeDiscount offerId freeMonths discount =
  Left $ SEDecodeError ("offer " <> offerId <> ": invalid free_months/discount combination: " <> tshow (freeMonths, discount))

rowToOffer :: OfferRow -> Either ServiceError BadgeOffer
rowToOffer (offerId, priceId, monthsInt, freeMonthsInt, discountPercentInt, status, createdAt) = do
  months <- word8FromInt ("offer " <> offerId <> " months") monthsInt
  freeMonths <- mapM (word8FromInt ("offer " <> offerId <> " free_months")) freeMonthsInt
  discountPercent <- mapM (word8FromInt ("offer " <> offerId <> " discount")) discountPercentInt
  discount <- decodeDiscount offerId freeMonths discountPercent
  Right BadgeOffer {offerId = BadgeOfferId offerId, priceId = BadgePriceId <$> priceId, months, discount, status, createdAt, total = Nothing}

priceSelectColumns :: Query
priceSelectColumns = "price_id, badge_type, month_price, currency, status, created_at"

offerSelectColumns :: Query
offerSelectColumns = "offer_id, price_id, months, free_months, discount, status, created_at"

-- | Prices and offers with status 'BISActive' or 'BISDeprecated'. The offers query joins to
-- prices under the same status filter, which is what makes both guarantees hold at once: an
-- offer with a NULL @price_id@ has nothing to join to and is dropped, and an offer pinned to
-- a price that isn't itself active\/deprecated fails the join too. Every offer this returns
-- therefore has a resolvable price in the same result, which is the invariant
-- 'BadgeService.Catalog.catalogTotals' relies on to stay a total function.
getActiveCatalog :: DB.Connection -> ExceptT ServiceError IO BadgeCatalog
getActiveCatalog db = do
  priceRows <-
    liftIO $
      DB.query
        db
        ("SELECT " <> priceSelectColumns <> " FROM sx_badge_service_badge_prices WHERE status IN (?,?)")
        (BISActive, BISDeprecated)
  offerRows <-
    liftIO $
      DB.query
        db
        [sql|
          SELECT o.offer_id, o.price_id, o.months, o.free_months, o.discount, o.status, o.created_at
          FROM sx_badge_service_badge_offers o
          JOIN sx_badge_service_badge_prices p ON o.price_id = p.price_id
          WHERE o.status IN (?,?) AND p.status IN (?,?)
        |]
        (BISActive, BISDeprecated, BISActive, BISDeprecated)
  offers <- liftEither $ mapM rowToOffer offerRows
  prices <- liftEither $ mapM rowToPrice priceRows
  pure BadgeCatalog {prices, offers}

-- | Reachable regardless of status (including 'BISDisabled'), unlike 'getActiveCatalog'.
getPriceById :: DB.Connection -> BadgePriceId -> ExceptT ServiceError IO (Maybe BadgePrice)
getPriceById db (BadgePriceId priceId) = do
  rows <-
    liftIO $
      DB.query
        db
        ("SELECT " <> priceSelectColumns <> " FROM sx_badge_service_badge_prices WHERE price_id = ?")
        (Only priceId)
  case rows of
    [] -> pure Nothing
    (row : _) -> Just <$> liftEither (rowToPrice row)

-- | Reachable regardless of status, same as 'getPriceById'.
getOfferById :: DB.Connection -> BadgeOfferId -> ExceptT ServiceError IO (Maybe BadgeOffer)
getOfferById db (BadgeOfferId offerId) = do
  rows <-
    liftIO $
      DB.query
        db
        ("SELECT " <> offerSelectColumns <> " FROM sx_badge_service_badge_offers WHERE offer_id = ?")
        (Only offerId)
  case rows of
    [] -> pure Nothing
    (row : _) -> Just <$> liftEither (rowToOffer row)

-- | One of the only two production writers of a catalog status (with 'setOfferStatus'):
-- operators use it to deprecate a price on repricing.
setPriceStatus :: DB.Connection -> BadgePriceId -> BadgeItemStatus -> ExceptT ServiceError IO ()
setPriceStatus db (BadgePriceId priceId) status = do
  rows <-
    liftIO $
      DB.query
        db
        "UPDATE sx_badge_service_badge_prices SET status = ? WHERE price_id = ? RETURNING price_id"
        (status, priceId)
  when (null (rows :: [Only Text])) $ throwError SEPriceNotFound

setOfferStatus :: DB.Connection -> BadgeOfferId -> BadgeItemStatus -> ExceptT ServiceError IO ()
setOfferStatus db (BadgeOfferId offerId) status = do
  rows <-
    liftIO $
      DB.query
        db
        "UPDATE sx_badge_service_badge_offers SET status = ? WHERE offer_id = ? RETURNING offer_id"
        (status, offerId)
  when (null (rows :: [Only Text])) $ throwError SEOfferNotFound

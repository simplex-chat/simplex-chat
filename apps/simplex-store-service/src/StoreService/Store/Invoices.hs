{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

module StoreService.Store.Invoices
  ( InvoiceRow (..),
    InvoiceProduct (..),
    InvoicePayment (..),
    paymentHolds,
    NewInvoice (..),
    NewNameInvoice (..),
    CreateError (..),
    StoreDecodeError (..),
    ProviderRefConflict (..),
    createInvoiceRows,
    insertNameInvoice,
    executeChanging,
    getInvoice,
    getInvoiceByProviderRef,
    unpaidRefs,
    providerText,
    codeHashExists,
    OverdueInvoice (..),
    overdueInvoices,
    expireOverdueInvoice,
    lockInvoice,
    unfundedQuery,
    newInvoiceId,
    readCatalogRows,
    seedCatalog,
    truncateToSecond,
    settlementInvoice,
    settlementCodeHash,
    upsertPayment,
    paymentStatusText,
    cryptoCurrencyText,
    textToCryptoCurrency,
    textToInvoiceStatus,
    invoiceStatusText,
    updateInvoiceStatus,
    markCodePaid,
  )
where

import Control.Exception (Exception)
import qualified Control.Exception as E
import Crypto.Random (getRandomBytes)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Base64.URL as B64U
import Data.Int (Int64)
import qualified Data.ByteString.Char8 as BC8
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import Data.Word (Word8, Word32)
import Simplex.Chat.Badges (BadgeType (..))
import Simplex.Chat.StoreService (BadgePrice (..), BadgeOffer (..))
import Simplex.Chat.Badges.Types (BadgeCodePaymentStatus (..), BadgeItemStatus (..), BadgeOfferId (..), BadgePriceId (..), OfferDiscount (..))
import Simplex.Chat.Store.Shared (insertedRowId)
import Simplex.Chat.PaymentService.Types (CardProvider (..), CryptoCurrency (..), CurrencyAmount (..), InvoiceId (..), InvoiceStatus (..), PaymentProvider (..), PaymentStatus (..), ServicePaymentDestination (..))
import Simplex.Messaging.Agent.Store.Common (DBStore, withConnection, withTransaction)
import qualified Simplex.Messaging.Agent.Store.DB as DB
import Simplex.Messaging.Encoding.String (textEncode)
import Simplex.Messaging.Util (safeDecodeUtf8, tshow)
import StoreService.Names.Phases (RegistrationId (..))
import StoreService.Providers (Confirmed (..), Received (..))

#if defined(dbPostgres)
import StoreService.Store.Postgres.Migrations (servicePrefix, withPrefix)
import Database.PostgreSQL.Simple (Only (..), Query, ToRow, (:.) (..))
import qualified Database.PostgreSQL.Simple as PSQL
import Database.PostgreSQL.Simple.Errors (ConstraintViolation (..), constraintViolation)
#else
import StoreService.Store.SQLite.Migrations (servicePrefix, withPrefix)
import Database.SQLite.Simple (Only (..), Query, ToRow, (:.) (..))
import qualified Database.SQLite.Simple as SQL
#endif

data InvoiceRow = InvoiceRow
  { irInvoiceId :: InvoiceId,
    irProvider :: PaymentProvider,
    irProviderRef :: Text,
    irProduct :: InvoiceProduct,
    irPrice :: CurrencyAmount,
    irAmount :: CurrencyAmount,
    irCurrency :: Text,
    irDestination :: ServicePaymentDestination,
    irExpiresAt :: UTCTime,
    irStatus :: InvoiceStatus,
    irCreatedAt :: UTCTime,
    irPayment :: Maybe InvoicePayment
  }
  deriving (Eq, Show)

-- | A badge's type and months, or the name registration the invoice pays for.
data InvoiceProduct = IPBadge BadgeType Word8 | IPName RegistrationId
  deriving (Eq, Show)

paymentHolds :: InvoicePayment -> Bool
paymentHolds InvoicePayment {ipAmount, ipCryptoPaid, ipPaidInFull} =
  maybe False (\(CurrencyAmount a) -> a > 0) ipAmount || ipCryptoPaid /= Nothing || ipPaidInFull

data InvoicePayment = InvoicePayment
  { ipAmount :: Maybe CurrencyAmount,
    ipCryptoPaid :: Maybe Text,
    ipCryptoDue :: Maybe Text,
    ipPaidInFull :: Bool,
    ipStatus :: Text,
    ipUpdatedAt :: UTCTime,
    ipConfirmed :: Confirmed
  }
  deriving (Eq, Show)

data NewInvoice = NewInvoice
  { niInvoiceId :: InvoiceId,
    niProviderRef :: Text,
    niCodeHash :: ByteString,
    niPriceId :: BadgePriceId,
    niOfferId :: Maybe BadgeOfferId,
    niBadgeType :: BadgeType,
    niMonths :: Word8,
    niPrice :: CurrencyAmount,
    niAmount :: CurrencyAmount,
    niCurrency :: Text,
    niProvider :: PaymentProvider,
    niDestination :: ServicePaymentDestination,
    niExpiresAt :: UTCTime,
    niCreatedAt :: UTCTime
  }

-- | The invoice row of a name order; the registration links it, with its provider ref, in the same transaction.
data NewNameInvoice = NewNameInvoice
  { nniInvoiceId :: InvoiceId,
    nniProvider :: PaymentProvider,
    nniAmount :: CurrencyAmount,
    nniCurrency :: Text,
    nniDestination :: ServicePaymentDestination,
    nniExpiresAt :: UTCTime,
    nniCreatedAt :: UTCTime
  }

data CreateError = CECodeConflict | CERefConflict | CEOther Text
  deriving (Eq, Show)

newtype StoreDecodeError = StoreDecodeError Text
  deriving (Eq, Show)

instance Exception StoreDecodeError

-- | A provider ref held by both a badge invoice and a name invoice, so a payment to it cannot be credited to either.
newtype ProviderRefConflict = ProviderRefConflict Text
  deriving (Eq, Show)

instance Exception ProviderRefConflict

-- | SQLite stores timestamps as text, so `expires_at < ?` compares as strings and only sorts chronologically when every value has the same width.
truncateToSecond :: UTCTime -> UTCTime
truncateToSecond = posixSecondsToUTCTime . fromInteger . truncate . utcTimeToPOSIXSeconds

#if defined(dbPostgres)
mkQuery :: Text -> Query
mkQuery raw = fromString (T.unpack (withPrefix servicePrefix raw))
#else
mkQuery :: Text -> Query
mkQuery raw = withPrefix servicePrefix (fromString (T.unpack raw))
#endif

-- Backslash-newline string gaps do not survive CPP, so these queries are joined with <>.
qInsertInvoice :: Query
qInsertInvoice =
  mkQuery $
    "INSERT INTO @invoices "
      <> "(invoice_id, provider, price, discount_amount, credit_amount, amount, currency, "
      <> "payment_url, payment_address, payment_crypto_currency, payment_crypto_amount, "
      <> "expires_at, status, created_at, updated_at) "
      <> "VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)"

qInsertBadgeCodeInvoice :: Query
qInsertBadgeCodeInvoice =
  mkQuery $
    "INSERT INTO @badge_code_invoices "
      <> "(invoice_id, badge_code_id, price_id, offer_id, provider_ref, created_at) "
      <> "VALUES (?,?,?,?,?,?)"

qInsertBadgeCode :: Query
qInsertBadgeCode =
  mkQuery $
    "INSERT INTO @badge_codes (code_hash, badge_type, months, code_payment_status, created_at) "
      <> "VALUES (?,?,?,?,?)"

-- | Every invoice has a badge code row or a name registration row, never both.
productJoins :: Text
productJoins =
  "LEFT JOIN @badge_code_invoices ci ON ci.invoice_id = i.invoice_id "
    <> "LEFT JOIN @badge_codes bc ON bc.badge_code_id = ci.badge_code_id "
    <> "LEFT JOIN @name_invoices ni ON ni.invoice_id = i.invoice_id "

providerRefColumn :: Text
providerRefColumn = "COALESCE(ci.provider_ref, ni.provider_ref)"

invoiceRowSelect :: Text
invoiceRowSelect =
  "SELECT i.invoice_id, i.provider, "
    <> providerRefColumn
    <> ", bc.badge_type, bc.months, ni.registration_id, "
    <> "i.price, i.amount, i.currency, i.payment_url, i.payment_address, i.payment_crypto_currency, "
    <> "i.payment_crypto_amount, i.expires_at, i.status, i.created_at, "
    <> "p.amount, p.crypto_paid, p.crypto_due, p.paid_in_full, p.status, p.updated_at, p.confirmed_amount, p.confirmed_units "
    <> "FROM @invoices i "
    <> productJoins
    <> "LEFT JOIN @payments p ON p.invoice_id = i.invoice_id "

qGetInvoice :: Query
qGetInvoice = mkQuery (invoiceRowSelect <> "WHERE i.invoice_id = ?")

qGetInvoiceByProviderRef :: Query
qGetInvoiceByProviderRef =
  mkQuery $
    invoiceRowSelect
      <> "WHERE i.invoice_id IN (SELECT invoice_id FROM @badge_code_invoices WHERE provider_ref = ? "
      <> "UNION ALL SELECT invoice_id FROM @name_invoices WHERE provider_ref = ?)"

qUnpaidRefs :: Query
qUnpaidRefs =
  mkQuery $
    "SELECT i.provider, "
      <> providerRefColumn
      <> " FROM @invoices i "
      <> productJoins
      <> "WHERE i.status <> 'paid' AND i.created_at >= ? "
      -- created_at is whole seconds; the id breaks ties the same way on SQLite and Postgres
      <> "ORDER BY i.created_at, i.invoice_id"

qCodeHashExists :: Query
qCodeHashExists = mkQuery "SELECT 1 FROM @badge_codes WHERE code_hash = ? LIMIT 1"

-- | No payment holding money was read for the invoice whose id is in this column, as paymentHolds reads one.
-- The sweep and the overdue close of a name order must agree on it, so both use this.
unfundedBy :: Text -> Text
unfundedBy invoiceId =
  "AND NOT EXISTS (SELECT 1 FROM @payments p "
    <> ("WHERE p.invoice_id = " <> invoiceId <> " AND (")
    <> "COALESCE(p.amount, 0) > 0 OR p.crypto_paid IS NOT NULL OR p.paid_in_full = 1)) "

unfundedQuery :: Text -> Query
unfundedQuery = mkQuery . (" " <>) . unfundedBy

unfundedOnly :: Text
unfundedOnly = unfundedBy "@invoices.invoice_id"

qOverdueInvoices :: Query
qOverdueInvoices =
  mkQuery $
    "SELECT @invoices.invoice_id, @invoices.provider, COALESCE(ci.provider_ref, ni.provider_ref), @invoices.created_at FROM @invoices "
      <> "LEFT JOIN @badge_code_invoices ci ON ci.invoice_id = @invoices.invoice_id "
      <> "LEFT JOIN @name_invoices ni ON ni.invoice_id = @invoices.invoice_id "
      <> "WHERE status = 'open' AND expires_at < ? "
      <> unfundedOnly
      <> "ORDER BY @invoices.created_at"

qExpireOverdue :: Query
qExpireOverdue =
  mkQuery $
    "UPDATE @invoices SET status = 'expired', updated_at = ? "
      <> "WHERE status = 'open' AND expires_at < ? AND invoice_id = ? "
      <> unfundedOnly

qCodeHashForInvoice :: Query
qCodeHashForInvoice =
  mkQuery $
    "SELECT bc.code_hash FROM @badge_code_invoices ci "
      <> "JOIN @badge_codes bc ON bc.badge_code_id = ci.badge_code_id "
      <> "WHERE ci.invoice_id = ?"

-- | SQLite's two-argument MAX is GREATEST in Postgres, where MAX is an aggregate.
largerOf :: Text
#if defined(dbPostgres)
largerOf = "GREATEST"
#else
largerOf = "MAX"
#endif

-- Once a row is settled it stays settled, so a later pending update cannot overwrite it. The confirmed figures and received units
-- only rise, so a settled sum that falls (a payment turned invalid, a reorg) leaves them; the invoice's rate is written once.
qUpsertPayment :: Query
qUpsertPayment =
  mkQuery $
    "INSERT INTO @payments "
      <> "(payment_id, invoice_id, provider, provider_ref, amount, currency, crypto_paid, crypto_due, paid_in_full, status, "
      <> "received_units, confirmed_amount, confirmed_units, invoice_rate, created_at, updated_at) "
      <> "VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?) "
      <> "ON CONFLICT (payment_id) DO UPDATE SET "
      <> "amount = "
      <> largerOf
      <> "(COALESCE(@payments.amount, 0), excluded.amount), "
      <> "received_units = "
      <> largerOfNullable "received_units"
      <> ", confirmed_amount = "
      <> largerOf
      <> "(@payments.confirmed_amount, excluded.confirmed_amount), "
      <> "confirmed_units = "
      <> largerOfNullable "confirmed_units"
      <> ", invoice_rate = COALESCE(@payments.invoice_rate, excluded.invoice_rate), "
      <> "crypto_paid = CASE WHEN excluded.amount > COALESCE(@payments.amount, 0) OR @payments.crypto_paid IS NULL "
      <> "THEN excluded.crypto_paid ELSE @payments.crypto_paid END, "
      <> "crypto_due = COALESCE(excluded.crypto_due, @payments.crypto_due), "
      <> "paid_in_full = "
      <> largerOf
      <> "(@payments.paid_in_full, excluded.paid_in_full), "
      <> "status = CASE WHEN "
      <> alreadySettled
      <> " THEN @payments.status ELSE excluded.status END, "
      <> "updated_at = CASE WHEN "
      <> alreadySettled
      <> " THEN @payments.updated_at ELSE excluded.updated_at END"
  where
    alreadySettled = "@payments.status = '" <> paymentStatusText PSSettled <> "'"
    -- SQLite's MAX of a NULL is NULL, so each side falls back to the other
    largerOfNullable column = largerOf <> "(COALESCE(@payments." <> column <> ", excluded." <> column <> "), COALESCE(excluded." <> column <> ", @payments." <> column <> "))"

qLockInvoice :: Query
qLockInvoice = mkQuery "UPDATE @invoices SET updated_at = ? WHERE invoice_id = ?"

qUpdateInvoiceStatus :: Query
qUpdateInvoiceStatus =
  mkQuery "UPDATE @invoices SET status = ?, updated_at = ? WHERE invoice_id = ? AND status = ?"

qMarkCodePaid :: Query
qMarkCodePaid =
  mkQuery $
    "UPDATE @badge_codes SET code_payment_status = ?, expires_at = ? "
      <> "WHERE code_hash = ? AND code_payment_status = ?"

qBadgePrices :: Query
qBadgePrices =
  mkQuery $
    "SELECT price_id, badge_type, month_price, currency, status, created_at "
      <> "FROM @badge_prices WHERE status <> 'disabled'"

qBadgeOffers :: Query
qBadgeOffers =
  mkQuery $
    "SELECT offer_id, price_id, months, free_months, discount, status, created_at "
      <> "FROM @badge_offers WHERE status <> 'disabled'"

qSeedBadgePrice :: Query
qSeedBadgePrice =
  mkQuery $
    "INSERT INTO @badge_prices (price_id, badge_type, month_price, currency, status, created_at) "
      <> "VALUES (?,?,?,?,?,?) ON CONFLICT (price_id) DO NOTHING"

qSeedBadgeOffer :: Query
qSeedBadgeOffer =
  mkQuery $
    "INSERT INTO @badge_offers (offer_id, price_id, months, free_months, discount, status, created_at) "
      <> "VALUES (?,?,?,?,?,?,?) ON CONFLICT (offer_id) DO NOTHING"

providerText :: PaymentProvider -> Text
providerText = \case
  PPApple -> "apple"
  PPGoogle -> "google"
  PPStripe -> "stripe"
  PPCrypto -> "crypto"
  PPCode -> "code"
  PPReceipt -> "receipt"

textToProvider :: Text -> Maybe PaymentProvider
textToProvider = \case
  "apple" -> Just PPApple
  "google" -> Just PPGoogle
  "stripe" -> Just PPStripe
  "crypto" -> Just PPCrypto
  "code" -> Just PPCode
  "receipt" -> Just PPReceipt
  _ -> Nothing

cryptoCurrencyText :: CryptoCurrency -> Text
cryptoCurrencyText CCBtc = "btc"
cryptoCurrencyText CCXmr = "xmr"

textToCryptoCurrency :: Text -> Maybe CryptoCurrency
textToCryptoCurrency "btc" = Just CCBtc
textToCryptoCurrency "xmr" = Just CCXmr
textToCryptoCurrency _ = Nothing

invoiceStatusText :: InvoiceStatus -> Text
invoiceStatusText ISOpen = "open"
invoiceStatusText ISPaid = "paid"
invoiceStatusText ISExpired = "expired"

textToInvoiceStatus :: Text -> Maybe InvoiceStatus
textToInvoiceStatus "open" = Just ISOpen
textToInvoiceStatus "paid" = Just ISPaid
textToInvoiceStatus "expired" = Just ISExpired
textToInvoiceStatus _ = Nothing

itemStatusText :: BadgeItemStatus -> Text
itemStatusText BISActive = "active"
itemStatusText BISDeprecated = "deprecated"
itemStatusText BISDisabled = "disabled"

textToItemStatus :: Text -> Maybe BadgeItemStatus
textToItemStatus "active" = Just BISActive
textToItemStatus "deprecated" = Just BISDeprecated
textToItemStatus "disabled" = Just BISDisabled
textToItemStatus _ = Nothing

paymentStatusText :: PaymentStatus -> Text
paymentStatusText = \case
  PSPending -> "pending"
  PSSettled -> "settled"
  PSFailed _ -> "failed"

decodeDiscount :: Maybe Word8 -> Maybe Word8 -> Maybe OfferDiscount
decodeDiscount (Just f) _ = Just (ODFreeMonths f)
decodeDiscount Nothing (Just d) = Just (ODDiscount d)
decodeDiscount Nothing Nothing = Nothing

discountCols :: OfferDiscount -> (Maybe Word32, Maybe Word32)
discountCols (ODFreeMonths f) = (Just (fromIntegral f), Nothing)
discountCols (ODDiscount d) = (Nothing, Just (fromIntegral d))

mkDestination :: Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe ServicePaymentDestination
mkDestination (Just url) _ _ _ = Just (SPDCard CPStripe url)
mkDestination Nothing (Just addr) (Just curTxt) (Just amt) = (\cur -> SPDCrypto cur addr amt) <$> textToCryptoCurrency curTxt
mkDestination _ _ _ _ = Nothing

destinationCols :: ServicePaymentDestination -> (Maybe Text, Maybe Text, Maybe Text, Maybe Text)
destinationCols (SPDCard _ url) = (Just url, Nothing, Nothing, Nothing)
destinationCols (SPDCrypto cur addr amt) = (Nothing, Just addr, Just (cryptoCurrencyText cur), Just amt)

note :: Text -> Maybe a -> Either Text a
note ctx = maybe (Left ctx) Right

type InvoiceCols =
  (Text, Text, Text, Maybe BadgeType, Maybe Word32, Maybe Text)
    :. (Word32, Word32, Text, Maybe Text, Maybe Text, Maybe Text)
    :. (Maybe Text, UTCTime, Text, UTCTime)
    :. (Maybe Word32, Maybe Text, Maybe Text, Maybe Int, Maybe Text, Maybe UTCTime, Maybe Word32, Maybe Int64)

mkInvoiceRow :: InvoiceCols -> Either Text InvoiceRow
mkInvoiceRow
  ( (invId, providerTxt, providerRef, badgeType, months, registrationId)
      :. (price, amount, currency, url, addr, cryptoCur)
      :. (cryptoAmt, expiresAt, statusTxt, createdAt)
      :. (pAmount, pCryptoPaid, pCryptoDue, pPaidInFull, pStatus, pUpdatedAt, pConfirmedAmount, pConfirmedUnits)
    ) = do
    provider <- note "invoices.provider" (textToProvider providerTxt)
    invoiceProduct <- case (badgeType, months, registrationId) of
      (Just bt, Just m, Nothing) -> Right (IPBadge bt (fromIntegral m))
      (Nothing, Nothing, Just rid) -> Right (IPName (RegistrationId rid))
      _ -> Left ("invoice product: " <> invId)
    status <- note "invoices.status" (textToInvoiceStatus statusTxt)
    destination <- note "invoice payment destination" (mkDestination url addr cryptoCur cryptoAmt)
    pure
      InvoiceRow
        { irInvoiceId = InvoiceId invId,
          irProvider = provider,
          irProviderRef = providerRef,
          irProduct = invoiceProduct,
          irPrice = CurrencyAmount price,
          irAmount = CurrencyAmount amount,
          irCurrency = currency,
          irDestination = destination,
          irExpiresAt = expiresAt,
          irStatus = status,
          irCreatedAt = createdAt,
          irPayment = mkPayment pAmount pCryptoPaid pCryptoDue pPaidInFull pStatus pUpdatedAt pConfirmedAmount pConfirmedUnits
        }

mkPayment :: Maybe Word32 -> Maybe Text -> Maybe Text -> Maybe Int -> Maybe Text -> Maybe UTCTime -> Maybe Word32 -> Maybe Int64 -> Maybe InvoicePayment
mkPayment amt cryptoPaid cryptoDue paidInFull status updatedAt confirmedAmount confirmedUnits =
  InvoicePayment (CurrencyAmount <$> amt) cryptoPaid cryptoDue (paidInFull == Just 1)
    <$> status
    <*> updatedAt
    <*> ((\cents -> Confirmed {cfCents = CurrencyAmount cents, cfUnits = confirmedUnits}) <$> confirmedAmount)

selectInvoiceRow :: DB.Connection -> Query -> Text -> IO (Maybe InvoiceRow)
selectInvoiceRow db q param = do
  rows <- DB.query db q (Only param)
  case rows of
    [] -> pure Nothing
    (row : _) -> Just <$> decodeInvoiceRow row

decodeInvoiceRow :: InvoiceCols -> IO InvoiceRow
decodeInvoiceRow = either (E.throwIO . StoreDecodeError) pure . mkInvoiceRow

type BadgePriceCols = (Text, BadgeType, Word32, Text, Text, UTCTime)

mkBadgePrice :: BadgePriceCols -> Either Text BadgePrice
mkBadgePrice (priceId, badgeType, monthPrice, currency, statusTxt, createdAt) = do
  status <- note "badge_prices.status" (textToItemStatus statusTxt)
  pure
    BadgePrice
      { priceId = BadgePriceId priceId,
        badgeType,
        monthPrice = CurrencyAmount monthPrice,
        currency,
        status,
        createdAt
      }

type BadgeOfferCols = (Text, Maybe Text, Word32, Maybe Word32, Maybe Word32, Text, UTCTime)

mkBadgeOffer :: BadgeOfferCols -> Either Text BadgeOffer
mkBadgeOffer (offerId, priceId, months, freeMonths, discountPct, statusTxt, createdAt) = do
  status <- note "badge_offers.status" (textToItemStatus statusTxt)
  discount <- note "badge_offers discount" (decodeDiscount (fromIntegral <$> freeMonths) (fromIntegral <$> discountPct))
  pure
    BadgeOffer
      { offerId = BadgeOfferId offerId,
        priceId = BadgePriceId <$> priceId,
        months = fromIntegral months,
        discount,
        status,
        createdAt
      }

createInvoiceRows :: DBStore -> NewInvoice -> IO (Either CreateError ())
createInvoiceRows st ni =
  (Right <$> withTransaction st (`insertInvoiceRows` ni))
    `E.catch` (pure . Left . classifyCreateError)

insertInvoiceRows :: DB.Connection -> NewInvoice -> IO ()
insertInvoiceRows db NewInvoice {..} = do
  let InvoiceId invId = niInvoiceId
      CurrencyAmount price = niPrice
      CurrencyAmount amount = niAmount
      createdAt = truncateToSecond niCreatedAt
      BadgePriceId priceId = niPriceId
      offerId = (\(BadgeOfferId o) -> o) <$> niOfferId
      months = fromIntegral niMonths :: Word32
  insertInvoiceRow db niInvoiceId niProvider price amount niCurrency niDestination niExpiresAt createdAt
  DB.execute
    db
    qInsertBadgeCode
    (DB.Binary niCodeHash, niBadgeType, months, textEncode CPSUnpaid, createdAt)
  badgeCodeId <- insertedRowId db
  DB.execute
    db
    qInsertBadgeCodeInvoice
    (invId, badgeCodeId, priceId, offerId, niProviderRef, createdAt)

-- | A name is sold at its chain price, with no discount.
insertNameInvoice :: DB.Connection -> NewNameInvoice -> IO ()
insertNameInvoice db NewNameInvoice {..} =
  insertInvoiceRow db nniInvoiceId nniProvider amount amount nniCurrency nniDestination nniExpiresAt nniCreatedAt
  where
    CurrencyAmount amount = nniAmount

insertInvoiceRow :: DB.Connection -> InvoiceId -> PaymentProvider -> Word32 -> Word32 -> Text -> ServicePaymentDestination -> UTCTime -> UTCTime -> IO ()
insertInvoiceRow db (InvoiceId invId) provider price amount currency destination expiresAt' createdAt' =
  DB.execute
    db
    qInsertInvoice
    ( (invId, providerText provider, price, price - amount, Nothing :: Maybe Word32, amount, currency)
        :. (url, addr, cryptoCur, cryptoAmt, truncateToSecond expiresAt', invoiceStatusText ISOpen, createdAt, createdAt)
    )
  where
    (url, addr, cryptoCur, cryptoAmt) = destinationCols destination
    createdAt = truncateToSecond createdAt'

#if defined(dbPostgres)
classifyCreateError :: DB.SQLError -> CreateError
classifyCreateError e = case constraintViolation e of
  Just (UniqueViolation name)
    | "code_hash" `T.isInfixOf` nameText -> CECodeConflict
    | "provider_ref" `T.isInfixOf` nameText -> CERefConflict
    | otherwise -> CEOther (tshow e)
    where
      nameText = safeDecodeUtf8 name
  _ -> CEOther (tshow e)
#else
classifyCreateError :: DB.SQLError -> CreateError
classifyCreateError e
  | SQL.sqlError e == SQL.ErrorConstraint =
      if "code_hash" `T.isInfixOf` details
        then CECodeConflict
        else
          if "provider_ref" `T.isInfixOf` details
            then CERefConflict
            else CEOther details
  | otherwise = CEOther (tshow e)
  where
    details = SQL.sqlErrorDetails e
#endif

getInvoice :: DBStore -> InvoiceId -> IO (Maybe InvoiceRow)
getInvoice st (InvoiceId invId) = withConnection st $ \db -> selectInvoiceRow db qGetInvoice invId

-- | Throws ProviderRefConflict for a ref both a badge invoice and a name invoice hold.
getInvoiceByProviderRef :: DBStore -> Text -> IO (Maybe InvoiceRow)
getInvoiceByProviderRef st ref = withConnection st $ \db ->
  DB.query db qGetInvoiceByProviderRef (ref, ref) >>= \case
    [] -> pure Nothing
    [row] -> Just <$> decodeInvoiceRow row
    _ -> E.throwIO (ProviderRefConflict ref)

unpaidRefs :: DBStore -> UTCTime -> IO [(Text, Text)]
unpaidRefs st since = withConnection st $ \db -> DB.query db qUnpaidRefs (Only since)

codeHashExists :: DBStore -> ByteString -> IO Bool
codeHashExists st codeHash = withConnection st $ \db ->
  not . null <$> (DB.query db qCodeHashExists (Only (DB.Binary codeHash)) :: IO [Only Int])

data OverdueInvoice = OverdueInvoice
  { oiInvoiceId :: InvoiceId,
    oiProvider :: PaymentProvider,
    oiProviderRef :: Text,
    oiCreatedAt :: UTCTime
  }
  deriving (Eq, Show)

overdueInvoices :: DBStore -> UTCTime -> IO [OverdueInvoice]
overdueInvoices st cutoff = withConnection st $ \db -> do
  rows <- DB.query db qOverdueInvoices (Only (truncateToSecond cutoff))
  either (E.throwIO . StoreDecodeError) pure (traverse toOverdue rows)
  where
    toOverdue (i, providerTxt, oiProviderRef, oiCreatedAt) = case textToProvider providerTxt of
      Just oiProvider -> Right OverdueInvoice {oiInvoiceId = InvoiceId i, oiProvider, oiProviderRef, oiCreatedAt}
      Nothing -> Left ("invoices.provider: " <> providerTxt)

-- | Expires the invoice if it is still open, has received no money, and is past the cutoff.
expireOverdueInvoice :: DB.Connection -> UTCTime -> InvoiceId -> IO Bool
expireOverdueInvoice db cutoff' (InvoiceId i) = (> 0) <$> executeChanging db qExpireOverdue (cutoff, cutoff, i)
  where
    cutoff = truncateToSecond cutoff'

readCatalogRows :: DBStore -> IO ([BadgePrice], [BadgeOffer])
readCatalogRows st = withConnection st $ \db -> do
  priceRows <- DB.query_ db qBadgePrices
  offerRows <- DB.query_ db qBadgeOffers
  prices <- either (E.throwIO . StoreDecodeError) pure (traverse mkBadgePrice priceRows)
  offers <- either (E.throwIO . StoreDecodeError) pure (traverse mkBadgeOffer offerRows)
  pure (prices, offers)

seedCatalog :: DBStore -> [BadgePrice] -> [BadgeOffer] -> IO (Int, Int)
seedCatalog st prices offers = withTransaction st $ \db -> do
  seededPrices <- sum <$> mapM (seedPrice db) prices
  seededOffers <- sum <$> mapM (seedOffer db) offers
  pure (seededPrices, seededOffers)
  where
    seedPrice db BadgePrice {priceId = BadgePriceId pId, badgeType = bType, monthPrice = CurrencyAmount mPrice, currency = cur, status = pStatus, createdAt = at} =
      executeChanging db qSeedBadgePrice (pId, bType, mPrice, cur, itemStatusText pStatus, truncateToSecond at)
    seedOffer db BadgeOffer {offerId = BadgeOfferId oId, priceId = oPriceId, months = mMonths, discount = disc, status = oStatus, createdAt = at} =
      let (freeMonths, discountPct) = discountCols disc
       in executeChanging
            db
            qSeedBadgeOffer
            ( oId,
              (\(BadgePriceId p) -> p) <$> oPriceId,
              fromIntegral mMonths :: Word32,
              freeMonths,
              discountPct,
              itemStatusText oStatus,
              truncateToSecond at
            )

executeChanging :: ToRow q => DB.Connection -> Query -> q -> IO Int
#if defined(dbPostgres)
executeChanging db q params = fromIntegral <$> PSQL.execute db q params
#else
executeChanging db q params = DB.execute db q params >> SQL.changes (DB.conn db)
#endif

settlementInvoice :: DB.Connection -> InvoiceId -> IO (Maybe InvoiceRow)
settlementInvoice db (InvoiceId invId) = selectInvoiceRow db qGetInvoice invId

settlementCodeHash :: DB.Connection -> InvoiceId -> IO (Maybe ByteString)
settlementCodeHash db (InvoiceId invId) = do
  rows <- DB.query db qCodeHashForInvoice (Only invId) :: IO [Only (Maybe (DB.Binary ByteString))]
  pure $ case rows of
    (Only codeHash : _) -> DB.fromBinary <$> codeHash
    [] -> Nothing

upsertPayment :: DB.Connection -> InvoiceRow -> PaymentStatus -> Received -> Bool -> UTCTime -> IO ()
upsertPayment db InvoiceRow {irInvoiceId, irProvider, irProviderRef, irCurrency} status received paidInFull at' =
  DB.execute
    db
    qUpsertPayment
    ( (invId, invId, providerText irProvider, irProviderRef, amount)
        :. (irCurrency, rcvCrypto, rcvDue, if paidInFull then 1 :: Int else 0, paymentStatusText status)
        :. (rcvUnits, confirmedCents, cfUnits, rcvRate, at, at)
    )
  where
    InvoiceId invId = irInvoiceId
    Received {rcvAmount = CurrencyAmount amount, rcvCrypto, rcvDue, rcvUnits, rcvRate, rcvConfirmed = Confirmed {cfCents = CurrencyAmount confirmedCents, cfUnits}} = received
    at = truncateToSecond at'

-- | Takes the invoice row's lock for the rest of the transaction; SQLite's transactions hold the whole database already.
lockInvoice :: DB.Connection -> InvoiceId -> UTCTime -> IO ()
lockInvoice db (InvoiceId invId) at = DB.execute db qLockInvoice (truncateToSecond at, invId)

updateInvoiceStatus :: DB.Connection -> InvoiceId -> InvoiceStatus -> InvoiceStatus -> UTCTime -> IO Bool
updateInvoiceStatus db (InvoiceId invId) observed new at =
  (> 0) <$> executeChanging db qUpdateInvoiceStatus (invoiceStatusText new, truncateToSecond at, invId, invoiceStatusText observed)

markCodePaid :: DB.Connection -> ByteString -> UTCTime -> IO ()
markCodePaid db codeHash expiresAt =
  DB.execute
    db
    qMarkCodePaid
    (textEncode CPSPaid, truncateToSecond expiresAt, DB.Binary codeHash, textEncode CPSUnpaid)

newInvoiceId :: IO InvoiceId
newInvoiceId = InvoiceId . safeDecodeUtf8 . BC8.filter (/= '=') . B64U.encode <$> getRandomBytes 16

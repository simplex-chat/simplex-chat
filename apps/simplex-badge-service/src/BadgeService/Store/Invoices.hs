{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

module BadgeService.Store.Invoices
  ( InvoiceRow (..),
    InvoicePayment (..),
    paymentHolds,
    NewInvoice (..),
    CreateError (..),
    StoreDecodeError (..),
    createInvoiceRows,
    executeChanging,
    getInvoice,
    getInvoiceByProviderRef,
    unpaidRefs,
    providerText,
    codeHashExists,
    expireOverdue,
    cancelOpenInvoice,
    newInvoiceId,
    readCatalogRows,
    seedCatalog,
    truncateToSecond,
    settlementInvoice,
    settlementCodeHash,
    upsertPayment,
    paymentStatusText,
    cryptoCurrencyText,
    textToInvoiceStatus,
    invoiceStatusText,
    updateInvoiceStatus,
    markCodePaid,
  )
where

import Control.Exception (Exception)
import qualified Control.Exception as E
import Control.Monad (unless)
import Crypto.Random (getRandomBytes)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Base64.URL as B64U
import qualified Data.ByteString.Char8 as BC8
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import Data.Word (Word8, Word32)
import Simplex.Chat.Badges (BadgeType (..))
import Simplex.Chat.Badges.Service (BadgePrice (..), BadgeOffer (..))
import Simplex.Chat.Badges.Types (BadgeCodePaymentStatus (..), BadgeItemStatus (..), BadgeOfferId (..), BadgePriceId (..), OfferDiscount (..))
import Simplex.Chat.PaymentService.Types (CardProvider (..), CryptoCurrency (..), CurrencyAmount (..), InvoiceId (..), InvoiceStatus (..), PaymentProvider (..), PaymentStatus (..), ServicePaymentDestination (..))
import Simplex.Messaging.Agent.Store.Common (DBStore, withConnection, withTransaction)
import qualified Simplex.Messaging.Agent.Store.DB as DB
import Simplex.Messaging.Encoding.String (textEncode)
import Simplex.Messaging.Util (safeDecodeUtf8, tshow)

#if defined(dbPostgres)
import BadgeService.Store.Postgres.Migrations (servicePrefix, withPrefix)
import Database.PostgreSQL.Simple (Only (..), Query, ToRow, (:.) (..))
import qualified Database.PostgreSQL.Simple as PSQL
import Database.PostgreSQL.Simple.Errors (ConstraintViolation (..), constraintViolation)
#else
import BadgeService.Store.SQLite.Migrations (servicePrefix, withPrefix)
import Database.SQLite.Simple (Only (..), Query, ToRow, (:.) (..))
import qualified Database.SQLite.Simple as SQL
#endif

data InvoiceRow = InvoiceRow
  { irInvoiceId :: InvoiceId,
    irProvider :: PaymentProvider,
    irProviderRef :: Text,
    irBadgeType :: BadgeType,
    irMonths :: Word8,
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

-- | Whether anything the buyer sent is riding on this invoice. A crypto amount that rounds
-- to nothing still counts, and so does the provider saying it is paid before its figures move.
paymentHolds :: InvoicePayment -> Bool
paymentHolds InvoicePayment {ipAmount, ipCryptoAmount, ipPaidInFull} =
  maybe False (\(CurrencyAmount a) -> a > 0) ipAmount || ipCryptoAmount /= Nothing || ipPaidInFull

data InvoicePayment = InvoicePayment
  { ipAmount :: Maybe CurrencyAmount,
    ipCryptoAmount :: Maybe Text,
    ipCryptoDue :: Maybe Text,
    ipPaidInFull :: Bool,
    ipStatus :: Text,
    ipUpdatedAt :: UTCTime
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

data CreateError = CECodeConflict | CERefConflict | CEOther Text
  deriving (Eq, Show)

newtype StoreDecodeError = StoreDecodeError Text
  deriving (Eq, Show)

instance Exception StoreDecodeError

-- | SQLite keeps timestamps as text, so `expires_at < ?` sorts as strings and is only
-- chronological if every value is the same width. The listener imports this rather than
-- truncating separately, so the expiry it reports is the one we stored.
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
      <> "(invoice_id, price_id, offer_id, months, created_at, code_hash, provider_ref) "
      <> "VALUES (?,?,?,?,?,?,?)"

qInsertBadgeCode :: Query
qInsertBadgeCode =
  mkQuery $
    "INSERT INTO @badge_codes (code_hash, badge_type, months, code_payment_status, created_at) "
      <> "VALUES (?,?,?,?,?)"

invoiceRowSelect :: Text
invoiceRowSelect =
  "SELECT i.invoice_id, i.provider, ci.provider_ref, bc.badge_type, bc.months, "
    <> "i.price, i.amount, i.currency, i.payment_url, i.payment_address, i.payment_crypto_currency, "
    <> "i.payment_crypto_amount, i.expires_at, i.status, i.created_at, "
    <> "p.amount, p.crypto_amount, p.crypto_due, p.paid_in_full, p.status, p.updated_at "
    <> "FROM @invoices i "
    <> "JOIN @badge_code_invoices ci ON ci.invoice_id = i.invoice_id "
    <> "JOIN @badge_codes bc ON bc.code_hash = ci.code_hash "
    <> "LEFT JOIN @payments p ON p.invoice_id = i.invoice_id "

qGetInvoice :: Query
qGetInvoice = mkQuery (invoiceRowSelect <> "WHERE i.invoice_id = ?")

qGetInvoiceByProviderRef :: Query
qGetInvoiceByProviderRef = mkQuery (invoiceRowSelect <> "WHERE ci.provider_ref = ?")

-- | Every invoice we are still waiting on: not yet paid, and created recently enough to be worth
-- asking about. A paid row is finished and one created before the cutoff is past help, and
-- neither is asked about again. The provider comes back with the ref, so a row this build cannot
-- attribute to one is seen rather than skipped.
qUnpaidRefs :: Query
qUnpaidRefs =
  mkQuery $
    "SELECT i.provider, ci.provider_ref FROM @invoices i "
      <> "JOIN @badge_code_invoices ci ON ci.invoice_id = i.invoice_id "
      <> "WHERE i.status <> 'paid' AND i.created_at >= ? "
      <> "ORDER BY i.created_at"

qCodeHashExists :: Query
qCodeHashExists = mkQuery "SELECT 1 FROM @badge_codes WHERE code_hash = ? LIMIT 1"

-- | An invoice the buyer has already paid into is never swept, however long the chain
-- takes to confirm it: expiring it would take real money for a code that stays unpaid.
-- The rate hold is what the window bounds, and that stops mattering once payment lands.
unfundedOnly :: Text
unfundedOnly =
  "AND NOT EXISTS (SELECT 1 FROM @payments p "
    <> "WHERE p.invoice_id = @invoices.invoice_id AND ("
    <> "COALESCE(p.amount, 0) > 0 OR p.crypto_amount IS NOT NULL OR p.paid_in_full = 1)) "

qOverdueInvoiceIds :: Query
qOverdueInvoiceIds =
  mkQuery $
    "SELECT invoice_id FROM @invoices "
      <> "WHERE status = 'open' AND expires_at < ? "
      <> unfundedOnly
      <> "AND EXISTS (SELECT 1 FROM @badge_code_invoices ci WHERE ci.invoice_id = @invoices.invoice_id)"

qExpireOverdue :: Query
qExpireOverdue =
  mkQuery $
    "UPDATE @invoices SET status = 'expired', updated_at = ? "
      <> "WHERE status = 'open' AND expires_at < ? "
      <> unfundedOnly
      <> "AND EXISTS (SELECT 1 FROM @badge_code_invoices ci WHERE ci.invoice_id = @invoices.invoice_id)"

qCodeHashForInvoice :: Query
qCodeHashForInvoice = mkQuery "SELECT code_hash FROM @badge_code_invoices WHERE invoice_id = ?"

-- | SQLite's two-argument MAX is GREATEST in Postgres, where MAX is an aggregate.
largerOf :: Text
#if defined(dbPostgres)
largerOf = "GREATEST"
#else
largerOf = "MAX"
#endif

-- Amounts are running totals, so keeping the larger makes a repeated event harmless.
-- Once a row is settled it stays settled: without that guard a transaction that read the
-- invoice as open before the settling one committed would write pending back over it.
qUpsertPayment :: Query
qUpsertPayment =
  mkQuery $
    "INSERT INTO @payments "
      <> "(payment_id, invoice_id, provider, provider_ref, amount, currency, crypto_amount, crypto_due, paid_in_full, status, created_at, updated_at) "
      <> "VALUES (?,?,?,?,?,?,?,?,?,?,?,?) "
      <> "ON CONFLICT (payment_id) DO UPDATE SET "
      <> "amount = "
      <> largerOf
      <> "(COALESCE(@payments.amount, 0), excluded.amount), "
      <> "crypto_amount = CASE WHEN excluded.amount > COALESCE(@payments.amount, 0) OR @payments.crypto_amount IS NULL "
      <> "THEN excluded.crypto_amount ELSE @payments.crypto_amount END, "
      -- the provider recomputes what is owed on every read, so the newest answer always wins
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

-- Insert or do nothing, never update, so a price someone withdrew stays withdrawn.
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
  (Text, Text, Text, BadgeType, Word32)
    :. (Word32, Word32, Text, Maybe Text, Maybe Text, Maybe Text)
    :. (Maybe Text, UTCTime, Text, UTCTime)
    :. (Maybe Word32, Maybe Text, Maybe Text, Maybe Int, Maybe Text, Maybe UTCTime)

mkInvoiceRow :: InvoiceCols -> Either Text InvoiceRow
mkInvoiceRow
  ( (invId, providerTxt, providerRef, badgeType, months)
      :. (price, amount, currency, url, addr, cryptoCur)
      :. (cryptoAmt, expiresAt, statusTxt, createdAt)
      :. (pAmount, pCryptoAmount, pCryptoDue, pPaidInFull, pStatus, pUpdatedAt)
    ) = do
    provider <- note "invoices.provider" (textToProvider providerTxt)
    status <- note "invoices.status" (textToInvoiceStatus statusTxt)
    destination <- note "invoice payment destination" (mkDestination url addr cryptoCur cryptoAmt)
    pure
      InvoiceRow
        { irInvoiceId = InvoiceId invId,
          irProvider = provider,
          irProviderRef = providerRef,
          irBadgeType = badgeType,
          irMonths = fromIntegral months,
          irPrice = CurrencyAmount price,
          irAmount = CurrencyAmount amount,
          irCurrency = currency,
          irDestination = destination,
          irExpiresAt = expiresAt,
          irStatus = status,
          irCreatedAt = createdAt,
          irPayment = mkPayment pAmount pCryptoAmount pCryptoDue pPaidInFull pStatus pUpdatedAt
        }

mkPayment :: Maybe Word32 -> Maybe Text -> Maybe Text -> Maybe Int -> Maybe Text -> Maybe UTCTime -> Maybe InvoicePayment
mkPayment amt cryptoAmt cryptoDue paidInFull status updatedAt =
  InvoicePayment (CurrencyAmount <$> amt) cryptoAmt cryptoDue (paidInFull == Just 1) <$> status <*> updatedAt

selectInvoiceRow :: DB.Connection -> Query -> Text -> IO (Maybe InvoiceRow)
selectInvoiceRow db q param = do
  rows <- DB.query db q (Only param)
  case rows of
    [] -> pure Nothing
    (row : _) -> either (E.throwIO . StoreDecodeError) (pure . Just) (mkInvoiceRow row)

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

-- | The caller's code-hash check and this write are not one operation, so two requests
-- with the same hash can both arrive. The UNIQUE index is what stops the duplicate.
createInvoiceRows :: DBStore -> NewInvoice -> IO (Either CreateError ())
createInvoiceRows st ni =
  (Right <$> withTransaction st (`insertInvoiceRows` ni))
    `E.catch` (pure . Left . classifyCreateError)

insertInvoiceRows :: DB.Connection -> NewInvoice -> IO ()
insertInvoiceRows db NewInvoice {..} = do
  let InvoiceId invId = niInvoiceId
      CurrencyAmount price = niPrice
      CurrencyAmount amount = niAmount
      discountAmount = price - amount
      (url, addr, cryptoCur, cryptoAmt) = destinationCols niDestination
      expiresAt = truncateToSecond niExpiresAt
      createdAt = truncateToSecond niCreatedAt
      BadgePriceId priceId = niPriceId
      offerId = (\(BadgeOfferId o) -> o) <$> niOfferId
      months = fromIntegral niMonths :: Word32
  DB.execute
    db
    qInsertInvoice
    ( (invId, providerText niProvider, price, discountAmount, Nothing :: Maybe Word32, amount, niCurrency)
        :. (url, addr, cryptoCur, cryptoAmt, expiresAt, invoiceStatusText ISOpen, createdAt, createdAt)
    )
  DB.execute
    db
    qInsertBadgeCodeInvoice
    (invId, priceId, offerId, months, createdAt, DB.Binary niCodeHash, niProviderRef)
  DB.execute
    db
    qInsertBadgeCode
    (DB.Binary niCodeHash, niBadgeType, months, textEncode CPSUnpaid, createdAt)

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

getInvoiceByProviderRef :: DBStore -> Text -> IO (Maybe InvoiceRow)
getInvoiceByProviderRef st ref = withConnection st $ \db -> selectInvoiceRow db qGetInvoiceByProviderRef ref

unpaidRefs :: DBStore -> UTCTime -> IO [(Text, Text)]
unpaidRefs st since = withConnection st $ \db -> DB.query db qUnpaidRefs (Only since)

codeHashExists :: DBStore -> ByteString -> IO Bool
codeHashExists st codeHash = withConnection st $ \db ->
  not . null <$> (DB.query db qCodeHashExists (Only (DB.Binary codeHash)) :: IO [Only Int])

-- | Returns which invoices it expired, so the caller can wake browsers waiting on them.
expireOverdue :: DBStore -> UTCTime -> IO [InvoiceId]
expireOverdue st cutoff' = withTransaction st $ \db -> do
  let cutoff = truncateToSecond cutoff'
  ids <- DB.query db qOverdueInvoiceIds (Only cutoff) :: IO [Only Text]
  unless (null ids) $ DB.execute db qExpireOverdue (cutoff, cutoff)
  pure (map (\(Only i) -> InvoiceId i) ids)

-- | Expires an open invoice ahead of its clock. Unlike the sweep this does not spare a funded
-- one: the provider has already been told to invalidate it by the time this runs, so leaving the
-- row open would advertise an address nothing can be sent to. What did arrive is on the payment
-- row, and the poller settles it or reports it for a refund.
cancelOpenInvoice :: DBStore -> InvoiceId -> UTCTime -> IO Bool
cancelOpenInvoice st invId at = withTransaction st $ \db -> updateInvoiceStatus db invId ISOpen ISExpired at

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

-- | 'DB.execute' throws the row count away on both backends, so we ask the driver.
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

upsertPayment :: DB.Connection -> InvoiceRow -> PaymentStatus -> CurrencyAmount -> Maybe Text -> Maybe Text -> Bool -> UTCTime -> IO ()
upsertPayment db InvoiceRow {irInvoiceId, irProvider, irProviderRef, irCurrency} status (CurrencyAmount amount) cryptoAmount cryptoDue paidInFull at' =
  DB.execute
    db
    qUpsertPayment
    ( (invId, invId, providerText irProvider, irProviderRef, amount)
        :. (irCurrency, cryptoAmount, cryptoDue, if paidInFull then 1 :: Int else 0, paymentStatusText status, at, at)
    )
  where
    InvoiceId invId = irInvoiceId
    at = truncateToSecond at'

updateInvoiceStatus :: DB.Connection -> InvoiceId -> InvoiceStatus -> InvoiceStatus -> UTCTime -> IO Bool
updateInvoiceStatus db (InvoiceId invId) observed new at =
  (> 0) <$> executeChanging db qUpdateInvoiceStatus (invoiceStatusText new, truncateToSecond at, invId, invoiceStatusText observed)

markCodePaid :: DB.Connection -> ByteString -> UTCTime -> IO ()
markCodePaid db codeHash expiresAt =
  DB.execute
    db
    qMarkCodePaid
    (textEncode CPSPaid, truncateToSecond expiresAt, DB.Binary codeHash, textEncode CPSUnpaid)

-- | Anyone holding this can read the order, so it comes from the CSPRNG.
newInvoiceId :: IO InvoiceId
newInvoiceId = InvoiceId . safeDecodeUtf8 . BC8.filter (/= '=') . B64U.encode <$> getRandomBytes 16

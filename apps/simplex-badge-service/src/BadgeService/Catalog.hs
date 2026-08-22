{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The single source of badge pricing (decision 8): the default catalog values, the one
-- function that turns a price and an offer into a total, and idempotent seeding into the
-- badge service's own database. No other module computes a total; every response that
-- leaves the service (B6's RPC, D4's /api/catalog) must go through 'catalogTotals'.
module BadgeService.Catalog
  ( defaultCatalog,
    offerTotal,
    catalogTotals,
    seedCatalog,
  )
where

import Data.List (find)
import Data.Text (Text)
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.Word (Word8)
import Simplex.Chat.Badges (BadgeType (..))
import Simplex.Chat.Badges.Service (BadgeCatalog (..), BadgeOffer (..), BadgePrice (..))
import Simplex.Chat.Badges.Types (BadgeItemStatus (..), BadgeOfferId (..), BadgePriceId (..), OfferDiscount (..))
import Simplex.Chat.PaymentService.Types (CurrencyAmount (..))
import Simplex.Messaging.Agent.Store.Common (DBStore, withTransaction)
import qualified Simplex.Messaging.Agent.Store.DB as DB
import Simplex.Messaging.Encoding.String (textEncode)

-- Prices and offers are seeded by literal id so re-seeding is idempotent: a fresh restart
-- inserts the same rows it always did instead of minting new ones every time.

supporterPriceId :: BadgePriceId
supporterPriceId = BadgePriceId "2170da16-66e5-481f-9c75-6949e2dd14e1"

legendPriceId :: BadgePriceId
legendPriceId = BadgePriceId "6a778279-753e-45db-8acc-0e382c1d054a"

supporter3MonthsOfferId :: BadgeOfferId
supporter3MonthsOfferId = BadgeOfferId "29e35444-2f85-43fb-8933-16875e6d3776"

supporter12MonthsOfferId :: BadgeOfferId
supporter12MonthsOfferId = BadgeOfferId "71bd8ad1-15c4-4735-b3d4-b12a670dfb7e"

legend3MonthsOfferId :: BadgeOfferId
legend3MonthsOfferId = BadgeOfferId "88885a0a-c407-4aaa-bf53-90f9de7bdfc0"

legend12MonthsOfferId :: BadgeOfferId
legend12MonthsOfferId = BadgeOfferId "35ca9daa-4dca-4cc1-af74-631674906cc9"

-- | The default catalog: two prices (UX §1: supporter $7/month, legend $70/month) and four
-- offers pinned to them, one per badge type and duration (UX §6.12's 1x / 2x / 6x monthly
-- pricing). One month has no offer and is priced at 'monthPrice' (core §4). The 3-month
-- offer is 'ODFreeMonths' 1, never 'ODDiscount': at a 700 minor-unit monthly price, the
-- required 1400 total sits strictly between 'ODDiscount' 33 (1407) and 'ODDiscount' 34
-- (1386), so no 'Word8' percent can express it.
defaultCatalog :: UTCTime -> BadgeCatalog
defaultCatalog createdAt =
  BadgeCatalog
    { prices = [supporterPrice, legendPrice],
      offers = [supporter3Months, supporter12Months, legend3Months, legend12Months]
    }
  where
    supporterPrice =
      BadgePrice
        { priceId = supporterPriceId,
          badgeType = BTSupporter,
          monthPrice = CurrencyAmount 700,
          currency = "usd",
          status = BISActive,
          createdAt
        }
    legendPrice =
      BadgePrice
        { priceId = legendPriceId,
          badgeType = BTLegend,
          monthPrice = CurrencyAmount 7000,
          currency = "usd",
          status = BISActive,
          createdAt
        }
    supporter3Months =
      BadgeOffer
        { offerId = supporter3MonthsOfferId,
          priceId = Just supporterPriceId,
          months = 3,
          discount = ODFreeMonths 1,
          status = BISActive,
          createdAt,
          total = Nothing
        }
    supporter12Months =
      BadgeOffer
        { offerId = supporter12MonthsOfferId,
          priceId = Just supporterPriceId,
          months = 12,
          discount = ODFreeMonths 6,
          status = BISActive,
          createdAt,
          total = Nothing
        }
    legend3Months =
      BadgeOffer
        { offerId = legend3MonthsOfferId,
          priceId = Just legendPriceId,
          months = 3,
          discount = ODFreeMonths 1,
          status = BISActive,
          createdAt,
          total = Nothing
        }
    legend12Months =
      BadgeOffer
        { offerId = legend12MonthsOfferId,
          priceId = Just legendPriceId,
          months = 12,
          discount = ODFreeMonths 6,
          status = BISActive,
          createdAt,
          total = Nothing
        }

-- | The only place a total is computed. 'CurrencyAmount' has no 'Num' instance, so every
-- step unwraps to 'Word32', computes, and re-wraps. 'Nothing' means exactly one month
-- (there is no unpriced multi-month path: a longer duration is only ever expressed as an
-- offer). A 'freeMonths' offer charges for the months that aren't free; an 'ODDiscount'
-- offer floors the discounted total, computed over integers so no floating point appears
-- anywhere in the pricing path.
offerTotal :: BadgePrice -> Maybe BadgeOffer -> CurrencyAmount
offerTotal BadgePrice {monthPrice = CurrencyAmount monthPriceMinor} Nothing =
  CurrencyAmount monthPriceMinor
offerTotal BadgePrice {monthPrice = CurrencyAmount monthPriceMinor} (Just BadgeOffer {months, discount}) =
  CurrencyAmount $ case discount of
    ODFreeMonths freeMonths -> fromIntegral (months - freeMonths) * monthPriceMinor
    ODDiscount percent -> (fromIntegral months * monthPriceMinor * fromIntegral (100 - percent)) `div` 100

-- | Fills every offer's 'total' (A2) with 'offerTotal' applied to that offer's pinned
-- price. Overwrites unconditionally, so it is idempotent to call again. It is a total
-- function: an offer whose price isn't found in the given catalog (which shouldn't happen,
-- relying on B1's invariant that every returned offer's pinned price is also returned) gets
-- 'total = Nothing' rather than a crash, same as an unpinned offer.
catalogTotals :: BadgeCatalog -> BadgeCatalog
catalogTotals BadgeCatalog {prices, offers} =
  BadgeCatalog {prices, offers = map fillTotal offers}
  where
    fillTotal offer@BadgeOffer {priceId} =
      offer {total = offerTotal <$> pricedBy priceId <*> pure (Just offer)}
    pricedBy Nothing = Nothing
    pricedBy (Just pid) = find (\BadgePrice {priceId = pid'} -> pid' == pid) prices

-- | Inserts the default catalog's prices and offers, by literal id, into the badge
-- service's own tables. Never updates or deletes an existing row: repricing appends a new
-- price and deprecates the old one (UX §3) via B1's 'setPriceStatus', not a seed edit, so a
-- price deprecated out from under a re-seed stays deprecated.
seedCatalog :: DBStore -> IO ()
seedCatalog st = do
  createdAt <- getCurrentTime
  let BadgeCatalog {prices, offers} = defaultCatalog createdAt
  withTransaction st $ \db -> do
    mapM_ (insertPrice db) prices
    mapM_ (insertOffer db) offers

insertPrice :: DB.Connection -> BadgePrice -> IO ()
insertPrice db BadgePrice {priceId = BadgePriceId pid, badgeType, monthPrice = CurrencyAmount amt, currency, status, createdAt} =
  DB.execute
    db
    "INSERT INTO sx_badge_service_badge_prices (price_id, badge_type, month_price, currency, status, created_at) \
    \VALUES (?,?,?,?,?,?) ON CONFLICT (price_id) DO NOTHING"
    (pid, textEncode badgeType, amt, currency, itemStatusText status, createdAt)

insertOffer :: DB.Connection -> BadgeOffer -> IO ()
insertOffer db BadgeOffer {offerId = BadgeOfferId oid, priceId, months, discount, status, createdAt} =
  DB.execute
    db
    "INSERT INTO sx_badge_service_badge_offers (offer_id, price_id, months, free_months, discount, status, created_at) \
    \VALUES (?,?,?,?,?,?,?) ON CONFLICT (offer_id) DO NOTHING"
    (oid, unBadgePriceId <$> priceId, months, freeMonthsColumn, discountColumn, itemStatusText status, createdAt)
  where
    unBadgePriceId (BadgePriceId pid) = pid
    (freeMonthsColumn, discountColumn) = case discount of
      ODFreeMonths freeMonths -> (Just freeMonths, Nothing :: Maybe Word8)
      ODDiscount percent -> (Nothing :: Maybe Word8, Just percent)

-- Not a TextEncoding instance: BadgeItemStatus has no wire representation of its own
-- outside JSON (see Badges/Types.hs), and this spelling only ever round-trips through the
-- column it is written to.
itemStatusText :: BadgeItemStatus -> Text
itemStatusText = \case
  BISActive -> "active"
  BISDeprecated -> "deprecated"
  BISDisabled -> "disabled"

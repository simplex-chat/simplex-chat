{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module BadgeService.Catalog
  ( OfferInvalid (..),
    CatalogRefusal (..),
    offerTotal,
    PricedOffer (..),
    priceOffer,
    catalogCurrency,
    defaultCatalog,
  )
where

import Data.List (find)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Data.Word (Word32, Word64, Word8)
import Simplex.Chat.Badges (BadgeType (..))
import Simplex.Chat.Badges.Service (BadgeOffer (..), BadgePrice (..))
import Simplex.Chat.Badges.Types (BadgeItemStatus (..), BadgeOfferId (..), BadgePriceId (..), OfferDiscount (..))
import Simplex.Chat.PaymentService.Types (CurrencyAmount (..))

data OfferInvalid = OIZeroMonths | OIFreeMonthsExceedTerm | OIDiscountTooLarge | OIAmountUnsellable
  deriving (Eq, Show)

-- | The full price is formed before the division, so nothing is rounded part way.
offerTotal :: CurrencyAmount -> Maybe BadgeOffer -> Either OfferInvalid (Word8, CurrencyAmount, CurrencyAmount)
offerTotal (CurrencyAmount p) = \case
  Nothing -> charge 1 (gross 1)
  Just BadgeOffer {months, discount}
    | months == 0 -> Left OIZeroMonths
    | otherwise -> case discount of
        ODFreeMonths f
          | f >= months -> Left OIFreeMonthsExceedTerm
          | otherwise -> charge months (gross (months - f))
        ODDiscount d
          | d >= 100 -> Left OIDiscountTooLarge
          | otherwise -> charge months (gross months * (100 - fromIntegral d) `div` 100)
  where
    gross :: Word8 -> Word64
    gross m = fromIntegral p * fromIntegral m
    maxAmount :: Word64 -- $1,000,000 in minor units
    maxAmount = 100000000
    charge :: Word8 -> Word64 -> Either OfferInvalid (Word8, CurrencyAmount, CurrencyAmount)
    charge m c
      -- both figures, not just the charge: 100000000 a month over 43 months with 42 free
      -- charges 100000000, but the full price wraps a Word32 and the discount underflows
      | c == 0 || c > maxAmount || gross m > maxAmount = Left OIAmountUnsellable
      | otherwise = Right (m, CurrencyAmount (fromIntegral (gross m)), CurrencyAmount (fromIntegral c))

data PricedOffer = PricedOffer
  { poBadgeType :: BadgeType,
    poMonths :: Word8,
    poPrice :: CurrencyAmount,
    poAmount :: CurrencyAmount,
    poCurrency :: Text
  }
  deriving (Eq, Show)

data CatalogRefusal
  = CRUnknownPrice
  | CRDisabledPrice
  | CRUnknownOffer
  | CRDisabledOffer
  | CROfferNotForPrice
  | CRUnsoldBadgeType BadgeType
  | CRUnpriced OfferInvalid
  deriving (Eq, Show)

priceOffer :: [BadgePrice] -> [BadgeOffer] -> BadgePriceId -> Maybe BadgeOfferId -> Either CatalogRefusal PricedOffer
priceOffer prices offers wantedPriceId wantedOfferId = do
  BadgePrice {badgeType, monthPrice, currency, status = priceStatus} <- known CRUnknownPrice (findPrice wantedPriceId)
  soldBadgeType badgeType
  active CRDisabledPrice priceStatus
  chosenOffer <- resolveOffer wantedPriceId wantedOfferId
  (months, price, amount) <- either (Left . CRUnpriced) Right (offerTotal monthPrice chosenOffer)
  pure PricedOffer {poBadgeType = badgeType, poMonths = months, poPrice = price, poAmount = amount, poCurrency = currency}
  where
    known refusal = maybe (Left refusal) Right
    findPrice p = find (\BadgePrice {priceId} -> priceId == p) prices
    findOffer o = find (\BadgeOffer {offerId} -> offerId == o) offers
    soldBadgeType bt
      | bt == BTSupporter || bt == BTLegend = Right ()
      | otherwise = Left (CRUnsoldBadgeType bt)
    active _ BISActive = Right ()
    active _ BISDeprecated = Right ()
    active refusal BISDisabled = Left refusal
    resolveOffer _ Nothing = Right Nothing
    resolveOffer p (Just o) = do
      chosen@BadgeOffer {priceId = offerPriceId, status = offerStatus} <- known CRUnknownOffer (findOffer o)
      active CRDisabledOffer offerStatus
      case offerPriceId of
        Nothing -> Right (Just chosen)
        Just op
          | op == p -> Right (Just chosen)
          | otherwise -> Left CROfferNotForPrice

catalogCurrency :: Text
catalogCurrency = "usd"

-- | Copied from web/src/catalog.ts, and a test checks they still agree. Seeded
-- insert-only, so repricing means a new price id here plus deprecating the old row.
defaultCatalog :: UTCTime -> ([BadgePrice], [BadgeOffer])
defaultCatalog seededAt = (prices, offers)
  where
    prices =
      [ mkPrice "price_supporter" BTSupporter 700,
        mkPrice "price_legend" BTLegend 7000
      ]
    offers =
      [ mkOffer "offer_3m" "price_legend" 3 (ODFreeMonths 1),
        mkOffer "offer_12m" "price_legend" 12 (ODDiscount 50),
        mkOffer "offer_3m_s" "price_supporter" 3 (ODFreeMonths 1),
        mkOffer "offer_12m_s" "price_supporter" 12 (ODDiscount 50)
      ]
    mkPrice :: Text -> BadgeType -> Word32 -> BadgePrice
    mkPrice pId bType mPrice =
      BadgePrice
        { priceId = BadgePriceId pId,
          badgeType = bType,
          monthPrice = CurrencyAmount mPrice,
          currency = catalogCurrency,
          status = BISActive,
          createdAt = seededAt
        }
    mkOffer :: Text -> Text -> Word8 -> OfferDiscount -> BadgeOffer
    mkOffer oId pId mMonths mDiscount =
      BadgeOffer
        { offerId = BadgeOfferId oId,
          priceId = Just (BadgePriceId pId),
          months = mMonths,
          discount = mDiscount,
          status = BISActive,
          createdAt = seededAt
        }

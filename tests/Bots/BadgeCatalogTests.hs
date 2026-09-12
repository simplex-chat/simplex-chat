{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Bots.BadgeCatalogTests where

import BadgeService.Catalog
import Control.Monad (when)
import Data.List (find)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Text.Read (decimal)
import Data.Time.Clock (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Word (Word32, Word8)
import Simplex.Chat.Badges (BadgeType (..))
import Simplex.Chat.Badges.Service (BadgeOffer (..), BadgePrice (..))
import Simplex.Chat.Badges.Types (BadgeItemStatus (..), BadgeOfferId (..), BadgePriceId (..), OfferDiscount (..))
import Simplex.Chat.PaymentService.Types (CurrencyAmount (..))
import Test.Hspec

badgeCatalogTests :: Spec
badgeCatalogTests = describe "badge catalog" $ do
  it "prices the six sellable pairs at their literal totals" testLiteralTotals
  it "delivers the free months it does not charge for" testFreeMonths
  it "truncates a percentage discount in the buyer's favour" testDiscountTruncates
  it "refuses a pair it cannot price" testRefusals
  it "refuses a gross that would wrap, even where the charge is sellable" testGrossIsBounded
  it "agrees with the catalog compiled into the page" testNoDriftFromWeb
  it "refuses an unknown priceId" testPriceOfferUnknownPrice
  it "refuses a badge type this service does not sell" testPriceOfferUnsoldBadgeType
  it "refuses a disabled price" testPriceOfferDisabledPrice
  it "refuses an unknown offerId" testPriceOfferUnknownOffer
  it "refuses a disabled offer" testPriceOfferDisabledOffer
  it "refuses an offer that names a different price" testPriceOfferMismatchedPrice
  it "refuses a pair offerTotal itself cannot price" testPriceOfferUnpriced
  it "refuses on the badge type before it checks whether the price is disabled" testPriceOfferOrderUnsoldBeforeDisabled
  it "prices a deprecated price rather than refusing it" testPriceOfferDeprecatedPricePriced
  it "prices a deprecated offer rather than refusing it" testPriceOfferDeprecatedOfferPriced
  it "carries the term's gross, not the per-month rate, for a free-months offer" testPriceOfferGrossFreeMonths
  it "carries the term's gross, not the per-month rate, for a percentage-discount offer" testPriceOfferGrossDiscount
  it "applies an offer with no priceId to any price" testPriceOfferAnyPrice

offer :: Word8 -> OfferDiscount -> BadgeOffer
offer months discount =
  BadgeOffer {offerId = BadgeOfferId "o", priceId = Nothing, months, discount, status = BISActive, createdAt = epoch}

epoch :: UTCTime
epoch = posixSecondsToUTCTime 0

testLiteralTotals :: IO ()
testLiteralTotals = do
  amountOf 700 Nothing `shouldBe` Right 700
  amountOf 700 (Just (offer 3 (ODFreeMonths 1))) `shouldBe` Right 1400
  amountOf 700 (Just (offer 12 (ODDiscount 50))) `shouldBe` Right 4200
  amountOf 7000 Nothing `shouldBe` Right 7000
  amountOf 7000 (Just (offer 3 (ODFreeMonths 1))) `shouldBe` Right 14000
  amountOf 7000 (Just (offer 12 (ODDiscount 50))) `shouldBe` Right 42000
  where
    amountOf p o = (\(_, _, CurrencyAmount a) -> a) <$> offerTotal (CurrencyAmount p) o

testFreeMonths :: IO ()
testFreeMonths =
  offerTotal (CurrencyAmount 7000) (Just (offer 3 (ODFreeMonths 1)))
    `shouldBe` Right (3, CurrencyAmount 21000, CurrencyAmount 14000)

testDiscountTruncates :: IO ()
testDiscountTruncates =
  offerTotal (CurrencyAmount 333) (Just (offer 3 (ODDiscount 33)))
    `shouldBe` Right (3, CurrencyAmount 999, CurrencyAmount 669)

testRefusals :: IO ()
testRefusals = do
  offerTotal (CurrencyAmount 700) (Just (offer 0 (ODDiscount 10))) `shouldBe` Left OIZeroMonths
  offerTotal (CurrencyAmount 700) (Just (offer 3 (ODFreeMonths 3))) `shouldBe` Left OIFreeMonthsExceedTerm
  offerTotal (CurrencyAmount 700) (Just (offer 3 (ODDiscount 100))) `shouldBe` Left OIDiscountTooLarge
  offerTotal (CurrencyAmount 0) Nothing `shouldBe` Left OIAmountUnsellable
  offerTotal (CurrencyAmount 20000000) (Just (offer 12 (ODDiscount 50))) `shouldBe` Left OIAmountUnsellable

testGrossIsBounded :: IO ()
testGrossIsBounded = do
  let chargedOf p o = (\(_, _, CurrencyAmount a) -> a) <$> offerTotal (CurrencyAmount p) o
  chargedOf 100000000 (Just (offer 1 (ODDiscount 0))) `shouldBe` Right 100000000
  offerTotal (CurrencyAmount 100000000) (Just (offer 43 (ODFreeMonths 42))) `shouldBe` Left OIAmountUnsellable
  offerTotal (CurrencyAmount 100000000) (Just (offer 2 (ODDiscount 50))) `shouldBe` Left OIAmountUnsellable
  offerTotal (CurrencyAmount 100000000) Nothing
    `shouldBe` Right (1, CurrencyAmount 100000000, CurrencyAmount 100000000)

mkPrice :: T.Text -> BadgeType -> Word32 -> BadgeItemStatus -> BadgePrice
mkPrice priceIdText priceBadgeType monthPriceAmount priceStatus =
  BadgePrice
    { priceId = BadgePriceId priceIdText,
      badgeType = priceBadgeType,
      monthPrice = CurrencyAmount monthPriceAmount,
      currency = "usd",
      status = priceStatus,
      createdAt = epoch
    }

mkOffer :: T.Text -> Maybe T.Text -> Word8 -> OfferDiscount -> BadgeItemStatus -> BadgeOffer
mkOffer offerIdText offerPriceIdText offerMonths offerDiscount offerStatus =
  BadgeOffer
    { offerId = BadgeOfferId offerIdText,
      priceId = BadgePriceId <$> offerPriceIdText,
      months = offerMonths,
      discount = offerDiscount,
      status = offerStatus,
      createdAt = epoch
    }

testPriceOfferUnknownPrice :: IO ()
testPriceOfferUnknownPrice =
  priceOffer [] [] (BadgePriceId "missing") Nothing `shouldBe` Left CRUnknownPrice

testPriceOfferUnsoldBadgeType :: IO ()
testPriceOfferUnsoldBadgeType =
  priceOffer [mkPrice "p1" BTInvestor 1000 BISActive] [] (BadgePriceId "p1") Nothing
    `shouldBe` Left (CRUnsoldBadgeType BTInvestor)

testPriceOfferDisabledPrice :: IO ()
testPriceOfferDisabledPrice =
  priceOffer [mkPrice "p1" BTSupporter 700 BISDisabled] [] (BadgePriceId "p1") Nothing
    `shouldBe` Left CRDisabledPrice

testPriceOfferUnknownOffer :: IO ()
testPriceOfferUnknownOffer =
  priceOffer [mkPrice "p1" BTSupporter 700 BISActive] [] (BadgePriceId "p1") (Just (BadgeOfferId "missing"))
    `shouldBe` Left CRUnknownOffer

testPriceOfferDisabledOffer :: IO ()
testPriceOfferDisabledOffer =
  priceOffer
    [mkPrice "p1" BTSupporter 700 BISActive]
    [mkOffer "o1" (Just "p1") 3 (ODFreeMonths 1) BISDisabled]
    (BadgePriceId "p1")
    (Just (BadgeOfferId "o1"))
    `shouldBe` Left CRDisabledOffer

testPriceOfferMismatchedPrice :: IO ()
testPriceOfferMismatchedPrice =
  priceOffer
    [mkPrice "p1" BTSupporter 700 BISActive, mkPrice "p2" BTSupporter 700 BISActive]
    [mkOffer "o1" (Just "p2") 3 (ODFreeMonths 1) BISActive]
    (BadgePriceId "p1")
    (Just (BadgeOfferId "o1"))
    `shouldBe` Left CROfferNotForPrice

testPriceOfferUnpriced :: IO ()
testPriceOfferUnpriced =
  priceOffer
    [mkPrice "p1" BTSupporter 700 BISActive]
    [mkOffer "o1" (Just "p1") 0 (ODDiscount 10) BISActive]
    (BadgePriceId "p1")
    (Just (BadgeOfferId "o1"))
    `shouldBe` Left (CRUnpriced OIZeroMonths)

testPriceOfferOrderUnsoldBeforeDisabled :: IO ()
testPriceOfferOrderUnsoldBeforeDisabled =
  priceOffer [mkPrice "p1" BTInvestor 1000 BISDisabled] [] (BadgePriceId "p1") Nothing
    `shouldBe` Left (CRUnsoldBadgeType BTInvestor)

testPriceOfferDeprecatedPricePriced :: IO ()
testPriceOfferDeprecatedPricePriced =
  priceOffer [mkPrice "p1" BTSupporter 700 BISDeprecated] [] (BadgePriceId "p1") Nothing
    `shouldBe` Right PricedOffer {poBadgeType = BTSupporter, poMonths = 1, poPrice = CurrencyAmount 700, poAmount = CurrencyAmount 700, poCurrency = "usd"}

testPriceOfferDeprecatedOfferPriced :: IO ()
testPriceOfferDeprecatedOfferPriced =
  priceOffer
    [mkPrice "p1" BTSupporter 700 BISActive]
    [mkOffer "o1" (Just "p1") 3 (ODFreeMonths 1) BISDeprecated]
    (BadgePriceId "p1")
    (Just (BadgeOfferId "o1"))
    `shouldBe` Right PricedOffer {poBadgeType = BTSupporter, poMonths = 3, poPrice = CurrencyAmount 2100, poAmount = CurrencyAmount 1400, poCurrency = "usd"}

testPriceOfferGrossFreeMonths :: IO ()
testPriceOfferGrossFreeMonths =
  priceOffer
    [mkPrice "p1" BTLegend 7000 BISActive]
    [mkOffer "o1" (Just "p1") 3 (ODFreeMonths 1) BISActive]
    (BadgePriceId "p1")
    (Just (BadgeOfferId "o1"))
    `shouldBe` Right PricedOffer {poBadgeType = BTLegend, poMonths = 3, poPrice = CurrencyAmount 21000, poAmount = CurrencyAmount 14000, poCurrency = "usd"}

testPriceOfferGrossDiscount :: IO ()
testPriceOfferGrossDiscount =
  priceOffer
    [mkPrice "p1" BTLegend 7000 BISActive]
    [mkOffer "o1" (Just "p1") 12 (ODDiscount 50) BISActive]
    (BadgePriceId "p1")
    (Just (BadgeOfferId "o1"))
    `shouldBe` Right PricedOffer {poBadgeType = BTLegend, poMonths = 12, poPrice = CurrencyAmount 84000, poAmount = CurrencyAmount 42000, poCurrency = "usd"}

testPriceOfferAnyPrice :: IO ()
testPriceOfferAnyPrice = do
  let prices = [mkPrice "p1" BTSupporter 700 BISActive, mkPrice "p2" BTSupporter 700 BISActive]
      offers = [mkOffer "o1" Nothing 3 (ODFreeMonths 1) BISActive]
      expected = Right PricedOffer {poBadgeType = BTSupporter, poMonths = 3, poPrice = CurrencyAmount 2100, poAmount = CurrencyAmount 1400, poCurrency = "usd"}
  priceOffer prices offers (BadgePriceId "p1") (Just (BadgeOfferId "o1")) `shouldBe` expected
  priceOffer prices offers (BadgePriceId "p2") (Just (BadgeOfferId "o1")) `shouldBe` expected

testNoDriftFromWeb :: IO ()
testNoDriftFromWeb = do
  src <- T.readFile "apps/simplex-badge-service/web/src/catalog.ts"
  case parseCatalogSource src of
    Nothing -> expectationFailure "could not parse CATALOG out of web/src/catalog.ts -- its shape has changed"
    Just (webPrices, webOffers) -> do
      when (null webPrices || null webOffers) $
        expectationFailure "parsed zero prices or offers out of web/src/catalog.ts -- its shape has changed"
      mapM_ checkPrice webPrices
      mapM_ (checkOffer webPrices) webOffers
  where
    literalTotals :: Map.Map T.Text (Word32, Word32, Word32)
    literalTotals = Map.fromList [("supporter", (700, 1400, 4200)), ("legend", (7000, 14000, 42000))]
    checkPrice :: WebPrice -> IO ()
    checkPrice WebPrice {wpPriceId, wpBadgeType, wpMonthPrice} =
      case Map.lookup wpBadgeType literalTotals of
        Nothing -> expectationFailure ("no literal total for badge type " <> T.unpack wpBadgeType <> " (price " <> T.unpack wpPriceId <> ")")
        Just (base, _, _) ->
          offerTotal (CurrencyAmount wpMonthPrice) Nothing
            `shouldBe` Right (1, CurrencyAmount wpMonthPrice, CurrencyAmount base)
    checkOffer :: [WebPrice] -> WebOffer -> IO ()
    checkOffer webPrices WebOffer {woPriceId, woMonths, woDiscount} =
      case find (\WebPrice {wpPriceId} -> wpPriceId == woPriceId) webPrices of
        Nothing -> expectationFailure ("offer names price " <> T.unpack woPriceId <> ", which is not in the catalog")
        Just WebPrice {wpBadgeType, wpMonthPrice} -> case Map.lookup wpBadgeType literalTotals of
          Nothing -> expectationFailure ("no literal total for badge type " <> T.unpack wpBadgeType)
          Just (_, threeMonths, twelveMonths) -> do
            expectedAmount <- case woMonths of
              3 -> pure threeMonths
              12 -> pure twelveMonths
              n -> expectationFailure ("no literal total for a " <> show n <> "-month offer") >> pure 0
            offerTotal (CurrencyAmount wpMonthPrice) (Just (offer woMonths woDiscount))
              `shouldBe` Right (woMonths, CurrencyAmount (wpMonthPrice * fromIntegral woMonths), CurrencyAmount expectedAmount)

data WebPrice = WebPrice {wpPriceId :: T.Text, wpBadgeType :: T.Text, wpMonthPrice :: Word32, wpCurrency :: T.Text}
  deriving (Eq, Ord, Show)

data WebOffer = WebOffer {woOfferId :: T.Text, woPriceId :: T.Text, woMonths :: Word8, woDiscount :: OfferDiscount}
  deriving (Eq, Show)

parseCatalogSource :: T.Text -> Maybe ([WebPrice], [WebOffer])
parseCatalogSource src = do
  pricesBlock <- betweenMarkers "prices: [" "]" src
  offersBlock <- betweenMarkers "offers: [" "]" src
  prices <- mapM parseWebPrice (topLevelObjects pricesBlock)
  offers <- mapM parseWebOffer (topLevelObjects offersBlock)
  pure (prices, offers)

parseWebPrice :: T.Text -> Maybe WebPrice
parseWebPrice obj =
  WebPrice
    <$> fieldStr "priceId" obj
    <*> fieldStr "badgeType" obj
    <*> (fieldNum "monthPrice" obj :: Maybe Word32)
    <*> fieldStr "currency" obj

parseWebOffer :: T.Text -> Maybe WebOffer
parseWebOffer obj = do
  wOfferId <- fieldStr "offerId" obj
  wPriceId <- fieldStr "priceId" obj
  wMonths <- (fieldNum "months" obj :: Maybe Word8)
  discObj <- betweenMarkers "discount: {" "}" obj
  discType <- fieldStr "type" discObj
  wDiscount <- case discType of
    "freeMonths" -> ODFreeMonths <$> (fieldNum "freeMonths" discObj :: Maybe Word8)
    "discount" -> ODDiscount <$> (fieldNum "discount" discObj :: Maybe Word8)
    _ -> Nothing
  pure WebOffer {woOfferId = wOfferId, woPriceId = wPriceId, woMonths = wMonths, woDiscount = wDiscount}

topLevelObjects :: T.Text -> [T.Text]
topLevelObjects = go (0 :: Int) "" . T.unpack
  where
    go :: Int -> String -> String -> [T.Text]
    go _ _ [] = []
    go 0 _ (c : cs)
      | c == '{' = go 1 [c] cs
      | otherwise = go 0 "" cs
    go depth buf (c : cs)
      | c == '{' = go (depth + 1) (c : buf) cs
      | c == '}' && depth == 1 = T.pack (reverse (c : buf)) : go 0 "" cs
      | c == '}' = go (depth - 1) (c : buf) cs
      | otherwise = go depth (c : buf) cs

betweenMarkers :: T.Text -> T.Text -> T.Text -> Maybe T.Text
betweenMarkers start end t = do
  let (_, afterStart0) = T.breakOn start t
  afterStart <- if T.null afterStart0 then Nothing else Just (T.drop (T.length start) afterStart0)
  let (block, endPart) = T.breakOn end afterStart
  if T.null endPart then Nothing else Just block

fieldStr :: T.Text -> T.Text -> Maybe T.Text
fieldStr key obj = do
  let marker = key <> ": \""
      (_, afterMarker0) = T.breakOn marker obj
  afterMarker <- if T.null afterMarker0 then Nothing else Just (T.drop (T.length marker) afterMarker0)
  let (val, rest) = T.breakOn "\"" afterMarker
  if T.null rest then Nothing else Just val

fieldNum :: Integral a => T.Text -> T.Text -> Maybe a
fieldNum key obj = do
  let marker = key <> ": "
      (_, afterMarker0) = T.breakOn marker obj
  afterMarker <- if T.null afterMarker0 then Nothing else Just (T.drop (T.length marker) afterMarker0)
  case decimal afterMarker of
    Right (n, _) -> Just n
    Left _ -> Nothing

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module ViewTests where

import qualified Data.Map.Strict as M
import Data.Time
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import NameResolver (contactNameRecord)
import Simplex.Chat.Controller (SimplexNameAvailability (..))
import Simplex.Chat.Library.Commands (nameAvailability)
import Simplex.Chat.View
import Simplex.Messaging.Names.Record (NamePricing (..), NameRegistration (..), NameReservedReason (..), USDCents (..))
import Simplex.Messaging.SimplexName (SimplexDomain (..), SimplexTLD (..))
import Simplex.Messaging.SystemTime (RoundedSystemTime (..))
import Test.Hspec

viewTests :: Spec
viewTests = do
  testRecent
  testPremiumUsd
  testNameAvailability

-- SimplexNameAvailability restates NameRegistration so the chat API stays
-- independent of the wire format. The two have drifted twice; this pins the map.
testNameAvailability :: Spec
testNameAvailability = describe "name availability" $ do
  let dom d = SimplexDomain {nameTLD = TLDSimplex, domain = d, subDomain = []}
      -- 3 and 4 priced specially, everything else basePrice, under 3 unregistrable
      prices =
        NamePricing
          { registrationPrices = M.fromList [(3, USDCents 30000), (4, USDCents 4000)],
            basePrice = USDCents 500,
            minLabelLength = 3
          }
      registered e g r =
        NRRegistered
          { expires = RoundedSystemTime <$> e,
            graceUntil = RoundedSystemTime <$> g,
            reservedReason_ = r,
            nameRecord = contactNameRecord "abc.simplex" "https://simplex.chat/contact#/?v=2"
          }
  it "carries the dates over as UTC, and the reservation with them" $
    nameAvailability (dom "abc") (registered (Just 1780000000) (Just 1787776000) (Just NRRInternal))
      `shouldBe` SNARegistered
        { expires = Just (posixSecondsToUTCTime 1780000000),
          graceUntil = Just (posixSecondsToUTCTime 1787776000),
          reserved = Just NRRInternal
        }
  it "a v20/v21 router sent the record alone, so there are no dates" $
    nameAvailability (dom "abc") (registered Nothing Nothing Nothing)
      `shouldBe` SNARegistered {expires = Nothing, graceUntil = Nothing, reserved = Nothing}
  it "prices the label's own length, which only this side knows" $
    nameAvailability (dom "abc") NRAvailable {pricing = prices}
      `shouldBe` SNAAvailable {yearPriceUSD = Just 30000, minLabelLength = 3}
  it "falls back to basePrice for a length the registry does not price specially" $
    nameAvailability (dom "abcdefgh") NRAvailable {pricing = prices}
      `shouldBe` SNAAvailable {yearPriceUSD = Just 500, minLabelLength = 3}
  it "quotes nothing for a label the registry would refuse" $
    nameAvailability (dom "ab") NRAvailable {pricing = prices}
      `shouldBe` SNAAvailable {yearPriceUSD = Nothing, minLabelLength = 3}
  it "a reserved name carries its reason" $
    nameAvailability (dom "abc") NRReserved {reservedReason = NRRTrademark}
      `shouldBe` SNAReserved {reason = NRRTrademark}

-- the registry prices in cents, which no one reads at a glance
testPremiumUsd :: Spec
testPremiumUsd = describe "name price in USD" $ do
  it "shows dollars and cents" $ do
    usd 12793 `shouldBe` "$127.93"
    usd 100 `shouldBe` "$1.00"
  it "pads the cents" $ do
    usd 105 `shouldBe` "$1.05"
    usd 5 `shouldBe` "$0.05"
  it "shows a free name as zero rather than blank" $
    usd 0 `shouldBe` "$0.00"

testRecent :: Spec
testRecent = describe "recent" $ do
  let tz = hoursToTimeZone 1
      now1159 = UTCTime (fromGregorian 2023 6 7) (secondsToDiffTime $ 10 * 3600 + 59 * 60) -- 11:59 in tz
      now1200 = UTCTime (fromGregorian 2023 6 7) (secondsToDiffTime $ 11 * 3600) -- 12:00 in tz
      today0000 = UTCTime (fromGregorian 2023 6 6) (secondsToDiffTime $ 23 * 3600) -- 00:00 in tz
      today0600 = UTCTime (fromGregorian 2023 6 7) (secondsToDiffTime $ 5 * 3600) -- 06:00 in tz
      today1200 = UTCTime (fromGregorian 2023 6 7) (secondsToDiffTime $ 11 * 3600) -- 12:00 in tz
      today1800 = UTCTime (fromGregorian 2023 6 7) (secondsToDiffTime $ 17 * 3600) -- 18:00 in tz
      today2359 = UTCTime (fromGregorian 2023 6 7) (secondsToDiffTime $ 22 * 3600 + 59 * 60) -- 23:59 in tz
      yesterday0000 = UTCTime (fromGregorian 2023 6 5) (secondsToDiffTime $ 23 * 3600) -- 00:00 in tz
      yesterday1759 = UTCTime (fromGregorian 2023 6 6) (secondsToDiffTime $ 16 * 3600 + 59 * 60) -- 17:59 in tz
      yesterday1800 = UTCTime (fromGregorian 2023 6 6) (secondsToDiffTime $ 17 * 3600) -- 18:00 in tz
      yesterday2359 = UTCTime (fromGregorian 2023 6 6) (secondsToDiffTime $ 22 * 3600 + 59 * 60) -- 23:59 in tz
      sameDayLastMonth1900 = UTCTime (fromGregorian 2023 5 7) (secondsToDiffTime $ 18 * 3600) -- 19:00 in tz
      prevDayLastMonth1900 = UTCTime (fromGregorian 2023 5 6) (secondsToDiffTime $ 18 * 3600) -- 19:00 in tz
      sameDayLastYear1900 = UTCTime (fromGregorian 2022 6 7) (secondsToDiffTime $ 18 * 3600) -- 19:00 in tz
      prevDayLastYear1900 = UTCTime (fromGregorian 2022 6 6) (secondsToDiffTime $ 18 * 3600) -- 19:00 in tz
      tomorrow0000 = UTCTime (fromGregorian 2023 6 7) (secondsToDiffTime $ 23 * 3600) -- 00:00 in tz
      tomorrow1759 = UTCTime (fromGregorian 2023 6 8) (secondsToDiffTime $ 16 * 3600 + 59 * 60) -- 17:59 in tz
      tomorrow1800 = UTCTime (fromGregorian 2023 6 8) (secondsToDiffTime $ 17 * 3600) -- 18:00 in tz
      tomorrow2359 = UTCTime (fromGregorian 2023 6 8) (secondsToDiffTime $ 22 * 3600 + 59 * 60) -- 23:59 in tz
      sameDayNextMonth1900 = UTCTime (fromGregorian 2023 7 7) (secondsToDiffTime $ 18 * 3600) -- 19:00 in tz
      prevDayNextMonth1900 = UTCTime (fromGregorian 2023 7 6) (secondsToDiffTime $ 18 * 3600) -- 19:00 in tz
      sameDayNextYear1900 = UTCTime (fromGregorian 2024 6 7) (secondsToDiffTime $ 18 * 3600) -- 19:00 in tz
      prevDayNextYear1900 = UTCTime (fromGregorian 2024 6 6) (secondsToDiffTime $ 18 * 3600) -- 19:00 in tz
  test tz now1159 today0000 True
  test tz now1159 today0600 True
  test tz now1159 today1200 True
  test tz now1159 today1800 True
  test tz now1159 today2359 True
  test tz now1159 yesterday0000 False
  test tz now1159 yesterday1759 False
  test tz now1159 yesterday1800 True
  test tz now1159 yesterday2359 True
  test tz now1159 sameDayLastMonth1900 False
  test tz now1159 prevDayLastMonth1900 False
  test tz now1159 sameDayLastYear1900 False
  test tz now1159 prevDayLastYear1900 False
  test tz now1159 tomorrow0000 False
  test tz now1159 tomorrow1759 False
  test tz now1159 tomorrow1800 False
  test tz now1159 tomorrow2359 False
  test tz now1159 sameDayNextMonth1900 False
  test tz now1159 prevDayNextMonth1900 False
  test tz now1159 sameDayNextYear1900 False
  test tz now1159 prevDayNextYear1900 False

  test tz now1200 today0000 True
  test tz now1200 today0600 True
  test tz now1200 today1200 True
  test tz now1200 today1800 True
  test tz now1200 today2359 True
  test tz now1200 yesterday0000 False
  test tz now1200 yesterday1759 False
  test tz now1200 yesterday1800 False
  test tz now1200 yesterday2359 False
  test tz now1200 sameDayLastMonth1900 False
  test tz now1200 prevDayLastMonth1900 False
  test tz now1200 sameDayLastYear1900 False
  test tz now1200 prevDayLastYear1900 False
  test tz now1200 tomorrow0000 False
  test tz now1200 tomorrow1759 False
  test tz now1200 tomorrow1800 False
  test tz now1200 tomorrow2359 False
  test tz now1200 sameDayNextMonth1900 False
  test tz now1200 prevDayNextMonth1900 False
  test tz now1200 sameDayNextYear1900 False
  test tz now1200 prevDayNextYear1900 False
  where
    test tz now time expected =
      it ("returns " <> show expected <> " for time " <> show time <> " when time zone is " <> show tz <> " and current time is " <> show now) $
        recent now tz time `shouldBe` expected

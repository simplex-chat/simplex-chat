-- | Calendar-month arithmetic shared by the badge ledger (BadgeService.Ledger) and the client,
-- which needs 'addMonths' to render a badge's paid-through date. The clamping rule lives here
-- once so the two sides can't drift into different roundings of "31 January plus one month".
module Simplex.Chat.Badges.Months
  ( addMonths,
    fullMonthsBetween,
    sundayAfter,
  )
where

import Data.Time.Calendar (addDays, addGregorianMonthsClip, toGregorian)
import Data.Time.Calendar.WeekDate (toWeekDate)
import Data.Time.Clock (UTCTime (..), secondsToDiffTime)

-- | Add @n@ months to a time, clamping the day to the last valid day of the target month (31
-- January plus one month is 28 or 29 February) and preserving the time of day.
addMonths :: Int -> UTCTime -> UTCTime
addMonths n (UTCTime day tod) = UTCTime (addGregorianMonthsClip (fromIntegral n) day) tod

-- | The largest @m >= 0@ with @addMonths m start <= t@. Returns 0 when @t < start@.
--
-- 'addMonths' is monotonic and moves to a new calendar month on every step, so the plain
-- year/month difference between @start@ and @t@ is never more than one month away from the
-- answer; at most one correction step is needed either way.
fullMonthsBetween :: UTCTime -> UTCTime -> Int
fullMonthsBetween start t
  | t < start = 0
  | addMonths (approx + 1) start <= t = approx + 1
  | addMonths approx start > t = approx - 1
  | otherwise = approx
  where
    (sy, sm, _) = toGregorian (utctDay start)
    (ty, tm, _) = toGregorian (utctDay t)
    approx = fromInteger (ty - sy) * 12 + (tm - sm)

-- | 23:59:59 UTC of the next Sunday strictly after @t@. A @t@ that already falls on a Sunday
-- yields the following Sunday (7 days later), never the same day.
sundayAfter :: UTCTime -> UTCTime
sundayAfter (UTCTime day _) = UTCTime (addDays daysToSunday day) endOfDay
  where
    (_, _, dow) = toWeekDate day -- 1 = Monday .. 7 = Sunday
    daysToSunday = if dow == 7 then 7 else toInteger (7 - dow)
    endOfDay = secondsToDiffTime (23 * 3600 + 59 * 60 + 59)

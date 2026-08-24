-- | Calendar-month arithmetic shared by the badge ledger (BadgeService.Ledger) and the client,
-- which needs 'addMonths' to render a badge's paid-through date. The clamping rule lives here
-- once so the two sides can't drift into different roundings of "31 January plus one month".
module Simplex.Chat.Badges.Months
  ( addMonths,
    fullMonthsBetween,
    sundayAfter,
  )
where

import Data.Time.Calendar (addDays, addGregorianMonthsClip)
import Data.Time.Calendar.WeekDate (toWeekDate)
import Data.Time.Clock (UTCTime (..), secondsToDiffTime)

-- | Add @n@ months to a time, clamping the day to the last valid day of the target month (31
-- January plus one month is 28 or 29 February) and preserving the time of day.
addMonths :: Int -> UTCTime -> UTCTime
addMonths n (UTCTime day tod) = UTCTime (addGregorianMonthsClip (fromIntegral n) day) tod

-- | The largest @m >= 0@ with the @m@-times-iterated one-month step from @start@ landing at or
-- before @t@. Returns 0 when @t < start@.
--
-- This deliberately steps one month at a time via 'addMonths' 1, rather than jumping straight to
-- @addMonths m start@ for a candidate @m@: 'addMonths' is /not/ additive under clamping (a Feb
-- clamp encountered partway through a multi-month span permanently lowers the day-of-month for
-- every later step), so a direct @m@-month jump from @start@ can land on a different date than
-- @m@ single-month steps chained through the same intermediate clamps. That divergence is
-- path-dependent, so there is no O(1) closed form here — the only way to agree with 'issue'
-- (which always advances one month at a time) is to step the same way.
fullMonthsBetween :: UTCTime -> UTCTime -> Int
fullMonthsBetween start t = length (takeWhile (<= t) (drop 1 (iterate (addMonths 1) start)))

-- | 23:59:59 UTC of the next Sunday strictly after @t@. A @t@ that already falls on a Sunday
-- yields the following Sunday (7 days later), never the same day.
sundayAfter :: UTCTime -> UTCTime
sundayAfter (UTCTime day _) = UTCTime (addDays daysToSunday day) endOfDay
  where
    (_, _, dow) = toWeekDate day -- 1 = Monday .. 7 = Sunday
    daysToSunday = if dow == 7 then 7 else toInteger (7 - dow)
    endOfDay = secondsToDiffTime (23 * 3600 + 59 * 60 + 59)

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Simplex.Chat.Badges.Ledger
  ( LedgerBalance (..),
    BadgePeriod (..),
    LedgerRow (..),
    LedgerPlan (..),
    ledgerPlan,
    paidThrough,
    elapsedMonths,
    advanceBalance,
    grantMonths,
    issueMonth,
    addMonths,
    endOfSundayAfter,
    entryTypeColumns,
    entryTypeFromColumns,
    creditTypeTag,
    debitTypeTag,
  )
where

import Data.Text (Text)
import Data.Time.Calendar (addDays, addGregorianMonthsClip)
import Data.Time.Calendar.WeekDate (toWeekDate)
import Data.Time.Clock (UTCTime (..))
import Simplex.Chat.Badges (BadgeType)
import Simplex.Chat.Badges.Service (StatementCreditType (..), StatementDebitType (..), StatementEntryType (..))

-- | balanceAnchorTs is the start of the current run of months, and every month boundary in that
-- run is counted from it. Counting from balanceStartTs instead would compound the day-of-month
-- clipping: a run from 31 Jan would reach 28 Feb and stay on the 28th for good.
data LedgerBalance = LedgerBalance
  { balanceMonths :: Int,
    balanceStartTs :: UTCTime,
    balanceAnchorTs :: UTCTime,
    balanceBadgeType :: BadgeType
  }
  deriving (Eq, Show)

-- | balanceStartTs is always a whole number of months from the anchor; this is that number.
monthsFromAnchor :: LedgerBalance -> Int
monthsFromAnchor LedgerBalance {balanceStartTs, balanceAnchorTs} =
  length $ takeWhile (\m -> addMonths m balanceAnchorTs <= balanceStartTs) [1 ..]

-- | The start of the month that follows n more months of this run.
monthAfter :: LedgerBalance -> Int -> UTCTime
monthAfter b n = addMonths (toInteger $ monthsFromAnchor b + n) (balanceAnchorTs b)

-- | periodStart is stored, not derived from periodEnd: subtracting a month does not undo adding
-- one (31 Jan + 1 month = 28 Feb, - 1 month = 28 Jan).
data BadgePeriod = BadgePeriod
  { periodStart :: UTCTime,
    periodEnd :: UTCTime,
    badgeExpiry :: UTCTime
  }
  deriving (Eq, Show)

paidThrough :: LedgerBalance -> UTCTime
paidThrough b = monthAfter b (balanceMonths b)

elapsedMonths :: UTCTime -> LedgerBalance -> Int
elapsedMonths t b = length $ takeWhile (\m -> monthAfter b m <= t) [1 .. balanceMonths b]

-- | Runs before every grant and issue.
advanceBalance :: UTCTime -> LedgerBalance -> Maybe LedgerBalance
advanceBalance t b@LedgerBalance {balanceMonths}
  | k == 0 = Nothing
  | otherwise = Just b {balanceMonths = balanceMonths - k, balanceStartTs = monthAfter b k}
  where
    k = elapsedMonths t b

-- | New months start where the current coverage ends, or at t if it has already lapsed - so they
-- are neither spent on the month still running nor backdated over a gap.
grantMonths :: UTCTime -> Int -> LedgerBalance -> LedgerBalance
grantMonths t n b@LedgerBalance {balanceMonths, balanceStartTs}
  -- only a lapsed run restarts; topping up before coverage ends continues the run on its anchor,
  -- so buying a month at a time keeps the same day of month as buying a year at once
  | lapsed = b {balanceMonths = n, balanceStartTs = t, balanceAnchorTs = t}
  | otherwise = b {balanceMonths = balanceMonths + n}
  where
    lapsed = balanceMonths == 0 && t > balanceStartTs

-- | Runs after advanceBalance. Nothing when the balance is empty, or when it starts in the future
-- because the current month is already issued - the caller then replies with the stored credential.
issueMonth :: UTCTime -> LedgerBalance -> Maybe (BadgePeriod, LedgerBalance)
issueMonth t b@LedgerBalance {balanceMonths, balanceStartTs}
  | balanceMonths <= 0 || balanceStartTs > t = Nothing
  | otherwise = Just (period, b {balanceMonths = balanceMonths - 1, balanceStartTs = periodEnd})
  where
    periodEnd = monthAfter b 1
    period = BadgePeriod {periodStart = balanceStartTs, periodEnd, badgeExpiry = endOfSundayAfter periodEnd}

data LedgerRow = LedgerRow
  { rowChange :: Int,
    rowBalance :: LedgerBalance,
    rowType :: StatementEntryType
  }
  deriving (Show)

-- | The rows to write, worked out before any of them is written. The issuance row is kept
-- separate because the credential is stored against that row and no other.
data LedgerPlan = LedgerPlan
  { planRows :: [LedgerRow],
    planIssuance :: Maybe (LedgerRow, BadgePeriod)
  }
  deriving (Show)

-- | A redemption and an issue request run the same steps; only a redemption passes a credit.
ledgerPlan :: UTCTime -> Maybe (Int, StatementCreditType) -> LedgerBalance -> LedgerPlan
ledgerPlan t grant_ b0 = case issueMonth t granted of
  Just (p, issued) -> LedgerPlan rows $ Just (row granted issued $ SEDebit SDBadge, p)
  Nothing -> LedgerPlan rows Nothing
  where
    (lapseRows, advanced) = case advanceBalance t b0 of
      Just b -> ([row b0 b $ SEDebit SDLapse], b)
      Nothing -> ([], b0)
    (grantRows, granted) = case grant_ of
      Just (n, ct) -> let b = grantMonths t n advanced in ([row advanced b $ SECredit ct], b)
      Nothing -> ([], advanced)
    rows = lapseRows <> grantRows
    -- read off the two states, so a row cannot disagree with the balance it carries
    row before after rowType = LedgerRow {rowChange = balanceMonths after - balanceMonths before, rowBalance = after, rowType}

-- | The tag stored is the string the service sent, so a type this version does not know is kept
-- as received and can be read once it does.
entryTypeColumns :: StatementEntryType -> (Text, Maybe Text, Maybe Text)
entryTypeColumns = \case
  SECredit c -> ("credit", Just $ creditTypeTag c, Nothing)
  SEDebit d -> ("debit", Nothing, Just $ debitTypeTag d)

creditTypeTag :: StatementCreditType -> Text
creditTypeTag = \case
  SCPayment _ -> "payment"
  SCCode -> "code"
  SCCharge _ -> "charge"
  SCSupport -> "support"
  SCTransferIn _ -> "transferIn"
  SCOpening -> "opening"
  SCUnknown {tag} -> tag

debitTypeTag :: StatementDebitType -> Text
debitTypeTag = \case
  SDRefund -> "refund"
  SDUpgrade _ -> "upgrade"
  SDTransferOut _ -> "transferOut"
  SDSupport -> "support"
  SDBadge -> "badge"
  SDLapse -> "lapse"
  SDUnknown {tag} -> tag

-- | Only the types a tag alone rebuilds, which is those whose constructor has no fields. The rest
-- name an invoice, a charge or another purchase, kept in a column this is not given, and reading
-- them back is not implemented - Nothing rather than a type with an invented payload.
-- TODO [badges] take the reference columns and rebuild payment, charge, transferIn, upgrade and
-- transferOut, without which a statement carrying one cannot be re-emitted or read back.
entryTypeFromColumns :: Text -> Maybe Text -> Maybe Text -> Maybe StatementEntryType
entryTypeFromColumns entryType credit_ debit_ = case (entryType, credit_, debit_) of
  ("credit", Just t, _) -> SECredit <$> creditType t
  ("debit", _, Just t) -> SEDebit <$> debitType t
  _ -> Nothing
  where
    creditType = \case
      "code" -> Just SCCode
      "support" -> Just SCSupport
      "opening" -> Just SCOpening
      _ -> Nothing
    debitType = \case
      "badge" -> Just SDBadge
      "lapse" -> Just SDLapse
      "refund" -> Just SDRefund
      "support" -> Just SDSupport
      _ -> Nothing

addMonths :: Integer -> UTCTime -> UTCTime
addMonths n (UTCTime d t) = UTCTime (addGregorianMonthsClip n d) t

-- Every badge in a week expires together, revealing nothing about when it was bought.
-- The end of a Sunday is the next Monday at 00:00, so this returns a Monday and 8 is right.
endOfSundayAfter :: UTCTime -> UTCTime
endOfSundayAfter (UTCTime d _) =
  let (_, _, dayOfWeek) = toWeekDate d -- 1 Monday .. 7 Sunday
   in UTCTime (addDays (toInteger (8 - dayOfWeek)) d) 0

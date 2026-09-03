{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The badge ledger, shared by the service that writes the rows and the client that replicates
-- them. Every row carries the state after it, so the last row is the balance - neither side sums.
module Simplex.Chat.Badges.Ledger
  ( LedgerBalance (..),
    BadgePeriod (..),
    LedgerRow (..),
    LedgerPass (..),
    ledgerPass,
    passAllRows,
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

-- | Coverage is @[balanceStartTs, addMonths balanceMonths balanceStartTs)@.
data LedgerBalance = LedgerBalance
  { balanceMonths :: Int,
    balanceStartTs :: UTCTime,
    balanceBadgeType :: BadgeType
  }
  deriving (Eq, Show)

-- | 'periodStart' cannot be recovered from 'periodEnd': month arithmetic clips, so
-- 31 Jan + 1 month - 1 month = 28 Jan.
data BadgePeriod = BadgePeriod
  { periodStart :: UTCTime,
    periodEnd :: UTCTime,
    badgeExpiry :: UTCTime
  }
  deriving (Eq, Show)

paidThrough :: LedgerBalance -> UTCTime
paidThrough LedgerBalance {balanceMonths, balanceStartTs} = addMonths (toInteger balanceMonths) balanceStartTs

elapsedMonths :: UTCTime -> LedgerBalance -> Int
elapsedMonths t LedgerBalance {balanceMonths, balanceStartTs} =
  length $ takeWhile (\m -> addMonths m balanceStartTs <= t) [1 .. toInteger balanceMonths]

-- | Runs before every grant and issue.
advanceBalance :: UTCTime -> LedgerBalance -> Maybe LedgerBalance
advanceBalance t b@LedgerBalance {balanceMonths, balanceStartTs}
  | k == 0 = Nothing
  | otherwise = Just b {balanceMonths = balanceMonths - k, balanceStartTs = addMonths (toInteger k) balanceStartTs}
  where
    k = elapsedMonths t b

-- | An exhausted balance restarts at @t@, so months bought after a gap are not spent on it.
grantMonths :: UTCTime -> Int -> LedgerBalance -> LedgerBalance
grantMonths t n b@LedgerBalance {balanceMonths, balanceStartTs}
  | balanceMonths == 0 = b {balanceMonths = n, balanceStartTs = max balanceStartTs t}
  | otherwise = b {balanceMonths = balanceMonths + n}

-- | Runs after 'advanceBalance'. A balance starting in the future is a month already issued, so
-- topping up inside an issued period does not issue it twice.
issueMonth :: UTCTime -> LedgerBalance -> Maybe (BadgePeriod, LedgerBalance)
issueMonth t b@LedgerBalance {balanceMonths, balanceStartTs}
  | balanceMonths <= 0 || balanceStartTs > t = Nothing
  | otherwise = Just (period, b {balanceMonths = balanceMonths - 1, balanceStartTs = periodEnd})
  where
    periodEnd = addMonths 1 balanceStartTs
    period = BadgePeriod {periodStart = balanceStartTs, periodEnd, badgeExpiry = endOfSundayAfter periodEnd}

data LedgerRow = LedgerRow
  { rowChange :: Int,
    rowBalance :: LedgerBalance,
    rowType :: StatementEntryType
  }
  deriving (Show)

-- | 'passIssue' is apart from 'passRows' so the issuance references the @debit(badge)@ entry
-- itself, rather than whichever row a caller inserted last.
data LedgerPass = LedgerPass
  { passRows :: [LedgerRow],
    passIssue :: Maybe (LedgerRow, BadgePeriod),
    passBalance :: LedgerBalance
  }
  deriving (Show)

passAllRows :: LedgerPass -> [LedgerRow]
passAllRows LedgerPass {passRows, passIssue} = passRows <> maybe [] (pure . fst) passIssue

-- | What a redemption and an issue request both do, differing only in the credit.
ledgerPass :: UTCTime -> Maybe (Int, StatementCreditType) -> LedgerBalance -> LedgerPass
ledgerPass t grant_ b0 = case issueMonth t granted of
  Just (p, issued) -> LedgerPass rows (Just (row granted issued $ SEDebit SDBadge, p)) issued
  Nothing -> LedgerPass rows Nothing granted
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

-- | The stored tag is the wire tag, so a row replicated from a newer service keeps its type.
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

-- | Only the reference-free types, which are the only ones this version writes; the rest also
-- need their reference column and arrive with subscriptions and payments.
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

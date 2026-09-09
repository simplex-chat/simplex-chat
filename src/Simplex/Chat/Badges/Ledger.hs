{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Simplex.Chat.Badges.Ledger
  ( emptyEntry,
    lapseEntry,
    grantEntry,
    issueEntry,
    paidThrough,
    balanceChecked,
    addMonths,
    endOfMondayAfter,
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
import Simplex.Chat.Badges.Service (StatementCreditType (..), StatementDebitType (..), StatementEntry (..), StatementEntryType (..))

-- | balanceStartTs is always a whole number of months from the anchor; this is that number.
monthsFromAnchor :: StatementEntry -> Int
monthsFromAnchor StatementEntry {balanceStartTs, balanceAnchorTs} =
  length $ takeWhile (\m -> addMonths m balanceAnchorTs <= balanceStartTs) [1 ..]

-- | The start of the month that follows n more months of this run.
monthAfter :: StatementEntry -> Int -> UTCTime
monthAfter e n = addMonths (toInteger $ monthsFromAnchor e + n) (balanceAnchorTs e)

paidThrough :: StatementEntry -> UTCTime
paidThrough e = monthAfter e (balanceMonths e)

elapsedMonths :: UTCTime -> StatementEntry -> Int
elapsedMonths t e = length $ takeWhile (\m -> monthAfter e m <= t) [1 .. balanceMonths e]

-- | What a purchase with no ledger starts from: no months, and a run starting now.
emptyEntry :: UTCTime -> BadgeType -> StatementEntry
emptyEntry t badgeType =
  StatementEntry
    { entryId = "",
      changeMonths = 0,
      balanceMonths = 0,
      balanceStartTs = t,
      balanceAnchorTs = t,
      balanceBadgeType = badgeType,
      wasPausedSince = Nothing,
      createdAt = t,
      entryType = SECredit SCOpening
    }

-- | Writes off the months that have passed.
lapseEntry :: UTCTime -> Text -> StatementEntry -> Maybe StatementEntry
lapseEntry t entryId e@StatementEntry {balanceMonths}
  | k == 0 = Nothing
  | otherwise =
      Just
        e
          { entryId,
            createdAt = t,
            changeMonths = negate k,
            balanceMonths = balanceMonths - k,
            balanceStartTs = monthAfter e k,
            entryType = SEDebit SDLapse
          }
  where
    k = elapsedMonths t e

-- | New months start where the current coverage ends, or at t if it has already lapsed - so they
-- are neither spent on the month still running nor backdated over a gap.
grantEntry :: UTCTime -> Text -> Int -> StatementCreditType -> StatementEntry -> StatementEntry
grantEntry t entryId n credit e@StatementEntry {balanceMonths, balanceStartTs}
  -- only a lapsed run restarts; topping up before coverage ends continues the run on its anchor,
  -- so buying a month at a time keeps the same day of month as buying a year at once
  | lapsed = credited {balanceMonths = n, balanceStartTs = t, balanceAnchorTs = t}
  | otherwise = credited {balanceMonths = balanceMonths + n}
  where
    lapsed = balanceMonths == 0 && t > balanceStartTs
    credited = e {entryId, createdAt = t, changeMonths = n, entryType = SECredit credit}

-- | The period issued runs from the previous entry's balanceStartTs to this one's.
issueEntry :: UTCTime -> Text -> StatementEntry -> Maybe StatementEntry
issueEntry t entryId e@StatementEntry {balanceMonths, balanceStartTs}
  | balanceMonths <= 0 || balanceStartTs > t = Nothing
  | otherwise =
      Just
        e
          { entryId,
            createdAt = t,
            changeMonths = -1,
            balanceMonths = balanceMonths - 1,
            balanceStartTs = monthAfter e 1,
            entryType = SEDebit SDBadge
          }

-- | Pairs each arriving entry with whether its balance follows from the one before it, the stored
-- tip standing in for the first one's predecessor. 'Nothing' is "not checked".
-- TODO [badges] do the arithmetic.
balanceChecked :: Maybe StatementEntry -> [StatementEntry] -> [(StatementEntry, Maybe Bool)]
balanceChecked _tip = map (\e -> (e, Nothing))

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

-- | Only the types a tag alone rebuilds, which is those whose constructor has no fields; the rest
-- answer Nothing rather than a type with an invented payload. The client also stores each type's
-- JSON and reads that first, so this is its fallback; the service has no such column.
-- TODO [badges] take the reference columns and rebuild payment, charge, transferIn, upgrade and
-- transferOut, without which the service cannot re-emit a statement carrying one.
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
-- The end of a Monday is the next Tuesday at 00:00, so this returns a Tuesday and 9 is right.
-- Returning a Monday instead would put the expiry on Sunday evening in the Americas, leaving a
-- renewal that failed there waiting for weekend support.
endOfMondayAfter :: UTCTime -> UTCTime
endOfMondayAfter (UTCTime d _) =
  let (_, _, dayOfWeek) = toWeekDate d -- 1 Monday .. 7 Sunday
   in UTCTime (addDays (toInteger (9 - dayOfWeek)) d) 0

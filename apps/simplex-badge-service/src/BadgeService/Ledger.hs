{-# LANGUAGE NamedFieldPuns #-}

-- | Pure, database-free transitions over the badge ledger state (UX \"Transitions\"). Kept free
-- of 'BadgeService.Store' so every boundary case can be exercised exhaustively by
-- @Bots.BadgeLedgerTests@ without a database.
--
-- Calling convention: 'advance' is run before every 'credit', 'debitAll' and 'issue' call, by the
-- caller, against the same timestamp; a 'Just' result is one @debit(lapse)@ row the caller writes
-- before writing the row for the credit/debit/issue that follows.
module BadgeService.Ledger
  ( LedgerState (..),
    advance,
    credit,
    debitAll,
    issue,
    initialLedgerState,
  )
where

import Data.Time.Clock (UTCTime)
import Simplex.Chat.Badges (BadgeType)
import Simplex.Chat.Badges.Months (addMonths, fullMonthsBetween)
import Simplex.Chat.Badges.Service (StatementCreditType, StatementDebitType)

data LedgerState = LedgerState
  { balanceMonths :: Int,
    balanceStartTs :: UTCTime,
    balanceBadgeType :: BadgeType
  }
  deriving (Eq, Show)

-- | One @debit(lapse)@ transition for the fully elapsed, unissued months since 'balanceStartTs'.
-- @k = min balanceMonths (fullMonthsBetween balanceStartTs t)@: capped at the balance so a long
-- absence on a small or zero balance can never lapse more months than are actually owed. Returns
-- 'Nothing' (state unchanged) when @k@ would be 0 — never 'Just (0, _)'.
--
-- The new 'balanceStartTs' is reached by stepping 'addMonths' 1 forward @k@ times, not by a
-- direct @addMonths k@ jump: 'fullMonthsBetween' counts months the same iterated way (see its
-- Haddock), and 'issue' always advances one month at a time, so all three must agree on the same
-- stepwise sequence or a boundary computed here can land past where 'issue' would place it,
-- letting a re-'issue' at the same instant slip through the guard below.
advance :: UTCTime -> LedgerState -> Maybe (Int, LedgerState)
advance t st@LedgerState {balanceMonths, balanceStartTs}
  | k <= 0 = Nothing
  | otherwise = Just (k, st {balanceMonths = balanceMonths - k, balanceStartTs = iterate (addMonths 1) balanceStartTs !! k})
  where
    k = min balanceMonths (fullMonthsBetween balanceStartTs t)

-- | @grant(src) +n@: a zero balance restarts the coverage window at @max balanceStartTs t@ (the
-- settlement time, or the old start if that's already later); a positive balance just grows,
-- since the months are fungible and already counted from the existing start.
credit :: UTCTime -> Int -> StatementCreditType -> LedgerState -> LedgerState
credit t n _creditType st@LedgerState {balanceMonths, balanceStartTs}
  | balanceMonths == 0 = st {balanceMonths = n, balanceStartTs = max balanceStartTs t}
  | otherwise = st {balanceMonths = balanceMonths + n}

-- | @debit(reason)@: zeroes the balance without moving 'balanceStartTs' (refund, upgrade
-- conversion, transfer-out, correction — the caller identifies which via 'StatementDebitType').
debitAll :: StatementDebitType -> LedgerState -> LedgerState
debitAll _reason st = st {balanceMonths = 0}

-- | @consume@: issues a credential for @[balanceStartTs, addMonths 1 balanceStartTs)@, debiting
-- one month. 'Nothing' when @balanceMonths == 0@ (nothing to issue) or when the current month is
-- already issued (@balanceStartTs > t@).
--
-- __A caller telling those two apart must test @balanceStartTs > t@, never the balance.__ The two
-- reasons are not exclusive: once the LAST funded month has been issued both hold at once, and a
-- caller reading the balance there calls an already-issued month exhausted — refusing to hand back
-- a credential that was already issued and stored, which is what @badges-rpc.md@ §Idempotency is
-- about. That was a real defect in @BadgeService.Service.planLedger@ (found and fixed in B10, plan
-- §9); this comment used to instruct it.
--
-- This guard only works because 'advance' steps to the same month boundaries 'issue' does (see
-- 'advance''s Haddock): whenever 'advance' is not capped by a low balance, its resulting
-- @balanceStartTs@ is @<= t@ and the /next/ boundary after it is @> t@ by construction, so the
-- 'issue' that follows always leaves @balanceStartTs' > t@ — a second 'issue' at the same @t@ is
-- correctly rejected. A freshly 'credit'ed balance sets @balanceStartTs == t@ exactly, which must
-- stay issuable, so this guard is strictly @>@, never @>=@.
issue :: UTCTime -> LedgerState -> Maybe (LedgerState, UTCTime, UTCTime)
issue t st@LedgerState {balanceMonths, balanceStartTs}
  | balanceMonths == 0 = Nothing
  | balanceStartTs > t = Nothing
  | otherwise = Just (st {balanceMonths = balanceMonths - 1, balanceStartTs = periodEnd}, periodStart, periodEnd)
  where
    periodStart = balanceStartTs
    periodEnd = addMonths 1 periodStart

-- | The state of a purchase with no ledger entry yet: zero balance, 'balanceStartTs' the given
-- time. A purchase created in the same transaction has no prior row to read, so this is where its
-- first 'credit' starts from.
initialLedgerState :: UTCTime -> BadgeType -> LedgerState
initialLedgerState t badgeType = LedgerState {balanceMonths = 0, balanceStartTs = t, balanceBadgeType = badgeType}

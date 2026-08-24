{-# LANGUAGE LambdaCase #-}

-- | Property tests for the pure, database-free badge ledger transitions (BadgeService.Ledger).
-- Registered under the "Supporter badges" hspec path (not "SimpleX Badge service bot"): these
-- tests need no database and must run in CI.
module Bots.BadgeLedgerTests (badgeLedgerTests) where

import BadgeService.Ledger
import Data.List (foldl')
import Data.Time.Calendar (addDays, fromGregorian)
import Data.Time.Calendar.WeekDate (toWeekDate)
import Data.Time.Clock (DiffTime, UTCTime (..), addUTCTime, nominalDay, secondsToDiffTime)
import Simplex.Chat.Badges (BadgeType (..))
import Simplex.Chat.Badges.Months (addMonths, fullMonthsBetween, sundayAfter)
import Simplex.Chat.Badges.Service (StatementCreditType (..), StatementDebitType (..))
import Test.Hspec
import Test.Hspec.QuickCheck (modifyMaxSuccess, prop)
import Test.QuickCheck (Gen, Property, chooseInt, discard, elements, forAll, oneof, property, vectorOf)

badgeLedgerTests :: Spec
badgeLedgerTests = modifyMaxSuccess (const 500) $ do
  describe "addMonths clamping" $ do
    it "clamps 31 January to 28 February in a non-leap year, preserving time of day" $
      addMonths 1 (UTCTime (fromGregorian 2025 1 31) noon) `shouldBe` UTCTime (fromGregorian 2025 2 28) noon
    it "clamps 31 January to 29 February in a leap year, preserving time of day" $
      addMonths 1 (UTCTime (fromGregorian 2028 1 31) noon) `shouldBe` UTCTime (fromGregorian 2028 2 29) noon
  prop "sundayAfter a time already on a Sunday returns the following Sunday at 23:59:59 UTC (property 7)" prop_sundayAfterSunday
  prop "every recorded row matches the transition applied to its predecessor (property 1)" prop_rowsMatchTransitions
  prop "balance stays non-negative, changeMonths sums to it, start is non-decreasing (property 2)" prop_invariants
  prop "issue always debits exactly one month for exactly one period (property 3)" prop_issueDebitsOneMonth
  prop "re-running issue inside an already-issued period appends nothing (property 4)" prop_issueIdempotentWithinPeriod
  prop "advance lapses only fully elapsed, unissued months (property 5)" prop_advanceOnlyFullyElapsed
  it "reproduces the worked example: buy 3 months, app off a month, reissue (property 6)" testWorkedExample
  it "does not double-issue when a Feb clamp puts balanceStartTs exactly on t (property 4 regression, seeds 6/7)" testFebClampRegression

noon :: DiffTime
noon = secondsToDiffTime (12 * 3600)

-- | Independent oracle for "n months forward, stepping one month at a time": the ledger's own
-- month-boundary definition (see 'fullMonthsBetween' and 'advance' in BadgeService.Ledger), used
-- here instead of a direct @addMonths n@ jump because the two disagree once a Feb clamp falls
-- partway through the span (regression for the property-4 counterexample below).
iterAddMonths :: Int -> UTCTime -> UTCTime
iterAddMonths n t0 = iterate (addMonths 1) t0 !! n

endOfDay :: DiffTime
endOfDay = secondsToDiffTime (23 * 3600 + 59 * 60 + 59)

-- Generators

genBadgeType :: Gen BadgeType
genBadgeType = elements [BTSupporter, BTLegend, BTInvestor]

genCreditType :: Gen StatementCreditType
genCreditType = elements [SCSupport, SCOpening]

genDebitType :: Gen StatementDebitType
genDebitType = elements [SDRefund, SDBadge, SDLapse, SDSupport]

genTime :: Gen UTCTime
genTime = oneof [genUniformTime, genClampProneTime]

genUniformTime :: Gen UTCTime
genUniformTime = do
  dayOffset <- chooseInt (0, 3000)
  secOfDay <- chooseInt (0, 86399)
  pure $ UTCTime (addDays (toInteger dayOffset) (fromGregorian 2020 1 1)) (secondsToDiffTime (toInteger secOfDay))

-- | Dates on the 28th-31st of a month, the only days where 'addMonths' can clamp. Direct-jump vs.
-- iterated-step boundary computations only diverge when a clamp falls partway through a span, so
-- a uniformly-random day-of-month (as in 'genUniformTime') samples that region far too thinly —
-- the property-4 regression below needed 493 QuickCheck cases to surface it. Picking the
-- day-of-month explicitly from the clamp-prone range, with month/year otherwise random, makes
-- every generated history exercise that region directly instead of by chance.
genClampProneTime :: Gen UTCTime
genClampProneTime = do
  year <- chooseInt (2020, 2028)
  month <- chooseInt (1, 12)
  day <- elements [28, 29, 30, 31]
  secOfDay <- chooseInt (0, 86399)
  pure $ UTCTime (fromGregorian (toInteger year) month day) (secondsToDiffTime (toInteger secOfDay))

genLedgerState :: Gen LedgerState
genLedgerState = do
  months <- chooseInt (0, 36)
  start <- genTime
  badgeType <- genBadgeType
  pure LedgerState {balanceMonths = months, balanceStartTs = start, balanceBadgeType = badgeType}

-- | A state paired with a time offset from that state's own start (positive or negative), so a
-- decent fraction of generated pairs land in every interesting region: before the start, inside
-- the current period, and many months past it.
genStateAndTime :: Gen (LedgerState, UTCTime)
genStateAndTime = do
  st <- genLedgerState
  offsetDays <- chooseInt (-60, 400)
  let t = addUTCTime (fromIntegral offsetDays * nominalDay) (balanceStartTs st)
  pure (st, t)

data Cmd = CmdCredit Int StatementCreditType | CmdDebit StatementDebitType | CmdIssue
  deriving (Show)

genCmd :: Gen Cmd
genCmd =
  oneof
    [ CmdCredit <$> chooseInt (1, 24) <*> genCreditType,
      CmdDebit <$> genDebitType,
      pure CmdIssue
    ]

-- | A genesis time, badge type and a bounded, time-ordered sequence of commands with random
-- non-negative day gaps between them (0 days apart is allowed, so consecutive same-instant calls
-- are exercised too).
genRun :: Gen (UTCTime, BadgeType, [(UTCTime, Cmd)])
genRun = do
  t0 <- genTime
  badgeType <- genBadgeType
  n <- chooseInt (0, 15)
  deltasAndCmds <- vectorOf n ((,) <$> chooseInt (0, 400) <*> genCmd)
  let times = drop 1 $ scanl (\t d -> addUTCTime (fromIntegral d * nominalDay) t) t0 (map fst deltasAndCmds)
  pure (t0, badgeType, zip times (map snd deltasAndCmds))

-- History replay

data EntryKind = EKLapse Int | EKCredit Int | EKDebit Int | EKConsume

-- | kind, time of the call, state before, state after.
data Row = Row EntryKind UTCTime LedgerState LedgerState

rowKind :: Row -> EntryKind
rowKind (Row k _ _ _) = k

rowNext :: Row -> LedgerState
rowNext (Row _ _ _ next) = next

-- | advance-then-command, exactly the calling convention documented for the ledger: advance runs
-- before every credit, debit and issue.
applyStep :: UTCTime -> Cmd -> LedgerState -> (LedgerState, [Row])
applyStep t cmd st0 =
  let (st1, lapseRows) = case advance t st0 of
        Nothing -> (st0, [])
        Just (k, st1') -> (st1', [Row (EKLapse k) t st0 st1'])
   in case cmd of
        CmdCredit n ct ->
          let st2 = credit t n ct st1 in (st2, lapseRows <> [Row (EKCredit n) t st1 st2])
        CmdDebit reason ->
          let n = balanceMonths st1
              st2 = debitAll reason st1
           in (st2, lapseRows <> [Row (EKDebit n) t st1 st2])
        CmdIssue -> case issue t st1 of
          Nothing -> (st1, lapseRows)
          Just (st2, _periodStart, _periodEnd) -> (st2, lapseRows <> [Row EKConsume t st1 st2])

runFromGenesis :: UTCTime -> BadgeType -> [(UTCTime, Cmd)] -> (LedgerState, [Row])
runFromGenesis t0 badgeType = foldl' go (initialLedgerState t0 badgeType, [])
  where
    go (st, rows) (t, cmd) = let (st', rs) = applyStep t cmd st in (st', rows <> rs)

-- | Re-derives each row's next state from its predecessor using the UX "Transitions" formulas
-- directly (not by re-invoking advance/credit/debitAll/issue), so a bug in those functions'
-- arithmetic shows up as a mismatch here.
verifyRow :: Row -> Bool
verifyRow (Row kind t prev next) = case kind of
  EKLapse k ->
    balanceMonths next == balanceMonths prev - k
      && balanceStartTs next == iterAddMonths k (balanceStartTs prev)
      && balanceBadgeType next == balanceBadgeType prev
  EKCredit n ->
    balanceMonths next == balanceMonths prev + n
      && balanceBadgeType next == balanceBadgeType prev
      && balanceStartTs next == (if balanceMonths prev == 0 then max (balanceStartTs prev) t else balanceStartTs prev)
  EKDebit n ->
    balanceMonths prev == n
      && balanceMonths next == 0
      && balanceStartTs next == balanceStartTs prev
      && balanceBadgeType next == balanceBadgeType prev
  EKConsume ->
    balanceMonths next == balanceMonths prev - 1
      && balanceStartTs next == addMonths 1 (balanceStartTs prev)
      && balanceBadgeType next == balanceBadgeType prev

changeMonthsOf :: EntryKind -> Int
changeMonthsOf = \case
  EKLapse k -> negate k
  EKCredit n -> n
  EKDebit n -> negate n
  EKConsume -> -1

isNonDecreasing :: Ord a => [a] -> Bool
isNonDecreasing xs = and (zipWith (<=) xs (drop 1 xs))

-- Properties

prop_rowsMatchTransitions :: Property
prop_rowsMatchTransitions = forAll genRun $ \(t0, badgeType, steps) ->
  let (_, rows) = runFromGenesis t0 badgeType steps in property (all verifyRow rows)

prop_invariants :: Property
prop_invariants = forAll genRun $ \(t0, badgeType, steps) ->
  let (finalSt, rows) = runFromGenesis t0 badgeType steps
      allStates = initialLedgerState t0 badgeType : map rowNext rows
      totalChange = sum (map (changeMonthsOf . rowKind) rows)
   in property $
        all ((>= 0) . balanceMonths) allStates
          && totalChange == balanceMonths finalSt
          && isNonDecreasing (map balanceStartTs allStates)

prop_issueDebitsOneMonth :: Property
prop_issueDebitsOneMonth = forAll genStateAndTime $ \(st, t) -> case issue t st of
  Nothing -> discard
  Just (st', periodStart, periodEnd) ->
    property $
      balanceMonths st' == balanceMonths st - 1
        && periodStart == balanceStartTs st
        && periodEnd == addMonths 1 periodStart
        && balanceStartTs st' == periodEnd
        && balanceBadgeType st' == balanceBadgeType st

prop_issueIdempotentWithinPeriod :: Property
prop_issueIdempotentWithinPeriod = forAll genStateAndTime $ \(st, t) ->
  let st1 = maybe st snd (advance t st)
   in case issue t st1 of
        Nothing -> discard
        Just (st2, _, _) ->
          let st3 = maybe st2 snd (advance t st2)
           in property (issue t st3 == Nothing)

prop_advanceOnlyFullyElapsed :: Property
prop_advanceOnlyFullyElapsed = forAll genStateAndTime $ \(st, t) -> case advance t st of
  Nothing -> property (fullMonthsBetween (balanceStartTs st) t == 0 || balanceMonths st == 0)
  Just (k, st') ->
    property $
      k > 0
        && k <= balanceMonths st
        && iterAddMonths k (balanceStartTs st) <= t
        && (k == balanceMonths st || iterAddMonths (k + 1) (balanceStartTs st) > t)
        && balanceStartTs st' == iterAddMonths k (balanceStartTs st)
        && balanceMonths st' == balanceMonths st - k

genSunday :: Gen UTCTime
genSunday = do
  anyTime <- genTime
  secOfDay <- chooseInt (0, 86399)
  let day = utctDay anyTime
      (_, _, dow) = toWeekDate day
      toSunday = if dow == 7 then 0 else toInteger (7 - dow)
  pure $ UTCTime (addDays toSunday day) (secondsToDiffTime (toInteger secOfDay))

prop_sundayAfterSunday :: Property
prop_sundayAfterSunday = forAll genSunday $ \sunday ->
  let next = sundayAfter sunday
   in property (utctDay next == addDays 7 (utctDay sunday) && utctDayTime next == endOfDay)

-- Property 6 (worked example, UX §3): buy 3 months Tue Mar 10, 2026; issue right away; app off
-- Apr 5 - May 20; advance and reissue May 20. Reproduces the doc's four rows verbatim.
testWorkedExample :: IO ()
testWorkedExample = do
  let marTen = UTCTime (fromGregorian 2026 3 10) noon
      aprTen = addMonths 1 marTen
      mayTen = addMonths 2 marTen
      junTen = addMonths 3 marTen
      mayTwenty = UTCTime (fromGregorian 2026 5 20) noon
      st0 = initialLedgerState marTen BTSupporter
  -- row 1: grant(payment) +3, months=3, start=Mar10; paidThrough = Jun10
  let st1 = credit marTen 3 SCSupport st0
  balanceMonths st1 `shouldBe` 3
  balanceStartTs st1 `shouldBe` marTen
  addMonths (balanceMonths st1) (balanceStartTs st1) `shouldBe` junTen
  -- no lapse yet: the first month hasn't elapsed
  advance marTen st1 `shouldBe` Nothing
  -- row 2: consume -1, 2, Apr10; issuance Mar10-Apr10, expiry Sun Apr 12
  Just (st2, periodStart2, periodEnd2) <- pure (issue marTen st1)
  balanceMonths st2 `shouldBe` 2
  balanceStartTs st2 `shouldBe` aprTen
  periodStart2 `shouldBe` marTen
  periodEnd2 `shouldBe` aprTen
  sundayAfter periodEnd2 `shouldBe` UTCTime (fromGregorian 2026 4 12) endOfDay
  -- app off Apr5-May20: on the next contact (May20), advance lapses the one fully elapsed,
  -- unissued month (Apr10-May10)
  Just (lapsedMonths, st3) <- pure (advance mayTwenty st2)
  lapsedMonths `shouldBe` 1
  balanceMonths st3 `shouldBe` 1
  balanceStartTs st3 `shouldBe` mayTen
  -- row 4: consume -1, 0, Jun10; issuance May10-Jun10, expiry Sun Jun 14
  Just (st4, periodStart4, periodEnd4) <- pure (issue mayTwenty st3)
  balanceMonths st4 `shouldBe` 0
  balanceStartTs st4 `shouldBe` junTen
  periodStart4 `shouldBe` mayTen
  periodEnd4 `shouldBe` junTen
  sundayAfter periodEnd4 `shouldBe` UTCTime (fromGregorian 2026 6 14) endOfDay

-- | Exact counterexample from the property-4 falsification (QuickCheck seeds 6 and 7, 493 tests
-- in): a direct 11-month jump from Apr 29, 2020 overshoots Mar 28, 2021 by a day (it lands on Mar
-- 29), while the true, iterated 11th month boundary (stepping through the Feb 2021 clamp) lands
-- exactly on Mar 28. The old 'fullMonthsBetween' picked 10 months via its overshoot-correction
-- check, leaving 'advance' at Feb 28, and 'issue' then walked one more month to land its new
-- 'balanceStartTs' exactly on @t@ — which the old @balanceStartTs > t@ guard failed to reject on
-- a same-instant re-issue, so a second 'issue' fired and silently debited a month nobody paid
-- for. This pins the fix: 'fullMonthsBetween' must count the same 11 months 'advance' resolves
-- to, and the second 'issue' at the same @t@ must be rejected.
testFebClampRegression :: IO ()
testFebClampRegression = do
  let start = UTCTime (fromGregorian 2020 4 29) (secondsToDiffTime (15 * 3600 + 16 * 60 + 21))
      t = UTCTime (fromGregorian 2021 3 28) (secondsToDiffTime (15 * 3600 + 16 * 60 + 21))
      st0 = LedgerState {balanceMonths = 18, balanceStartTs = start, balanceBadgeType = BTInvestor}
  -- advance resolves 11 fully elapsed months (not 10): the direct jump and the iterated boundary
  -- must agree, and here the iterated boundary lands exactly on t.
  Just (lapsed, st1) <- pure (advance t st0)
  lapsed `shouldBe` 11
  balanceStartTs st1 `shouldBe` t
  balanceMonths st1 `shouldBe` 7
  -- issuing at t is legitimate: balanceStartTs == t, the boundary was just reached, not consumed.
  Just (st2, periodStart, periodEnd) <- pure (issue t st1)
  periodStart `shouldBe` t
  balanceStartTs st2 `shouldBe` periodEnd
  balanceStartTs st2 `shouldSatisfy` (> t)
  -- re-running advance-then-issue at the same t must now append nothing: no further lapse (the
  -- new start is already past t)...
  advance t st2 `shouldBe` Nothing
  -- ...and no second issuance.
  issue t st2 `shouldBe` Nothing

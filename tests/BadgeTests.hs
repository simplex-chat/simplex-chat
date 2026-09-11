{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-ambiguous-fields #-}

module BadgeTests (badgeTests) where

import BadgeService.Service (badgeErrorRetryAfter)
import Control.Concurrent.STM (atomically)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Data.Time.Calendar (fromGregorian)
import Data.Time.Calendar.WeekDate (toWeekDate)
import Data.Time.Clock (NominalDiffTime, UTCTime (..), addUTCTime, diffUTCTime, getCurrentTime, nominalDay)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import qualified Data.Aeson as J
import qualified Data.Aeson.KeyMap as KM
import Data.Maybe (fromMaybe, isNothing, maybeToList)
import qualified Simplex.Messaging.Crypto as C
import Simplex.Chat.Badges
import Simplex.Chat.Badges.Code
import Simplex.Chat.Badges.Ledger
import Simplex.Chat.Badges.Service
import Simplex.Chat (defaultChatConfig)
import Simplex.Chat.Controller (ChatError (..), ChatErrorType (..), badgeRetryInterval, chatErrorAgent)
import Simplex.Chat.Library.Commands (badgeErrorRetry, badgeRetryAfter, badgeStalledInterval)
import Simplex.Messaging.Agent.Protocol (AgentErrorType (..), AgentServiceError (..), SMPAgentError (..))
import Simplex.Messaging.Agent.RetryInterval (RetryInterval (..), nextRetryDelay)
import Simplex.Messaging.Crypto.BBS
import Simplex.Messaging.Protocol (BrokerErrorType (..), NetworkError (..))
import Simplex.Messaging.Version.Internal (Version (..))
import Test.Hspec

badgeTests :: Spec
badgeTests = do
  it "full workflow: request, issue, verify credential, generate and verify proof" testFullWorkflow
  it "should reject badge with tampered type" testTamperedType
  it "should reject badge with tampered expiry" testTamperedExpiry
  it "should reject badge with wrong server key" testWrongKey
  it "should report a key index missing from configured keys" testUnknownKeyIdx
  it "should compute badge status correctly" testExpiryCheck
  it "should accept unknown badge types" testUnknownBadgeType
  it "credential serializes to a paste-able token and back" testCredentialSerialization
  describe "redemption codes" $ do
    it "a generated code reads back" testCodeRoundTrip
    it "reads a code as typed - any case, separators, ambiguous characters" testCodeNormalisation
    it "rejects a code whose check character does not match" testCodeCheckCharacter
    it "hashes the canonical form, whatever was typed" testCodeHash
  describe "ledger transitions" $ do
    it "issues a twelve month code one month at a time, and no thirteenth" testTwelveMonths
    it "lapses only the elapsed months after a gap, leaving paidThrough unchanged" testLapseAfterGap
    it "keeps the balance non-negative and the start non-decreasing" testLedgerInvariants
    it "credits an exhausted balance from the grant, not from the date it ran out" testGrantAfterExhausted
    it "does not issue months topped up inside an issued period" testGrantInsideIssuedPeriod
    it "clips month ends without losing the issued period start" testMonthEndClipping
    it "counts the elapsed months of an absurd run in one step" testElapsedFarAnchor
    it "expires at the end of the Monday after the period" testMondayExpiry
    it "stores the wire tag of every entry type, and rebuilds each from its stored JSON" testEntryTypeColumns
  describe "checking a statement" $ do
    it "accepts a chain of entries, each against the one before it" testChecksChain
    it "checks the second entry against the first, not against the tip" testChecksAgainstStatement
    it "accepts an opening credit with no predecessor, and rejects anything else" testChecksOpening
    it "rejects an opening credit naming a badge type the purchase is not for" testChecksOpeningBadgeType
    it "accepts an opening credit restating the balance over a tip it does not follow" testChecksOpeningRestatement
    it "rejects a lapse writing off months that had not elapsed" testChecksOverLapse
    it "rejects a debit whose start or anchor moved" testChecksMovedStart
    it "rejects a grant restarting a run the predecessor still funds" testChecksGrantRestart
    it "rejects a credit of negative months" testChecksNegativeCredit
    it "leaves an entry ahead of the clock unjudged, and rejects one behind the entry it follows" testChecksTimestamps
    it "leaves an entry type it cannot derive unchecked, its months still checked" testChecksUnknownType
    it "rejects an entry it cannot rebuild whose coverage or months contradict the ledger" testChecksUncheckedInvariants
  describe "worker retry" $ do
    it "repeats a failure that can clear on its own, and no other" testRetryClassification
    it "backs off to the cap" testRetryBackoff
    it "floors the wait a service asks for, and honours anything above it" testServiceRetryFloor
    it "sends retryAfter with the transient service codes and no other" testServiceRetryAfter
  describe "service protocol JSON" $ do
    it "redeemBadgeCode request matches the schema" testRedeemRequestJSON
    it "badgeCredential response matches the schema" testCredentialResponseJSON
    it "error response matches the schema" testErrorResponseJSON
    it "statement entries round-trip, unknown entry types verbatim" testStatementJSON

proofOf :: BadgeProof -> BBSProof
proofOf (BadgeProof _ _ p _) = p

nonDecreasing :: Ord a => [a] -> Bool
nonDecreasing xs = and $ zipWith (<=) xs (drop 1 xs)

testKeyIdx :: Int
testKeyIdx = 1

keysFor :: BBSPublicKey -> Map Int BBSPublicKey
keysFor = M.singleton testKeyIdx

testFullWorkflow :: IO ()
testFullWorkflow = do
  Right (pk, sk) <- bbsKeyGen
  drg <- C.newRandom
  mk <- generateMasterKey drg
  let req = BadgeRequest {masterKey = mk, badgeInfo = BadgeInfo {badgeType = BTSupporter, badgeExpiry = futureTime, badgeExtra = ""}}
  Just vreq <- verifyPayment (BPRedeemCode "TEST") req
  Right cred <- issueBadge testKeyIdx sk vreq
  let BadgeCredential idx mk' _ _ = cred
  idx `shouldBe` testKeyIdx
  mk' `shouldBe` mk
  verifyCredential pk cred >>= (`shouldBe` True)
  Right badge <- generateBadgeProof pk cred (BBSPresHeader "nonce-1")
  -- the proof inherits the credential's key index, so receivers find the right key
  let BadgeProof {badgeKeyIdx} = badge
  badgeKeyIdx `shouldBe` testKeyIdx
  verifyBadge (keysFor pk) badge >>= (`shouldBe` Just True)
  Right badge2 <- generateBadgeProof pk cred (BBSPresHeader "nonce-2")
  verifyBadge (keysFor pk) badge2 >>= (`shouldBe` Just True)
  proofOf badge `shouldNotBe` proofOf badge2

testTamperedType :: IO ()
testTamperedType = do
  (pk, BadgeProof idx ph p info) <- issueBadgeProof BTSupporter futureTime
  verifyBadge (keysFor pk) (BadgeProof idx ph p info {badgeType = BTLegend}) >>= (`shouldBe` Just False)

testTamperedExpiry :: IO ()
testTamperedExpiry = do
  (pk, BadgeProof idx ph p info) <- issueBadgeProof BTSupporter futureTime
  verifyBadge (keysFor pk) (BadgeProof idx ph p info {badgeExpiry = pastTime}) >>= (`shouldBe` Just False)

testWrongKey :: IO ()
testWrongKey = do
  (_, badge) <- issueBadgeProof BTSupporter futureTime
  Right (pk2, _) <- bbsKeyGen
  verifyBadge (keysFor pk2) badge >>= (`shouldBe` Just False)

testUnknownKeyIdx :: IO ()
testUnknownKeyIdx = do
  (pk, badge) <- issueBadgeProof BTSupporter futureTime
  -- a key index not in the configured keys cannot be verified at all (Nothing)
  verifyBadge (M.singleton (testKeyIdx + 1) pk) badge >>= (`shouldBe` Nothing)

testExpiryCheck :: IO ()
testExpiryCheck = do
  now <- getCurrentTime
  let info expiry = BadgeInfo {badgeType = BTSupporter, badgeExpiry = expiry, badgeExtra = ""}
      futureInfo = info futureTime
      expiredAgo d = info $ addUTCTime (- (d * nominalDay)) now
  mkBadgeStatus now (Just True) futureInfo `shouldBe` BSActive
  -- the badge stays active for a week after its expiry
  mkBadgeStatus now (Just True) (expiredAgo 1) `shouldBe` BSActive
  mkBadgeStatus now (Just True) (expiredAgo 6) `shouldBe` BSActive
  -- then it is shown as expired for 31 days
  mkBadgeStatus now (Just True) (expiredAgo 8) `shouldBe` BSExpired
  mkBadgeStatus now (Just True) (expiredAgo 37) `shouldBe` BSExpired
  mkBadgeStatus now (Just True) (expiredAgo 39) `shouldBe` BSExpiredOld
  mkBadgeStatus now (Just True) (info pastTime) `shouldBe` BSExpiredOld
  mkBadgeStatus now (Just False) futureInfo `shouldBe` BSFailed
  mkBadgeStatus now Nothing futureInfo `shouldBe` BSUnknownKey

testUnknownBadgeType :: IO ()
testUnknownBadgeType = do
  (pk, badge) <- issueBadgeProof (BTUnknown "future_type") futureTime
  verifyBadge (keysFor pk) badge >>= (`shouldBe` Just True)

testCredentialSerialization :: IO ()
testCredentialSerialization = do
  Right (pk, sk) <- bbsKeyGen
  drg <- C.newRandom
  mk <- generateMasterKey drg
  let mkCred expiry = do
        Right cred <- issueBadge testKeyIdx sk (VerifiedBadgeRequest BadgeRequest {masterKey = mk, badgeInfo = BadgeInfo {badgeType = BTSupporter, badgeExpiry = expiry, badgeExtra = ""}})
        pure cred
  dated <- mkCred futureTime
  J.eitherDecode (J.encode dated) `shouldBe` Right dated
  -- a decoded credential still verifies against the issuing key
  case J.eitherDecode (J.encode dated) of
    Right cred -> verifyCredential pk cred >>= (`shouldBe` True)
    Left e -> expectationFailure e

-- Helpers

futureTime :: UTCTime
futureTime = posixSecondsToUTCTime 4102444800 -- 2099-12-31

pastTime :: UTCTime
pastTime = posixSecondsToUTCTime 1577836800 -- 2020-01-01

issueBadgeProof :: BadgeType -> UTCTime -> IO (BBSPublicKey, BadgeProof)
issueBadgeProof bt expiry = do
  Right (pk, sk) <- bbsKeyGen
  drg <- C.newRandom
  mk <- generateMasterKey drg
  let vreq = VerifiedBadgeRequest BadgeRequest {masterKey = mk, badgeInfo = BadgeInfo {badgeType = bt, badgeExpiry = expiry, badgeExtra = ""}}
  Right cred <- issueBadge testKeyIdx sk vreq
  Right badge <- generateBadgeProof pk cred (BBSPresHeader "test-nonce")
  pure (pk, badge)

-- Redemption codes

testCodeRoundTrip :: IO ()
testCodeRoundTrip = do
  drg <- C.newRandom
  code <- randomBadgeCode drg
  let formatted = formatBadgeCode code
  T.length formatted `shouldBe` 26 -- SB-XXXXX-XXXXX-XXXXX-XXXXX
  T.take 3 formatted `shouldBe` "SB-"
  T.length (badgeCodeText code) `shouldBe` 22 -- the canonical form drops the separators
  parseBadgeCode formatted `shouldBe` Just code
  parseBadgeCode (badgeCodeText code) `shouldBe` Just code

testCodeNormalisation :: IO ()
testCodeNormalisation = do
  drg <- C.newRandom
  code <- randomBadgeCode drg
  parseBadgeCode (T.toLower $ badgeCodeText code) `shouldBe` Just code
  parseBadgeCode (T.replace "-" " " $ formatBadgeCode code) `shouldBe` Just code
  -- a fixed code, because a random one contains no 0 or 1 about a quarter of the time and the
  -- folding would then be asserted against nothing
  let folded = T.map ambiguous fixedCode
  folded `shouldNotBe` fixedCode
  parseBadgeCode folded `shouldBe` parseBadgeCode fixedCode
  parseBadgeCode fixedCode `shouldNotBe` Nothing
  where
    fixedCode = "SB-0C0QS-XAQW1-N1VSA-R00Y3"
    ambiguous = \case
      '1' -> 'I'
      '0' -> 'O'
      c -> c

testCodeCheckCharacter :: IO ()
testCodeCheckCharacter = do
  drg <- C.newRandom
  code <- randomBadgeCode drg
  let canonical = badgeCodeText code
      -- every other value for the last character fails the check
      wrong = T.init canonical <> T.singleton (if T.last canonical == 'Z' then 'Y' else 'Z')
  parseBadgeCode wrong `shouldBe` Nothing
  parseBadgeCode "" `shouldBe` Nothing
  parseBadgeCode "SB-00000-00000-00000-0000" `shouldBe` Nothing
  parseBadgeCode (T.drop 2 canonical) `shouldBe` Nothing

testCodeHash :: IO ()
testCodeHash = do
  drg <- C.newRandom
  code <- randomBadgeCode drg
  Just typed <- pure $ parseBadgeCode $ T.toLower $ formatBadgeCode code
  badgeCodeHash typed `shouldBe` badgeCodeHash code

-- Ledger transitions, against plans/2026-07-30-supporter-badges-v3-ux.md §3

at :: Integer -> Int -> Int -> UTCTime
at y m d = UTCTime (fromGregorian y m d) (11 * 3600)

newBalance :: UTCTime -> StatementEntry
newBalance t = emptyEntry t BTSupporter

-- these tests never write, so the id each operation stamps on its entry is never read
grant :: UTCTime -> Int -> StatementEntry -> StatementEntry
grant t n = grantEntry t "" n SCCode

lapse :: UTCTime -> StatementEntry -> Maybe StatementEntry
lapse t = lapseEntry t ""

issue :: UTCTime -> StatementEntry -> Maybe StatementEntry
issue t = issueEntry t ""

-- StatementEntry and BadgeInfo carry fields of the same names, so the selectors are ambiguous here
bMonths :: StatementEntry -> Int
bMonths StatementEntry {balanceMonths} = balanceMonths

bStart :: StatementEntry -> UTCTime
bStart StatementEntry {balanceStartTs} = balanceStartTs

-- the moment the service claims it wrote the row, which is what the check reads it against
stampedAt :: UTCTime -> StatementEntry -> StatementEntry
stampedAt t e = e {createdAt = t}

-- one service pass: lapse what elapsed, then issue if a month is due, as the service chains them.
-- The period an issue covers is the previous entry's balance start to its own, so a run of starts
-- is what the period assertions read.
pass :: UTCTime -> StatementEntry -> [StatementEntry]
pass t e0 = maybeToList lapsed <> maybeToList (issue t $ fromMaybe e0 lapsed)
  where
    lapsed = lapse t e0

finalBalance :: StatementEntry -> [StatementEntry] -> StatementEntry
finalBalance e0 rows = last (e0 : rows)

-- each pass issues exactly one month, so the entries returned are the months issued, in order
issueAll :: StatementEntry -> [StatementEntry]
issueAll e = case pass (bStart e) e of
  [] -> []
  rows -> let e' = finalBalance e rows in e' : issueAll e'

testTwelveMonths :: IO ()
testTwelveMonths = do
  let start = at 2026 3 10
      granted = grant start 12 (newBalance start)
      -- each month is issued as soon as it falls due
      issued = issueAll granted
      spent = finalBalance granted issued
  length issued `shouldBe` 12
  bMonths spent `shouldBe` 0
  -- no month was skipped or issued twice: consecutive starts, so the periods tile the whole year
  map bStart (granted : issued) `shouldBe` map (\m -> addMonths m start) [0 .. 12]
  bStart spent `shouldBe` at 2027 3 10
  -- a thirteenth request issues nothing, whenever it is made
  issue (bStart spent) spent `shouldSatisfy` isNothing
  issue (at 2030 1 1) spent `shouldSatisfy` isNothing
  lapse (at 2030 1 1) spent `shouldSatisfy` isNothing

-- 3 months bought 10 Mar, first issued the same day, no pass until 20 May: April lapses unissued
testLapseAfterGap :: IO ()
testLapseAfterGap = do
  let start = at 2026 3 10
      granted = grant start 3 (newBalance start)
      rows1 = pass start granted
      afterFirst = finalBalance granted rows1
      rows2 = pass (at 2026 5 20) afterFirst
      afterSecond = finalBalance afterFirst rows2
  map bMonths rows1 `shouldBe` [2]
  -- March is issued: from the granting entry's start to the issued entry's own
  bStart granted `shouldBe` at 2026 3 10
  map bStart rows1 `shouldBe` [at 2026 4 10]
  -- one lapse row for April, then May is issued: two rows, not one and not three. The lapse row's
  -- start is where May begins, so it is also the start of the period the issue that follows covers
  map bMonths rows2 `shouldBe` [1, 0]
  map bStart rows2 `shouldBe` [at 2026 5 10, at 2026 6 10]
  -- a lapse moves months from unused to gone; it never changes what was paid for
  map paidThrough (granted : rows1 <> rows2) `shouldBe` replicate 4 (at 2026 6 10)
  bMonths afterSecond `shouldBe` 0

data LedgerStep = Grant UTCTime Int | Pass UTCTime

testLedgerInvariants :: IO ()
testLedgerInvariants = do
  let start = at 2026 1 15
      reopened = at 2027 9 9
      -- the April grant lands exactly where coverage ended and continues the run, the 2027 one
      -- lands past it and restarts, so both branches of grantEntry run under the invariants
      steps =
        [ Grant start 3,
          Pass start,
          Pass (at 2026 4 15),
          Grant (at 2026 4 15) 2,
          Pass (at 2026 5 1),
          Pass reopened,
          Grant reopened 1,
          Pass reopened
        ]
      step (e, rows) = \case
        Grant t n -> let e' = grant t n e in (e', rows <> [e'])
        Pass t -> let rs = pass t e in (finalBalance e rs, rows <> rs)
      (_, allRows) = foldl step (newBalance start, []) steps
  map bMonths allRows `shouldSatisfy` all (>= 0)
  map bStart allRows `shouldSatisfy` nonDecreasing

testGrantAfterExhausted :: IO ()
testGrantAfterExhausted = do
  -- the balance ran out on 10 Feb; the next code is redeemed on 1 Jun
  let spent = newBalance (at 2026 2 10)
      granted = grant (at 2026 6 1) 2 spent
  bStart granted `shouldBe` at 2026 6 1
  paidThrough granted `shouldBe` at 2026 8 1
  -- the four unsupported months are not backfilled, so nothing lapses immediately
  lapse (at 2026 6 1) granted `shouldSatisfy` isNothing

testGrantInsideIssuedPeriod :: IO ()
testGrantInsideIssuedPeriod = do
  let start = at 2026 1 10
      granted = grant start 1 (newBalance start)
      issued = finalBalance granted $ pass start granted
      -- topped up on 20 Jan, while the month issued on 10 Jan still runs
      toppedUp = grant (at 2026 1 20) 3 issued
  -- February is where the next period starts, the top-up having been spent on neither January nor a gap
  bStart toppedUp `shouldBe` at 2026 2 10
  paidThrough toppedUp `shouldBe` at 2026 5 10
  -- the balance starts in the future, so no second credential is issued for January
  issue (at 2026 1 20) toppedUp `shouldSatisfy` isNothing
  fmap bStart (issue (at 2026 2 10) toppedUp) `shouldBe` Just (at 2026 3 10)

testMonthEndClipping :: IO ()
testMonthEndClipping = do
  let start = at 2027 1 31
      granted = grant start 3 (newBalance start)
      issued = issueAll granted
  -- February clips to the 28th, and March goes back to the 31st: clipping does not accumulate.
  -- Each period runs from one start to the next, so these bounds are the three periods
  map bStart (granted : issued) `shouldBe` [at 2027 1 31, at 2027 2 28, at 2027 3 31, at 2027 4 30]
  -- the issued period start is the previous balance start, never periodEnd minus a month, which
  -- clipping would answer as 28 Jan
  addMonths (-1) (bStart (head issued)) `shouldNotBe` bStart granted
  -- across a leap day
  let leap = grant (at 2028 1 29) 2 (newBalance (at 2028 1 29))
  map bStart (issueAll leap) `shouldBe` [at 2028 2 29, at 2028 3 29]
  -- a month that ends on the leap day counts as elapsed the moment it ends, and not before
  fmap bMonths (lapse (at 2028 2 29) leap) `shouldBe` Just 1
  lapse (addUTCTime (-1) (at 2028 2 29)) leap `shouldSatisfy` isNothing
  -- buying a month at a time keeps the day of month that buying three at once keeps
  let jan = grant (at 2027 1 31) 1 (newBalance (at 2027 1 31))
  case issue (at 2027 1 31) jan of
    Just issuedJan -> do
      let feb = grant (at 2027 2 20) 1 issuedJan
      fmap bStart (issue (at 2027 2 28) feb) `shouldBe` Just (at 2027 3 31)
    Nothing -> expectationFailure "January was not issued"

-- The anchor and the month count are the service's, and a run claiming to have started a thousand
-- years ago with maxBound months is answered the same way as any other.
testElapsedFarAnchor :: IO ()
testElapsedFarAnchor = do
  let far = at 1000 1 10
      now = at 2026 1 10
      elapsed = (2026 - 1000) * 12
      huge = (newBalance far) {balanceMonths = maxBound}
      three = (newBalance far) {balanceMonths = 3}
  fmap bMonths (lapse now huge) `shouldBe` Just (maxBound - elapsed)
  fmap bStart (lapse now huge) `shouldBe` Just now
  -- and never writes off more months than the balance holds, however long ago it started
  fmap bMonths (lapse now three) `shouldBe` Just 0
  fmap bStart (lapse now three) `shouldBe` Just (at 1000 4 10)

testMondayExpiry :: IO ()
testMondayExpiry = do
  -- the end of Monday 13 Apr is Tuesday 14 Apr 00:00
  endOfMondayAfter (at 2026 4 10) `shouldBe` UTCTime (fromGregorian 2026 4 14) 0
  endOfMondayAfter (at 2026 6 10) `shouldBe` UTCTime (fromGregorian 2026 6 16) 0
  -- a period ending on a Monday still runs to the end of the following Monday, never to zero days
  endOfMondayAfter (at 2026 4 13) `shouldBe` UTCTime (fromGregorian 2026 4 21) 0
  let periodEnds = map (\d -> at 2026 4 d) [1 .. 30]
      expiries = map endOfMondayAfter periodEnds
  -- every expiry is a Tuesday midnight more than a day after its period, and at most eight
  expiries `shouldSatisfy` all (\(UTCTime d t) -> t == 0 && (\(_, _, wd) -> wd == 2) (toWeekDate d))
  zipWith diffUTCTime expiries periodEnds `shouldSatisfy` all (\d -> d > nominalDay && d <= 8 * nominalDay)

verdicts :: UTCTime -> Maybe StatementEntry -> [StatementEntry] -> [Maybe Bool]
verdicts now tip = map snd . balanceChecked now BTSupporter tip

testChecksChain :: IO ()
testChecksChain = do
  let opened = newBalance (at 2026 1 10)
      oneMonth = grant (at 2026 1 10) 1 opened
  Just spent <- pure $ issue (at 2026 1 10) oneMonth
  let granted = grant (at 2026 3 10) 3 spent
      rows1 = pass (at 2026 3 10) granted
      afterFirst = finalBalance granted rows1
      rows2 = pass (at 2026 5 20) afterFirst
      statement = granted : rows1 <> rows2
  -- grant, issue, lapse, issue - the first checked against the stored tip
  verdicts (at 2026 5 20) (Just spent) statement `shouldBe` replicate 4 (Just True)

-- The client stores what it received, and the next row is what the service computed from the row it
-- sent - never from the tip, which that row has already superseded.
testChecksAgainstStatement :: IO ()
testChecksAgainstStatement = do
  let start = at 2026 3 10
      granted = grant start 3 (newBalance start)
  Just firstRow <- pure $ issue start granted
  Just secondRow <- pure $ issue (at 2026 4 10) firstRow
  verdicts (at 2026 4 10) (Just granted) [firstRow, secondRow] `shouldBe` [Just True, Just True]
  -- against the tip it followed, the second entry does not add up
  verdicts (at 2026 4 10) (Just granted) [secondRow] `shouldBe` [Just False]

-- The seed is what redeemCode grants onto, so the row it authors is the one that verifies here.
testChecksOpening :: IO ()
testChecksOpening = do
  let t = at 2026 3 10
      opening = grant t 12 (newBalance t)
  verdicts t Nothing [opening] `shouldBe` [Just True]
  Just issued <- pure $ issue t opening
  verdicts t Nothing [issued] `shouldBe` [Just False]

-- The seed takes the purchase's badge type, not the statement's, so an opening row cannot assert a
-- badge the purchase was never for - the one field on that path with something to check it against.
testChecksOpeningBadgeType :: IO ()
testChecksOpeningBadgeType = do
  let t = at 2026 3 10
      opening = grant t 12 (newBalance t)
  verdicts t Nothing [opening] `shouldBe` [Just True]
  verdicts t Nothing [opening {balanceBadgeType = BTLegend}] `shouldBe` [Just False]

-- An opening credit resets the ledger to the amount it states, so it is the one entry whose
-- balance owes nothing to the row before it - a new device, or history discarded into a balance
-- brought forward. It still cannot state a balance other than the months it credits.
testChecksOpeningRestatement :: IO ()
testChecksOpeningRestatement = do
  let start = at 2026 3 10
      granted = grant start 3 (newBalance start)
      restated = granted {entryType = SECredit SCOpening, changeMonths = 3, balanceMonths = 3}
  -- the tip funds two months from April; the opening restates three from March and still holds
  Just spent <- pure $ issue start granted
  verdicts start (Just spent) [restated] `shouldBe` [Just True]
  verdicts start (Just spent) [restated {balanceMonths = 9}] `shouldBe` [Just False]
  verdicts start (Just spent) [restated {balanceBadgeType = BTLegend}] `shouldBe` [Just False]

-- Over-lapsing empties the balance while paidThrough stays where it was: the badge stops renewing
-- and the ledger still reads as paid up. The row is self-consistent with the one before it, so only
-- re-running the lapse against its own timestamp catches it.
testChecksOverLapse :: IO ()
testChecksOverLapse = do
  let start = at 2026 3 10
      granted = grant start 3 (newBalance start)
  Just issued <- pure $ issue start granted
  Just lapsed <- pure $ lapse (at 2026 5 20) issued
  Just overLapsed <- pure $ lapse (at 2026 8 20) issued
  verdicts (at 2026 5 20) (Just issued) [lapsed] `shouldBe` [Just True]
  verdicts (at 2026 5 20) (Just issued) [stampedAt (at 2026 5 20) overLapsed] `shouldBe` [Just False]
  bMonths overLapsed `shouldBe` bMonths lapsed - 1
  paidThrough overLapsed `shouldBe` paidThrough lapsed
  -- and a lapse claiming a month before any had elapsed: lapseEntry declines it altogether
  verdicts start (Just issued) [stampedAt start lapsed] `shouldBe` [Just False]

testChecksMovedStart :: IO ()
testChecksMovedStart = do
  let start = at 2026 3 10
      granted = grant start 3 (newBalance start)
  Just issued <- pure $ issue start granted
  verdicts start (Just granted) [issued] `shouldBe` [Just True]
  verdicts start (Just granted) [issued {balanceStartTs = addMonths 1 (bStart issued)}] `shouldBe` [Just False]
  verdicts start (Just granted) [issued {balanceAnchorTs = addMonths 1 start}] `shouldBe` [Just False]

testChecksGrantRestart :: IO ()
testChecksGrantRestart = do
  let t = at 2026 2 10
      opened = newBalance t
      oneMonth = grant t 1 opened
      funded = grant t 2 opened
  Just spent <- pure $ issue t oneMonth
  let restarted = grant (at 2026 6 1) 2 spent
  verdicts (at 2026 6 1) (Just spent) [restarted] `shouldBe` [Just True]
  -- the same entry after a predecessor with months left is a run moved to a later start
  verdicts (at 2026 6 1) (Just funded) [restarted] `shouldBe` [Just False]

testChecksNegativeCredit :: IO ()
testChecksNegativeCredit = do
  let t = at 2026 2 10
      funded = grant t 2 (newBalance t)
      negativeCredit = grant (at 2026 6 1) (-2) funded
  verdicts (at 2026 6 1) (Just funded) [negativeCredit] `shouldBe` [Just False]
  -- the sign is what rejects it: the row itself adds up, and the recompute would confirm it
  bMonths negativeCredit `shouldBe` bMonths funded - 2

testChecksTimestamps :: IO ()
testChecksTimestamps = do
  let start = at 2026 3 10
      granted = grant start 3 (newBalance start)
  Just issued <- pure $ issue start granted
  -- the two clocks are not the same clock, so a row from just ahead of this one is not evidence
  verdicts start (Just granted) [stampedAt (addUTCTime (30 * 60) start) issued] `shouldBe` [Just True]
  -- further ahead than that, we cannot tell their clock from ours, so the row is left unjudged
  verdicts start (Just granted) [stampedAt (addUTCTime (2 * 3600) start) issued] `shouldBe` [Nothing]
  -- behind the row it follows is the service against itself, with no clock of ours in it
  verdicts start (Just granted) [stampedAt (at 2026 3 1) issued] `shouldBe` [Just False]

-- Marking a row this version has no operation for as broken would report a newer service as
-- tampering, which is the opposite of the forward compatibility the rest of this code keeps.
testChecksUnknownType :: IO ()
testChecksUnknownType = do
  let start = at 2026 3 10
      granted = grant start 3 (newBalance start)
  Just issued <- pure $ issue start granted
  let unknown = issued {entryType = SEDebit SDUnknown {tag = "future", json = KM.empty}}
  verdicts start (Just granted) [unknown] `shouldBe` [Nothing]
  verdicts start (Just granted) [issued {entryType = SEDebit SDRefund}] `shouldBe` [Nothing]
  verdicts start (Just granted) [unknown {balanceMonths = 5}] `shouldBe` [Just False]
  verdicts start (Just granted) [stampedAt (at 2026 3 1) unknown] `shouldBe` [Just False]
  -- an unknown credit takes the same path: fall through to grantEntry and its negative count,
  -- which issued carries, would be rejected instead
  let unknownCredit = issued {entryType = SECredit SCUnknown {tag = "future", json = KM.empty}}
  verdicts start (Just granted) [unknownCredit] `shouldBe` [Nothing]

-- A tag with no operation behind it escapes the recompute, leaving only the months identity - which
-- holds while coverage moves back, or while the balance goes into debt.
testChecksUncheckedInvariants :: IO ()
testChecksUncheckedInvariants = do
  let start = at 2026 3 10
      granted = grant start 3 (newBalance start)
  Just issued <- pure $ issue start granted
  let shortened = issued {entryType = SEDebit SDRefund, changeMonths = 0, balanceStartTs = addMonths (-1) (bStart issued)}
      owing = issued {entryType = SEDebit SDRefund, changeMonths = -3, balanceMonths = -1}
  verdicts start (Just issued) [shortened] `shouldBe` [Just False]
  paidThrough shortened `shouldBe` addMonths (-1) (paidThrough issued)
  verdicts start (Just issued) [owing] `shouldBe` [Just False]
  bMonths owing `shouldBe` bMonths issued - 3

-- A failed renewal is otherwise left until the next chat start or activate, which on a desktop
-- left running can be days - long enough for a funded badge to lapse.
testRetryClassification :: IO ()
testRetryClassification = do
  let retryFor = badgeErrorRetry . chatErrorAgent
  -- an unanswered request is the likeliest renewal failure, and it is the agent's own error
  retryFor (AGENT (A_SERVICE ASETimeout)) `shouldBe` True
  retryFor (BROKER "localhost" TIMEOUT) `shouldBe` True
  retryFor (BROKER "localhost" (NETWORK NETimeoutError)) `shouldBe` True
  -- terminal: the same request would fail the same way, and repeating it would spin
  retryFor (AGENT (A_SERVICE ASEBadSignature)) `shouldBe` False
  retryFor (AGENT (A_SERVICE (ASERejected "no"))) `shouldBe` False
  badgeErrorRetry (ChatError (CECommandError "unexpected badge service response")) `shouldBe` False

-- A failure that never clears is repeated for as long as the balance funds a month, so the wait
-- has to grow: at a fixed interval an annual code would ask hundreds of times a day, all year.
testRetryBackoff :: IO ()
testRetryBackoff = do
  let ri@RetryInterval {initialInterval, maxInterval} = badgeRetryInterval defaultChatConfig
      advance (elapsed, delay) =
        let elapsed' = elapsed + delay
         in (elapsed', nextRetryDelay elapsed' delay ri)
      delays = map snd $ take 40 $ iterate advance (0, initialInterval)
  head delays `shouldBe` initialInterval
  delays `shouldSatisfy` all (\d -> d >= initialInterval && d <= maxInterval)
  delays `shouldSatisfy` nonDecreasing
  -- it reaches the cap rather than creeping towards it, and stays there
  last delays `shouldBe` maxInterval

-- A service answering retryAfter 0 would put the next attempt at now, and the worker would ask
-- again as fast as the round trip allows, for as long as the service kept answering that way.
testServiceRetryFloor :: IO ()
testServiceRetryFloor = do
  let ri@RetryInterval {initialInterval, maxInterval} = badgeRetryInterval defaultChatConfig
      floorWait = fromIntegral initialInterval / 1000000 :: NominalDiffTime
      aboveCap = 2 * fromIntegral maxInterval / 1000000 :: NominalDiffTime
  -- a code carrying no wait is terminal for this request, and waits what any stalled month waits
  badgeRetryAfter ri Nothing `shouldBe` badgeStalledInterval
  -- nothing the service names brings the wait below where a retry of its own would start
  badgeRetryAfter ri (Just 0) `shouldBe` floorWait
  badgeRetryAfter ri (Just 1) `shouldBe` floorWait
  badgeRetryAfter ri (Just $ round floorWait) `shouldBe` floorWait
  -- above that it is honoured as sent, and not capped: a service may know it is down for the day
  badgeRetryAfter ri (Just 600) `shouldBe` 600
  badgeRetryAfter ri (Just $ round aboveCap) `shouldBe` aboveCap

-- badges-rpc.md defines retryAfter as marking the transient codes, and every other code as
-- terminal for the command attempted. The client repeats a code that carries one on the service's
-- schedule, so the set is the protocol's and not a judgement to make per call site.
testServiceRetryAfter :: IO ()
testServiceRetryAfter = do
  badgeErrorRetryAfter BSEPaymentPending `shouldBe` Just 300
  badgeErrorRetryAfter BSEProviderUnavailable `shouldBe` Just 300
  badgeErrorRetryAfter BSERateLimited `shouldBe` Just 60
  -- internal is the one most likely to clear on its own, and is still terminal: repeating it on
  -- the service's cadence presses a service already failing, and the client has its own floor
  badgeErrorRetryAfter BSEInternal `shouldBe` Nothing
  mapM_
    (\code -> badgeErrorRetryAfter code `shouldBe` Nothing)
    [BSEBadRequest, BSEUnsupportedVersion, BSEUnknownPurchaseKey, BSECodeInvalid, BSECodeUsed, BSECodeExpired, BSEUnknown "future_code"]

-- The client replicates entry_credit_type / entry_debit_type verbatim, so a stored tag that
-- disagreed with the wire tag would put a different row on each side.
testEntryTypeColumns :: IO ()
testEntryTypeColumns = do
  k <- fst <$> (C.newRandom >>= \g -> atomically (C.generateKeyPair g) :: IO (C.KeyPair 'C.Ed25519))
  let credits = [SCPayment Nothing, SCCode, SCCharge "ch1", SCSupport, SCTransferIn k, SCOpening]
      debits = [SDRefund, SDUpgrade k, SDTransferOut k, SDSupport, SDBadge, SDLapse]
  mapM_ (\c -> wireTag (J.toJSON (SECredit c)) "credit" `shouldBe` Just (creditTypeTag c)) credits
  mapM_ (\d -> wireTag (J.toJSON (SEDebit d)) "debit" `shouldBe` Just (debitTypeTag d)) debits
  -- the three types this version writes survive a round trip through the columns
  mapM_
    (\t -> uncurry3 entryTypeFromColumns (entryTypeColumns t) `shouldSatisfy` sameEntryType t)
    [SECredit SCCode, SEDebit SDBadge, SEDebit SDLapse]
  -- a type that needs a reference column is not silently read back as something else
  uncurry3 entryTypeFromColumns (entryTypeColumns (SECredit (SCCharge "ch1"))) `shouldSatisfy` isNothing
  -- which is why every type is stored as its own JSON as well, and read from that first: the
  -- columns alone would answer a row naming an invoice or a purchase as no row at all
  mapM_ roundTrips credits
  mapM_ roundTrips debits
  where
    uncurry3 f (a, b, c) = f a b c
    sameEntryType t = maybe False ((J.toJSON t ==) . J.toJSON)
    wireTag v fld = case v of
      J.Object o | Just (J.Object inner) <- KM.lookup fld o, Just (J.String t) <- KM.lookup "type" inner -> Just t
      _ -> Nothing

-- Service protocol JSON, against docs/protocol/badges-rpc.schema.json

testRedeemRequestJSON :: IO ()
testRedeemRequestJSON = do
  drg <- C.newRandom
  mk <- generateMasterKey drg
  (k, _) <- atomically $ C.generateKeyPair drg :: IO (C.KeyPair 'C.Ed25519)
  code <- randomBadgeCode drg
  let req = BadgeServiceRequest {version = Version 1, purchaseKey = Just k, request = BSCRedeemBadgeCode {masterKey = mk, code = badgeCodeText code}}
  J.toJSON req
    `shouldBe` J.object
      [ "version" J..= (1 :: Int),
        "purchaseKey" J..= k,
        "request" J..= J.object ["type" J..= ("redeemBadgeCode" :: T.Text), "masterKey" J..= mk, "code" J..= badgeCodeText code]
      ]
  -- purchaseKey is optional in the schema, and a nullary command is a bare tagged object
  J.toJSON BadgeServiceRequest {version = Version 1, purchaseKey = Nothing, request = BSCGetBadgeCatalog}
    `shouldBe` J.object ["version" J..= (1 :: Int), "request" J..= J.object ["type" J..= ("getBadgeCatalog" :: T.Text)]]
  roundTrips req

testCredentialResponseJSON :: IO ()
testCredentialResponseJSON = do
  Right (_, sk) <- bbsKeyGen
  drg <- C.newRandom
  mk <- generateMasterKey drg
  let info = BadgeInfo {badgeType = BTSupporter, badgeExpiry = futureTime, badgeExtra = ""}
  Right cred <- issueBadge testKeyIdx sk (VerifiedBadgeRequest BadgeRequest {masterKey = mk, badgeInfo = info})
  let resp = BSPBadgeCredential {credential = Just cred, receipt = Nothing, statement = BadgeStatement {entries = [], previousEntryId = Nothing}}
  J.toJSON resp
    `shouldBe` J.object
      [ "type" J..= ("badgeCredential" :: T.Text),
        "credential" J..= cred,
        "statement" J..= J.object ["entries" J..= ([] :: [J.Value])]
      ]
  roundTrips resp

testErrorResponseJSON :: IO ()
testErrorResponseJSON = do
  let resp = BSPError {code = BSECodeInvalid, message = Nothing, retryAfter = Nothing}
  J.toJSON resp `shouldBe` J.object ["type" J..= ("error" :: T.Text), "code" J..= ("code_invalid" :: T.Text)]
  J.toJSON BSPError {code = BSERateLimited, message = Just "slow down", retryAfter = Just 30}
    `shouldBe` J.object ["type" J..= ("error" :: T.Text), "code" J..= ("rate_limited" :: T.Text), "message" J..= ("slow down" :: T.Text), "retryAfter" J..= (30 :: Int)]

testStatementJSON :: IO ()
testStatementJSON = do
  let entry =
        StatementEntry
          { entryId = "e1",
            changeMonths = 3,
            balanceMonths = 3,
            balanceStartTs = futureTime,
            balanceAnchorTs = futureTime,
            balanceBadgeType = BTSupporter,
            wasPausedSince = Nothing,
            createdAt = futureTime,
            entryType = SECredit {credit = SCPayment {invoiceId = Nothing}}
          }
  -- the whole entry: the required fields, and wasPausedSince omitted rather than sent as null
  J.toJSON entry
    `shouldBe` J.object
      [ "entryId" J..= ("e1" :: T.Text),
        "changeMonths" J..= (3 :: Int),
        "balanceMonths" J..= (3 :: Int),
        "balanceStartTs" J..= futureTime,
        "balanceAnchorTs" J..= futureTime,
        "balanceBadgeType" J..= ("supporter" :: T.Text),
        "createdAt" J..= futureTime,
        "entryType" J..= entryType entry
      ]
  J.toJSON entry {wasPausedSince = Just pastTime} `shouldNotBe` J.toJSON entry
  J.toJSON (entryType entry) `shouldBe` J.object ["type" J..= ("credit" :: T.Text), "credit" J..= J.object ["type" J..= ("payment" :: T.Text)]]
  J.toJSON SEDebit {debit = SDBadge} `shouldBe` J.object ["type" J..= ("debit" :: T.Text), "debit" J..= J.object ["type" J..= ("badge" :: T.Text)]]
  J.toJSON SEDebit {debit = SDLapse} `shouldBe` J.object ["type" J..= ("debit" :: T.Text), "debit" J..= J.object ["type" J..= ("lapse" :: T.Text)]]
  -- a code grant is its own credit type, not a payment whose invoiceId happens to be absent
  J.toJSON SECredit {credit = SCCode} `shouldBe` J.object ["type" J..= ("credit" :: T.Text), "credit" J..= J.object ["type" J..= ("code" :: T.Text)]]
  J.toJSON SECredit {credit = SCCode} `shouldNotBe` J.toJSON SECredit {credit = SCPayment {invoiceId = Nothing}}
  -- an entry type from a newer service is stored and re-emitted unchanged
  let futureCredit = J.object ["type" J..= ("grant" :: T.Text), "grantedBy" J..= ("operator" :: T.Text)]
  case J.fromJSON futureCredit of
    J.Success c@SCUnknown {tag} -> do
      tag `shouldBe` "grant"
      J.toJSON c `shouldBe` futureCredit
    r -> expectationFailure $ "expected SCUnknown, got " <> show (fmap (const ()) r)

-- decoding and re-encoding reproduces the encoding, without Eq on the protocol types
roundTrips :: (HasCallStack, J.ToJSON a, J.FromJSON a) => a -> IO ()
roundTrips x = case J.eitherDecode (J.encode x) of
  Right x' -> J.toJSON (x' `asTypeOf` x) `shouldBe` J.toJSON x
  Left e -> expectationFailure e

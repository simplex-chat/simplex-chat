{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-ambiguous-fields #-}

module BadgeTests (badgeTests) where

import BadgeService.Service (badgeErrorRetryAfter, shownServiceRequest, survive)
import BadgeService.StoreReceipts (StoreChecks (..), StoreReceipt (..), StoreRefusal (..), StoreVerifier (..), VerifiedStoreTransaction (..), toStoreReceipt)
import BadgeService.StoreReceipts.Mock (newMockStoreVerifier)
import Control.Concurrent (forkIO, killThread, threadDelay)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.STM (atomically)
import Control.Exception (SomeAsyncException, SomeException, catch, fromException, throwIO)
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Base64.URL as B64U
import qualified Data.ByteString.Lazy as LB
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Time.Calendar (fromGregorian)
import Data.Time.Calendar.WeekDate (toWeekDate)
import Data.Time.Clock (NominalDiffTime, UTCTime (..), addUTCTime, diffUTCTime, getCurrentTime, nominalDay)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import qualified Data.Aeson as J
import qualified Data.Aeson.KeyMap as KM
import Data.Maybe (fromMaybe, isJust, isNothing, maybeToList)
import qualified Simplex.Messaging.Crypto as C
import Simplex.Chat.Badges
import Simplex.Chat.Badges.Code
import Simplex.Chat.Badges.Ledger
import Simplex.Chat.Badges.Service
import Simplex.Chat.Badges.Types (BadgeAlertKind (..), BadgeIssueError (..), BadgeIssueFailure (..), BadgeSubscription (..))
import qualified Simplex.Chat.Badges.Types as BT
import Simplex.Chat (defaultChatConfig)
import Simplex.Chat.Controller (BadgeRedeemError (..), ChatError (..), ChatErrorType (..), badgeRetryInterval, chatErrorAgent)
import Simplex.Chat.Library.Commands (badgeErrorRetry, badgeFailureTransient, badgeIssueFailure, badgeRenewal, badgeRetryAfter, badgeServiceErrorText, badgeStalledInterval, boundedSubscriptionState, derivedBadgeAlert, resumableCharge, resumeHoldEnded, storeTransactionRef)
import Simplex.Chat.PaymentService (AppleTransactionFacts (..), ServicePayment (..), appleTransactionFacts, appleTransactionRef)
import Simplex.Chat.PaymentService.Types (InvoiceId (..), PaymentProvider (..), StoreTransactionRef (..))
import qualified Simplex.Chat.Store.Badges as SB
import Simplex.Chat.Types (BoolDef (..))
import Simplex.Messaging.Agent.Protocol (AgentErrorType (..), AgentServiceError (..), SMPAgentError (..))
import Simplex.Messaging.Agent.RetryInterval (RetryInterval (..), nextRetryDelay)
import Simplex.Messaging.Crypto.BBS
import Simplex.Messaging.Encoding.String
import Simplex.Messaging.Protocol (BrokerErrorType (..), ErrorType (AUTH), NetworkError (..))
import Simplex.Messaging.Util (safeDecodeUtf8, tshow)
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
  it "presentation headers encode and decode" testPresHeaderEncoding
  it "presentation headers with binary bytes round-trip through JSON" testPresHeaderJSON
  it "should reject a proof presented under another chat binding" testOtherChatBinding
  it "should accept a profile proof only with the header of its chat" testProfileProofHeader
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
    it "accepts every row a store charge adds, on each path a subscription renews by" testChargeRowsVerify
  describe "worker retry" $ do
    it "repeats a failure that can clear on its own, and no other" testRetryClassification
    it "backs off to the cap" testRetryBackoff
    it "floors the wait a service asks for, and honours anything above it" testServiceRetryFloor
    it "sends retryAfter with the transient service codes and no other" testServiceRetryAfter
  describe "recording a failed renewal" $ do
    it "records the agent error a request failed with, not the chat error around it" testIssueFailureClassification
    it "waits for the credential to lapse on internal, as on a refusal the service marks transient" testServiceErrorTransience
    it "stores every failure so that it reads back, whatever the service called its code" testIssueFailureEncoding
    it "bounds and strips a code this version does not know" testServiceErrorCodeBounded
  describe "service protocol JSON" $ do
    it "redeemBadgeCode request matches the schema" testRedeemRequestJSON
    it "purchaseBadge request matches the schema" testPurchaseRequestJSON
    it "badgeCredential response matches the schema" testCredentialResponseJSON
    it "badgeCredential carries a subscription's state, and reads one it does not know as unknown" testCredentialSubscriptionJSON
    it "error response matches the schema" testErrorResponseJSON
    it "statement entries round-trip, unknown entry types verbatim" testStatementJSON
  describe "badge state" $ do
    it "carries a subscription's plan and last seen state, renewing only when it was seen renewing, and bounds a state it does not know" testBadgeSubscriptionJSON
    it "derives one alert for a subscription in order of urgency, dated paidThrough, and leaves prepaid alerts as they were" testSubscriptionAlertDerivation
  describe "store purchases" $ do
    it "keys a purchase by the store's transaction id, not by the evidence signed over it" testStoreTransactionRef
    it "claims an Apple subscription by its original transaction id, and any other transaction by its own" testAppleTransactionRef
    it "reads an Apple transaction's id, purchase and expiry dates, revocation and type from its unverified payload" testAppleTransactionFacts
    it "resumes a retired subscription only with an Apple charge whose paid period runs, dated less than a day ahead" testResumableCharge
    it "shows a service request in the terminal as its type alone" testShownServiceRequest
    it "refuses for good a Play product id that could name another purchase, before any verifier" testGoogleProductIdPath
    it "does not send a Play token that could name another purchase, and leaves it retryable" testGoogleTokenPath
    it "has the dev mock vouch for the transaction its claim names, as a production purchase" testMockVouchesForClaim
  describe "badge service request loop" $ do
    it "survives a request that throws, and still stops when cancelled" testSurviveRequestFailure

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

testPresHeaderEncoding :: IO ()
testPresHeaderEncoding =
  mapM_
    (\ph -> strDecode (strEncode ph) `shouldBe` Right ph)
    [ PHTest "nonce",
      PHChat aliceBinding,
      PHFileInv {chatBinding = aliceBinding, fileSize = 139737},
      PHFileDescr {chatBinding = aliceBinding, fileSize = 139737, descrHash = "descr-hash", fileExpires = Nothing},
      PHFileDescr {chatBinding = aliceBinding, fileSize = 139737, descrHash = "descr-hash", fileExpires = Just futureTime},
      PHRequest requestCode,
      PHLink "link-key",
      PHUnknown 'Z' "payload"
    ]

testPresHeaderJSON :: IO ()
testPresHeaderJSON =
  mapM_
    ( \ph -> do
        J.eitherDecode (J.encode ph) `shouldBe` Right ph
        J.toJSON ph `shouldBe` J.toJSON (BBSPresHeader $ strEncode ph)
    )
    [ PHChat binaryBytes,
      PHFileInv {chatBinding = binaryBytes, fileSize = 139737},
      PHFileDescr {chatBinding = binaryBytes, fileSize = 139737, descrHash = binaryBytes, fileExpires = Just futureTime},
      PHRequest binaryBytes,
      PHLink binaryBytes,
      PHUnknown 'Z' binaryBytes
    ]
  where
    binaryBytes = "\0\1\31\34\92\127\128\195\255"

testOtherChatBinding :: IO ()
testOtherChatBinding = do
  let ph = PHFileInv {chatBinding = aliceBinding, fileSize = 139737}
      otherPh = PHFileInv {chatBinding = bobBinding, fileSize = 139737}
  (pk, BadgeProof idx _ p info) <- issueBadgeProofHeader BTSupporter futureTime ph
  verifyBadge (keysFor pk) (BadgeProof idx (BBSPresHeader $ strEncode ph) p info) >>= (`shouldBe` Just True)
  verifyBadge (keysFor pk) (BadgeProof idx (BBSPresHeader $ strEncode otherPh) p info) >>= (`shouldBe` Just False)

testProfileProofHeader :: IO ()
testProfileProofHeader = do
  (pk, unbound) <- issueBadgeProof BTSupporter futureTime
  (pk', bound) <- issueBadgeProofHeader BTSupporter futureTime (PHChat aliceBinding)
  (pk'', requested) <- issueBadgeProofHeader BTSupporter futureTime (PHRequest requestCode)
  acceptedProof Nothing unbound `shouldBe` True
  acceptedProof (Just $ PHChat aliceBinding) unbound `shouldBe` True
  acceptedProof (Just $ PHChat aliceBinding) bound `shouldBe` True
  acceptedProof (Just $ PHChat bobBinding) bound `shouldBe` False
  acceptedProof (Just $ PHRequest aliceBinding) bound `shouldBe` False
  acceptedProof Nothing bound `shouldBe` False
  acceptedProof (Just $ PHRequest requestCode) requested `shouldBe` True
  acceptedProof (Just $ PHRequest "other-request-code") requested `shouldBe` False
  acceptedProof (Just $ PHChat requestCode) requested `shouldBe` False
  verifyBadge (keysFor pk) unbound >>= (`shouldBe` Just True)
  verifyBadge (keysFor pk') bound >>= (`shouldBe` Just True)
  verifyBadge (keysFor pk'') requested >>= (`shouldBe` Just True)

aliceBinding :: ByteString
aliceBinding = "Galice-member-id"

bobBinding :: ByteString
bobBinding = "Gbob-member-id"

requestCode :: ByteString
requestCode = "request-code"

issueBadgeProof :: BadgeType -> UTCTime -> IO (BBSPublicKey, BadgeProof)
issueBadgeProof bt expiry = issueBadgeProofHeader bt expiry (PHTest "test-nonce")

issueBadgeProofHeader :: BadgeType -> UTCTime -> ProofPresHeader -> IO (BBSPublicKey, BadgeProof)
issueBadgeProofHeader bt expiry ph = do
  Right (pk, sk) <- bbsKeyGen
  drg <- C.newRandom
  mk <- generateMasterKey drg
  let vreq = VerifiedBadgeRequest BadgeRequest {masterKey = mk, badgeInfo = BadgeInfo {badgeType = bt, badgeExpiry = expiry, badgeExtra = ""}}
  Right cred <- issueBadge testKeyIdx sk vreq
  Right badge <- badgeProof pk cred ph
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

-- the service's step for a charge it grants: the planner's rows, then the issue at now
charged :: UTCTime -> UTCTime -> Int -> StatementEntry -> [StatementEntry]
charged periodStart now months tip = rows <> maybeToList (issue now $ finalBalance tip rows)
  where
    rows = chargeEntries periodStart now months "ch1" ("", "", "") tip

rowKind :: StatementEntry -> T.Text
rowKind StatementEntry {entryType} = case entryType of
  SECredit c -> creditTypeTag c
  SEDebit d -> debitTypeTag d

rowAt :: StatementEntry -> UTCTime
rowAt StatementEntry {createdAt} = createdAt

bAnchor :: StatementEntry -> UTCTime
bAnchor StatementEntry {balanceAnchorTs} = balanceAnchorTs

-- the client checks a statement from the first row the service wrote, as a new purchase receives it
allVerify :: HasCallStack => [StatementEntry] -> Expectation
allVerify rows = verdicts (rowAt $ last rows) Nothing rows `shouldBe` map (const $ Just True) rows

-- A grant dated to the store's period start is checked by re-running it at that start, after rows
-- written earlier and before the lapses and issue written at the request: each path a subscription
-- renews by must keep both the dates in order and the recompute exact.
testChargeRowsVerify :: IO ()
testChargeRowsVerify = do
  let p0 = at 2026 3 10
      days n = addUTCTime (n * nominalDay)
      firstCharge = charged p0 (addUTCTime 600 p0) 1 (newBalance p0)
      afterFirst = finalBalance (newBalance p0) firstCharge
  -- a first charge, granted onto the seed the service starts a purchase from
  map rowKind firstCharge `shouldBe` ["charge", "badge"]
  allVerify firstCharge
  -- monthly renewals, each charged at the store's period start and asked about some days after it
  let renew (tip, rows) (k, late) =
        let periodStart = addMonths k p0
            new = charged periodStart (days late periodStart) 1 tip
         in (finalBalance tip new, rows <> new)
      (_, monthly) = foldl renew (afterFirst, firstCharge) [(1, 3), (2, 6), (3, 1)]
  map rowKind monthly `shouldBe` concat (replicate 4 ["charge", "badge"])
  allVerify monthly
  -- a period start equal to the balance start continues the run on its anchor
  let sameStart = charged (bStart afterFirst) (days 4 $ bStart afterFirst) 1 afterFirst
  map rowKind sameStart `shouldBe` ["charge", "badge"]
  map bAnchor (take 1 sameStart) `shouldBe` [p0]
  allVerify (firstCharge <> sameStart)
  -- and a store clock a minute either side of it still verifies, whichever branch the grant takes
  mapM_
    ( \offset -> do
        let rows = charged (addUTCTime offset $ bStart afterFirst) (days 4 $ bStart afterFirst) 1 afterFirst
        map rowKind rows `shouldBe` ["charge", "badge"]
        allVerify (firstCharge <> rows)
    )
    [-60, 60]
  -- a renewal after billing retry: the store's new period starts when the charge recovered, and the
  -- run restarts there, so coverage follows the store's new renewal date
  let recoveredAt = days 9 (addMonths 1 p0)
      recovered = charged recoveredAt (days 1 recoveredAt) 1 afterFirst
  map rowKind recovered `shouldBe` ["charge", "badge"]
  map bStart (take 1 recovered) `shouldBe` [recoveredAt]
  map paidThrough (take 1 recovered) `shouldBe` [addMonths 1 recoveredAt]
  allVerify (firstCharge <> recovered)
  -- annual: the first month issued at purchase, then one at each month's start
  let annualFirst = charged p0 p0 12 (newBalance p0)
      annualTip = finalBalance (newBalance p0) annualFirst
      yearEnd = addMonths 12 p0
      monthsIssued n = take n (issueAll annualTip)
      -- the device was offline for the year's last month, which is still unissued at the renewal
      tenMore = monthsIssued 10
      monthLeft = finalBalance annualTip tenMore
      renewedAnnual = charged yearEnd (days 3 yearEnd) 12 monthLeft
  bMonths monthLeft `shouldBe` 1
  map rowKind renewedAnnual `shouldBe` ["lapse", "charge", "badge"]
  allVerify (annualFirst <> tenMore <> renewedAnnual)
  -- the same, with the store's year starting an hour before the ledger's: that month has not run out
  -- at the period start, so it is carried and lapses after the grant
  let earlyAnnual = charged (addUTCTime (-3600) yearEnd) (days 3 yearEnd) 12 monthLeft
  map rowKind earlyAnnual `shouldBe` ["charge", "lapse", "badge"]
  allVerify (annualFirst <> tenMore <> earlyAnnual)
  -- away from month nine until 45 days past the year end, while the renewal failed and recovered
  -- inside the store's grace period, which keeps the original period start
  let eightMore = monthsIssued 8
      awayTip = finalBalance annualTip eightMore
      graced = charged yearEnd (days 45 yearEnd) 12 awayTip
  bMonths awayTip `shouldBe` 3
  map rowKind graced `shouldBe` ["lapse", "charge", "lapse", "badge"]
  allVerify (annualFirst <> eightMore <> graced)

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
    [BSEBadRequest, BSEUnsupportedVersion, BSEUnknownPurchaseKey, BSECodeInvalid, BSECodeUsed, BSECodeExpired, BSEProviderNotConfigured, BSETestPurchase, BSEUnknown "future_code"]

-- The app shows the recorded failure in a sentence, so an agent error is stored as the agent
-- error and not as the chat error wrapping it, whether or not it can clear on its own.
testIssueFailureClassification :: IO ()
testIssueFailureClassification = do
  let failureFor = badgeIssueFailure . chatErrorAgent
      timeout = BROKER "localhost" TIMEOUT
      auth = SMP "localhost" AUTH
  failureFor (AGENT (A_SERVICE ASETimeout)) `shouldBe` BIFServiceTimeout
  failureFor timeout `shouldBe` BIFNetwork {agentError = tshow timeout}
  failureFor auth `shouldBe` BIFUnexpected {message = tshow auth}
  badgeIssueFailure (ChatError (CECommandError "unexpected badge service response")) `shouldBe` BIFUnexpected {message = "unexpected badge service response"}
  badgeIssueFailure (ChatError (CEBadgeRedeemError BREUnknownKeyIndex)) `shouldBe` BIFInvalidCredential
  badgeIssueFailure (ChatError (CEBadgeRedeemError BRECredentialNotVerified)) `shouldBe` BIFInvalidCredential

-- A refusal the service marks transient is not worth a word before the credential lapses. Internal
-- comes without retryAfter so that a failing service is not pressed, not because the fault is
-- final, so it waits the same way; any other refusal is told at once.
testServiceErrorTransience :: IO ()
testServiceErrorTransience = do
  badgeFailureTransient BIFServiceError {code = BSERateLimited, retryable = True} `shouldBe` True
  badgeFailureTransient BIFServiceError {code = BSEInternal, retryable = False} `shouldBe` True
  badgeFailureTransient BIFServiceError {code = BSEUnknownPurchaseKey, retryable = False} `shouldBe` False
  badgeFailureTransient BIFServiceError {code = BSEUnknown "future_code", retryable = False} `shouldBe` False
  badgeFailureTransient BIFServiceError {code = BSEUnknown "future_code", retryable = True} `shouldBe` True

-- The failure is stored as text and read back by getBadgePurchase, so every value this version
-- writes must read back as itself, and any other value must still read: a row it cannot parse
-- would otherwise fail every read of the purchase.
testIssueFailureEncoding :: IO ()
testIssueFailureEncoding = do
  mapM_
    (\f -> strDecode (strEncode f) `shouldBe` Right f)
    [ BIFServiceError {code = BSEUnknownPurchaseKey, retryable = False},
      BIFServiceError {code = BSERateLimited, retryable = True},
      BIFServiceError {code = BSEUnknown "future_code", retryable = True},
      BIFServiceError {code = BSEUnknown "code with spaces", retryable = False},
      BIFServiceError {code = BSEUnknown "", retryable = False},
      BIFServiceTimeout,
      BIFNetwork {agentError = "BROKER \"smp://x\" NETWORK"},
      BIFInvalidCredential,
      BIFUnexpected {message = "unexpected badge service response"},
      BIFUnexpected {message = "several words and : punctuation"}
    ]
  -- the row the CLI prints, and what a reader of the database sees
  strEncode BIFServiceError {code = BSECodeUsed, retryable = False} `shouldBe` "service_error final code_used"
  strEncode BIFServiceError {code = BSERateLimited, retryable = True} `shouldBe` "service_error retry rate_limited"
  strEncode BIFServiceTimeout `shouldBe` "service_timeout"
  -- a row a later version wrote, or one edited by hand, still reads - as the text it holds
  strDecode "future_failure with text" `shouldBe` Right BIFUnexpected {message = "future_failure with text"}
  strDecode "service_timeout trailing" `shouldBe` Right BIFUnexpected {message = "service_timeout trailing"}
  strDecode "service_error maybe code_used" `shouldBe` Right BIFUnexpected {message = "service_error maybe code_used"}

-- The text of a code this version does not know is chosen by the service, and the app shows it in
-- a sentence of its own - so it is bounded and stripped of anything but a code before it is stored.
testServiceErrorCodeBounded :: IO ()
testServiceErrorCodeBounded = do
  badgeServiceErrorText BSECodeUsed `shouldBe` "code_used"
  badgeServiceErrorText (BSEUnknown "future_code") `shouldBe` "future_code"
  badgeServiceErrorText (BSEUnknown "two words") `shouldBe` "twowords"
  badgeServiceErrorText (BSEUnknown (T.replicate 100 "a")) `shouldBe` T.replicate 32 "a"
  badgeServiceErrorText (BSEUnknown "Visit evil.example.com!") `shouldBe` "isitevilexamplecom"

-- The client replicates entry_credit_type / entry_debit_type verbatim, so a stored tag that
-- disagreed with the wire tag would put a different row on each side.
testEntryTypeColumns :: IO ()
testEntryTypeColumns = do
  k <- fst <$> (C.newRandom >>= \g -> atomically (C.generateKeyPair g) :: IO (C.KeyPair 'C.Ed25519))
  let credits = [SCPayment Nothing, SCCode, SCCharge "ch1", SCSupport, SCTransferIn k, SCOpening]
      debits = [SDRefund, SDUpgrade k, SDTransferOut k, SDSupport, SDBadge, SDLapse]
  mapM_ (\c -> wireTag (J.toJSON (SECredit c)) "credit" `shouldBe` Just (creditTypeTag c)) credits
  mapM_ (\d -> wireTag (J.toJSON (SEDebit d)) "debit" `shouldBe` Just (debitTypeTag d)) debits
  -- the types this version writes survive a round trip through the columns, a payment credit
  -- through the payment its row references, and a charge credit through its charge
  mapM_
    (\t -> uncurry3 (entryTypeFromColumns Nothing Nothing) (entryTypeColumns t) `shouldSatisfy` sameEntryType t)
    [SECredit SCCode, SEDebit SDBadge, SEDebit SDLapse]
  let storeCredit = SECredit (SCPayment Nothing)
      invoiceCredit = SECredit (SCPayment (Just (InvoiceId "inv1")))
      chargeCredit = SECredit (SCCharge "ch1")
  uncurry3 (entryTypeFromColumns (Just Nothing) Nothing) (entryTypeColumns storeCredit) `shouldSatisfy` sameEntryType storeCredit
  uncurry3 (entryTypeFromColumns (Just (Just (InvoiceId "inv1"))) Nothing) (entryTypeColumns invoiceCredit) `shouldSatisfy` sameEntryType invoiceCredit
  uncurry3 (entryTypeFromColumns Nothing (Just "ch1")) (entryTypeColumns chargeCredit) `shouldSatisfy` sameEntryType chargeCredit
  -- a type that needs a reference column is not silently read back as something else
  uncurry3 (entryTypeFromColumns Nothing Nothing) (entryTypeColumns storeCredit) `shouldSatisfy` isNothing
  uncurry3 (entryTypeFromColumns Nothing Nothing) (entryTypeColumns chargeCredit) `shouldSatisfy` isNothing
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

testPurchaseRequestJSON :: IO ()
testPurchaseRequestJSON = do
  drg <- C.newRandom
  mk <- generateMasterKey drg
  (k, _) <- atomically $ C.generateKeyPair drg :: IO (C.KeyPair 'C.Ed25519)
  let payment = SPGoogle {productId = "badge_supporter_01", token = "token"}
      req = BadgeServiceRequest {version = Version 1, purchaseKey = Just k, request = BSCPurchaseBadge {masterKey = mk, payment, upgrade = Nothing}}
  -- the client states its master key and the store's evidence, and nothing a receipt already decides
  J.toJSON req
    `shouldBe` J.object
      [ "version" J..= (1 :: Int),
        "purchaseKey" J..= k,
        "request"
          J..= J.object
            [ "type" J..= ("purchaseBadge" :: T.Text),
              "masterKey" J..= mk,
              "payment" J..= J.object ["type" J..= ("google" :: T.Text), "productId" J..= ("badge_supporter_01" :: T.Text), "token" J..= ("token" :: T.Text)]
            ]
      ]
  J.toJSON SPApple {jws = "a.b.c"} `shouldBe` J.object ["type" J..= ("apple" :: T.Text), "jws" J..= ("a.b.c" :: T.Text)]
  roundTrips req

testStoreTransactionRef :: IO ()
testStoreTransactionRef = do
  let jws transactionId signature = T.intercalate "." [part "{\"alg\":\"ES256\"}", part ("{\"transactionId\":\"" <> transactionId <> "\",\"productId\":\"BADGE_SUPPORTER_01\"}"), signature]
      part = safeDecodeUtf8 . B64U.encodeUnpadded . encodeUtf8
      apple = storeTransactionRef . SPApple
  -- the store may sign the same transaction again, and a retry must find the keys it stashed
  apple (jws "2000000812345671" "c2lnbmVkIG9uY2U") `shouldBe` Just (StoreTransactionRef PPApple "2000000812345671")
  apple (jws "2000000812345671" "c2lnbmVkIGFnYWlu") `shouldBe` apple (jws "2000000812345671" "c2lnbmVkIG9uY2U")
  apple (jws "2000000812345672" "c2lnbmVkIG9uY2U") `shouldNotBe` apple (jws "2000000812345671" "c2lnbmVkIG9uY2U")
  apple "not.a-jws" `shouldBe` Nothing
  apple (T.intercalate "." [part "{}", part "{\"productId\":\"BADGE_SUPPORTER_01\"}", "sig"]) `shouldBe` Nothing
  -- a Play token is a bearer secret, so it is kept only as its hash
  let google = storeTransactionRef SPGoogle {productId = "badge_supporter_01", token = "play-token"}
  google `shouldSatisfy` maybe False (\(StoreTransactionRef provider ref) -> provider == PPGoogle && ref /= "play-token")
  google `shouldBe` storeTransactionRef SPGoogle {productId = "badge_supporter_01", token = "play-token"}
  storeTransactionRef SPInvoice {invoiceId = InvoiceId "inv"} `shouldBe` Nothing

-- every renewal of a subscription is a new transaction with a new id, and must still find the purchase its first charge made
testAppleTransactionRef :: IO ()
testAppleTransactionRef = do
  let jws fields = T.intercalate "." [part "{\"alg\":\"ES256\"}", part (safeDecodeUtf8 $ LB.toStrict $ J.encode $ J.object fields), "c2lnbmVk"]
      part = safeDecodeUtf8 . B64U.encodeUnpadded . encodeUtf8
      ids = ["transactionId" J..= ("2000000900000002" :: T.Text), "originalTransactionId" J..= ("2000000900000001" :: T.Text)]
      ofType t = ("type" J..= (t :: T.Text)) : ids
  appleTransactionRef (jws $ ofType "Auto-Renewable Subscription") `shouldBe` Just "2000000900000001"
  appleTransactionRef (jws $ ofType "Consumable") `shouldBe` Just "2000000900000002"
  appleTransactionRef (jws ids) `shouldBe` Just "2000000900000002"
  -- a subscription is never claimed by its own transaction id, which the next renewal would not share
  appleTransactionRef (jws $ take 2 (ofType "Auto-Renewable Subscription")) `shouldBe` Nothing

testAppleTransactionFacts :: IO ()
testAppleTransactionFacts = do
  let jws fields = T.intercalate "." [part "{\"alg\":\"ES256\"}", part (safeDecodeUtf8 $ LB.toStrict $ J.encode $ J.object fields), "c2lnbmVk"]
      part = safeDecodeUtf8 . B64U.encodeUnpadded . encodeUtf8
      renewal = ["transactionId" J..= ("2000000900000002" :: T.Text), "type" J..= ("Auto-Renewable Subscription" :: T.Text)]
      facts purchaseDate revoked autoRenewable = Just AppleTransactionFacts {transactionId = "2000000900000002", purchaseDate, expiresDate = Nothing, revoked, autoRenewable}
  appleTransactionFacts (jws $ ("purchaseDate" J..= (1769904000000 :: Int)) : renewal) `shouldBe` facts (Just 1769904000000) False True
  appleTransactionFacts (jws $ ("expiresDate" J..= (1772582400000 :: Int)) : renewal) `shouldBe` ((\f -> f {expiresDate = Just 1772582400000}) <$> facts Nothing False True)
  appleTransactionFacts (jws $ ("expiresDate" J..= ("next month" :: T.Text)) : renewal) `shouldBe` facts Nothing False True
  -- the service reads a null revocationDate as not revoked, and so does the client
  appleTransactionFacts (jws $ ("revocationDate" J..= J.Null) : renewal) `shouldBe` facts Nothing False True
  appleTransactionFacts (jws $ ("revocationDate" J..= (1770000000000 :: Int)) : renewal) `shouldBe` facts Nothing True True
  -- a date that does not read is absent, and leaves the rest of the payload readable
  appleTransactionFacts (jws $ ("purchaseDate" J..= ("yesterday" :: T.Text)) : renewal) `shouldBe` facts Nothing False True
  appleTransactionFacts (jws $ take 1 renewal <> ["type" J..= ("Consumable" :: T.Text)]) `shouldBe` facts Nothing False False
  appleTransactionFacts (jws $ drop 1 renewal) `shouldBe` Nothing
  appleTransactionFacts "not.a-jws" `shouldBe` Nothing

testResumableCharge :: IO ()
testResumableCharge = do
  let nowSeconds = 1770000000 :: Integer
      now = posixSecondsToUTCTime $ fromInteger nowSeconds
      secondsAhead s = addUTCTime (fromInteger s) now
      epochMs s = (nowSeconds + s) * 1000
      oneDay = 86400
      oneMonth = 30 * oneDay
      jws fields = T.intercalate "." [part "{\"alg\":\"ES256\"}", part (safeDecodeUtf8 $ LB.toStrict $ J.encode $ J.object fields), "c2lnbmVk"]
      part = safeDecodeUtf8 . B64U.encodeUnpadded . encodeUtf8
      ofType t fields = SPApple {jws = jws $ ["transactionId" J..= ("2000000900000002" :: T.Text), "type" J..= (t :: T.Text)] <> fields}
      renewalWith = ofType "Auto-Renewable Subscription"
      dated bought expires = renewalWith ["purchaseDate" J..= epochMs bought, "expiresDate" J..= epochMs expires]
  resumableCharge now (dated 0 oneMonth) `shouldBe` Just now
  resumableCharge now (dated 60 oneMonth) `shouldBe` Just (secondsAhead 60)
  resumableCharge now (dated (oneDay - 1) oneMonth) `shouldBe` Just (secondsAhead (oneDay - 1))
  -- dated as far ahead as the allowed skew or further, as a forged payload can be
  resumableCharge now (dated oneDay oneMonth) `shouldBe` Nothing
  resumableCharge now (dated (365 * oneDay) (400 * oneDay)) `shouldBe` Nothing
  -- the paid period it bought has ended
  resumableCharge now (dated (-oneMonth) 1) `shouldBe` Just (secondsAhead (-oneMonth))
  resumableCharge now (dated (-oneMonth) 0) `shouldBe` Nothing
  resumableCharge now (renewalWith ["purchaseDate" J..= epochMs 0, "expiresDate" J..= epochMs oneMonth, "revocationDate" J..= epochMs 0]) `shouldBe` Nothing
  resumableCharge now (ofType "Non-Renewing Subscription" ["purchaseDate" J..= epochMs 0, "expiresDate" J..= epochMs oneMonth]) `shouldBe` Nothing
  resumableCharge now (renewalWith ["expiresDate" J..= epochMs oneMonth]) `shouldBe` Nothing
  resumableCharge now (renewalWith ["purchaseDate" J..= epochMs 0]) `shouldBe` Nothing
  resumableCharge now SPGoogle {productId = "subscr_badge_supporter_01", token = "fake-play-token"} `shouldBe` Nothing
  resumeHoldEnded (secondsAhead oneDay) now `shouldBe` True
  resumeHoldEnded (secondsAhead (oneDay - 1)) now `shouldBe` False

testShownServiceRequest :: IO ()
testShownServiceRequest = do
  drg <- C.newRandom
  mk <- generateMasterKey drg
  (k, _) <- atomically $ C.generateKeyPair drg :: IO (C.KeyPair 'C.Ed25519)
  let shown request = case J.toJSON BadgeServiceRequest {version = Version 1, purchaseKey = Just k, request} of
        J.Object o -> J.Object (shownServiceRequest o)
        _ -> J.Null
      typeOnly t = J.object ["request" J..= J.object ["type" J..= (t :: T.Text)]]
  shown BSCPurchaseBadge {masterKey = mk, payment = SPApple {jws = "a.b.c"}, upgrade = Nothing} `shouldBe` typeOnly "purchaseBadge"
  shown BSCRedeemBadgeCode {masterKey = mk, code = "SB-00000-00000-00000-00001"} `shouldBe` typeOnly "redeemBadgeCode"

testGoogleProductIdPath :: IO ()
testGoogleProductIdPath = do
  let refused productId = case toStoreReceipt uncalledVerifier SPGoogle {productId, token = validPlayToken} of
        Just (Left SRInvalid {}) -> True
        _ -> False
  mapM_ (\p -> refused p `shouldBe` True) ["badge_legend_01/tokens/other?", "badge_legend_01?x", "badge_legend_01#x", "..", "../badge_legend_01", "Badge_legend_01", ""]
  case toStoreReceipt uncalledVerifier SPGoogle {productId = "badge_supporter_01", token = validPlayToken} of
    Just (Right StoreReceipt {txRef = StoreTransactionRef {provider}}) -> provider `shouldBe` PPGoogle
    _ -> expectationFailure "a valid product id and token were refused"

testGoogleTokenPath :: IO ()
testGoogleTokenPath = do
  let unsent token = case toStoreReceipt uncalledVerifier SPGoogle {productId = "badge_supporter_01", token} of
        Just (Left SRUnreachable {}) -> True
        _ -> False
  mapM_ (\t -> unsent t `shouldBe` True) ["a/b", "../x", "t?x", "t#x", "t x", ""]

uncalledVerifier :: StoreVerifier
uncalledVerifier = StoreVerifier {verifyApple = Nothing, verifyGoogle = Just $ \_ _ -> error "verifier called", acknowledgeGoogle = Nothing, acknowledgeGoogleSubscription = Nothing, checkSubscription = Nothing, verifyTimeout = 500000, storeChecks = ChecksAfterCoverage}

validPlayToken :: T.Text
validPlayToken = "fake-play-token.AO-J1Oz9x2kqE7wYt3"

testMockVouchesForClaim :: IO ()
testMockVouchesForClaim = do
  mockStoreVerifier <- newMockStoreVerifier getCurrentTime
  let part = safeDecodeUtf8 . B64U.encodeUnpadded . encodeUtf8
      signed = T.intercalate "." [part "{\"alg\":\"ES256\"}", part "{\"transactionId\":\"2000000812345671\",\"productId\":\"BADGE_SUPPORTER_01\"}", "c2lnbmVk"]
      vouchesForClaim payment = case toStoreReceipt mockStoreVerifier payment of
        Just (Right StoreReceipt {txRef = StoreTransactionRef {transactionRef = providerRef}, verifyReceipt}) ->
          verifyReceipt >>= \case
            Right VerifiedStoreTransaction {transactionRef, testPurchase} -> (transactionRef, testPurchase) `shouldBe` (providerRef, False)
            Left refusal -> expectationFailure ("the mock refused: " <> show refusal)
        _ -> expectationFailure "refused before the mock was asked"
  vouchesForClaim SPApple {jws = signed}
  vouchesForClaim SPGoogle {productId = "badge_supporter_01", token = "fake-play-token.AO-J1Oz9x2kqE7wYt3"}

testSurviveRequestFailure :: IO ()
testSurviveRequestFailure = do
  survive "a request" (throwIO $ userError "failed") `shouldReturn` ()
  started <- newEmptyMVar
  stopped <- newEmptyMVar
  t <- forkIO $ survive "a request" (putMVar started () >> threadDelay 10000000) `catch` (putMVar stopped . asyncException)
  takeMVar started
  killThread t
  takeMVar stopped `shouldReturn` True
  where
    asyncException :: SomeException -> Bool
    asyncException e = isJust (fromException e :: Maybe SomeAsyncException)

testCredentialResponseJSON :: IO ()
testCredentialResponseJSON = do
  Right (_, sk) <- bbsKeyGen
  drg <- C.newRandom
  mk <- generateMasterKey drg
  let info = BadgeInfo {badgeType = BTSupporter, badgeExpiry = futureTime, badgeExtra = ""}
  Right cred <- issueBadge testKeyIdx sk (VerifiedBadgeRequest BadgeRequest {masterKey = mk, badgeInfo = info})
  let resp = BSPBadgeCredential {credential = Just cred, receipt = Nothing, statement = BadgeStatement {entries = [], previousEntryId = Nothing}, subscription = Nothing}
  J.toJSON resp
    `shouldBe` J.object
      [ "type" J..= ("badgeCredential" :: T.Text),
        "credential" J..= cred,
        "statement" J..= J.object ["entries" J..= ([] :: [J.Value])]
      ]
  roundTrips resp

testCredentialSubscriptionJSON :: IO ()
testCredentialSubscriptionJSON = do
  let states = [(SSRenewing, "renewing"), (SSCancelled, "cancelled"), (SSBillingRetry, "billingRetry"), (SSEnded, "ended")]
  mapM_ (\(s, t) -> (J.toJSON s, J.fromJSON (J.String t)) `shouldBe` (J.String t, J.Success s)) states
  let response subscription_ = J.object $ ["type" J..= ("badgeCredential" :: T.Text), "statement" J..= J.object ["entries" J..= ([] :: [J.Value])]] <> maybe [] (\s -> ["subscription" J..= (s :: T.Text)]) subscription_
      subscriptionOf v = case J.fromJSON v of
        J.Success BSPBadgeCredential {subscription} -> Right subscription
        J.Success _ -> Left "not a badgeCredential"
        J.Error e -> Left e
  subscriptionOf (response Nothing) `shouldBe` Right Nothing
  subscriptionOf (response (Just "billingRetry")) `shouldBe` Right (Just SSBillingRetry)
  -- a state added by a newer service does not fail the response, and is not read as ended
  subscriptionOf (response (Just "paused")) `shouldBe` Right (Just (SSUnknown "paused"))
  J.toJSON (SSUnknown "paused") `shouldBe` J.String "paused"
  let resp = BSPBadgeCredential {credential = Nothing, receipt = Nothing, statement = BadgeStatement {entries = [], previousEntryId = Nothing}, subscription = Just SSRenewing}
  J.toJSON resp `shouldBe` response (Just "renewing")
  roundTrips resp

testBadgeSubscriptionJSON :: IO ()
testBadgeSubscriptionJSON = do
  drg <- C.newRandom
  (k, _) <- atomically $ C.generateKeyPair drg :: IO (C.KeyPair 'C.Ed25519)
  let badgeState subscription =
        BT.BadgeState {badgePurchaseId = 1, purchaseKey = k, badgeType = BTSupporter, shown = BoolDef True, monthsLeft = 0, paidThrough = futureTime, renewsAt = Nothing, willRenew = False, alert = Nothing, issueError = Nothing, nextWakeAt = Nothing, subscription}
      subscriptionField v = case J.toJSON v of
        J.Object o -> KM.lookup "subscription" o
        _ -> Nothing
  subscriptionField (badgeState Nothing) `shouldBe` Nothing
  subscriptionField (badgeState $ Just BadgeSubscription {planMonths = 12, status = Just SSCancelled, transactionRef = Nothing})
    `shouldBe` Just (J.object ["planMonths" J..= (12 :: Int), "status" J..= ("cancelled" :: T.Text)])
  -- not yet observed: the field is left out rather than sent as null
  subscriptionField (badgeState $ Just BadgeSubscription {planMonths = 1, status = Nothing, transactionRef = Nothing})
    `shouldBe` Just (J.object ["planMonths" J..= (1 :: Int)])
  let withRef = BadgeSubscription {planMonths = 1, status = Just SSRenewing, transactionRef = Just "2000000812345680"}
  subscriptionField (badgeState $ Just withRef)
    `shouldBe` Just (J.object ["planMonths" J..= (1 :: Int), "status" J..= ("renewing" :: T.Text), "transactionRef" J..= ("2000000812345680" :: T.Text)])
  J.fromJSON (J.toJSON withRef) `shouldBe` J.Success withRef
  -- as an older core sends it
  J.fromJSON (J.object ["planMonths" J..= (1 :: Int)]) `shouldBe` J.Success BadgeSubscription {planMonths = 1, status = Nothing, transactionRef = Nothing}
  let renewal status = badgeRenewal futureTime $ Just BadgeSubscription {planMonths = 1, status, transactionRef = Nothing}
  renewal (Just SSRenewing) `shouldBe` (Just futureTime, True)
  mapM_ (\s -> renewal s `shouldBe` (Nothing, False)) [Nothing, Just SSCancelled, Just SSBillingRetry, Just SSEnded, Just (SSUnknown "renewing soon")]
  badgeRenewal futureTime Nothing `shouldBe` (Nothing, False)
  boundedSubscriptionState SSBillingRetry `shouldBe` SSBillingRetry
  boundedSubscriptionState (SSUnknown "onHold") `shouldBe` SSUnknown "onHold"
  boundedSubscriptionState (SSUnknown "Visit evil.example.com!\ESC[2J") `shouldBe` SSUnknown "Visitevilexamplecom2J"
  boundedSubscriptionState (SSUnknown (T.replicate 100 "a")) `shouldBe` SSUnknown (T.replicate 32 "a")
  boundedSubscriptionState (SSUnknown "") `shouldBe` SSUnknown "unknown"

testSubscriptionAlertDerivation :: IO ()
testSubscriptionAlertDerivation = do
  drg <- C.newRandom
  (k, pk) <- atomically $ C.generateKeyPair drg :: IO (C.KeyPair 'C.Ed25519)
  mk <- generateMasterKey drg
  let start = at 2026 3 10
      ends = addMonths 1 start
      balance = grant start 1 (newBalance start)
      failedAt = at 2026 3 20
      failure = BadgeIssueError {failedSince = failedAt, lastAttemptAt = failedAt, reason = BIFServiceError {code = BSEUnknownPurchaseKey, retryable = False}}
      early = addUTCTime (-10 * nominalDay) ends
      soon = addUTCTime (-nominalDay) ends
      past = addUTCTime nominalDay ends
      subscription months st = SB.PKSubscription BadgeSubscription {planMonths = months, status = st, transactionRef = Nothing}
      alertOf t kind' isShown err =
        let p = SB.UserBadgePurchase {badgePurchaseId = 1, purchaseKey = k, purchasePrivKey = pk, masterKey = mk, badgeType = BTSupporter, shown = isShown, alertAcked = Nothing, alertSnoozeUntil = Nothing, issueError = err, nextWakeAt = Nothing, unrenewedRun = Nothing, purchaseKind = kind'}
         in (\BT.BadgeAlert {kind, date} -> (kind, date)) <$> derivedBadgeAlert t p Nothing balance
      renewing = Just SSRenewing
      cancelled = Just SSCancelled
      billingRetry = Just SSBillingRetry
  mapM_
    (\(t, kind', isShown, err, expected) -> alertOf t kind' isShown err `shouldBe` expected)
    [ -- retired at or after paidThrough, whatever else holds
      (past, subscription 1 renewing, False, Nothing, Just (BASubscriptionEnded, ends)),
      (past, subscription 12 billingRetry, False, Just failure, Just (BASubscriptionEnded, ends)),
      (past, subscription 1 cancelled, False, Nothing, Just (BASubscriptionEnded, ends)),
      -- still shown past paidThrough: waiting for the store to charge
      (past, subscription 1 renewing, True, Nothing, Nothing),
      (past, subscription 12 Nothing, True, Nothing, Nothing),
      (past, subscription 1 cancelled, True, Nothing, Nothing),
      -- billing retry before and after paidThrough, over a renewal failure
      (early, subscription 12 billingRetry, True, Nothing, Just (BAPaymentIssue, ends)),
      (past, subscription 1 billingRetry, True, Just failure, Just (BAPaymentIssue, ends)),
      -- a renewal failure over a cancellation and a reminder
      (soon, subscription 1 cancelled, True, Just failure, Just (BAIssueFailed, failedAt)),
      (soon, subscription 12 renewing, True, Just failure, Just (BAIssueFailed, failedAt)),
      (early, subscription 1 cancelled, True, Nothing, Just (BASubscriptionCancelled, ends)),
      (soon, subscription 12 cancelled, True, Nothing, Just (BASubscriptionCancelled, ends)),
      -- reminded only for a renewing annual plan, and only in the last three days
      (soon, subscription 12 renewing, True, Nothing, Just (BARenewalApproaching, ends)),
      (early, subscription 12 renewing, True, Nothing, Nothing),
      (soon, subscription 1 renewing, True, Nothing, Nothing),
      (soon, subscription 12 Nothing, True, Nothing, Nothing),
      (soon, subscription 12 (Just $ SSUnknown "paused"), True, Nothing, Nothing),
      (soon, subscription 1 (Just SSEnded), True, Nothing, Nothing),
      -- prepaid, as before
      (past, SB.PKPrepaid, True, Nothing, Just (BASupportEnded, ends)),
      (past, SB.PKPrepaid, False, Just failure, Just (BASupportEnded, ends)),
      (soon, SB.PKPrepaid, True, Just failure, Just (BAIssueFailed, failedAt)),
      (soon, SB.PKPrepaid, True, Nothing, Nothing)
    ]
  -- the reminder starts exactly three days before paidThrough
  alertOf (addUTCTime (-3 * nominalDay) ends) (subscription 12 renewing) True Nothing `shouldBe` Just (BARenewalApproaching, ends)
  alertOf (addUTCTime (-3 * nominalDay - 1) ends) (subscription 12 renewing) True Nothing `shouldBe` Nothing

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

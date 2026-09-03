{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-ambiguous-fields #-}

module BadgeTests (badgeTests) where

import Control.Concurrent.STM (atomically)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Data.Time.Calendar (fromGregorian)
import Data.Time.Calendar.WeekDate (toWeekDate)
import Data.Time.Clock (UTCTime (..), addUTCTime, diffUTCTime, getCurrentTime, nominalDay)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import qualified Data.Aeson as J
import qualified Data.Aeson.KeyMap as KM
import Data.Maybe (isNothing)
import qualified Simplex.Messaging.Crypto as C
import Simplex.Chat.Badges
import Simplex.Chat.Badges.Code
import Simplex.Chat.Badges.Ledger
import Simplex.Chat.Badges.Service
import Simplex.Messaging.Crypto.BBS
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
    it "expires at the end of the Sunday after the period" testSundayExpiry
    it "stores the wire tag of every entry type, and reads back the ones it writes" testEntryTypeColumns
  describe "service protocol JSON" $ do
    it "redeemBadgeCode request matches the schema" testRedeemRequestJSON
    it "badgeCredential response matches the schema" testCredentialResponseJSON
    it "error response matches the schema" testErrorResponseJSON
    it "statement entries round-trip, unknown entry types verbatim" testStatementJSON

proofOf :: BadgeProof -> BBSProof
proofOf (BadgeProof _ _ p _) = p

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
  T.length formatted `shouldBe` 27 -- SXB-XXXXX-XXXXX-XXXXX-XXXXX
  T.take 4 formatted `shouldBe` "SXB-"
  T.length (badgeCodeText code) `shouldBe` 23 -- the canonical form drops the separators
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
    fixedCode = "SXB-0C0QS-XAQW1-N1VSA-R00Y3"
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
  parseBadgeCode "SXB-00000-00000-00000-0000" `shouldBe` Nothing
  parseBadgeCode (T.drop 3 canonical) `shouldBe` Nothing

testCodeHash :: IO ()
testCodeHash = do
  drg <- C.newRandom
  code <- randomBadgeCode drg
  Just typed <- pure $ parseBadgeCode $ T.toLower $ formatBadgeCode code
  badgeCodeHash typed `shouldBe` badgeCodeHash code

-- Ledger transitions, against plans/2026-07-30-supporter-badges-v3-ux.md §3

at :: Integer -> Int -> Int -> UTCTime
at y m d = UTCTime (fromGregorian y m d) (11 * 3600)

newBalance :: UTCTime -> LedgerBalance
newBalance t = LedgerBalance {balanceMonths = 0, balanceStartTs = t, balanceBadgeType = BTSupporter}

-- StatementEntry and BadgeInfo carry fields of the same names, so the selectors are ambiguous here
bMonths :: LedgerBalance -> Int
bMonths LedgerBalance {balanceMonths} = balanceMonths

bStart :: LedgerBalance -> UTCTime
bStart LedgerBalance {balanceStartTs} = balanceStartTs

pStart :: BadgePeriod -> UTCTime
pStart BadgePeriod {periodStart} = periodStart

pEnd :: BadgePeriod -> UTCTime
pEnd BadgePeriod {periodEnd} = periodEnd

-- one service pass: advance, then issue if a month is due. Returns the rows each transition
-- writes, in order, and the issued period.
pass :: UTCTime -> LedgerBalance -> ([LedgerBalance], Maybe BadgePeriod)
pass t b0 = case issueMonth t b1 of
  Just (p, b2) -> (lapsed <> [b2], Just p)
  Nothing -> (lapsed, Nothing)
  where
    (lapsed, b1) = maybe ([], b0) (\b -> ([b], b)) $ advanceBalance t b0

finalBalance :: LedgerBalance -> [LedgerBalance] -> LedgerBalance
finalBalance b0 rows = last (b0 : rows)

testTwelveMonths :: IO ()
testTwelveMonths = do
  let start = at 2026 3 10
      granted = grantMonths start 12 (newBalance start)
      -- each month is issued as soon as it falls due
      issueAll b ps = case pass (bStart b) b of
        (rows, Just p) -> issueAll (finalBalance b rows) (p : ps)
        (_, Nothing) -> (b, reverse ps)
      (spent, periods) = issueAll granted []
  length periods `shouldBe` 12
  bMonths spent `shouldBe` 0
  -- no month was skipped or issued twice: the periods tile the whole year
  map pStart periods `shouldBe` map (\m -> addMonths m start) [0 .. 11]
  zipWith (\p p' -> pEnd p == pStart p') periods (drop 1 periods) `shouldSatisfy` and
  pEnd (last periods) `shouldBe` at 2027 3 10
  -- a thirteenth request issues nothing, whenever it is made
  issueMonth (bStart spent) spent `shouldBe` Nothing
  issueMonth (at 2030 1 1) spent `shouldBe` Nothing
  advanceBalance (at 2030 1 1) spent `shouldBe` Nothing

-- the worked example: 3 months bought Tue 10 Mar 2026, app off 5 Apr - 20 May, issue 20 May
testLapseAfterGap :: IO ()
testLapseAfterGap = do
  let start = at 2026 3 10
      granted = grantMonths start 3 (newBalance start)
      (rows1, p1) = pass start granted
      afterFirst = finalBalance granted rows1
      (rows2, p2) = pass (at 2026 5 20) afterFirst
      afterSecond = finalBalance afterFirst rows2
  map bMonths rows1 `shouldBe` [2]
  fmap pStart p1 `shouldBe` Just (at 2026 3 10)
  fmap pEnd p1 `shouldBe` Just (at 2026 4 10)
  -- one lapse row for April, then May is issued: two rows, not one and not three
  map bMonths rows2 `shouldBe` [1, 0]
  map bStart rows2 `shouldBe` [at 2026 5 10, at 2026 6 10]
  fmap pStart p2 `shouldBe` Just (at 2026 5 10)
  fmap pEnd p2 `shouldBe` Just (at 2026 6 10)
  -- a lapse moves months from unused to gone; it never changes what was paid for
  map paidThrough (granted : rows1 <> rows2) `shouldBe` replicate 4 (at 2026 6 10)
  bMonths afterSecond `shouldBe` 0

testLedgerInvariants :: IO ()
testLedgerInvariants = do
  let start = at 2026 1 15
      -- grants and passes interleaved, including passes long after the balance ran out
      steps = [Left 3, Right (at 2026 1 15), Right (at 2026 4 20), Left 2, Right (at 2026 5 1), Right (at 2027 9 9), Left 1, Right (at 2027 9 9)]
      step (b, rows) = \case
        Left n -> let b' = grantMonths (bStart b) n b in (b', rows <> [b'])
        Right t -> let (rs, _) = pass t b in (finalBalance b rs, rows <> rs)
      (_, allRows) = foldl step (newBalance start, []) steps
  map bMonths allRows `shouldSatisfy` all (>= 0)
  map bStart allRows `shouldSatisfy` nonDecreasing
  where
    nonDecreasing xs = and $ zipWith (<=) xs (drop 1 xs)

testGrantAfterExhausted :: IO ()
testGrantAfterExhausted = do
      -- the balance ran out on 10 Feb; the next code is redeemed on 1 Jun
  let spent = newBalance (at 2026 2 10)
      granted = grantMonths (at 2026 6 1) 2 spent
  bStart granted `shouldBe` at 2026 6 1
  paidThrough granted `shouldBe` at 2026 8 1
  -- the four unsupported months are not backfilled, so nothing lapses immediately
  advanceBalance (at 2026 6 1) granted `shouldBe` Nothing

testGrantInsideIssuedPeriod :: IO ()
testGrantInsideIssuedPeriod = do
  let start = at 2026 1 10
      granted = grantMonths start 1 (newBalance start)
      (rows, _) = pass start granted
      issued = finalBalance granted rows
      -- topped up on 20 Jan, while the month issued on 10 Jan still runs
      toppedUp = grantMonths (at 2026 1 20) 3 issued
  bStart toppedUp `shouldBe` at 2026 2 10
  paidThrough toppedUp `shouldBe` at 2026 5 10
  -- the balance starts in the future, so no second credential is issued for January
  issueMonth (at 2026 1 20) toppedUp `shouldBe` Nothing
  fmap (pStart . fst) (issueMonth (at 2026 2 10) toppedUp) `shouldBe` Just (at 2026 2 10)

testMonthEndClipping :: IO ()
testMonthEndClipping = do
  let start = at 2027 1 31
      granted = grantMonths start 3 (newBalance start)
      issueAll b ps = case pass (bStart b) b of
        (rows, Just p) -> issueAll (finalBalance b rows) (p : ps)
        (_, Nothing) -> reverse ps
      periods = issueAll granted []
  -- 31 Jan clips to 28 Feb and does not spring back to the 31st
  map pStart periods `shouldBe` [at 2027 1 31, at 2027 2 28, at 2027 3 28]
  map pEnd periods `shouldBe` [at 2027 2 28, at 2027 3 28, at 2027 4 28]
  -- the issued period start is the balance start, never periodEnd minus a month, which clipping
  -- would answer as 28 Jan
  addMonths (-1) (pEnd (head periods)) `shouldNotBe` pStart (head periods)
  -- across a leap day
  let leap = grantMonths (at 2028 1 29) 2 (newBalance (at 2028 1 29))
      leapPeriods = issueAll leap []
  map pEnd leapPeriods `shouldBe` [at 2028 2 29, at 2028 3 29]
  -- a month that ends on the leap day counts as elapsed the moment it ends
  elapsedMonths (at 2028 2 29) leap `shouldBe` 1
  elapsedMonths (addUTCTime (-1) (at 2028 2 29)) leap `shouldBe` 0

testSundayExpiry :: IO ()
testSundayExpiry = do
  -- the end of Sunday 12 Apr is Monday 13 Apr 00:00
  endOfSundayAfter (at 2026 4 10) `shouldBe` UTCTime (fromGregorian 2026 4 13) 0
  endOfSundayAfter (at 2026 6 10) `shouldBe` UTCTime (fromGregorian 2026 6 15) 0
  -- a period ending on a Monday still runs to the end of the following Sunday, never to zero days
  endOfSundayAfter (at 2026 4 13) `shouldBe` UTCTime (fromGregorian 2026 4 20) 0
  let periodEnds = map (\d -> at 2026 4 d) [1 .. 30]
      expiries = map endOfSundayAfter periodEnds
  -- every expiry is a Monday midnight strictly after its period, by at most a week
  expiries `shouldSatisfy` all (\(UTCTime d t) -> t == 0 && (\(_, _, wd) -> wd == 1) (toWeekDate d))
  zipWith diffUTCTime expiries periodEnds `shouldSatisfy` all (\d -> d > 0 && d <= 7 * nominalDay)

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

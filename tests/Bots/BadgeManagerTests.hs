{-# LANGUAGE CPP #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

-- | Tests for the @BadgeManager@ worker: what one pass writes when a response is applied, and
-- that the signals arriving during a pass run one more pass rather than a concurrent one.
--
-- Registered under the "Supporter badges" hspec path so CI runs it (plan §4 rule 8), inside
-- 'testBracket', which is what gives these tests a chat controller. No badge service is started
-- and no RPC is sent: the test controllers configure no 'badgeServiceAddress', so
-- 'sendBadgeRequest' fails locally without contacting anything, and the tests apply a response
-- to the pass directly, composing the pass's two production halves ('storeBadgeIssueResponse' under the
-- badge lock in production, then 'presentBadgeChange' outside it) exactly as
-- 'issueDueBadgePeriod' and 'badgeManagerPass' compose them.
--
-- __No test waits on 'badgePassInterval'.__ A pass is driven by @\/_badge state@ (which signals
-- the worker) and paced by the injected 'badgeCurrentTime', which every pass reads exactly once,
-- before it reads the purchase or ledger it would act on (though 'badgeManagerPass' has already
-- read the user row by then) — so the test can count passes and hold one open without sleeping.
-- The gate ('BadgeGate' and friends) is exported because C5 drives a REDEMPTION with it — the
-- redeem path reads 'badgeCurrentTime' at the same point a pass does, after the request is sent
-- and before any row is written — and a second copy of it in another test module would be the
-- same fixture with a second set of defects.
module Bots.BadgeManagerTests (badgeManagerTests, BadgeGate (..), newBadgeGate, gatedClockAt, allowPass, waitPasses) where

import ChatClient
import ChatTests.DBUtils
import ChatTests.Profiles (testBadgeKeys)
import ChatTests.Utils
import Control.Concurrent.STM (retry)
import Control.Monad (forM_, unless)
import Control.Monad.Except (runExceptT)
import Control.Monad.Reader (runReaderT)
import qualified Data.Aeson as J
import Data.ByteString (ByteString)
import Data.Int (Int64)
import Data.Text (Text)
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime (..), secondsToDiffTime)
import Simplex.Chat.Badges (BadgeCredential (..), BadgeInfo (..), BadgeRequest (..), BadgeType (..), VerifiedBadgeRequest (..), generateMasterKey, issueBadge)
import Simplex.Chat.Badges.Months (addMonths, sundayAfter)
import Simplex.Chat.Badges.Service
  ( BadgeServiceResponse (..),
    BadgeStatement (..),
    StatementCreditType (..),
    StatementDebitType (..),
    StatementEntry (..),
    StatementEntryType (..),
  )
import Simplex.Chat.Badges.Types (BadgeLedgerEntry (..))
import Simplex.Chat.Controller (ChatConfig (..), ChatController (..), CM)
import Simplex.Chat.Library.Commands (presentBadgeChange, storeBadgeIssueResponse)
import Simplex.Chat.Store.Badges (UserBadgePurchase (..), createCodePayment, createPurchase, getLastBadgeLedgerEntry, insertLedgerEntries, setShownPurchase)
import Simplex.Chat.Types (User (..))
import Simplex.Messaging.Agent.Store.Common (withTransaction)
import Simplex.Messaging.Agent.Store.DB (Binary (..))
import qualified Simplex.Messaging.Agent.Store.DB as DB
import qualified Simplex.Messaging.Crypto as C
import Simplex.Messaging.Crypto.BBS (BBSPublicKey, BBSSecretKey, bbsKeyGen)
import Test.Hspec hiding (it)
import System.Timeout (timeout)
import UnliftIO.STM
#if defined(dbPostgres)
import Database.PostgreSQL.Simple (Only (..))
#else
import Database.SQLite.Simple (Only (..))
#endif

badgeManagerTests :: SpecWith TestParams
badgeManagerTests = do
  describe "applying an issueBadge response" $ do
    it "stores the statement, writes the issuance, sets the profile badge and reports the change" testIssuedCredentialApplied
    it "discards a credential signed by another key and writes nothing at all" testForeignCredentialRejected
    it "an exhausted balance stores the statement, writes no issuance and keeps the badge" testExhaustedBalanceApplied
  describe "the badge worker" $ do
    it "collapses the signals that arrive during a pass into one more pass, never a concurrent one" testSignalsDuringPassRunOnePass
    it "applies a pass for an inactive profile without switching which profile is active" testInactiveProfilePassDoesNotSwitchActiveUser

-- Fixtures --------------------------------------------------------------------

-- | The instant the fixture's balance starts running, and the start of the period the pass
-- issues. Whole seconds: Postgres @TIMESTAMPTZ@ keeps microseconds while 'getCurrentTime' has
-- nanosecond resolution on Linux, so only a rounded instant survives both backends.
periodStart :: UTCTime
periodStart = UTCTime (fromGregorian 2026 3 1) (secondsToDiffTime 0)

-- | The end of that period, which is where @issue@ leaves @balanceStartTs@.
periodEnd :: UTCTime
periodEnd = addMonths 1 periodStart

-- | Inside the period, so the fixture's balance funds an unissued month at this instant.
passNow :: UTCTime
passNow = UTCTime (fromGregorian 2026 3 15) (secondsToDiffTime 0)

heldEntryUuid :: Text
heldEntryUuid = "held-credit"

-- | The purchase's ledger before the pass: two months credited, running from 'periodStart'.
creditEntry :: StatementEntry
creditEntry = fixtureEntry heldEntryUuid 2 2 periodStart (SECredit SCPayment {invoiceId = Nothing})

-- | The @debit(badge)@ an issue writes, and the only thing the client can read the issued period
-- back from: one month spent, and @balanceStartTs@ moved to the END of the period issued.
issuedDebitEntry :: StatementEntry
issuedDebitEntry = fixtureEntry "issued-debit" (-1) 1 periodEnd (SEDebit SDBadge)

fixtureEntry :: Text -> Int -> Int -> UTCTime -> StatementEntryType -> StatementEntry
fixtureEntry entryId changeMonths balanceMonths balanceStartTs entryType =
  StatementEntry
    { entryId,
      changeMonths,
      balanceMonths,
      balanceStartTs,
      balanceBadgeType = BTSupporter,
      wasPausedSince = Nothing,
      -- the service's clock, identical for every entry of one statement
      createdAt = passNow,
      entryType
    }

-- | Counts and paces the badge passes of a controller. Every pass reads 'badgeCurrentTime'
-- exactly once, before it reads the purchase or ledger, so 'passes' is the number of passes that
-- have STARTED and a pass gets no further until the test grants a permit. (One store read —
-- 'badgeManagerPass'\'s own @getUser@ — does happen before the gate; it is not one of the rows
-- these tests seed or assert on, so it cannot race them.)
--
-- Tests that apply a response themselves grant none: that parks the pass chat start signals
-- before it can read the purchase or ledger, so it can never race their fixtures.
data BadgeGate = BadgeGate {passes :: TVar Int, permits :: TVar Int}

newBadgeGate :: IO BadgeGate
newBadgeGate = BadgeGate <$> newTVarIO 0 <*> newTVarIO 0

-- | The gated clock, reporting @now@ once a permit is granted. C5 needs a different instant from
-- these tests, and nothing else about the gate differs, so the instant is the argument.
gatedClockAt :: UTCTime -> BadgeGate -> IO UTCTime
gatedClockAt now BadgeGate {passes, permits} = do
  atomically $ modifyTVar' passes (+ 1)
  atomically $ readTVar permits >>= \n -> if n > 0 then writeTVar permits (n - 1) else retry
  pure now

gatedClock :: BadgeGate -> IO UTCTime
gatedClock = gatedClockAt passNow

allowPass :: BadgeGate -> IO ()
allowPass BadgeGate {permits} = atomically $ modifyTVar' permits (+ 1)

-- | Waits for @n@ passes to have started. Bounded, so a worker that never runs fails the example
-- instead of hanging it; it is not a wait on badge time.
waitPasses :: HasCallStack => BadgeGate -> Int -> Expectation
waitPasses BadgeGate {passes} n =
  timeout 10000000 (atomically $ readTVar passes >>= \m -> unless (m >= n) retry) >>= \case
    Just () -> pure ()
    Nothing -> expectationFailure $ "timed out waiting for badge pass " <> show n

-- | Nothing more is reported: the assertion that a pass emitted no event and raised no error.
reportsNothing :: HasCallStack => TestCC -> Expectation
reportsNothing cc = cc <// 500000

-- | A controller with the test issuer key at index 1 and a gated badge clock.
withBadgeChat :: HasCallStack => TestParams -> BBSPublicKey -> (BadgeGate -> TestCC -> IO ()) -> IO ()
withBadgeChat ps pk test = do
  gate <- newBadgeGate
  let cfg = testCfg {badgePublicKeys = testBadgeKeys pk, badgeCurrentTime = gatedClock gate}
  withNewTestChatCfg ps cfg "alice" aliceProfile $ test gate

-- | A shown, issued purchase with two months credited, and the last ledger entry a pass would
-- assert to the service.
setupShownPurchase :: TestCC -> IO (UserBadgePurchase, BadgeLedgerEntry)
setupShownPurchase cc = do
  User {userId} <- testUser cc
  drg <- C.newRandom
  (pubKey, privKey) <- atomically $ C.generateKeyPair drg
  mk <- generateMasterKey drg
  r <- withChatStore cc $ \db -> do
    paymentId <- createCodePayment db periodStart
    p@UserBadgePurchase {badgePurchaseId = pId} <- createPurchase db userId pubKey privKey mk BTSupporter paymentId periodStart
    runExceptT $ do
      setShownPurchase db userId pId
      insertLedgerEntries db pId BadgeStatement {entries = [creditEntry], previousEntryId = Nothing} periodStart
      (p,) <$> getLastBadgeLedgerEntry db pId
  case r of
    Right (p, Just e) -> pure (p, e)
    _ -> fail "badge fixture: the purchase or its credit entry was not stored"

testUser :: TestCC -> IO User
testUser TestCC {chatController = ChatController {currentUser}} =
  readTVarIO currentUser >>= maybe (fail "no current user") pure

withChatStore :: TestCC -> (DB.Connection -> IO a) -> IO a
withChatStore TestCC {chatController = ChatController {chatStore}} = withTransaction chatStore

runCM :: TestCC -> CM a -> IO a
runCM TestCC {chatController} action = runReaderT (runExceptT action) chatController >>= either (fail . show) pure

-- | The pass's second half, composed exactly as 'badgeManagerPass' composes it: the response is
-- stored (in production under the per-user badge lock) and the change it made is then presented
-- and reported (in production with that lock released). Runs for the currently ACTIVE user.
applyIssueResponse :: TestCC -> UserBadgePurchase -> Maybe BadgeLedgerEntry -> BadgeServiceResponse -> IO ()
applyIssueResponse cc purchase heldEntry_ resp = testUser cc >>= \user -> applyIssueResponseFor cc user purchase heldEntry_ resp

-- | As 'applyIssueResponse', but for an explicit user rather than whichever one happens to be
-- active. Production's 'badgeManagerPass' always fetches the WORKER's own user by id
-- (@withFastStore (\`getUser\` userId)@) rather than reading 'currentUser', so a pass can run for
-- a profile that is not the active one — this is what 'testInactiveProfilePassDoesNotSwitchActiveUser'
-- exercises (plan §9, review round).
applyIssueResponseFor :: TestCC -> User -> UserBadgePurchase -> Maybe BadgeLedgerEntry -> BadgeServiceResponse -> IO ()
applyIssueResponseFor cc user purchase heldEntry_ resp =
  runCM cc $ storeBadgeIssueResponse passNow user purchase heldEntry_ resp >>= presentBadgeChange

-- | A credential for the fixture's period, signed by the given issuer key. The master key is the
-- PURCHASE's, as the request the worker sends carries it.
issuedCredential :: BBSSecretKey -> UserBadgePurchase -> IO BadgeCredential
issuedCredential sk UserBadgePurchase {masterKey} = do
  let badgeInfo = BadgeInfo {badgeType = BTSupporter, badgeExpiry = Just (sundayAfter periodEnd), badgeExtra = ""}
  issueBadge 1 sk (VerifiedBadgeRequest BadgeRequest {masterKey, badgeInfo}) >>= either fail pure

badgeCredentialResponse :: Maybe BadgeCredential -> BadgeStatement -> BadgeServiceResponse
badgeCredentialResponse credential statement = BSPBadgeCredential {credential, receipt = Nothing, statement}

-- Assertions ------------------------------------------------------------------

lastLedgerEntry :: TestCC -> Int64 -> IO (Maybe BadgeLedgerEntry)
lastLedgerEntry cc pId =
  withChatStore cc (\db -> runExceptT $ getLastBadgeLedgerEntry db pId) >>= either (fail . show) pure

ledgerUuids :: TestCC -> Int64 -> IO [Text]
ledgerUuids cc pId =
  map fromOnly
    <$> withChatStore cc (\db -> DB.query db "SELECT entry_uuid FROM badge_ledger WHERE badge_purchase_id = ? ORDER BY entry_id ASC" (Only pId))

-- | @(badge_purchase_id, badge_type, period_start, period_end, expiry, entry_id)@ of every
-- issuance, read from the table rather than from what 'createIssuance' returned.
issuanceRows :: TestCC -> IO [(Int64, Text, UTCTime, UTCTime, UTCTime, Maybe Int64)]
issuanceRows cc =
  withChatStore cc $ \db ->
    DB.query_ db "SELECT badge_purchase_id, badge_type, period_start, period_end, expiry, entry_id FROM badge_issuances ORDER BY period_start ASC"

-- | The credential stored with the only issuance, decoded from the JSON it is stored as.
storedCredential :: TestCC -> IO BadgeCredential
storedCredential cc = do
  rows <- withChatStore cc $ \db -> DB.query_ db "SELECT credential FROM badge_issuances"
  case rows of
    [Only (Binary bs)] -> maybe (fail "stored badge credential does not decode") pure (J.decodeStrict (bs :: ByteString))
    _ -> fail $ "expected exactly one badge issuance, got " <> show (length rows)

hasNoBadge :: HasCallStack => TestCC -> Expectation
hasNoBadge cc = do
  cc ##> "/p"
  cc <## "user profile: alice (Alice)"
  cc <## "use /p <name> [<bio>] to change it"

hasSupporterBadge :: HasCallStack => TestCC -> Expectation
hasSupporterBadge cc = do
  cc ##> "/p"
  cc <## "user profile: alice (Alice, * supporter)"
  cc <## "use /p <name> [<bio>] to change it"

-- Tests -----------------------------------------------------------------------

-- | The whole write set of one issued period, in one place: the statement's rows, the issuance
-- naming the period the @debit(badge)@ implies, the credential on the profile, and the event.
testIssuedCredentialApplied :: HasCallStack => TestParams -> IO ()
testIssuedCredentialApplied ps = do
  Right (pk, sk) <- bbsKeyGen
  withBadgeChat ps pk $ \_gate alice -> do
    (purchase@UserBadgePurchase {badgePurchaseId = pId}, heldEntry) <- setupShownPurchase alice
    cred <- issuedCredential sk purchase
    let statement = BadgeStatement {entries = [issuedDebitEntry], previousEntryId = Just heldEntryUuid}
    applyIssueResponse alice purchase (Just heldEntry) (badgeCredentialResponse (Just cred) statement)
    -- the balance left, and the date it is paid through: one month from the END of the period
    -- just issued, never the credential's expiry
    alice <## "supporter badge 1 (shown): issued, 1 month(s) left, paid through 2026-05-01"
    ledgerUuids alice pId `shouldReturn` [heldEntryUuid, "issued-debit"]
    -- period_start is the balanceStartTs in force before the debit(badge) and period_end is the
    -- one the debit left; entry_id stays NULL (plan §9)
    issuanceRows alice `shouldReturn` [(pId, "supporter", periodStart, periodEnd, sundayAfter periodEnd, Nothing)]
    storedCredential alice `shouldReturn` cred
    hasSupporterBadge alice

-- | The credential verifies against a key the controller is not configured with, so the whole
-- response is discarded — the statement included, since a response whose credential is not ours
-- is not one to trust the rest of.
testForeignCredentialRejected :: HasCallStack => TestParams -> IO ()
testForeignCredentialRejected ps = do
  Right (pk, _sk) <- bbsKeyGen
  Right (_otherPk, otherSk) <- bbsKeyGen
  withBadgeChat ps pk $ \_gate alice -> do
    (purchase@UserBadgePurchase {badgePurchaseId = pId}, heldEntry) <- setupShownPurchase alice
    cred <- issuedCredential otherSk purchase
    let statement = BadgeStatement {entries = [issuedDebitEntry], previousEntryId = Just heldEntryUuid}
    applyIssueResponse alice purchase (Just heldEntry) (badgeCredentialResponse (Just cred) statement)
    alice <## "badge service error: internal, badge credential does not verify against configured key"
    ledgerUuids alice pId `shouldReturn` [heldEntryUuid]
    issuanceRows alice `shouldReturn` []
    hasNoBadge alice
    reportsNothing alice

-- | @credential = Nothing@ is the exhausted balance, not an error: the statement is stored, no
-- issuance is written, the profile's badge is left alone, and the event is emitted only because
-- the stored state actually changed — re-delivering the same statement emits nothing.
testExhaustedBalanceApplied :: HasCallStack => TestParams -> IO ()
testExhaustedBalanceApplied ps = do
  Right (pk, _sk) <- bbsKeyGen
  withBadgeChat ps pk $ \_gate alice -> do
    (purchase@UserBadgePurchase {badgePurchaseId = pId}, heldEntry) <- setupShownPurchase alice
    let lapsed = fixtureEntry "lapse-1" (-2) 0 (addMonths 2 periodStart) (SEDebit SDLapse)
        statement = BadgeStatement {entries = [lapsed], previousEntryId = Just heldEntryUuid}
    applyIssueResponse alice purchase (Just heldEntry) (badgeCredentialResponse Nothing statement)
    alice <## "supporter badge 1 (shown): issued, 0 month(s) left, paid through 2026-05-01"
    ledgerUuids alice pId `shouldReturn` [heldEntryUuid, "lapse-1"]
    issuanceRows alice `shouldReturn` []
    hasNoBadge alice
    -- the same statement again changes nothing, so nothing is reported
    heldEntry' <- lastLedgerEntry alice pId
    applyIssueResponse alice purchase heldEntry' (badgeCredentialResponse Nothing statement)
    ledgerUuids alice pId `shouldReturn` [heldEntryUuid, "lapse-1"]
    reportsNothing alice

-- | Three signals arriving while a pass runs leave the worker with ONE more pass to run, not
-- three and not a concurrent one. The gated clock is what makes this deterministic: the pass
-- chat start signalled is parked at its first line, so every signal below provably arrives
-- while it is still running.
testSignalsDuringPassRunOnePass :: HasCallStack => TestParams -> IO ()
testSignalsDuringPassRunOnePass ps = do
  Right (pk, _sk) <- bbsKeyGen
  withBadgeChat ps pk $ \gate alice -> do
    waitPasses gate 1
    _ <- setupShownPurchase alice
    forM_ [1 :: Int .. 3] $ \_ -> do
      alice ##> "/_badge state 1"
      alice <## "supporter badge 1 (shown): issued, 2 month(s) left, paid through 2026-05-01"
      alice <## "badge site: not configured"
    -- the three signals did not start a pass of their own: the first one is still running
    readTVarIO (passes gate) `shouldReturn` 1
    -- released, it finds the month due and reports what its send path answers: this controller
    -- has no 'badgeServiceAddress', so the request fails locally before anything is sent (C4)
    allowPass gate
    alice <## "badge service error: internal, badge service address is not configured"
    waitPasses gate 2
    allowPass gate
    alice <## "badge service error: internal, badge service address is not configured"
    -- and that is the only pass the three signals bought, however many they were
    reportsNothing alice
    readTVarIO (passes gate) `shouldReturn` 2

-- | Every user gets a badge worker (plan §9 review round), so a pass need not run for the
-- ACTIVE profile. Proves the guard that keeps such a pass from switching which profile is
-- active: without it, applying a credential response for alice's purchase below would leave
-- 'currentUser' pointing at alice even though \"secret\" was made active in between — because
-- production's 'badgeManagerPass' fetches its own user by id rather than through 'currentUser',
-- exactly like 'applyIssueResponseFor' does here.
testInactiveProfilePassDoesNotSwitchActiveUser :: HasCallStack => TestParams -> IO ()
testInactiveProfilePassDoesNotSwitchActiveUser ps = do
  Right (pk, sk) <- bbsKeyGen
  withBadgeChat ps pk $ \_gate alice -> do
    -- alice is active when her purchase is created, so it is created under HER userId
    aliceUser@User {userId = aliceUserId} <- testUser alice
    (purchase, heldEntry) <- setupShownPurchase alice
    -- a second profile becomes active; alice's is now the inactive one
    alice ##> "/create user secret"
    alice <## "user profile: secret"
    alice <## "use /p <name> [<bio>] to change it"
    User {userId = secretUserId} <- testUser alice
    cred <- issuedCredential sk purchase
    let statement = BadgeStatement {entries = [issuedDebitEntry], previousEntryId = Just heldEntryUuid}
    -- the pass runs for ALICE (explicit, as her worker's would), while "secret" stays active
    applyIssueResponseFor alice aliceUser purchase (Just heldEntry) (badgeCredentialResponse (Just cred) statement)
    -- the event still reports alice's own change, prefixed since she is not the active profile
    alice <## "[user: alice] supporter badge 1 (shown): issued, 1 month(s) left, paid through 2026-05-01"
    -- alice's own row was written despite her being inactive
    storedCredential alice `shouldReturn` cred
    -- but the active profile did not move: still "secret", never back to alice
    User {userId = activeAfter} <- testUser alice
    activeAfter `shouldBe` secretUserId
    activeAfter `shouldNotBe` aliceUserId

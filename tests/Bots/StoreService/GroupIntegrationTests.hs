{-# LANGUAGE CPP #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

-- Members' messages reach a console over separate connections in no fixed order, so gating is asserted from the bot's store.
module Bots.StoreService.GroupIntegrationTests (badgeGroupIntegrationTests, namesBootTests, namesCodesChatTests, namesRefundChatTests, namesWorkerBootTests) where

import StoreService.Config (BadgeIssuerKey (..), GroupConfig (..))
import StoreService.Group (GroupAction (..), GroupEvent (..), codeInTracker, ensureManagedGroup)
import StoreService.Group.Command (CmdAction (..), groupCmdAction, maxUses)
import StoreService.Names.Abi (selector, uint256)
import StoreService.Names.Codes (issueNameCode, reserveCode, revokeNameCode)
import StoreService.Names.Phases (RegistrationId (..))
import StoreService.Names.Store (NamePhase (..), NameRegistration (..), PrepaidCode (..), VersionWrite (..), getPrepaidCode, spendPrepaidCode)
import StoreService.Names.Turns (newTurns, withCodeTurn)
import StoreService.Options (StoreServiceOpts (..))
import StoreService.Orders (cancelOpenInvoice, expireOverdue)
import StoreService.Service (ServiceState (..), storeService, newServiceState)
import StoreService.Store (ManagedGroup (..), getManagedGroup)
import StoreService.Waiters (newWaiters)
import Bots.StoreService.BotTests (badgeBotName, badgeCmdUsage, badgeProfile, badgeTypeOf, credentialOf, entryOf, issueCode, issueRaw, mkStoreServiceOpts, newPurchaseKeys, operatorCmd, redeemWithQueue, requestObject, revokeRaw, serviceDbPrefix, shouldAnswerError, statementOf, stopStoreService, testIssuerKeyIdx, withStoreService)
import Bots.StoreService.ConfigTests (chainSection, controllerAddr, namesSection)
import Bots.StoreService.FakeEthNode (CallAnswer (..), FakeEthNode (..), answerCall, awaitSent, withFakeEthNode)
import Bots.StoreService.WebTests (capturingStderr, digestFixture)
import Bots.StoreService.NamesSearchTests (aliceHex)
import qualified Bots.StoreService.NamesBalancesTests as Balances
import Bots.StoreService.NamesStoreTests (order, ownerA, placeCode, placePaid, reg, t0, write)
import ChatClient
import ChatTests.DBUtils
import ChatTests.Utils
import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.Async (Async, async, cancel, poll, wait, waitCatchSTM)
import Control.Concurrent.STM (TQueue, atomically, newTQueueIO, orElse, readTMVar, readTQueue, readTVarIO, tryReadTMVar, writeTQueue)
import Control.Exception (IOException, bracket, catch, finally, throwIO, try)
import Control.Monad (forM_, guard, mfilter, replicateM_, unless, void, when)
import qualified Data.Aeson as J
import Data.Either (isLeft, isRight)
import Data.Int (Int64)
import Data.List (isInfixOf)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, isJust, isNothing, listToMaybe)
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (NominalDiffTime, UTCTime (..), addUTCTime, getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Network.HTTP.Client (RequestBody (..), defaultManagerSettings, httpLbs, method, newManager, parseRequest, requestBody, responseBody, responseHeaders, responseStatus)
import Network.HTTP.Types.Status (statusCode)
import Network.Socket (Family (..), SockAddr (..), SocketType (..), close, connect, defaultProtocol, socket, tupleToHostAddress)
import Network.Wai.Handler.Warp (openFreePort)
import Simplex.Chat.Badges (BadgeType (..))
import Simplex.Chat.Badges.Code (NameCode (..), NamePrepaidCode, formatBadgeCode, formatNameCode, namePrepaidCodeHash, namePrepaidCodeLength, parseBadgeCode, parseNameCode, randomBadgeCode, randomNamePrepaidCode)
import Simplex.Chat.Badges.Types (BadgeCodePaymentStatus (..))
import Simplex.Chat.StoreService
import Simplex.Chat.Controller (ChatCommand (APIDeleteChatItem), ChatConfig (..), ChatController (..), ChatResponse (CRChatItemsDeleted, CRCustomChatResponse))
import Simplex.Chat.Core (sendChatCmd, sendChatCmdStr)
import Simplex.Chat.Messages (ChatRef (ChatRef), ChatType (CTGroup))
import Simplex.Chat.Messages.CIContent (CIDeleteMode (CIDMInternal))
import Simplex.Chat.PaymentService.Types (InvoiceId (..))
import Simplex.Chat.Types (AgentInvId (..))
import Simplex.Chat.Types.Preferences (ChatBotCommand (..), FullDeleteGroupPreference (..), GroupFeatureEnabled (..), GroupFeatureI (..), GroupPreferences (..), HistoryGroupPreference (..), SGroupFeature (..), commands_, emptyGroupPrefs, getGroupPreference)
import Simplex.Chat.Types.Shared (GroupMemberRole (..))
import Simplex.Messaging.Agent (disposeAgentClient)
import Simplex.Messaging.Agent.Protocol (AConnShortLink (..), ConnShortLink (..), ContactConnType (..))
import Simplex.Messaging.Agent.Store.Common (DBStore, withTransaction)
import qualified Simplex.Messaging.Agent.Store.DB as DB
import qualified Simplex.Messaging.Crypto as C
import Simplex.Messaging.Crypto.BBS (bbsKeyGen)
import Simplex.Messaging.Encoding.String (strDecode)
import Simplex.Messaging.Util (tshow)
import System.Directory (createDirectoryIfMissing)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.Timeout (timeout)
#if defined(dbPostgres)
import Database.PostgreSQL.Simple (FromRow, Only (..))
#else
import Database.SQLite.Simple (FromRow, Only (..))
#endif
import Test.Hspec hiding (it)

badgeGroupIntegrationTests :: SpecWith TestParams
badgeGroupIntegrationTests = do
  it "creates the group and a join link on first start; on restart reuses it, re-advertises commands and keeps other preferences" testGroupCreateReuse
  it "keeps the group's name and description when the configured ones change" testConfigNotApplied
  it "promotes the first joiner to owner and leaves a later joiner a member" testFirstJoinerPromoted
  it "promotes nobody after the first promotion failed" testFailedPromotionNotRetried
  it "promotes the earliest member on start when their join was never handled" testMissedOwnerPromotedOnStart
  it "promotes the earliest member, not a later joiner, when no owner was set up" testEarliestPromotedOnLaterJoin
  it "skips a member still joining when promoting on start" testJoiningMemberSkippedOnStart
  it "only records an owner made by hand on start, promoting nobody" testHandMadeOwnerKeptOnStart
  it "promotes a member on start when the owner made by hand has left" testLeftOwnerReplacedOnStart
  it "promotes nobody on start once the owner was set up, even with no owner left" testSetUpOwnerNotReplacedOnStart
  it "does not serve a group its owner deleted" testDeletedGroupNotServed
  it "does not serve a group the service itself deleted" testOwnDeletedGroupNotServed
  it "drops the old group's trackers when it creates a new group" testReplacedGroupDropsTrackers
  it "stops rather than create a second group when the group lookup fails" testGroupLookupFailureStops
  it "stops the service's lanes when its runner is cancelled" testCancelStopsLanes
  it "issues on a privileged member's command, ignores a plain member's" testRoleGatedIssue
  it "ignores a command from a privileged member blocked for all" testBlockedMemberCommandIgnored
  it "ignores a command sent in a member support scope" testSupportScopeIgnored
  it "ignores a command sent as a live message" testLiveMessageIgnored
  it "ignores an event for another group" testOtherGroupEventIgnored
  it "issues a batch of redeemable single-use codes of the asked type and months on /bulk" testBulkIssue
  it "issues an /issue code for the months the command asked for" testIssueMonths
  it "answers a failed /issue or //issue without naming what failed in the store" testIssueFailureIsOpaque
  it "lists the codes a /bulk issued before an insert failed" testBulkPartialFailureListsIssued
  it "answers a failed /revoke without naming what failed in the store" testRevokeFailureIsOpaque
  it "tracks a multi-use code in one message across redemptions and refuses it when used up" testMultiUseTracker
  it "leaves a used-up code's tracker as it is on a stale refresh" testStaleRefreshIgnored
  it "keeps each multi-use code's counter to its own claims" testCountersKeptApart
  it "refreshes the tracker for a redemption served with no group lane" testLanelessRedeemRefreshesTracker
  it "refreshes the tracker for a redemption from the service's request queue" testQueuedRequestRefreshesTracker
  it "refreshes the tracker inline for a queued redemption after a restart without [group]" testQueuedRequestWithoutGroupConfig
  it "reposts and re-anchors the tracker past the edit window" testTrackerRepost
  it "reposts and re-anchors the tracker when the core refuses the edit" testRefusedEditReposted
  it "keeps the tracker anchored when the repost cannot be sent" testFailedRepostDropped
  it "never reposts a tracker deleted on the service's side, publishing the code no further" testDeletedTrackerNotReposted
  it "never reposts a tracker an owner deleted for everyone" testModeratedTrackerNotReposted
  it "revokes a code whose tracker was deleted on the service's side without reposting it" testDeletedTrackerNotRepostedOnRevoke
  it "publishes nothing when a code whose tracker was deleted is used up" testDeletedTrackerUsedUp
  it "revokes a code whose tracker an owner deleted for everyone without reposting it" testModeratedTrackerNotRepostedOnRevoke
  it "revokes a code on /revoke, answers a repeat as already revoked and an unknown code as no such code" testGroupRevoke
  it "retires the tracker message when a multi-use code is revoked" testRevokeRetiresTracker
  it "retires the tracker message when the service command revokes the code" testServiceRevokeRetiresTracker
  it "retires a tracker left behind when the code is revoked again" testGroupRevokeRepairsTracker
  it "retires a tracker left behind when the service command revokes again" testServiceRevokeRepairsTracker
  it "posts one retired tracker however often a revoke past the window repeats" testRevokeRepeatPastWindow
  it "reconciles a used-up tracker whose refresh was lost, in place" testUsedUpTrackerReconciledOnRestart
  it "reconciles a tracker with uses left whose refresh was lost" testStalledTrackerReconciledOnRestart
  it "retires on restart a tracker whose revoke never reached it" testRevokedTrackerReconciledOnRestart
  it "reconciles every stalled tracker on restart, not only the first" testEveryStalledTrackerReconciled
  it "leaves a tracker it cannot edit alone rather than publishing the code again" testUneditableTrackerLeftAlone

namesBootTests :: SpecWith TestParams
namesBootTests = it "boots with [names] and [chain], routing fixed /api/name paths ahead of a search, find ahead of registrations, registrations to GET, another name, a reveal and a retry to POST, a path no route knows and a taken order's code to 404, code checks and orders under one limit, and another name and retries under another" testNamesBoot

testNamesBoot :: HasCallStack => TestParams -> IO ()
testNamesBoot ps = do
  svc <- prepareGroupService ps
  (t, cc, _, port) <- startGroupService svc {gsIniSections = "" : namesSection <> chainSection} Nothing
  flip finally (stopGroupService t cc) $ do
    pollUntilTrue (listening port)
    mgr <- newManager defaultManagerSettings
    let request path = parseRequest $ "http://127.0.0.1:" <> show port <> path
        answer req = (\r -> (statusCode $ responseStatus r, J.decode $ responseBody r)) <$> httpLbs req mgr
        refused tag = (400, Just $ J.object ["error" J..= (tag :: Text)])
    unknown <- request "/api/names/unknown"
    statusCode . responseStatus <$> httpLbs unknown mgr `shouldReturn` 404
    records <- request "/api/name/records"
    answer records {method = "POST", requestBody = RequestBodyLBS "{}"} `shouldReturn` refused "bad_request"
    register <- request "/api/name/register"
    answer register {method = "POST", requestBody = RequestBodyLBS "{}"} `shouldReturn` refused "bad_request"
    search <- request $ "/api/name/" <> T.unpack aliceHex <> "?length=5"
    answer search `shouldReturn` refused "invalid_name"
    registration <- request "/api/registration/t2Jk8RmQ4vXw9NcP3yLb6g"
    answer registration `shouldReturn` (404, Just $ J.object ["error" J..= ("not_found" :: Text)])
    posted <- httpLbs registration {method = "POST"} mgr
    (statusCode $ responseStatus posted, lookup "Allow" $ responseHeaders posted) `shouldBe` (405, Just "GET")
    found <- request "/api/registration/find"
    answer found {method = "POST", requestBody = RequestBodyLBS "{}"} `shouldReturn` refused "bad_request"
    foundByGet <- httpLbs found mgr
    (statusCode $ responseStatus foundByGet, lookup "Allow" $ responseHeaders foundByGet) `shouldBe` (405, Just "POST")
    owned <- request "/api/registration/t2Jk8RmQ4vXw9NcP3yLb6g/name"
    answer owned {method = "POST", requestBody = RequestBodyLBS "{}"} `shouldReturn` refused "bad_request"
    coded <- request "/api/registration/t2Jk8RmQ4vXw9NcP3yLb6g/code"
    answer coded {method = "POST", requestBody = RequestBodyLBS "{}"} `shouldReturn` (404, Just $ J.object ["error" J..= ("not_found" :: Text)])
    revealed <- request "/api/registration/t2Jk8RmQ4vXw9NcP3yLb6g/reveal"
    answer revealed {method = "POST", requestBody = RequestBodyLBS "{}"} `shouldReturn` (404, Just $ J.object ["error" J..= ("not_found" :: Text)])
    revealedByGet <- httpLbs revealed mgr
    (statusCode $ responseStatus revealedByGet, lookup "Allow" $ responseHeaders revealedByGet) `shouldBe` (405, Just "POST")
    retried <- request "/api/registration/t2Jk8RmQ4vXw9NcP3yLb6g/retry"
    answer retried {method = "POST"} `shouldReturn` (404, Just $ J.object ["error" J..= ("not_found" :: Text)])
    retriedByGet <- httpLbs retried mgr
    (statusCode $ responseStatus retriedByGet, lookup "Allow" $ responseHeaders retriedByGet) `shouldBe` (405, Just "POST")
    -- another name and the retry above took 2 of this client's 5 create requests a minute
    replicateM_ 3 $ statusCode . responseStatus <$> httpLbs retried {method = "POST"} mgr `shouldReturn` 404
    statusCode . responseStatus <$> httpLbs retried {method = "POST"} mgr `shouldReturn` 429
    -- the register above took the first of this client's 10 code requests a minute, which the code check shares
    let codeRequest path = request path >>= \r -> statusCode . responseStatus <$> httpLbs r {method = "POST", requestBody = RequestBodyLBS "{}"} mgr
    forM_ (take 9 $ cycle ["/api/code/check", "/api/name/register"]) $ \path -> (path,) <$> codeRequest path `shouldReturn` (path, 400)
    forM_ ["/api/code/check", "/api/name/register"] $ \path -> (path,) <$> codeRequest path `shouldReturn` (path, 429)
    poll t >>= (`shouldSatisfy` isNothing)

namesWorkerBootTests :: SpecWith TestParams
namesWorkerBootTests = it "starts the workers with the service, sending the commit of an order queued after the start" testWorkerBoot

testWorkerBoot :: HasCallStack => TestParams -> IO ()
testWorkerBoot ps = withFakeEthNode $ \node@FakeEthNode {fnUrl} -> do
  svc <- prepareGroupService ps
  let onFakeNode line = if "rpc_url" `T.isPrefixOf` line then "rpc_url = " <> T.pack fnUrl else line
      controller = either error id $ strDecode $ encodeUtf8 controllerAddr
  -- the order's commitment, digestFixture 1, not yet committed
  forM_ [("minCommitmentAge()", "", 60 :: Int), ("maxCommitmentAge()", "", 86400), ("commitments(bytes32)", digestFixture 1, 0)] $ \(signature, args, value) ->
    answerCall node controller (selector signature <> args) $ Returns $ uint256 value
  (t, cc, _, port) <- startGroupService svc {gsIniSections = "" : namesSection <> map onFakeNode chainSection} Nothing
  flip finally (stopGroupService t cc) $ do
    pollUntilTrue (listening port)
    let st = chatStore cc
    code <- issueNameRaw cc "name 8"
    row <- fromMaybe (error "no prepaid code row") <$> withTransaction st (`getPrepaidCode` namePrepaidCodeHash code)
    placeCode st "r1" ownerA (order "alicebakery" 1) row `shouldReturn` Right ()
    timeout waitLimit (awaitSent node 1) >>= (`shouldSatisfy` isJust)
    poll t >>= (`shouldSatisfy` isNothing)

namesCodesChatTests :: SpecWith TestParams
namesCodesChatTests = describe "StoreService names: prepaid codes" $ do
  it "issues prepaid codes on /issue name and /bulk name, answering the usage to years below 2" testGroupNameCodes
  it "issues prepaid codes on //issue name, checks them through the names router and revokes them on //revoke" testOperatorNameCodes
  it "revokes a prepaid code in the code's turn, after a reservation holding it" testRevokeTakesCodeTurn

testGroupNameCodes :: HasCallStack => TestParams -> IO ()
testGroupNameCodes ps =
  withGroupOwner ps $ \_ cc _ alice -> do
    one <- prepaidIn . T.stripPrefix "Code: " =<< replyTo cc alice "/issue name 8 years 5"
    namePrepaidCodeLength one `shouldBe` 8
    prepaidTerms cc one `shouldReturn` Just (8, 5, CPSFree)
    bulk <- mapM (prepaidIn . Just) . T.lines =<< replyTo cc alice "/bulk name 6 count 3"
    length bulk `shouldBe` 3
    forM_ bulk $ \code -> (namePrepaidCodeLength code,) <$> prepaidTerms cc code `shouldReturn` (6, Just (6, 2, CPSFree))
    replyTo cc alice "/issue name 8 years 1" `shouldReturn` "Usage: /issue <type> [months <M>] [uses <N>], or name <length> [years <Y>]"
    countRows cc "sx_badge_service_name_prepaid_codes" `shouldReturn` 4

testOperatorNameCodes :: HasCallStack => TestParams -> IO ()
testOperatorNameCodes ps = do
  svc <- prepareGroupService ps
  (t, cc, _, port) <- startGroupService svc {gsIniSections = "" : namesSection <> chainSection} Nothing
  flip finally (stopGroupService t cc) $ do
    pollUntilTrue (listening port)
    forM_ ["name", "name 5", "name 64", "name 07", "name 7 years 1", "name 7 years 11", "name 7 gratis"] $ \args ->
      (args,) <$> issueRaw cc args `shouldReturn` (args, Left badgeCmdUsage)
    forM_ [(5, 2), (64, 2), (7, 1), (7, 11)] $ \(len, years) ->
      ((len, years),) . isLeft <$> issueNameCode cc len years CPSFree `shouldReturn` ((len, years), True)
    countRows cc "sx_badge_service_name_prepaid_codes" `shouldReturn` 0
    paid <- issueNameRaw cc "name 7 years 3 paid"
    unpaid <- issueNameRaw cc "name 6 unpaid"
    prepaidTerms cc paid `shouldReturn` Just (7, 3, CPSPaid)
    prepaidTerms cc unpaid `shouldReturn` Just (6, 2, CPSUnpaid)
    let check code = checkPrepaid port (formatNameCode (NCPrepaid code))
        coverage :: Int -> Int -> Text -> (Int, Maybe J.Value)
        coverage minLength years state = (200, Just $ J.object ["minLength" J..= minLength, "years" J..= years, "state" J..= state])
        revoke = revokeRaw cc . formatNameCode . NCPrepaid
    check paid `shouldReturn` coverage 7 3 "unused"
    check unpaid `shouldReturn` coverage 6 2 "unpaid"
    revokeRaw cc (formatNameCode (NCPrepaid paid) <> " junk") `shouldReturn` Left badgeCmdUsage
    revoke paid `shouldReturn` Right "Revoked."
    revoke paid `shouldReturn` Right "Already revoked."
    check paid `shouldReturn` coverage 7 3 "revoked"
    unknown <- C.newRandom >>= (`randomNamePrepaidCode` 7)
    revoke unknown `shouldReturn` Left "No such code."
    held <- issueNameRaw cc "name 8"
    spent <- issueNameRaw cc "name 8"
    let st = chatStore cc
        rowOf code = fromMaybe (error "no prepaid code row") <$> withTransaction st (`getPrepaidCode` namePrepaidCodeHash code)
    rowOf held >>= placeCode st "r1" ownerA (order "alicebakery" 1) >>= (`shouldBe` Right ())
    rowOf spent >>= placeCode st "r2" ownerA (order "bobsbakery" 2) >>= (`shouldBe` Right ())
    spentAt <- getCurrentTime
    PrepaidCode {pcId = spentId} <- rowOf spent
    withTransaction st (\db -> spendPrepaidCode db spentId (RegistrationId "r2") spentAt) `shouldReturn` True
    revoke held `shouldReturn` Right "Revoked. The order holding it keeps it."
    revoke spent `shouldReturn` Left "Already used. It cannot be revoked."
    check spent `shouldReturn` coverage 8 2 "used"

testRevokeTakesCodeTurn :: HasCallStack => TestParams -> IO ()
testRevokeTakesCodeTurn ps = do
  svc <- prepareGroupService ps
  (t, cc, _, port) <- startGroupService svc {gsIniSections = "" : namesSection <> chainSection} Nothing
  flip finally (stopGroupService t cc) $ do
    pollUntilTrue (listening port)
    turns <- newTurns
    let st = chatStore cc
        revokeAfter code release = do
          revoking <- async $ revokeNameCode turns cc code
          threadDelay 200000
          poll revoking >>= (`shouldSatisfy` isNothing)
          release
          finished revoking
    held <- issueNameRaw cc "name 8"
    holder <- holdTurn $ \body -> withCodeTurn turns (namePrepaidCodeHash held) body
    revokeAfter held (releaseTurn holder) `shouldReturn` Right "Revoked."
    finished (thAction holder) `shouldReturn` ()
    now <- getCurrentTime
    placePaid st "r1" ownerA (order "alicebakery" 1) (addUTCTime 3600 now) `shouldReturn` Right ()
    reserved <- issueNameRaw cc "name 8"
    reserving <- holdTurn $ \body -> reserveCode turns st now (namePrepaidCodeHash reserved) (RegistrationId "r1") 8 (\_ -> Right <$> body :: IO (Either Text ()))
    revokeAfter reserved (releaseTurn reserving) `shouldReturn` Right "Revoked. The order holding it keeps it."
    finished (thAction reserving) `shouldReturn` Right (Right ())

namesRefundChatTests :: SpecWith TestParams
namesRefundChatTests = describe "StoreService names: operator refund" $ do
  it "shows a balance with what paid for it, and records refunds by registration or invoice id, adding them up" testRefundRecorded
  it "refuses an unknown reference, an order without a balance and another amount, and answers the usage to a malformed command" testRefundRefusals
  it "refuses a balance held by an open order, naming it and its expiry, and records it once that order's invoice is cancelled" testRefundOpenHolder
  it "refuses a balance held by an overdue order, naming its expiry, and records it once the sweep closed that order" testRefundOverdueHolder
  it "refuses a balance held by an order whose invoice holds money past its expiry" testRefundFundedHolder
  it "lists every order of a chain of three that the balance came through" testRefundChain
  it "stops the chain of a late-paid difference order at itself, its cents marked late" testRefundLateChain
  it "is not a group command" $ \_ -> groupCmdAction GROwner "/refund s 10000" `shouldBe` IgnoreMsg

refundRaw :: HasCallStack => ChatController -> Text -> IO (Either Text Text)
refundRaw cc args = operatorCmd cc ("//refund " <> args)

minuteText :: UTCTime -> Text
minuteText = T.pack . formatTime defaultTimeLocale "%Y-%m-%d %H:%M UTC"

testRefundRecorded :: HasCallStack => TestParams -> IO ()
testRefundRecorded ps = withStoreService ps $ \_ _ cc -> do
  let st = chatStore cc
      paidBy = "order s: $100.00 by btc on invoice inv-s"
  Balances.source st "s" 10000
  refundRaw cc "s" `shouldReturn` Right ("Order s: balance $100.00 (credited $100.00, refunded $0.00). Paid by: " <> paidBy <> ". To record its refund: //refund s 10000")
  logs <- capturingStderr $ refundRaw cc "inv-s 10000" `shouldReturn` Right ("Recorded a refund of $100.00 for order s. Refund it now, by: " <> paidBy <> ".")
  logs `shouldSatisfy` T.isInfixOf "names: recorded a refund of 10000 cents for order s"
  Balances.figures st "s" t0 `shouldReturn` Just (0, 10000, 10000, Nothing)
  refundRaw cc "inv-s" `shouldReturn` Right ("Order s: balance $0.00 (credited $100.00, refunded $100.00). Paid by: " <> paidBy <> ". Nothing to refund.")
  Balances.credit st "s" 2505
  refundRaw cc "s 2505" `shouldReturn` Right ("Recorded a refund of $25.05 for order s. Refund it now, by: " <> paidBy <> ".")
  Balances.figures st "s" t0 `shouldReturn` Just (0, 12505, 12505, Nothing)

testRefundRefusals :: HasCallStack => TestParams -> IO ()
testRefundRefusals ps = withStoreService ps $ \_ _ cc -> do
  let st = chatStore cc
  Balances.placeOrderRow st "plain" False 20000 t0
  Balances.source st "s" 10000
  refundRaw cc "nothing" `shouldReturn` Left "No order has the reference nothing."
  refundRaw cc "nothing 10000" `shouldReturn` Left "Nothing recorded: no order has the reference nothing."
  refundRaw cc "inv-plain" `shouldReturn` Left "Order plain has no balance."
  refundRaw cc "inv-plain 20000" `shouldReturn` Left "Nothing recorded: the order of inv-plain has no balance."
  refundRaw cc "s 9000" `shouldReturn` Left "Nothing recorded: the balance is $100.00 now, not $90.00."
  refundRaw cc "s 10001" `shouldReturn` Left "Nothing recorded: the balance is $100.00 now, not $100.01."
  forM_ ["", "s 0", "s -5", "s 100.00", "s 10000 junk", "s 99999999999999999999"] $ \args ->
    (args,) <$> refundRaw cc args `shouldReturn` (args, Left badgeCmdUsage)
  Balances.figures st "s" t0 `shouldReturn` Just (10000, 10000, 0, Nothing)

-- | The source s with a balance of $100.00 and the order h holding it, whose invoice expires at the given time.
heldSource :: ChatController -> UTCTime -> IO ()
heldSource cc expiry = do
  Balances.source (chatStore cc) "s" 10000
  Balances.holder (chatStore cc) "h" "s" 10000 expiry `shouldReturn` Right ()

heldRefusals :: HasCallStack => ChatController -> Text -> IO ()
heldRefusals cc held = do
  refundRaw cc "s" `shouldReturn` Right ("Order s: balance $100.00 (credited $100.00, refunded $0.00). Paid by: order s: $100.00 by btc on invoice inv-s. It cannot be refunded now: " <> held <> ".")
  refundRaw cc "inv-s 10000" `shouldReturn` Left ("Nothing recorded: " <> held <> ".")
  Balances.figures (chatStore cc) "s" t0 >>= (`shouldSatisfy` \case Just (10000, 10000, 0, Just _) -> True; _ -> False)

recordedS :: Either Text Text
recordedS = Right "Recorded a refund of $100.00 for order s. Refund it now, by: order s: $100.00 by btc on invoice inv-s."

testRefundOpenHolder :: HasCallStack => TestParams -> IO ()
testRefundOpenHolder ps = withStoreService ps $ \_ _ cc -> do
  now <- getCurrentTime
  let expiry = addUTCTime 3600 now
  heldSource cc expiry
  heldRefusals cc $ "held by order h, its invoice open until " <> minuteText expiry <> "; run the command again then"
  waiters <- newWaiters
  cancelOpenInvoice (chatStore cc) waiters Nothing (InvoiceId "inv-h") now `shouldReturn` True
  refundRaw cc "s 10000" `shouldReturn` recordedS

testRefundOverdueHolder :: HasCallStack => TestParams -> IO ()
testRefundOverdueHolder ps = withStoreService ps $ \_ _ cc -> do
  now <- getCurrentTime
  let expiry = addUTCTime (-120) now
  heldSource cc expiry
  heldRefusals cc $ "held by order h, its invoice expired at " <> minuteText expiry <> "; start the service so the poller closes that order, 10 minutes after the expiry at the earliest, then stop it and run //refund again"
  waiters <- newWaiters
  expireOverdue (chatStore cc) waiters Nothing now now [InvoiceId "inv-h"] `shouldReturn` [InvoiceId "inv-h"]
  refundRaw cc "s 10000" `shouldReturn` recordedS

testRefundFundedHolder :: HasCallStack => TestParams -> IO ()
testRefundFundedHolder ps = withStoreService ps $ \_ _ cc -> do
  now <- getCurrentTime
  let expiry = addUTCTime (-120) now
  heldSource cc expiry
  Balances.payment (chatStore cc) "inv-h" (Balances.cardPaid 100 0) now
  heldRefusals cc $ "held by order h, whose invoice expired at " <> minuteText expiry <> " holding money; it holds until that payment settles or the invoice closes; run the command again then"

testRefundChain :: HasCallStack => TestParams -> IO ()
testRefundChain ps = withStoreService ps $ \_ _ cc -> do
  let st = chatStore cc
  Balances.source st "a" 20000
  Balances.holder st "b" "a" 20000 t0 `shouldReturn` Right ()
  b <- reg st "b"
  withTransaction st (`Balances.settleHolder` b) `shouldReturn` True
  taken st "b"
  Balances.credit st "b" 40000
  Balances.coveredSpend st "c" "b" 10000 40000 `shouldReturn` Right ()
  taken st "c"
  Balances.credit st "c" 10000
  refundRaw cc "c"
    `shouldReturn` Right
      ( "Order c: balance $100.00 (credited $100.00, refunded $0.00). Paid by: order c: $100.00 from the balance of order b; "
          <> "order b: $200.00 by btc on invoice inv-b, $200.00 from the balance of order a; order a: $200.00 by btc on invoice inv-a. "
          <> "To record its refund: //refund c 10000"
      )
  where
    taken :: DBStore -> Text -> IO ()
    taken st r = do
      queued <- reg st r
      write st queued {nrPhase = PTaken} `shouldReturn` Written (nrVersion queued + 1)

testRefundLateChain :: HasCallStack => TestParams -> IO ()
testRefundLateChain ps = withStoreService ps $ \_ _ cc -> do
  let st = chatStore cc
      paidBy = "order d: $24.00 late by btc on invoice inv-d"
  Balances.source st "s" 15000
  Balances.holder st "d" "s" 15000 t0 `shouldReturn` Right ()
  waiters <- newWaiters
  cancelOpenInvoice st waiters Nothing (InvoiceId "inv-d") t0 `shouldReturn` True
  Balances.creditLate st (Balances.lateCredit "d" 1 (Just 40000) 2400) `shouldReturn` True
  refundRaw cc "inv-d" `shouldReturn` Right ("Order d: balance $24.00 (credited $24.00, refunded $0.00). Paid by: " <> paidBy <> ". To record its refund: //refund inv-d 2400")
  refundRaw cc "inv-d 2400" `shouldReturn` Right ("Recorded a refund of $24.00 for order d. Refund it now, by: " <> paidBy <> ".")
  Balances.figures st "s" t0 `shouldReturn` Just (15000, 15000, 0, Nothing)

data TurnHolder a = TurnHolder {thAction :: Async a, thRelease :: MVar ()}

-- | Runs the action until its body is inside the turn, where the body waits for the release.
holdTurn :: (IO () -> IO a) -> IO (TurnHolder a)
holdTurn inTurn = do
  entered <- newEmptyMVar
  thRelease <- newEmptyMVar
  thAction <- async $ inTurn $ putMVar entered () >> takeMVar thRelease
  takeMVar entered
  pure TurnHolder {thAction, thRelease}

releaseTurn :: TurnHolder a -> IO ()
releaseTurn TurnHolder {thRelease} = putMVar thRelease ()

finished :: Async a -> IO a
finished a = timeout 5000000 (wait a) >>= maybe (fail "did not finish") pure

issueNameRaw :: HasCallStack => ChatController -> String -> IO NamePrepaidCode
issueNameRaw cc args =
  sendChatCmdStr cc ("//issue " <> args) >>= \case
    Right (CRCustomChatResponse _ response) -> prepaidIn (T.stripPrefix "Code: " response)
    r -> error $ "issue failed: " <> show (() <$ r)

prepaidIn :: HasCallStack => Maybe Text -> IO NamePrepaidCode
prepaidIn t = case parseNameCode =<< t of
  Just (NCPrepaid code) -> pure code
  _ -> error $ "no prepaid name code in: " <> show t

prepaidTerms :: ChatController -> NamePrepaidCode -> IO (Maybe (Int, Int, BadgeCodePaymentStatus))
prepaidTerms cc code =
  fmap (\PrepaidCode {pcMinLength, pcYears, pcPaymentStatus} -> (pcMinLength, pcYears, pcPaymentStatus))
    <$> withTransaction (chatStore cc) (`getPrepaidCode` namePrepaidCodeHash code)

checkPrepaid :: Int -> Text -> IO (Int, Maybe J.Value)
checkPrepaid port code = do
  mgr <- newManager defaultManagerSettings
  req <- parseRequest $ "http://127.0.0.1:" <> show port <> "/api/code/check"
  r <- httpLbs req {method = "POST", requestBody = RequestBodyLBS (J.encode (J.object ["code" J..= code]))} mgr
  pure (statusCode (responseStatus r), J.decode (responseBody r))

groupName :: String
groupName = "testbadges"

groupDescr :: Text
groupDescr = "badge ops desk"

testGroupConfig :: GroupConfig
testGroupConfig = GroupConfig {gDisplayName = T.pack groupName, gDescription = Just groupDescr}

revokeReply :: Text -> Text -> Text
revokeReply code outcome = code <> ": " <> outcome

revokeCmd :: Text -> String
revokeCmd code = "/revoke " <> T.unpack code

revokeInGroup :: HasCallStack => ChatController -> TestCC -> Text -> Text -> IO ()
revokeInGroup cc member code outcome = do
  sendGroupCmd member (revokeCmd code)
  waitStoredItem cc (revokeReply code outcome)
  awaitOnConsole member (revokeReply code outcome)

-- The name is quoted because console output quotes a name with a space.
botName :: String
botName = "'" <> T.unpack badgeBotName <> "'"

data GroupSvc = GroupSvc
  { gsCfg :: ChatConfig,
    gsKey :: BadgeIssuerKey,
    gsStaticDir :: FilePath,
    gsIniSections :: [Text],
    gsPs :: TestParams
  }

prepareGroupService :: HasCallStack => TestParams -> IO GroupSvc
prepareGroupService ps@TestParams {tmpPath} =
  bbsKeyGen >>= \case
    Left e -> error $ "bbsKeyGen failed: " <> e
    Right (pk, sk) -> do
      let gsKey = BadgeIssuerKey {keyIdx = testIssuerKeyIdx, secretKey = sk}
          gsCfg = testCfg {badgePublicKeys = M.singleton testIssuerKeyIdx pk}
          gsStaticDir = tmpPath </> "badge_group_static"
      createDirectoryIfMissing True gsStaticDir
      withNewTestChatCfg ps gsCfg serviceDbPrefix badgeProfile $ \_ -> pure ()
      pure GroupSvc {gsCfg, gsKey, gsStaticDir, gsIniSections = [], gsPs = ps}

runGroupService :: HasCallStack => GroupSvc -> (ChatController -> ServiceState -> Int64 -> IO a) -> IO a
runGroupService svc = runGroupServiceAs svc (Just testGroupConfig)

runGroupServiceAs :: HasCallStack => GroupSvc -> Maybe GroupConfig -> (ChatController -> ServiceState -> Int64 -> IO a) -> IO a
runGroupServiceAs svc groupCfg action = do
  (t, cc, env, port) <- startGroupService svc groupCfg
  -- The web listener starts after group start-up, so the stop cannot land inside start-up's database calls.
  let started = pollUntilTrue (listening port) >> pollUntil (getStoredGroup cc)
      -- A stop inside a lane's database call leaves a statement open, and closing the store then fails.
      settle gid = when (isJust groupCfg) $ do
        status <- memberStatus cc gid badgeBotName
        forM_ status $ \s -> unless (s `elem` ["removed", "left", "deleted"]) $ settleLane cc env gid
  (started >>= \ManagedGroup {mgGroupId} -> action cc env mgGroupId <* settle mgGroupId) `finally` stopGroupService t cc

-- It returns once chat has started, while group start-up may still run; the Int is its web listener's port.
startGroupService :: HasCallStack => GroupSvc -> Maybe GroupConfig -> IO (Async (), ChatController, ServiceState, Int)
startGroupService svc@GroupSvc {gsCfg} groupCfg = do
  (opts, port) <- groupServiceOpts svc groupCfg
  env <- newServiceState
  t <- async $ storeService opts gsCfg env
  -- A service that ends before it is ready fails the test with its own error, not the timeout.
  cc <-
    timeout waitLimit (atomically $ (Right <$> readTMVar (serviceCC env)) `orElse` (Left <$> waitCatchSTM t)) >>= \case
      Just (Right cc) -> pure cc
      Just (Left ended) -> either throwIO (\_ -> error "store service ended before it started") ended
      Nothing -> cancel t >> error "store service did not start"
  pure (t, cc, env, port)

-- Cancelling ends the service lanes while chat still runs; stopping chat first would leave them running.
stopGroupService :: Async () -> ChatController -> IO ()
stopGroupService t cc = do
  -- cancel discards how the service ended, so a crash is read first and rethrown after cleanup.
  ended <- poll t
  cancel t
  stopChat cc
  forM_ ended $ either throwIO pure

stopChat :: ChatController -> IO ()
stopChat cc = stopStoreService cc >> disposeAgentClient (smpAgent cc)

-- It writes the service's ini on a free port; Nothing omits the [group] section.
groupServiceOpts :: GroupSvc -> Maybe GroupConfig -> IO (StoreServiceOpts, Int)
groupServiceOpts GroupSvc {gsKey = BadgeIssuerKey {secretKey}, gsStaticDir, gsIniSections, gsPs = ps@TestParams {tmpPath}} groupCfg = do
  (port, sock) <- openFreePort
  close sock
  let iniPath = tmpPath </> "badge_group.ini"
  writeFile iniPath $
    unlines $
      [ "[listener]",
        "host = 127.0.0.1",
        "port = " <> show port,
        "static_dir = " <> gsStaticDir
      ]
        <> maybe [] groupSection groupCfg
        <> map T.unpack gsIniSections
  pure ((mkStoreServiceOpts ps secretKey) {serviceConfigFile = Just iniPath, noAddress = True}, port)
  where
    groupSection GroupConfig {gDisplayName, gDescription} =
      ["", "[group]", "display_name = " <> T.unpack gDisplayName]
        <> maybe [] (\d -> ["description = " <> T.unpack d]) gDescription

withGroupOwner :: HasCallStack => TestParams -> (BadgeIssuerKey -> ChatController -> ServiceState -> TestCC -> IO ()) -> IO ()
withGroupOwner ps action = do
  svc@GroupSvc {gsKey} <- prepareGroupService ps
  runWithOwner svc $ \cc env _ -> action gsKey cc env

runWithOwner :: HasCallStack => GroupSvc -> (ChatController -> ServiceState -> Int64 -> TestCC -> IO a) -> IO a
runWithOwner svc@GroupSvc {gsPs} action = runGroupService svc $ \cc env gid -> withOwnerJoined gsPs cc gid $ action cc env gid

withOwnerJoined :: HasCallStack => TestParams -> ChatController -> Int64 -> (TestCC -> IO a) -> IO a
withOwnerJoined ps cc gid action =
  withNewTestChat ps "alice" aliceProfile $ \alice -> do
    joinGroup cc alice
    waitMemberRole cc gid "alice" "owner"
    drainUntil alice ["#" <> groupName <> ": " <> botName <> " changed your role from member to owner"]
    r <- action alice
    drainConsole alice
    pure r

-- Bob joins after the first member and is connected to her.
withMemberJoined :: HasCallStack => TestParams -> ChatController -> TestCC -> (TestCC -> IO a) -> IO a
withMemberJoined ps cc alice action =
  withNewTestChat ps "bob" bobProfile $ \bob -> do
    joinGroup cc bob
    drainUntil alice ["#" <> groupName <> ": new member bob is connected"]
    r <- action bob
    drainConsole bob
    pure r

joinGroup :: HasCallStack => ChatController -> TestCC -> IO ()
joinGroup cc member = groupLink cc >>= send member . ("/c " <>)

sendGroupCmd :: TestCC -> String -> IO ()
sendGroupCmd member cmd = send member ("#" <> groupName <> " " <> cmd)

issueTracked :: HasCallStack => ChatController -> TestCC -> String -> Int -> IO (Int64, Text, Text)
issueTracked cc member badgeType uses = do
  sendGroupCmd member ("/issue " <> badgeType <> " uses " <> show uses)
  (citemId, body) <- waitTrackerItemOf cc uses
  awaitTrackerOnConsole member body
  pure (citemId, body, extractCode body)

testGroupCreateReuse :: HasCallStack => TestParams -> IO ()
testGroupCreateReuse ps = do
  svc <- prepareGroupService ps
  runGroupService svc $ \cc _ gid -> do
    ManagedGroup {mgGroupLink} <- storedGroup cc
    shortLinkContactType mgGroupLink `shouldBe` Right CCTGroup
    groupCount cc `shouldReturn` 1
    advertisedCommands cc gid `shouldReturn` Just expectedCommands
    profileDescription cc gid `shouldReturn` Just groupDescr
    profileFullNameShortDescr cc gid `shouldReturn` ("", Nothing)
    groupFeaturePreference cc gid SGFHistory `shouldReturn` HistoryGroupPreference {enable = FEOff}
    -- The first run never rewrites the profile, so only the restart can restore what is blanked here.
    clearCommandsSettingFullDelete cc gid
  advertisedAt <- runGroupService svc $ \cc _ gid -> do
    -- Commands are advertised as the group is resolved, so once they are back this run has found the stored group.
    waitAdvertisedCommands cc gid
    groupCount cc `shouldReturn` 1
    profileDisplayName cc gid `shouldReturn` T.pack groupName
    profileDescription cc gid `shouldReturn` Just groupDescr
    profileFullNameShortDescr cc gid `shouldReturn` ("", Nothing)
    groupFeaturePreference cc gid SGFFullDelete `shouldReturn` FullDeleteGroupPreference {enable = FEOn, role = Nothing}
    profileUpdatedAt cc gid
  -- The lane drains only after the group is resolved, so the promotion proves the start-up sync has run.
  runWithOwner svc $ \cc _ gid _ -> profileUpdatedAt cc gid `shouldReturn` advertisedAt

testConfigNotApplied :: HasCallStack => TestParams -> IO ()
testConfigNotApplied ps = do
  svc <- prepareGroupService ps
  -- Clearing the commands makes the restart rewrite the profile, the only path that could apply the new config.
  runGroupService svc $ \cc _ gid -> clearCommandsSettingFullDelete cc gid
  runGroupServiceAs svc (Just GroupConfig {gDisplayName = "renamedbadges", gDescription = Just "renamed desk"}) $ \cc env gid -> do
    waitAdvertisedCommands cc gid
    awaitLane cc env
    groupCount cc `shouldReturn` 1
    profileDisplayName cc gid `shouldReturn` T.pack groupName
    profileDescription cc gid `shouldReturn` Just groupDescr

testMissedOwnerPromotedOnStart :: HasCallStack => TestParams -> IO ()
testMissedOwnerPromotedOnStart ps = do
  svc <- prepareGroupService ps
  -- A demoted owner with the flag cleared looks like a join the lane never handled.
  withTwoMembersOwnerUnset svc $ \cc gid alice bob -> setMemberRoleSeen cc gid [alice, bob] "alice" "member"
  runGroupService svc $ \cc env gid -> do
    waitMemberRole cc gid "alice" "owner"
    awaitLane cc env
    memberRoles cc gid `shouldReturn` [("alice", "owner"), ("bob", "member")]

testEarliestPromotedOnLaterJoin :: HasCallStack => TestParams -> IO ()
testEarliestPromotedOnLaterJoin ps = do
  svc <- prepareGroupService ps
  runWithOwner svc $ \cc env gid alice -> do
    setMemberRole cc gid "alice" "member"
    executeSql cc "UPDATE sx_badge_service_group SET owner_bootstrapped = 0"
    withMemberJoined ps cc alice $ \bob -> do
      waitMemberRole cc gid "alice" "owner"
      awaitLaneOnConsoles cc env [alice, bob]
      memberRoles cc gid `shouldReturn` [("alice", "owner"), ("bob", "member")]

testJoiningMemberSkippedOnStart :: HasCallStack => TestParams -> IO ()
testJoiningMemberSkippedOnStart ps = do
  svc <- prepareGroupService ps
  withTwoMembersOwnerUnset svc $ \cc gid alice bob -> do
    setMemberRoleSeen cc gid [alice, bob] "alice" "member"
    withTransaction (chatStore cc) $ \db ->
      DB.execute db "UPDATE group_members SET member_status = 'accepted' WHERE group_id = ? AND local_display_name = 'alice'" (Only gid)
  runGroupService svc $ \cc env gid -> do
    waitMemberRole cc gid "bob" "owner"
    awaitLane cc env
    memberRoles cc gid `shouldReturn` [("alice", "member"), ("bob", "owner")]

testHandMadeOwnerKeptOnStart :: HasCallStack => TestParams -> IO ()
testHandMadeOwnerKeptOnStart ps = do
  svc <- prepareGroupService ps
  withTwoMembersOwnerUnset svc $ \cc gid alice bob -> do
    setMemberRoleSeen cc gid [alice, bob] "bob" "owner"
    setMemberRoleSeen cc gid [alice, bob] "alice" "member"
  runGroupService svc $ \cc env gid -> do
    awaitLane cc env
    memberRoles cc gid `shouldReturn` [("alice", "member"), ("bob", "owner")]
    fmap mgOwnerBootstrapped <$> getStoredGroup cc `shouldReturn` Just True

testDeletedGroupNotServed :: HasCallStack => TestParams -> IO ()
testDeletedGroupNotServed ps = do
  svc <- prepareGroupService ps
  runWithOwner svc $ \cc _ gid alice -> do
    drainConsole alice
    alice ##> ("/d #" <> groupName)
    alice <## ("#" <> groupName <> ": you deleted the group (signed)")
    pollUntilTrue $ (== Just "deleted") <$> memberStatus cc gid badgeBotName
  runGroupService svc $ \cc _ _ ->
    ensureManagedGroup cc testGroupConfig `shouldReturn` Nothing

testOwnDeletedGroupNotServed :: HasCallStack => TestParams -> IO ()
testOwnDeletedGroupNotServed ps = do
  svc <- prepareGroupService ps
  runGroupService svc $ \cc env gid -> do
    -- The harness cannot settle a deleted group, so the lane must be idle before the delete.
    settleLane cc env gid
    sendChatCmdStr cc ("/d #" <> groupName) >>= (`shouldSatisfy` isRight)
  runGroupService svc $ \cc _ _ ->
    ensureManagedGroup cc testGroupConfig `shouldReturn` Nothing

testReplacedGroupDropsTrackers :: HasCallStack => TestParams -> IO ()
testReplacedGroupDropsTrackers ps = do
  svc <- prepareGroupService ps
  runWithOwner svc $ \cc _ _ alice -> do
    void $ issueTracked cc alice "supporter" 3
    executeSql cc "DELETE FROM sx_badge_service_group"
  runGroupService svc $ \cc _ _ ->
    countRows cc "sx_badge_service_badge_codes WHERE group_item_id IS NOT NULL OR group_item_sent_at IS NOT NULL" `shouldReturn` 0

testGroupLookupFailureStops :: HasCallStack => TestParams -> IO ()
testGroupLookupFailureStops ps = do
  svc@GroupSvc {gsCfg} <- prepareGroupService ps
  runGroupService svc $ \cc _ _ ->
    executeSql cc "ALTER TABLE sx_badge_service_group RENAME TO sx_badge_service_group_hidden"
  (opts, _) <- groupServiceOpts svc (Just testGroupConfig)
  env <- newServiceState
  -- Falling through to create would keep the service running, so the timeout would fire instead.
  ended <- timeout waitLimit (try $ storeService opts gsCfg env)
  atomically (tryReadTMVar (serviceCC env)) >>= mapM_ stopChat
  ended `shouldBe` Just (Left (ExitFailure 1))

-- The web listener is one of the service's lanes and only the cancel precedes the check,
-- so its port closing shows the core ended the lanes.
testCancelStopsLanes :: HasCallStack => TestParams -> IO ()
testCancelStopsLanes ps = do
  svc <- prepareGroupService ps
  (t, cc, _, port) <- startGroupService svc (Just testGroupConfig)
  (pollUntilTrue (listening port) >> cancel t >> pollUntilTrue (not <$> listening port)) `finally` (cancel t >> stopChat cc)

listening :: Int -> IO Bool
listening port =
  bracket (socket AF_INET Stream defaultProtocol) close $ \s ->
    (True <$ connect s (SockAddrInet (fromIntegral port) (tupleToHostAddress (127, 0, 0, 1)))) `catch` \(_ :: IOException) -> pure False

testLeftOwnerReplacedOnStart :: HasCallStack => TestParams -> IO ()
testLeftOwnerReplacedOnStart ps = do
  svc <- prepareGroupService ps
  withTwoMembersOwnerUnset svc $ \cc gid alice bob -> do
    setMemberRoleSeen cc gid [alice, bob] "bob" "owner"
    setMemberRoleSeen cc gid [alice, bob] "alice" "member"
    send bob ("/l " <> groupName)
    pollUntilTrue $ (== Just "left") <$> memberStatus cc gid "bob"
    drainUntil bob ["#" <> groupName <> ": you left the group"]
    drainUntil alice ["#" <> groupName <> ": bob left the group"]
  runGroupService svc $ \cc _ gid -> waitMemberRole cc gid "alice" "owner"

testSetUpOwnerNotReplacedOnStart :: HasCallStack => TestParams -> IO ()
testSetUpOwnerNotReplacedOnStart ps = do
  svc <- prepareGroupService ps
  runWithOwner svc $ \cc _ gid alice -> setMemberRoleSeen cc gid [alice] "alice" "member"
  runGroupService svc $ \cc env gid -> do
    awaitLane cc env
    memberRoles cc gid `shouldReturn` [("alice", "member")]

memberStatus :: ChatController -> Int64 -> Text -> IO (Maybe Text)
memberStatus cc gid name =
  queryFirst cc $
    "SELECT member_status FROM group_members WHERE group_id = "
      <> show gid
      <> " AND local_display_name = '"
      <> T.unpack name
      <> "'"

withTwoMembersOwnerUnset :: HasCallStack => GroupSvc -> (ChatController -> Int64 -> TestCC -> TestCC -> IO ()) -> IO ()
withTwoMembersOwnerUnset svc@GroupSvc {gsPs = ps} action =
  runWithOwner svc $ \cc env gid alice ->
    withMemberJoined ps cc alice $ \bob -> do
      -- The lane must handle bob's join before the flag is cleared, or it would act on the cleared flag in this run.
      awaitLaneOnConsoles cc env [alice, bob]
      action cc gid alice bob
      executeSql cc "UPDATE sx_badge_service_group SET owner_bootstrapped = 0"

testFirstJoinerPromoted :: HasCallStack => TestParams -> IO ()
testFirstJoinerPromoted ps = do
  svc <- prepareGroupService ps
  runWithOwner svc $ \cc _ gid alice ->
    withMemberJoined ps cc alice $ \bob -> do
      waitMemberRole cc gid "bob" "member"
      -- The bot queues bob's join before it introduces him, so alice's code proves the join was handled.
      sendGroupCmd alice "/issue supporter"
      waitCodeOfType cc "supporter"
      memberRoles cc gid `shouldReturn` [("alice", "owner"), ("bob", "member")]
      let codeReply = "#" <> groupName <> " " <> botName <> "> Code: SB-"
      drainUntil alice [codeReply]
      drainUntil bob [codeReply, "#" <> groupName <> ": member alice"]

testFailedPromotionNotRetried :: HasCallStack => TestParams -> IO ()
testFailedPromotionNotRetried ps = do
  svc <- prepareGroupService ps
  runGroupService svc $ \cc env gid -> do
    -- An admin cannot make an owner, so the core refuses the promotion.
    setBotRole cc "admin"
    withNewTestChat ps "alice" aliceProfile $ \alice -> do
      joinGroup cc alice
      pollUntilTrue $ mgOwnerBootstrapped <$> storedGroup cc
      awaitLaneOnConsoles cc env [alice]
      setBotRole cc "owner"
      withMemberJoined ps cc alice $ \bob -> awaitLaneOnConsoles cc env [alice, bob]
      memberRoles cc gid `shouldReturn` [("alice", "member"), ("bob", "member")]
      drainConsole alice

testRoleGatedIssue :: HasCallStack => TestParams -> IO ()
testRoleGatedIssue ps = do
  svc <- prepareGroupService ps
  runWithOwner svc $ \cc _ gid alice ->
    withNewTestChat ps "bob" bobProfile $ \bob ->
      withNewTestChat ps "cath" cathProfile $ \cath -> do
        let announced name = "#" <> groupName <> ": " <> botName <> " added " <> name
            preMember name = "#" <> groupName <> ": member " <> name
            newMember name = "#" <> groupName <> ": new member " <> name <> " is connected"
            usageReply = "#" <> groupName <> " " <> botName <> "> Usage: /issue <type>"
        joinGroup cc bob
        waitMemberRole cc gid "bob" "member"
        -- cath joins only after alice sees bob announced, or cath's console shows a different line.
        drainUntil alice [announced "bob"]
        joinGroup cc cath
        waitMemberRole cc gid "cath" "member"
        drainUntil cath [preMember "alice", preMember "bob"]
        drainConsole cath
        sendGroupCmd alice "/issue supporter"
        getInAnyOrder
          dropTime
          cath
          [ StartsWith ("#" <> groupName <> " alice> /issue supporter"),
            StartsWith ("#" <> groupName <> " " <> botName <> "> Code: SB-")
          ]
        codeCount cc `shouldReturn` 1
        sendGroupCmd bob "/issue legend"
        waitStoredItem cc "/issue legend"
        ownerIssuesNext cc alice 2
        sendGroupCmd alice "/issue supporter uses 0"
        waitStoredItem cc "Usage: /issue <type> [months <M>] [uses <N>], or name <length> [years <Y>]"
        codeCount cc `shouldReturn` 2
        drainUntil alice [usageReply, newMember "bob", newMember "cath"]
        drainUntil bob [usageReply, preMember "alice", newMember "cath"]
        drainUntil cath [usageReply]
        drainConsole bob
        drainConsole cath

testBlockedMemberCommandIgnored :: HasCallStack => TestParams -> IO ()
testBlockedMemberCommandIgnored ps = do
  svc <- prepareGroupService ps
  runWithOwner svc $ \cc _ gid alice ->
    withMemberJoined ps cc alice $ \bob -> do
      waitMemberRole cc gid "bob" "member"
      -- bob is made moderator so that only the block can refuse his command.
      send alice ("/mr #" <> groupName <> " bob moderator")
      waitMemberRole cc gid "bob" "moderator"
      -- The block and the command travel over different connections, so the block is awaited first.
      send alice ("/block for all #" <> groupName <> " bob")
      waitMemberBlocked cc gid "bob"
      sendGroupCmd bob "/bulk legend months 255 count 100"
      (deleted, content) <- waitBlockedItem cc "/bulk legend months 255 count 100"
      deleted `shouldBe` blockedByAdminMark
      content `shouldSatisfy` T.isInfixOf "/bulk legend months 255 count 100"
      ownerIssuesNext cc alice 1
      awaitOnConsole alice "Code: SB-"
      awaitOnConsole bob "Code: SB-"

-- The lane is a single FIFO drainer, so the owner's next code proves every earlier command was handled.
ownerIssuesNext :: HasCallStack => ChatController -> TestCC -> Int -> IO ()
ownerIssuesNext cc owner total = do
  sendGroupCmd owner "/issue investor"
  waitCodeOfType cc "investor"
  codeCount cc `shouldReturn` total

-- This is the item_deleted value the core writes for a message from a member blocked for all.
blockedByAdminMark :: Int
blockedByAdminMark = 3

testSupportScopeIgnored :: HasCallStack => TestParams -> IO ()
testSupportScopeIgnored ps =
  withGroupOwner ps $ \_ cc _ alice -> do
    send alice "/_send #1(_support) text /issue legend"
    waitStoredItem cc "/issue legend"
    ownerIssuesNext cc alice 1
    awaitOnConsole alice "Code: SB-"

testLiveMessageIgnored :: HasCallStack => TestParams -> IO ()
testLiveMessageIgnored ps =
  withGroupOwner ps $ \_ cc _ alice -> do
    send alice ("/live #" <> groupName <> " /issue legend")
    waitStoredItem cc "/issue legend"
    ownerIssuesNext cc alice 1
    awaitOnConsole alice "Code: SB-"

testOtherGroupEventIgnored :: HasCallStack => TestParams -> IO ()
testOtherGroupEventIgnored ps = do
  svc <- prepareGroupService ps
  runGroupService svc $ \cc env gid -> do
    atomically $ writeTQueue (groupEventQ env) (GEInGroup (gid + 1) (GACommand GROwner "/issue legend"))
    awaitLane cc env
    queryColumn cc "SELECT badge_type FROM sx_badge_service_badge_codes" `shouldReturn` ["supporter" :: Text]

testBulkIssue :: HasCallStack => TestParams -> IO ()
testBulkIssue ps =
  withGroupOwner ps $ \gsKey cc env alice -> do
    codes <- map extractCode . T.lines <$> replyTo cc alice "/bulk legend months 2 count 3"
    length codes `shouldBe` 3
    codeCount cc `shouldReturn` 3
    singleUseCodeCount cc `shouldReturn` 3
    freeCodeCount cc `shouldReturn` 3
    forM_ codes $ \code -> do
      r <- redeemViaService gsKey cc env code
      badgeTypeOf r `shouldBe` Just BTLegend
      map (\e -> let (c, m, _) = entryOf e in (c, m)) (fst $ statementOf r) `shouldBe` [(2, 2), (-1, 1)]
    forM_ codes $ \code -> redeemViaService gsKey cc env code >>= (`shouldAnswerError` STECodeUsed)
    countRows cc "sx_badge_service_badge_codes WHERE group_item_id IS NOT NULL" `shouldReturn` 0

testIssueMonths :: HasCallStack => TestParams -> IO ()
testIssueMonths ps =
  withGroupOwner ps $ \_ cc _ alice -> do
    sendGroupCmd alice "/issue legend months 12"
    waitCodeMonths cc "legend" `shouldReturn` 12
    awaitOnConsole alice "Code: SB-"
    freeCodeCount cc `shouldReturn` 1

testBulkPartialFailureListsIssued :: HasCallStack => TestParams -> IO ()
testBulkPartialFailureListsIssued ps =
  withGroupOwner ps $ \gsKey cc env alice -> do
    codeLines <- withCodeTableCapped cc 2 $ do
      (codeLines, summary) <- splitAt 2 . T.lines <$> replyTo cc alice "/bulk supporter count 3"
      summary `shouldBe` ["Issued 2 of 3 codes. The remaining codes could not be issued."]
      pure codeLines
    codeCount cc `shouldReturn` 2
    forM_ codeLines $ redeemOk gsKey cc env . extractCode

testIssueFailureIsOpaque :: HasCallStack => TestParams -> IO ()
testIssueFailureIsOpaque ps =
  withGroupOwner ps $ \_ cc _ alice -> do
    withCodeTableHidden cc $ do
      replyTo cc alice "/issue supporter" `shouldReturn` "The code could not be issued."
      issueRaw cc "supporter" `shouldReturn` Left "The code could not be issued."
    void $ replyTo cc alice "/issue supporter"
    codeCount cc `shouldReturn` 1

testRevokeFailureIsOpaque :: HasCallStack => TestParams -> IO ()
testRevokeFailureIsOpaque ps =
  withGroupOwner ps $ \_ cc _ alice -> do
    code <- extractCode <$> replyTo cc alice "/issue supporter"
    withCodeTableHidden cc $
      replyTo cc alice (revokeCmd code) `shouldReturn` revokeReply code "The code could not be revoked."
    revokeInGroup cc alice code "Revoked."

withCodeTableHidden :: ChatController -> IO a -> IO a
withCodeTableHidden cc action = rename codeTable hidden >> (action `finally` rename hidden codeTable)
  where
    codeTable = "sx_badge_service_badge_codes"
    hidden = codeTable <> "_hidden"
    rename from to = executeSql cc $ "ALTER TABLE " <> from <> " RENAME TO " <> to

-- A test-only trigger fails every insert once the code table holds cap rows.
withCodeTableCapped :: ChatController -> Int -> IO a -> IO a
withCodeTableCapped cc cap action = mapM_ (executeSql cc) createCap >> (action `finally` mapM_ (executeSql cc) dropCap)
  where
#if defined(dbPostgres)
    createCap =
      [ "CREATE FUNCTION sx_badge_service_test_cap() RETURNS trigger AS $$ BEGIN IF (SELECT COUNT(*) FROM sx_badge_service_badge_codes) >= "
          <> show cap
          <> " THEN RAISE EXCEPTION 'code table full'; END IF; RETURN NEW; END; $$ LANGUAGE plpgsql",
        "CREATE TRIGGER sx_badge_service_test_cap BEFORE INSERT ON sx_badge_service_badge_codes FOR EACH ROW EXECUTE FUNCTION sx_badge_service_test_cap()"
      ]
    dropCap = ["DROP FUNCTION sx_badge_service_test_cap() CASCADE"]
#else
    createCap =
      [ "CREATE TRIGGER sx_badge_service_test_cap BEFORE INSERT ON sx_badge_service_badge_codes WHEN (SELECT COUNT(*) FROM sx_badge_service_badge_codes) >= "
          <> show cap
          <> " BEGIN SELECT RAISE(ABORT, 'code table full'); END"
      ]
    dropCap = ["DROP TRIGGER sx_badge_service_test_cap"]
#endif

testMultiUseTracker :: HasCallStack => TestParams -> IO ()
testMultiUseTracker ps =
  withGroupOwner ps $ \gsKey cc env alice -> do
    (trackerItemId, tracker0, code) <- issueTracked cc alice "supporter" 2
    tracker0 `shouldSatisfy` T.isInfixOf "2 of 2 uses remaining"
    redeemOk gsKey cc env code
    t1 <- waitItemText cc trackerItemId "1 of 2 uses remaining"
    t1 `shouldSatisfy` T.isInfixOf "last used"
    redeemOk gsKey cc env code
    usedUp <- waitItemText cc trackerItemId "All 2 uses redeemed"
    sentItemsWithCode cc code `shouldReturn` [usedUp]
    redeemViaService gsKey cc env code >>= (`shouldAnswerError` STECodeUsed)
    awaitTrackerOnConsole alice usedUp

testStaleRefreshIgnored :: HasCallStack => TestParams -> IO ()
testStaleRefreshIgnored ps =
  withGroupOwner ps $ \gsKey cc env alice -> do
    (trackerItemId, _, code) <- issueTracked cc alice "supporter" 2
    redeemOk gsKey cc env code
    redeemOk gsKey cc env code
    usedUp <- waitItemText cc trackerItemId "All 2 uses redeemed"
    replayRefresh cc env code 2
    awaitLaneOnConsoles cc env [alice]
    sentItemsWithCode cc code `shouldReturn` [usedUp]

testCountersKeptApart :: HasCallStack => TestParams -> IO ()
testCountersKeptApart ps =
  withGroupOwner ps $ \gsKey cc env alice -> do
    (firstItemId, _, firstCode) <- issueTracked cc alice "supporter" 5
    replicateM_ 3 (redeemOk gsKey cc env firstCode)
    void $ waitItemText cc firstItemId "2 of 5 uses remaining"
    (secondItemId, _, secondCode) <- issueTracked cc alice "legend" 3
    replicateM_ 3 (redeemOk gsKey cc env secondCode)
    secondUsedUp <- waitItemText cc secondItemId "All 3 uses redeemed"
    readItemText cc firstItemId >>= (`shouldSatisfy` T.isInfixOf "2 of 5 uses remaining")
    awaitTrackerOnConsole alice secondUsedUp

testLanelessRedeemRefreshesTracker :: HasCallStack => TestParams -> IO ()
testLanelessRedeemRefreshesTracker ps =
  withGroupOwner ps $ \gsKey cc _ alice -> do
    offsetCodeIds cc
    (trackerItemId, _, code) <- issueTracked cc alice "supporter" 2
    redeemOkWith gsKey cc Nothing code
    -- The refresh runs before the response, so the counter is already written here.
    t1 <- readItemText cc trackerItemId
    t1 `shouldSatisfy` T.isInfixOf "1 of 2 uses remaining"
    t1 `shouldSatisfy` T.isInfixOf "last used"
    redeemOkWith gsKey cc Nothing code
    usedUp <- readItemText cc trackerItemId
    usedUp `shouldSatisfy` T.isInfixOf "All 2 uses redeemed"
    redeemedTrackerCount cc `shouldReturn` 1
    awaitTrackerOnConsole alice usedUp

testQueuedRequestRefreshesTracker :: HasCallStack => TestParams -> IO ()
testQueuedRequestRefreshesTracker ps =
  withGroupOwner ps $ \_ cc env alice -> do
    (trackerItemId, _, code) <- issueTracked cc alice "supporter" 2
    queueRedemption cc env code
    waitItemText cc trackerItemId "1 of 2 uses remaining" >>= awaitTrackerOnConsole alice

testQueuedRequestWithoutGroupConfig :: HasCallStack => TestParams -> IO ()
testQueuedRequestWithoutGroupConfig ps = do
  svc <- prepareGroupService ps
  (trackerItemId, code) <- runWithOwner svc $ \cc _ _ alice ->
    (\(i, _, c) -> (i, c)) <$> issueTracked cc alice "supporter" 2
  runGroupServiceAs svc Nothing $ \cc env _ -> do
    queueRedemption cc env code
    void $ waitItemText cc trackerItemId "1 of 2 uses remaining"

testTrackerRepost :: HasCallStack => TestParams -> IO ()
testTrackerRepost ps =
  withGroupOwner ps $ \gsKey cc env alice -> do
    (itemId0, tracker0, code) <- issueTracked cc alice "supporter" 2
    backdated <- backdateTracker cc 2
    redeemOk gsKey cc env code
    (_, tracker1) <- waitTrackerRepost cc 2 itemId0
    tracker1 `shouldSatisfy` T.isInfixOf "1 of 2 uses remaining"
    readItemText cc itemId0 `shouldReturn` tracker0
    sentAt <- trackerSentAt cc 2
    sentAt `shouldSatisfy` (> backdated)
    awaitTrackerOnConsole alice tracker1

testRefusedEditReposted :: HasCallStack => TestParams -> IO ()
testRefusedEditReposted ps =
  withGroupOwner ps $ \gsKey cc env alice -> do
    (itemId0, tracker0, code) <- issueTracked cc alice "supporter" 2
    backdateTrackerItem cc itemId0
    redeemOk gsKey cc env code
    (itemId1, tracker1) <- waitTrackerRepost cc 2 itemId0
    tracker1 `shouldSatisfy` T.isInfixOf "1 of 2 uses remaining"
    redeemOk gsKey cc env code
    usedUp <- waitItemText cc itemId1 "All 2 uses redeemed"
    sentItemsWithCode cc code `shouldReturn` [tracker0, usedUp]
    awaitTrackerOnConsole alice usedUp

testFailedRepostDropped :: HasCallStack => TestParams -> IO ()
testFailedRepostDropped ps =
  withGroupOwner ps $ \gsKey cc env alice -> do
    (itemId0, tracker0, code) <- issueTracked cc alice "supporter" 2
    void $ backdateTracker cc 2
    setBotRole cc "observer"
    redeemOk gsKey cc env code
    awaitLane cc env
    sentItemsWithCode cc code `shouldReturn` [tracker0]
    readItemText cc itemId0 `shouldReturn` tracker0
    trackerAnchor cc 2 `shouldReturn` Just itemId0
    setBotRole cc "owner"
    redeemOk gsKey cc env code
    (_, tracker1) <- waitTrackerRepost cc 2 itemId0
    tracker1 `shouldSatisfy` T.isInfixOf "All 2 uses redeemed"
    awaitTrackerOnConsole alice tracker1

testDeletedTrackerNotReposted :: HasCallStack => TestParams -> IO ()
testDeletedTrackerNotReposted ps =
  withGroupOwner ps $ \gsKey cc env alice -> do
    (itemId0, _, code) <- issueTracked cc alice "supporter" 2
    deleteItem cc itemId0
    readItemText cc itemId0 `shouldReturn` ""
    trackerNeverReposted gsKey cc env alice code itemId0 []

testDeletedTrackerNotRepostedOnRevoke :: HasCallStack => TestParams -> IO ()
testDeletedTrackerNotRepostedOnRevoke ps =
  withGroupOwner ps $ \_ cc _ alice -> do
    (itemId0, _, code) <- issueTracked cc alice "supporter" 2
    deleteItem cc itemId0
    -- Past the edit window the revoke goes straight to the repost path and its guard.
    void $ backdateTracker cc 2
    revokeNeverReposts cc alice code []

-- Both claims fall inside the edit window, so each edit fails because the message is gone.
testDeletedTrackerUsedUp :: HasCallStack => TestParams -> IO ()
testDeletedTrackerUsedUp ps =
  withGroupOwner ps $ \gsKey cc env alice -> do
    (itemId0, _, code) <- issueTracked cc alice "supporter" 2
    deleteItem cc itemId0
    replicateM_ 2 $ redeemOk gsKey cc env code
    awaitLaneOnConsoles cc env [alice]
    sentItemsWithCode cc code `shouldReturn` []

testModeratedTrackerNotRepostedOnRevoke :: HasCallStack => TestParams -> IO ()
testModeratedTrackerNotRepostedOnRevoke ps =
  withGroupOwner ps $ \_ cc _ alice -> do
    (itemId0, tracker0, code) <- issueTracked cc alice "supporter" 2
    moderateBotItem alice tracker0
    void $ waitModeratedItem cc itemId0
    revokeNeverReposts cc alice code [tracker0]

testModeratedTrackerNotReposted :: HasCallStack => TestParams -> IO ()
testModeratedTrackerNotReposted ps =
  withGroupOwner ps $ \gsKey cc env alice -> do
    (itemId0, tracker0, code) <- issueTracked cc alice "supporter" 2
    moderateBotItem alice tracker0
    (deleted, byMember, content) <- waitModeratedItem cc itemId0
    deleted `shouldBe` moderatedMark
    byMember `shouldSatisfy` isJust
    content `shouldSatisfy` T.isInfixOf code
    readItemText cc itemId0 `shouldReturn` tracker0
    trackerNeverReposted gsKey cc env alice code itemId0 [tracker0]

-- This is the item_deleted value the core writes for a message a member deleted for everyone.
moderatedMark :: Int
moderatedMark = 1

-- The code must be the 2-use tracked one: two claims, the second past the edit window and using it up,
-- leave the code's bot items as they were.
trackerNeverReposted :: HasCallStack => BadgeIssuerKey -> ChatController -> ServiceState -> TestCC -> Text -> Int64 -> [Text] -> IO ()
trackerNeverReposted key cc env member code itemId0 codeItems = do
  redeemOk key cc env code
  awaitLaneOnConsoles cc env [member]
  sentItemsWithCode cc code `shouldReturn` codeItems
  trackerAnchor cc 2 `shouldReturn` Just itemId0
  void $ backdateTracker cc 2
  redeemOk key cc env code
  awaitLaneOnConsoles cc env [member]
  sentItemsWithCode cc code `shouldReturn` codeItems
  trackerAnchor cc 2 `shouldReturn` Just itemId0

revokeNeverReposts :: HasCallStack => ChatController -> TestCC -> Text -> [Text] -> IO ()
revokeNeverReposts cc member code codeItems = do
  revokeInGroup cc member code "Revoked."
  sentItemsWithCode cc code `shouldReturn` codeItems <> [revokeReply code "Revoked."]
  revokedTrackerCount cc `shouldReturn` 0

testGroupRevoke :: HasCallStack => TestParams -> IO ()
testGroupRevoke ps =
  withGroupOwner ps $ \gsKey cc env alice -> do
    code <- extractCode <$> replyTo cc alice "/issue supporter"
    other <- extractCode <$> replyTo cc alice "/issue legend"
    revokeInGroup cc alice code "Revoked."
    redeemViaService gsKey cc env code >>= (`shouldAnswerError` STECodeInvalid)
    redeemOk gsKey cc env other
    revokeInGroup cc alice code "Already revoked."
    -- The reply follows the tracker step, so the revoke has already passed it here.
    revokedTrackerCount cc `shouldReturn` 0
    unissued <- formatBadgeCode <$> randomBadgeCode (random cc)
    revokeInGroup cc alice unissued "No such code."

testGroupRevokeRepairsTracker :: HasCallStack => TestParams -> IO ()
testGroupRevokeRepairsTracker ps =
  withGroupOwner ps $ \_ cc _ alice -> do
    offsetCodeIds cc
    (trackerItemId, tracker0, code) <- issueTracked cc alice "supporter" 2
    revokeInStore cc 2
    readItemText cc trackerItemId `shouldReturn` tracker0
    sendGroupCmd alice (revokeCmd code)
    retired <- waitItemText cc trackerItemId "Revoked"
    retired `shouldBe` retiredText code
    waitStoredItem cc (revokeReply code "Already revoked.")
    awaitOnConsole alice (revokeReply code "Already revoked.")
    revokedTrackerCount cc `shouldReturn` 1

testRevokeRepeatPastWindow :: HasCallStack => TestParams -> IO ()
testRevokeRepeatPastWindow ps =
  withGroupOwner ps $ \_ cc _ alice -> do
    offsetCodeIds cc
    (itemId0, tracker0, code) <- issueTracked cc alice "supporter" 2
    void $ backdateTracker cc 2
    revokeInGroup cc alice code "Revoked."
    (itemId1, retired) <- waitTrackerRepost cc 2 itemId0
    retired `shouldBe` retiredText code
    revokedTrackerCount cc `shouldReturn` 1
    void $ backdateTracker cc 2
    revokeInGroup cc alice code "Already revoked."
    revokedTrackerCount cc `shouldReturn` 1
    trackerAnchor cc 2 `shouldReturn` Just itemId1
    readItemText cc itemId1 `shouldReturn` retiredText code
    readItemText cc itemId0 `shouldReturn` tracker0

testUsedUpTrackerReconciledOnRestart :: HasCallStack => TestParams -> IO ()
testUsedUpTrackerReconciledOnRestart ps = do
  svc@GroupSvc {gsKey} <- prepareGroupService ps
  (lostItemId, lostCode) <- runWithOwner svc $ \cc env _ alice -> do
    (doneItemId, _, doneCode) <- issueTracked cc alice "supporter" 2
    redeemOk gsKey cc env doneCode
    redeemOk gsKey cc env doneCode
    void $ waitItemText cc doneItemId "All 2 uses redeemed"
    (lostItemId, lost0, lostCode) <- issueTracked cc alice "legend" 3
    replicateM_ 3 (redeemLosingRefresh gsKey cc lostCode)
    readItemText cc lostItemId `shouldReturn` lost0
    trackerAnchor cc 2 `shouldReturn` Just doneItemId
    dateRedemption cc 3 lostRedeemedAt
    pure (lostItemId, lostCode)
  runGroupService svc $ \cc env _ -> do
    corrected <- waitItemText cc lostItemId "All 3 uses redeemed"
    corrected `shouldBe` (lostCode <> "\nAll 3 uses redeemed, last used 2026-02-28 23:50 UTC")
    awaitLane cc env
    sentItemsWithCode cc lostCode `shouldReturn` [corrected]
    redeemedTrackerCount cc `shouldReturn` 2

-- This date is far from any day the test runs on, so a body dated now cannot match it.
lostRedeemedAt :: UTCTime
lostRedeemedAt = UTCTime (fromGregorian 2026 2 28) (23 * 3600 + 50 * 60)

testStalledTrackerReconciledOnRestart :: HasCallStack => TestParams -> IO ()
testStalledTrackerReconciledOnRestart ps = do
  svc@GroupSvc {gsKey} <- prepareGroupService ps
  (stalledItemId, code) <- runWithOwner svc $ \cc env _ alice -> do
    (stalledItemId, _, code) <- issueTracked cc alice "supporter" 5
    redeemOk gsKey cc env code
    redeemOk gsKey cc env code
    awaitLaneOnConsoles cc env [alice]
    stalled <- waitItemText cc stalledItemId "3 of 5 uses remaining"
    redeemLosingRefresh gsKey cc code
    readItemText cc stalledItemId `shouldReturn` stalled
    dateRedemption cc 5 lostRedeemedAt
    pure (stalledItemId, code)
  runGroupService svc $ \cc env _ -> do
    corrected <- waitItemText cc stalledItemId "2 of 5 uses remaining"
    corrected `shouldBe` ("!2 " <> code <> "!\n2 of 5 uses remaining, last used 2026-02-28 23:50 UTC")
    awaitLane cc env
    sentItemsWithCode cc code `shouldReturn` [corrected]

testRevokedTrackerReconciledOnRestart :: HasCallStack => TestParams -> IO ()
testRevokedTrackerReconciledOnRestart ps = do
  svc <- prepareGroupService ps
  (trackerItemId, code) <- runWithOwner svc $ \cc _ _ alice -> do
    (trackerItemId, tracker0, code) <- issueTracked cc alice "supporter" 2
    revokeInStore cc 2
    readItemText cc trackerItemId `shouldReturn` tracker0
    pure (trackerItemId, code)
  runGroupService svc $ \cc env _ -> do
    retired <- waitItemText cc trackerItemId "Revoked"
    retired `shouldBe` retiredText code
    awaitLane cc env
    revokedTrackerCount cc `shouldReturn` 1

testEveryStalledTrackerReconciled :: HasCallStack => TestParams -> IO ()
testEveryStalledTrackerReconciled ps = do
  svc@GroupSvc {gsKey} <- prepareGroupService ps
  (firstItemId, secondItemId) <- runWithOwner svc $ \cc _ _ alice -> do
    (firstItemId, first0, firstCode) <- issueTracked cc alice "supporter" 5
    replicateM_ 2 (redeemLosingRefresh gsKey cc firstCode)
    (secondItemId, second0, secondCode) <- issueTracked cc alice "legend" 3
    redeemLosingRefresh gsKey cc secondCode
    readItemText cc firstItemId `shouldReturn` first0
    readItemText cc secondItemId `shouldReturn` second0
    pure (firstItemId, secondItemId)
  runGroupService svc $ \cc env _ -> do
    first1 <- waitItemText cc firstItemId "3 of 5 uses remaining"
    first1 `shouldSatisfy` T.isInfixOf "last used "
    second1 <- waitItemText cc secondItemId "2 of 3 uses remaining"
    second1 `shouldSatisfy` T.isInfixOf "last used "
    awaitLane cc env
    redeemedTrackerCount cc `shouldReturn` 2

testUneditableTrackerLeftAlone :: HasCallStack => TestParams -> IO ()
testUneditableTrackerLeftAlone ps = do
  svc@GroupSvc {gsKey} <- prepareGroupService ps
  (staleItemId, stale0, staleCode, liveItemId, liveCode) <- runWithOwner svc $ \cc _ _ alice -> do
    (staleItemId, stale0, staleCode) <- issueTracked cc alice "supporter" 2
    redeemLosingRefresh gsKey cc staleCode
    backdateTrackerItem cc staleItemId
    readItemText cc staleItemId `shouldReturn` stale0
    -- The lane drains only after the pass returns, so this code's update marks the pass as done.
    (liveItemId, _, liveCode) <- issueTracked cc alice "legend" 3
    pure (staleItemId, stale0, staleCode, liveItemId, liveCode)
  runGroupService svc $ \cc env _ -> do
    redeemOk gsKey cc env liveCode
    void $ waitItemText cc liveItemId "2 of 3 uses remaining"
    readItemText cc staleItemId `shouldReturn` stale0
    sentItemsWithCode cc staleCode `shouldReturn` [stale0]
    trackerAnchor cc 2 `shouldReturn` Just staleItemId

testRevokeRetiresTracker :: HasCallStack => TestParams -> IO ()
testRevokeRetiresTracker ps =
  withGroupOwner ps $ \gsKey cc env alice -> do
    offsetCodeIds cc
    (trackerItemId, _, code) <- issueTracked cc alice "supporter" 2
    redeemOk gsKey cc env code
    void $ waitItemText cc trackerItemId "1 of 2 uses remaining"
    revokeInGroup cc alice code "Revoked."
    retired <- waitItemText cc trackerItemId "Revoked"
    retired `shouldBe` retiredText code
    revokedTrackerCount cc `shouldReturn` 1
    replayRefresh cc env code 2
    awaitLaneOnConsoles cc env [alice]
    readItemText cc trackerItemId `shouldReturn` retired
    revokedTrackerCount cc `shouldReturn` 1
    redeemViaService gsKey cc env code >>= (`shouldAnswerError` STECodeInvalid)

testServiceRevokeRetiresTracker :: HasCallStack => TestParams -> IO ()
testServiceRevokeRetiresTracker ps =
  withGroupOwner ps $ \gsKey cc env alice -> do
    offsetCodeIds cc
    (trackerItemId, _, code) <- issueTracked cc alice "supporter" 2
    redeemOk gsKey cc env code
    void $ waitItemText cc trackerItemId "1 of 2 uses remaining"
    revokeRaw cc code `shouldReturn` Right "Revoked."
    readItemText cc trackerItemId `shouldReturn` retiredText code
    revokedTrackerCount cc `shouldReturn` 1
    revokeRaw cc code `shouldReturn` Right "Already revoked."
    revokedTrackerCount cc `shouldReturn` 1
    single <- extractCode <$> replyTo cc alice "/issue legend"
    revokeRaw cc single `shouldReturn` Right "Revoked."
    revokedTrackerCount cc `shouldReturn` 1

testServiceRevokeRepairsTracker :: HasCallStack => TestParams -> IO ()
testServiceRevokeRepairsTracker ps =
  withGroupOwner ps $ \_ cc _ alice -> do
    offsetCodeIds cc
    (trackerItemId, tracker0, code) <- issueTracked cc alice "supporter" 2
    revokeInStore cc 2
    readItemText cc trackerItemId `shouldReturn` tracker0
    revokeRaw cc code `shouldReturn` Right "Already revoked."
    readItemText cc trackerItemId `shouldReturn` retiredText code
    revokedTrackerCount cc `shouldReturn` 1
    awaitTrackerOnConsole alice (retiredText code)

-- A discarded code makes later code ids differ from the group id, so swapped arguments fail.
offsetCodeIds :: HasCallStack => ChatController -> IO ()
offsetCodeIds cc = void $ issueCode cc BTSupporter 1

redeemOk :: HasCallStack => BadgeIssuerKey -> ChatController -> ServiceState -> Text -> IO ()
redeemOk key cc env = redeemOkWith key cc (Just (groupEventQ env))

redeemOkWith :: HasCallStack => BadgeIssuerKey -> ChatController -> Maybe (TQueue GroupEvent) -> Text -> IO ()
redeemOkWith key cc trackerQ_ code = do
  r <- redeemWithQueue key cc trackerQ_ code
  credentialOf r `shouldSatisfy` isJust

redeemViaService :: HasCallStack => BadgeIssuerKey -> ChatController -> ServiceState -> Text -> IO StoreServiceResponse
redeemViaService key cc env = redeemWithQueue key cc (Just (groupEventQ env))

-- The reply to the made-up request id fails after the redemption, and the service only logs it.
queueRedemption :: HasCallStack => ChatController -> ServiceState -> Text -> IO ()
queueRedemption cc env codeText = do
  user <- readTVarIO (currentUser cc) >>= maybe (error "no current user") pure
  (purchaseKey, masterKey) <- newPurchaseKeys
  let request = requestObject purchaseKey STCRedeemBadgeCode {masterKey, code = codeText}
  atomically $ writeTQueue (serviceRequestQ env) (user, AgentInvId "test-request", Just purchaseKey, request)

redeemLosingRefresh :: HasCallStack => BadgeIssuerKey -> ChatController -> Text -> IO ()
redeemLosingRefresh key cc codeText = newTQueueIO >>= \q -> redeemOkWith key cc (Just q) codeText

replayRefresh :: HasCallStack => ChatController -> ServiceState -> Text -> Int -> IO ()
replayRefresh cc env codeText uses = case parseBadgeCode codeText of
  Nothing -> error $ "not a badge code: " <> T.unpack codeText
  Just code -> do
    badgeCodeId <- trackedCodeId cc uses
    atomically $ writeTQueue (groupEventQ env) (GETracker badgeCodeId code)

getStoredGroup :: ChatController -> IO (Maybe ManagedGroup)
getStoredGroup cc = withTransaction (chatStore cc) getManagedGroup

shortLinkContactType :: Text -> Either String ContactConnType
shortLinkContactType linkText = case strDecode (encodeUtf8 linkText) of
  Right (ACSL _ (CSLContact _ ct _ _)) -> Right ct
  Right (ACSL _ CSLInvitation {}) -> Left "invitation short link"
  Left e -> Left e

groupLink :: HasCallStack => ChatController -> IO String
groupLink cc = T.unpack . mgGroupLink <$> storedGroup cc

storedGroup :: HasCallStack => ChatController -> IO ManagedGroup
storedGroup cc = getStoredGroup cc >>= maybe (error "no managed group recorded") pure

groupCount :: ChatController -> IO Int
groupCount cc = countRows cc "groups"

-- The lane runs events in order, so once this command's code exists every event queued before it is done.
awaitLane :: HasCallStack => ChatController -> ServiceState -> IO ()
awaitLane cc env = do
  ManagedGroup {mgGroupId} <- storedGroup cc
  issued <- codeCount cc
  atomically $ writeTQueue (groupEventQ env) (GEInGroup mgGroupId (GACommand GROwner "/issue supporter"))
  pollUntilTrue $ (> issued) <$> codeCount cc

-- The lane's reply to awaitLane's command reaches each member after every message the lane sent before it.
awaitLaneOnConsoles :: HasCallStack => ChatController -> ServiceState -> [TestCC] -> IO ()
awaitLaneOnConsoles cc env members = awaitLane cc env >> forM_ members (`awaitOnConsole` "Code: SB-")

-- Saving a tracker's item id is the lane's last write for its command, so the lane is idle after it.
settleLane :: HasCallStack => ChatController -> ServiceState -> Int64 -> IO ()
settleLane cc env gid = do
  anchoredBefore <- anchored
  atomically $ writeTQueue (groupEventQ env) (GEInGroup gid (GACommand GROwner ("/issue supporter uses " <> tshow settleUses)))
  pollUntilTrue $ (> anchoredBefore) <$> anchored
  where
    anchored = countRows cc ("sx_badge_service_badge_codes WHERE redeem_limit = " <> show settleUses <> " AND group_item_id IS NOT NULL")

-- No test tracks a code with this many uses, so the settling codes never match a test's lookup.
settleUses :: Int
settleUses = maxUses

codeCount :: ChatController -> IO Int
codeCount cc = countRows cc "sx_badge_service_badge_codes"

freeCodeCount :: ChatController -> IO Int
freeCodeCount cc = countRows cc "sx_badge_service_badge_codes WHERE code_payment_status = 'free'"

singleUseCodeCount :: ChatController -> IO Int
singleUseCodeCount cc = countRows cc "sx_badge_service_badge_codes WHERE redeem_limit = 1"

countRows :: ChatController -> String -> IO Int
countRows cc fromWhere = fromMaybe 0 <$> queryFirst cc ("SELECT COUNT(*) FROM " <> fromWhere)

-- A test finds each tracked code by its number of uses, so no two of its codes may share one.
trackedCodeColumn :: (HasCallStack, DB.FromField a) => ChatController -> Int -> String -> IO a
trackedCodeColumn cc uses column =
  queryOne cc ("no code issued with " <> show uses <> " uses") $
    "SELECT " <> column <> " FROM sx_badge_service_badge_codes WHERE redeem_limit = " <> show uses

trackedCodeId :: HasCallStack => ChatController -> Int -> IO Int64
trackedCodeId cc uses = trackedCodeColumn cc uses "badge_code_id"

queryRows :: FromRow r => ChatController -> String -> IO [r]
queryRows cc sql = withTransaction (chatStore cc) (\db -> DB.query_ db (fromString sql))

executeSql :: ChatController -> String -> IO ()
executeSql cc sql = withTransaction (chatStore cc) (\db -> DB.execute_ db (fromString sql))

queryColumn :: DB.FromField a => ChatController -> String -> IO [a]
queryColumn cc sql = map fromOnly <$> queryRows cc sql

queryFirst :: DB.FromField a => ChatController -> String -> IO (Maybe a)
queryFirst cc sql = listToMaybe <$> queryColumn cc sql

queryOne :: (HasCallStack, DB.FromField a) => ChatController -> String -> String -> IO a
queryOne cc err sql = queryColumn cc sql >>= maybe (error err) pure . singleRow sql

singleRow :: HasCallStack => String -> [a] -> Maybe a
singleRow sql = \case
  [] -> Nothing
  [r] -> Just r
  _ -> error $ "more than one row from: " <> sql

-- The list is spelled out rather than taken from the service, so a change there fails here.
expectedCommands :: [ChatBotCommand]
expectedCommands =
  [ CBCCommand "issue" "Generate a badge or name code" (Just "<type> [months <M>] [uses <N>], or name <length> [years <Y>]"),
    CBCCommand "bulk" "Generate many single-use codes" (Just "<type> [months <M>] count <B>, or name <length> [years <Y>] count <B>"),
    CBCCommand "revoke" "Revoke a code" (Just "<code>")
  ]

advertisedCommands :: HasCallStack => ChatController -> Int64 -> IO (Maybe [ChatBotCommand])
advertisedCommands cc gid =
  (>>= commands_)
    <$> (queryOne cc "no group profile to read commands from" (groupProfileSql "p.preferences" gid) :: IO (Maybe GroupPreferences))

profileDisplayName :: HasCallStack => ChatController -> Int64 -> IO Text
profileDisplayName cc gid =
  queryOne cc "no group profile to read a display name from" (groupProfileSql "p.display_name" gid)

profileFullNameShortDescr :: HasCallStack => ChatController -> Int64 -> IO (Text, Maybe Text)
profileFullNameShortDescr cc gid =
  queryRows cc sql >>= maybe (error "no group profile to read a full name from") pure . singleRow sql
  where
    sql = groupProfileSql "p.full_name, p.short_descr" gid

profileDescription :: HasCallStack => ChatController -> Int64 -> IO (Maybe Text)
profileDescription cc gid = queryOne cc "no group profile to read a description from" (groupProfileSql "p.description" gid)

profileUpdatedAt :: HasCallStack => ChatController -> Int64 -> IO UTCTime
profileUpdatedAt cc gid =
  queryOne cc "no group profile to read an update time from" (groupProfileSql "p.updated_at" gid)

groupFeaturePreference :: HasCallStack => ChatController -> Int64 -> SGroupFeature f -> IO (GroupFeaturePreference f)
groupFeaturePreference cc gid feature =
  getGroupPreference feature
    <$> (queryOne cc "no group profile to read preferences from" (groupProfileSql "p.preferences" gid) :: IO (Maybe GroupPreferences))

groupProfileSql :: String -> Int64 -> String
groupProfileSql columns gid =
  "SELECT "
    <> columns
    <> " FROM group_profiles p JOIN groups g ON g.group_profile_id = p.group_profile_id WHERE g.group_id = "
    <> show gid

clearCommandsSettingFullDelete :: ChatController -> Int64 -> IO ()
clearCommandsSettingFullDelete cc gid =
  withTransaction (chatStore cc) $ \db ->
    DB.execute
      db
      "UPDATE group_profiles SET preferences = ? WHERE group_profile_id IN (SELECT group_profile_id FROM groups WHERE group_id = ?)"
      (fullDeleteOnly, gid)
  where
    fullDeleteOnly =
      emptyGroupPrefs {fullDelete = Just FullDeleteGroupPreference {enable = FEOn, role = Nothing}} :: GroupPreferences

revokeInStore :: HasCallStack => ChatController -> Int -> IO ()
revokeInStore cc uses = getCurrentTime >>= setTrackedCodeTime cc uses "revoked_at"

backdateTracker :: HasCallStack => ChatController -> Int -> IO UTCTime
backdateTracker cc uses = do
  past <- addUTCTime pastEditWindow <$> getCurrentTime
  setTrackedCodeTime cc uses "group_item_sent_at" past
  pure past

-- This is an hour past the core's 24-hour edit window.
pastEditWindow :: NominalDiffTime
pastEditWindow = -25 * 3600

-- The code row keeps its sent time, so the service still tries an edit the core refuses.
backdateTrackerItem :: ChatController -> Int64 -> IO ()
backdateTrackerItem cc citemId = do
  past <- addUTCTime pastEditWindow <$> getCurrentTime
  withTransaction (chatStore cc) $ \db ->
    DB.execute db "UPDATE chat_items SET item_ts = ? WHERE chat_item_id = ?" (past, citemId)

-- Below author the core refuses every send from the bot.
setBotRole :: ChatController -> Text -> IO ()
setBotRole cc role =
  withTransaction (chatStore cc) $ \db ->
    DB.execute db "UPDATE group_members SET member_role = ? WHERE member_category = ?" (role, "user" :: Text)

dateRedemption :: HasCallStack => ChatController -> Int -> UTCTime -> IO ()
dateRedemption cc uses = setTrackedCodeTime cc uses "redeemed_at"

setTrackedCodeTime :: HasCallStack => ChatController -> Int -> String -> UTCTime -> IO ()
setTrackedCodeTime cc uses column ts = do
  badgeCodeId <- trackedCodeId cc uses
  withTransaction (chatStore cc) $ \db ->
    DB.execute db (fromString $ "UPDATE sx_badge_service_badge_codes SET " <> column <> " = ? WHERE badge_code_id = ?") (ts, badgeCodeId)

-- Member messages are excluded because a /revoke names the code too.
sentItemsWithCode :: ChatController -> Text -> IO [Text]
sentItemsWithCode cc code =
  queryColumn cc $
    "SELECT item_text FROM chat_items WHERE item_sent = 1 AND item_text LIKE '%"
      <> T.unpack code
      <> "%' ORDER BY chat_item_id"

-- It reads the code row, not a join, so it still answers after the message is deleted.
trackerAnchor :: HasCallStack => ChatController -> Int -> IO (Maybe Int64)
trackerAnchor cc uses = trackedCodeColumn cc uses "group_item_id"

trackerSentAt :: HasCallStack => ChatController -> Int -> IO UTCTime
trackerSentAt cc uses = trackedCodeColumn cc uses "group_item_sent_at"

deleteItem :: HasCallStack => ChatController -> Int64 -> IO ()
deleteItem cc citemId = do
  ManagedGroup {mgGroupId} <- storedGroup cc
  sendChatCmd cc (APIDeleteChatItem (ChatRef CTGroup mgGroupId Nothing) (citemId :| []) CIDMInternal) >>= \case
    Right CRChatItemsDeleted {} -> pure ()
    r -> error $ "deleting chat item " <> show citemId <> " failed: " <> show r

-- The member must have received the message before the command can find it.
moderateBotItem :: HasCallStack => TestCC -> Text -> IO ()
moderateBotItem member body = do
  void (pollUntil received :: IO Int64)
  send member ("\\\\ #" <> groupName <> " @" <> botName <> " " <> T.unpack header)
  where
    header = T.takeWhile (/= '\n') body
    received =
      withTransaction (chatStore $ chatController member) $ \db ->
        listToMaybe . map fromOnly <$> DB.query db "SELECT chat_item_id FROM chat_items WHERE item_sent = 0 AND item_text LIKE ? || '%'" (Only header)

waitModeratedItem :: HasCallStack => ChatController -> Int64 -> IO (Int, Maybe Int64, Text)
waitModeratedItem cc citemId =
  pollUntil $
    mfilter (\(deleted, _, _) -> deleted /= 0) . listToMaybe
      <$> queryRows cc ("SELECT item_deleted, item_deleted_by_group_member_id, item_content FROM chat_items WHERE chat_item_id = " <> show citemId)

memberRoles :: ChatController -> Int64 -> IO [(Text, Text)]
memberRoles cc gid =
  filter ((/= badgeBotName) . fst)
    <$> queryRows cc ("SELECT local_display_name, member_role FROM group_members WHERE group_id = " <> show gid <> " ORDER BY group_member_id")

readItemText :: ChatController -> Int64 -> IO Text
readItemText cc citemId = fromMaybe "" <$> firstText cc ("chat_item_id = " <> show citemId)

-- A line left unread when the test ends fails the per-core teardown check.
drainUntil :: HasCallStack => TestCC -> [String] -> IO ()
drainUntil cc markers =
  timeout waitLimit (go markers) >>= \case
    Just () -> pure ()
    Nothing -> error $ "drainUntil: console never showed " <> show markers
  where
    go [] = pure ()
    go unseen = do
      l <- atomically $ readTQueue (termQ cc)
      go $ filter (not . (`isInfixOf` l)) unseen

drainConsole :: TestCC -> IO ()
drainConsole cc =
  timeout quietPeriod (atomically (readTQueue (termQ cc))) >>= \case
    Just _ -> drainConsole cc
    Nothing -> pure ()

-- A console silent this long is taken as drained.
quietPeriod :: Int
quietPeriod = 500000

pollInterval :: Int
pollInterval = 200000

maxPolls :: Int
maxPolls = 150

waitLimit :: Int
waitLimit = maxPolls * pollInterval

pollUntil :: HasCallStack => IO (Maybe a) -> IO a
pollUntil act = go maxPolls
  where
    go n =
      act >>= \case
        Just a -> pure a
        Nothing
          | n <= (0 :: Int) -> error "pollUntil: timed out waiting for chat state"
          | otherwise -> threadDelay pollInterval >> go (n - 1)

pollUntilTrue :: HasCallStack => IO Bool -> IO ()
pollUntilTrue cond = pollUntil (guard <$> cond)

waitMemberRole :: HasCallStack => ChatController -> Int64 -> Text -> Text -> IO ()
waitMemberRole cc gid name role = pollUntilTrue $ elem (name, role) <$> memberRoles cc gid

setMemberRole :: HasCallStack => ChatController -> Int64 -> Text -> Text -> IO ()
setMemberRole cc gid name role = do
  sendChatCmdStr cc ("/mr #" <> groupName <> " " <> T.unpack name <> " " <> T.unpack role) >>= (`shouldSatisfy` isRight)
  waitMemberRole cc gid name role

-- Each member's console shows the bot's role change, naming the member or "your role".
setMemberRoleSeen :: HasCallStack => ChatController -> Int64 -> [TestCC] -> Text -> Text -> IO ()
setMemberRoleSeen cc gid members name role = do
  from <- fromMaybe (error $ "no member " <> T.unpack name) . lookup name <$> memberRoles cc gid
  setMemberRole cc gid name role
  forM_ members $ \member -> do
    viewer <- userName member
    let whose = if viewer == T.unpack name then "your role" else "the role of " <> T.unpack name
    drainUntil member ["#" <> groupName <> ": " <> botName <> " changed " <> whose <> " from " <> T.unpack from <> " to " <> T.unpack role]

waitMemberBlocked :: HasCallStack => ChatController -> Int64 -> Text -> IO ()
waitMemberBlocked cc gid name =
  pollUntilTrue $ (== Just (Just ("blocked" :: Text))) <$> queryFirst cc restrictionSql
  where
    restrictionSql =
      "SELECT member_restriction FROM group_members WHERE group_id = "
        <> show gid
        <> " AND local_display_name = '"
        <> T.unpack name
        <> "'"

waitBlockedItem :: HasCallStack => ChatController -> Text -> IO (Int, Text)
waitBlockedItem cc txt =
  pollUntil $
    mfilter ((/= 0) . fst) . listToMaybe
      <$> queryRows cc ("SELECT item_deleted, item_content FROM chat_items WHERE item_sent = 0 AND item_text = '" <> T.unpack txt <> "'")

waitAdvertisedCommands :: HasCallStack => ChatController -> Int64 -> IO ()
waitAdvertisedCommands cc gid = pollUntilTrue $ (== Just expectedCommands) <$> advertisedCommands cc gid

waitStoredItem :: HasCallStack => ChatController -> Text -> IO ()
waitStoredItem cc txt = void . pollUntil $ firstText cc ("item_text = '" <> T.unpack txt <> "'")

-- The reply is the first item the service sends after the command arrives.
replyTo :: HasCallStack => ChatController -> TestCC -> String -> IO Text
replyTo cc member cmd = do
  lastId <- maxChatItemId cc
  sendGroupCmd member cmd
  reply <- waitReceivedItemId cc lastId (T.pack cmd) >>= waitReplyAfter cc
  awaitOnConsole member reply
  pure reply

-- A line left unread when the test ends fails the per-core teardown check, however late the bot's reply arrives.
awaitOnConsole :: HasCallStack => TestCC -> Text -> IO ()
awaitOnConsole member reply =
  drainUntil member ["#" <> groupName <> " " <> botName <> "> " <> T.unpack (T.takeWhile (/= '\n') reply)]

-- A tracker shows as its code line, then its state line, which tells its post and its edits apart.
awaitTrackerOnConsole :: HasCallStack => TestCC -> Text -> IO ()
awaitTrackerOnConsole member body = drainUntil member [T.unpack $ T.drop 1 $ T.dropWhile (/= '\n') body]

maxChatItemId :: ChatController -> IO Int64
maxChatItemId cc = fromMaybe 0 <$> queryFirst cc "SELECT COALESCE(MAX(chat_item_id), 0) FROM chat_items"

waitReceivedItemId :: HasCallStack => ChatController -> Int64 -> Text -> IO Int64
waitReceivedItemId cc afterId txt =
  pollUntil . queryFirst cc $
    "SELECT chat_item_id FROM chat_items WHERE item_sent = 0 AND chat_item_id > " <> show afterId <> " AND item_text = '" <> T.unpack txt <> "'"

waitReplyAfter :: HasCallStack => ChatController -> Int64 -> IO Text
waitReplyAfter cc afterId =
  pollUntil . firstText cc $ "item_sent = 1 AND chat_item_id > " <> show afterId <> " ORDER BY chat_item_id LIMIT 1"

waitItemText :: HasCallStack => ChatController -> Int64 -> Text -> IO Text
waitItemText cc citemId marker =
  pollUntil $ mfilter (marker `T.isInfixOf`) . Just <$> readItemText cc citemId

waitCodeOfType :: HasCallStack => ChatController -> Text -> IO ()
waitCodeOfType cc badgeType =
  pollUntilTrue $ (> 0) <$> countRows cc ("sx_badge_service_badge_codes WHERE badge_type = '" <> T.unpack badgeType <> "'")

waitCodeMonths :: HasCallStack => ChatController -> Text -> IO Int
waitCodeMonths cc badgeType =
  pollUntil $ do
    months <- queryColumn cc $ "SELECT months FROM sx_badge_service_badge_codes WHERE badge_type = '" <> T.unpack badgeType <> "'"
    pure $ case months of
      [m] -> Just m
      _ -> Nothing

firstText :: ChatController -> String -> IO (Maybe Text)
firstText cc cond = queryFirst cc ("SELECT item_text FROM chat_items WHERE " <> cond)

trackerItem :: HasCallStack => ChatController -> Int -> IO (Maybe (Int64, Text))
trackerItem cc uses = singleRow sql <$> queryRows cc sql
  where
    sql =
      "SELECT c.group_item_id, ci.item_text "
        <> "FROM sx_badge_service_badge_codes c "
        <> "JOIN chat_items ci ON ci.chat_item_id = c.group_item_id "
        <> "WHERE c.redeem_limit = "
        <> show uses

waitTrackerItemOf :: HasCallStack => ChatController -> Int -> IO (Int64, Text)
waitTrackerItemOf cc = pollUntil . trackerItem cc

waitTrackerRepost :: HasCallStack => ChatController -> Int -> Int64 -> IO (Int64, Text)
waitTrackerRepost cc uses itemId0 = pollUntil $ mfilter ((/= itemId0) . fst) <$> trackerItem cc uses

redeemedTrackerCount :: ChatController -> IO Int
redeemedTrackerCount cc = countRows cc "chat_items WHERE item_text LIKE '%last used%'"

revokedTrackerCount :: ChatController -> IO Int
revokedTrackerCount cc = countRows cc ("chat_items WHERE item_text LIKE '" <> T.unpack (retiredText "SB-%") <> "'")

retiredText :: Text -> Text
retiredText code = code <> "\nRevoked, can no longer be redeemed"

extractCode :: HasCallStack => Text -> Text
extractCode t = maybe (error ("no badge code in: " <> T.unpack t)) formatBadgeCode (codeInTracker t)

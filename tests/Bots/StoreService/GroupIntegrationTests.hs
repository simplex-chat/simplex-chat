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

import StoreService.Config (BadgeIssuerKey (..), ChainConfig (cController, cServiceKey), GroupConfig (..), NamesConfig (nChain), ServiceConfig (devAcceptUnverifiedStoreReceipts, names), readServiceConfig)
import StoreService.Group (GroupAction (..), GroupEvent (..), codeInTracker, ensureManagedGroup, joinerAcceptance)
import StoreService.Group.Command (maxUses)
import StoreService.Names.Abi (addressWord, selector, uint256)
import StoreService.Names.Codes (issueNameCode, reserveCode, revokeNameCode)
import StoreService.Names.Phases (RegistrationId (..))
import StoreService.Names.Chain (SignedTx (..), TxRequest (..), newChainClient, signTransaction)
import StoreService.Names.Store (CodeIssuer (..), NamePhase (PFailed), NameRegistration (nrFailedAt, nrPhase), NewTransaction (..), PrepaidCode (..), TxFor (..), VersionWrite (Written), getPrepaidCode, insertTransaction, spendLandedPrepaidCode)
import StoreService.Names.Turns (newTurns, withCodeTurn)
import StoreService.Names.View (addressText)
import StoreService.Options (StoreServiceOpts (..))
import StoreService.Orders (cancelOpenInvoice)
import StoreService.Service (ServiceState (..), checkNodeChain, checkUnverifiedReceipts, newServiceState, redactPassword, requireDevKeys, storeService, trustDevKeys)
import StoreService.Store (ManagedGroup (..), getManagedGroup)
import StoreService.Store.Migrate (withInstanceLock)
import StoreService.Waiters (newWaiters)
import Bots.StoreService.BotTests (badgeBotName, badgeCmdUsage, badgeProfile, badgeTypeOf, credentialOf, devKeysIni, entryOf, issueCode, issueRaw, mkStoreServiceOpts, newPurchaseKeys, operatorCmd, redeemWithQueue, requestObject, revokeRaw, serviceDbPrefix, shouldAnswerError, statementOf, stopStoreService, testIssuerKeyIdx, withStoreService)
import Bots.StoreService.ConfigTests (chainSection, controllerAddr, namesSection, registrarAddr, withRpcUrl)
import Bots.StoreService.FakeEthNode (CallAnswer (..), FakeEthNode (..), Fault (..), address, answerCall, awaitSent, fakeNamePolicy, linkTermsOnNode, plainTerms, setFault, withFakeEthNode)
import Bots.StoreService.NamesContractsTests (nameExpiresCall, rentPriceAnswer, rentPriceCall, reservedNamesCall)
import Bots.StoreService.WebTests (capturingStderr, digestFixture)
import Bots.StoreService.NamesSearchTests (aliceHex)
import qualified Bots.StoreService.NamesBalancesTests as Balances
import Bots.StoreService.NamesStoreTests (issue, order, ownerA, placeCode, placePaid, reg, t0, usable, write)
import Bots.StoreService.NamesOwnerTests (labelBytes, ownerOfSelector)
import ChatClient
import ChatTests.DBUtils
import ChatTests.Utils
import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.Async (Async, async, cancel, poll, wait, waitCatchSTM)
import Control.Concurrent.STM (TQueue, atomically, newTMVarIO, newTQueueIO, orElse, readTMVar, readTQueue, readTVarIO, tryReadTMVar, writeTQueue)
import Control.Exception (IOException, bracket, catch, finally, throwIO, try)
import Control.Monad (forM_, guard, mfilter, replicateM_, unless, void, when)
import qualified Data.Aeson as J
import qualified Data.ByteArray as BA
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
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
import Network.HTTP.Client (RequestBody (..), defaultManagerSettings, httpLbs, method, newManager, parseRequest, requestBody, requestHeaders, responseBody, responseHeaders, responseStatus)
import Network.HTTP.Types.Status (statusCode)
import Network.Socket (Family (..), SockAddr (..), SocketType (..), close, connect, defaultProtocol, socket, tupleToHostAddress)
import Network.Wai.Handler.Warp (openFreePort)
import Simplex.Chat.Badges (BadgeType (..))
import Simplex.Chat.Badges.Code (NameCode (..), NamePrepaidCode, formatBadgeCode, formatNameCode, namePrepaidCodeHash, namePrepaidCodeLength, parseBadgeCode, parseNameCode, randomBadgeCode, randomNamePrepaidCode)
import Simplex.Chat.Badges.Types (BadgeCodePaymentStatus (..))
import Simplex.Chat.StoreService
import Simplex.Chat.Controller (ChatCommand (APIDeleteChatItem, APIGroupInfo), ChatConfig (..), ChatController (..), ChatResponse (CRChatItemsDeleted, CRCustomChatResponse, CRGroupCreated, CRGroupInfo))
import Simplex.Chat.Core (sendChatCmd, sendChatCmdStr)
import Simplex.Chat.Messages (ChatRef (ChatRef), ChatType (CTGroup))
import Simplex.Chat.Messages.CIContent (CIDeleteMode (CIDMInternal))
import Simplex.Chat.PaymentService.Types (InvoiceId (..))
import Simplex.Chat.Store.Profiles (GroupLinkInfo (..))
import Simplex.Chat.Types (AgentInvId (..), GroupInfo, GroupMemberAdmission (..), MemberCriteria (..))
import Simplex.Chat.Types.Preferences (ChatBotCommand (..), FullDeleteGroupPreference (..), GroupFeatureEnabled (..), GroupFeatureI (..), GroupPreferences (..), HistoryGroupPreference (..), SGroupFeature (..), commands_, emptyGroupPrefs, getGroupPreference)
import Simplex.Chat.Types.Shared (GroupAcceptance (..), GroupMemberRole (..))
import Simplex.Messaging.Agent (disposeAgentClient)
import Simplex.Messaging.Agent.Protocol (AConnShortLink (..), ConnShortLink (..), ContactConnType (..))
import Simplex.Messaging.Agent.Store.Common (withTransaction)
import qualified Simplex.Messaging.Agent.Store.DB as DB
import qualified Simplex.Messaging.Crypto as C
import Simplex.Messaging.Crypto.Secp256k1 (mkPrivateKey)
import Simplex.Messaging.Crypto.BBS (bbsKeyGen)
import Simplex.Messaging.Encoding.String (strDecode)
import Simplex.Messaging.Util (tshow)
import System.Directory (createDirectoryIfMissing)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.Timeout (timeout)
#if defined(dbPostgres)
import Database.PostgreSQL.Simple (FromRow, Only (..))
import qualified Database.PostgreSQL.Simple as PSQL
#else
import Database.SQLite.Simple (FromRow, Only (..))
#endif
import Test.Hspec hiding (it)

badgeGroupIntegrationTests :: SpecWith TestParams
badgeGroupIntegrationTests = do
  it "creates the group and a join link on first start; on restart reuses it, re-advertises commands and keeps other preferences" testGroupCreateReuse
  it "keeps the group's name and description when the configured ones change" testConfigNotApplied
  it "promotes the first joiner to owner and leaves a later joiner a member" testFirstJoinerPromoted
  it "requires member review once the group has an owner, again on start after it was cleared" testMemberReviewRequired
  it "holds a removed member who rejoins through the link for review" testRejoinerReviewed
  it "holds a joiner of the managed group for review once the owner was set up, whatever its profile says, and on a lookup failure, not one of another group" testJoinerAcceptance
  it "requires member review again when an owner turns it off" testReviewRestored
  it "requires member review again at the next join when the profile lost it" testReviewRepairedOnJoin
  it "keeps a code from a member pending review, who a moderator accepts" testModeratorAdmitsPendingMember
  it "promotes nobody after the first promotion failed, holding a later joiner for review" testFailedPromotionNotRetried
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
  it "refuses /issue name while names are off, still revoking name codes" testGroupNameCodesNamesOff
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
namesBootTests = do
  it "warns naming the indexes of trusted dev_keys, and says nothing without them" testDevKeysWarned
  it "refuses a node serving another chain than chain_id, and dev_keys unless the node answers, only warning without them" testNodeChain
  it "masks the database password in the start line, in a URI or as a keyword" testRedactPassword
  it "refuses unverified store receipts with an issuer key clients trust, and takes them with a dev key" testUnverifiedReceiptsKey
  it "refuses a second instance on the same Postgres database until the first ends, exits when the lock's connection fails, and takes no lock on SQLite" testInstanceLock
  it "exits before the chat core starts while a pending transaction was signed by another key" testOtherKeyPendingStops
  it "boots codes-only, refusing paid name orders 403 codes_only and answering code orders and code checks, the badges shell as built" testNamesBootCodesOnly
  it "boots with [names] and [chain], routing fixed /api/name paths ahead of a search, find ahead of registrations, the community terms and registrations to GET, another name, a reveal, a spend and a retry to POST, a path no route knows and a taken order's code to 404, and each route under its limit, shared by the routes of that limit" testNamesBoot
  it "searches with the configured price floor, answering a price at it and 503 to one cent under it" testNamesBootPriceFloor

testDevKeysWarned :: HasCallStack => TestParams -> IO ()
testDevKeysWarned _ = do
  Right (pk, sk) <- bbsKeyGen
  Right (_, devSk) <- bbsKeyGen
  let cfg = testCfg {badgePublicKeys = M.singleton testIssuerKeyIdx pk}
  Right withDev <- devKeysIni chainSection 9 devSk [(9, devSk), (12, devSk)]
  warned <- capturingStderr $ trustDevKeys (Just withDev) cfg >>= (`shouldSatisfy` isRight) . fmap badgePublicKeys
  filter ("[WARN " `T.isInfixOf`) (T.lines warned) `shouldSatisfy` any ("trusting issuer dev_keys at indexes 9, 12" `T.isInfixOf`)
  Right withoutDev <- devKeysIni chainSection testIssuerKeyIdx sk []
  quiet <- capturingStderr $ trustDevKeys (Just withoutDev) cfg >>= (`shouldSatisfy` isRight) . fmap badgePublicKeys
  filter ("dev_keys" `T.isInfixOf`) (T.lines quiet) `shouldBe` []

testInstanceLock :: HasCallStack => TestParams -> IO ()
testInstanceLock ps = do
  Right (_, sk) <- bbsKeyGen
  let locked :: IO a -> IO a
      locked = withInstanceLock 100000 (mkStoreServiceOpts ps sk)
#if defined(dbPostgres)
  locked (locked (pure ()) `shouldThrow` (== ExitFailure 1))
  locked (threadDelay 500000 >> pure True) `shouldReturn` True
  locked (terminateInstanceLockSession >> threadDelay 5000000) `shouldThrow` (== ExitFailure 1)
#else
  locked (locked (pure ()))
#endif
  locked (pure True) `shouldReturn` True

testOtherKeyPendingStops :: HasCallStack => TestParams -> IO ()
testOtherKeyPendingStops ps = do
  svc <- (\s -> s {gsIniSections = "" : namesSection <> chainSection}) <$> prepareGroupService ps
  (opts, serviceCfg, _) <- groupServiceOpts svc Nothing
  Just chainCfg <- pure $ nChain <$> names serviceCfg
  g <- C.newRandom
  otherKey <- newChainClient g chainCfg {cServiceKey = either error id $ mkPrivateKey $ BA.replicate 32 0x47}
  Right SignedTx {stRaw, stHash} <-
    signTransaction g otherKey TxRequest {trTo = cController chainCfg, trData = "", trNonce = 0, trGasLimit = 100000, trMaxFeePerGas = 1, trMaxPriorityFeePerGas = 1}
  (t, cc, _, _) <- startGroupService svc Nothing
  flip finally (stopGroupService t cc) $ do
    code <- issue (chatStore cc) 1 usable
    placeCode (chatStore cc) "r1" ownerA (order "alicebakery" 1) code `shouldReturn` Right ()
    withTransaction (chatStore cc) $ \db -> void $ insertTransaction db (TxCommit $ RegistrationId "r1") NewTransaction {ntxRaw = stRaw, ntxHash = stHash, ntxNonce = 0} t0
  env <- newServiceState
  ended <- timeout waitLimit (try $ storeService opts (Just serviceCfg) (gsCfg svc) env)
  started <- atomically (tryReadTMVar (serviceCC env))
  mapM_ stopChat started
  (isNothing started, ended) `shouldBe` (True, Just (Left (ExitFailure 1)))

#if defined(dbPostgres)
terminateInstanceLockSession :: IO ()
terminateInstanceLockSession =
  bracket (PSQL.connect testDBConnectInfo) PSQL.close $ \db ->
    PSQL.query
      db
      "SELECT pg_terminate_backend(pid) FROM pg_locks WHERE locktype = 'advisory' AND granted AND database = (SELECT oid FROM pg_database WHERE datname = current_database()) AND ((classid::bigint << 32) | objid::bigint) = ?"
      (Only (0x535853544f5245 :: Int64))
      `shouldReturn` [Only True]
#endif

testUnverifiedReceiptsKey :: HasCallStack => TestParams -> IO ()
testUnverifiedReceiptsKey _ = do
  Right (pk, sk) <- bbsKeyGen
  Right withDev <- devKeysIni chainSection 9 sk [(9, sk)]
  let unverified = Just withDev {devAcceptUnverifiedStoreReceipts = True}
      cfg = testCfg {badgePublicKeys = M.singleton testIssuerKeyIdx pk}
  checkUnverifiedReceipts unverified cfg BadgeIssuerKey {keyIdx = testIssuerKeyIdx, secretKey = sk}
    `shouldBe` Left ("[dev] accept_unverified_store_receipts is refused: issuer key index " <> show testIssuerKeyIdx <> " is one clients trust, so any receipt would mint badges they accept")
  checkUnverifiedReceipts unverified cfg BadgeIssuerKey {keyIdx = 9, secretKey = sk} `shouldBe` Right ()
  checkUnverifiedReceipts (Just withDev) cfg BadgeIssuerKey {keyIdx = testIssuerKeyIdx, secretKey = sk} `shouldBe` Right ()

testRedactPassword :: HasCallStack => TestParams -> IO ()
testRedactPassword _ = do
  redactPassword "postgresql://simplex:s3cr@t:x@127.0.0.1:5432/badge_service" `shouldBe` "postgresql://simplex:***@127.0.0.1:5432/badge_service"
  redactPassword "postgresql://simplex@127.0.0.1:5432/badge_service" `shouldBe` "postgresql://simplex@127.0.0.1:5432/badge_service"
  redactPassword "host=127.0.0.1 user=simplex password=s3cret dbname=badge_service" `shouldBe` "host=127.0.0.1 user=simplex password=*** dbname=badge_service"
  redactPassword "/var/opt/simplex_badge_service" `shouldBe` "/var/opt/simplex_badge_service"

testNodeChain :: HasCallStack => TestParams -> IO ()
testNodeChain _ = withFakeEthNode $ \node@FakeEthNode {fnUrl} -> do
  Right (_, devSk) <- bbsKeyGen
  let onNode = map (\l -> if "rpc_url =" `T.isPrefixOf` l then "rpc_url = " <> T.pack fnUrl else l) chainSection
  Right withDev <- devKeysIni onNode 9 devSk [(9, devSk)]
  Right noDev <- devKeysIni onNode testIssuerKeyIdx devSk []
  checkNodeChain (Just withDev) `shouldReturn` Right ()
  checkNodeChain (Just noDev) `shouldReturn` Right ()
  M.member 9 . badgePublicKeys <$> requireDevKeys (Just withDev) testCfg `shouldReturn` True
  setFault node "eth_chainId" (Answers (J.String "0x1"))
  let otherChain = "chain.chain_id is 31337, but the node at chain.rpc_url serves chain 1"
  checkNodeChain (Just withDev) `shouldReturn` Left otherChain
  checkNodeChain (Just noDev) `shouldReturn` Left otherChain
  requireDevKeys (Just noDev) testCfg `shouldThrow` (== ExitFailure 1)
  setFault node "eth_chainId" HttpFailure
  checkNodeChain (Just withDev) `shouldReturn` Left "issuer.dev_keys is refused: the node's chain id could not be read"
  warned <- capturingStderr $ checkNodeChain (Just noDev) `shouldReturn` Right ()
  warned `shouldSatisfy` T.isInfixOf "the node's chain id could not be read, so it is not checked"
  checkNodeChain Nothing `shouldReturn` Right ()

testNamesBoot :: HasCallStack => TestParams -> IO ()
testNamesBoot ps = do
  svc <- prepareGroupService ps
  (t, cc, _, port) <- startGroupService svc {gsIniSections = "" : namesSection <> chainSection, gsListener = ["trust_forwarded_for = on"]} Nothing
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
    terms <- request "/api/terms/0x12"
    answer terms `shouldReturn` refused "bad_request"
    termsPosted <- httpLbs terms {method = "POST"} mgr
    (statusCode $ responseStatus termsPosted, lookup "Allow" $ responseHeaders termsPosted) `shouldBe` (405, Just "GET")
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
    spent <- request "/api/registration/t2Jk8RmQ4vXw9NcP3yLb6g/spend"
    -- formats are checked before the order, so only a well-formed body reaches the 404
    let spendBody =
          J.object
            [ "labelHash" J..= aliceHex,
              "length" J..= (11 :: Int),
              "duration" J..= (69292800 :: Int),
              "commitment" J..= aliceHex,
              "sealed" J..= ("sealed" :: Text),
              "price" J..= (10000 :: Int),
              "balance" J..= (10000 :: Int),
              "terms" J..= plainTerms,
              "timestamp" J..= (1792574100 :: Int),
              "signature" J..= ("0x" <> T.replicate 32 "11" <> T.replicate 32 "22" <> "1b")
            ]
    answer spent {method = "POST", requestBody = RequestBodyLBS $ J.encode spendBody} `shouldReturn` (404, Just $ J.object ["error" J..= ("not_found" :: Text)])
    spentByGet <- httpLbs spent mgr
    (statusCode $ responseStatus spentByGet, lookup "Allow" $ responseHeaders spentByGet) `shouldBe` (405, Just "POST")
    retried <- request "/api/registration/t2Jk8RmQ4vXw9NcP3yLb6g/retry"
    answer retried {method = "POST"} `shouldReturn` (404, Just $ J.object ["error" J..= ("not_found" :: Text)])
    retriedByGet <- httpLbs retried mgr
    (statusCode $ responseStatus retriedByGet, lookup "Allow" $ responseHeaders retriedByGet) `shouldBe` (405, Just "POST")
    -- each limit's routes share one client's minute, whatever they answer; a client of its own per limit
    let rid = "/api/registration/t2Jk8RmQ4vXw9NcP3yLb6g"
        limits =
          [ ("create", 5, [("POST", rid <> "/name"), ("POST", rid <> "/retry"), ("POST", rid <> "/spend")]),
            ("code", 10, [("POST", "/api/name/register"), ("POST", "/api/code/check")]),
            ("read", 60, [("GET", rid), ("POST", rid <> "/reveal")]),
            ("search", 30, [("GET", "/api/name/" <> T.unpack aliceHex <> "?length=5"), ("GET", "/api/terms/0x12")]),
            ("preview", 20, [("POST", "/api/link/preview")]),
            ("find", 5, [("POST", "/api/registration/find")]),
            ("records", 10, [("POST", "/api/name/records")])
          ]
    forM_ (zip [1 :: Int ..] limits) $ \(i, (limit, perMinute, routes)) -> do
      let from (m, path) = do
            req <- request path
            r <- httpLbs req {method = m, requestBody = RequestBodyLBS "{}", requestHeaders = [("X-Forwarded-For", fromString $ "203.0.113." <> show i)]} mgr
            pure (limit, path, statusCode $ responseStatus r, isJust $ lookup "Retry-After" $ responseHeaders r)
      forM_ (take perMinute $ cycle routes) $ \route -> from route >>= \(_, path, status, _) -> (limit, path, status == 429) `shouldBe` (limit :: Text, path, False)
      forM_ routes $ \route@(_, path) -> from route `shouldReturn` (limit, path, 429, True)
    poll t >>= (`shouldSatisfy` isNothing)

testNamesBootCodesOnly :: HasCallStack => TestParams -> IO ()
testNamesBootCodesOnly ps = do
  svc@GroupSvc {gsStaticDir} <- prepareGroupService ps
  let shell = "<!doctype html><meta name=\"simplex-names-mode\" content=\"open\">"
  B.writeFile (gsStaticDir </> "index.html") shell
  (t, cc, _, port) <- startGroupService svc {gsIniSections = "" : codesOnlyNames <> chainSection} Nothing
  flip finally (stopGroupService t cc) $ do
    pollUntilTrue (listening port)
    mgr <- newManager defaultManagerSettings
    let posted path body = do
          req <- parseRequest $ "http://127.0.0.1:" <> show port <> path
          r <- httpLbs req {method = "POST", requestBody = RequestBodyLBS body} mgr
          pure (statusCode $ responseStatus r, J.decode $ responseBody r :: Maybe J.Value)
        answered tag status = (status, Just $ J.object ["error" J..= (tag :: Text)])
    posted "/api/invoice" "{\"kind\":\"name\"}" `shouldReturn` answered "codes_only" 403
    posted "/api/registration/t2Jk8RmQ4vXw9NcP3yLb6g/spend" "{}" `shouldReturn` answered "codes_only" 403
    posted "/api/name/register" "{}" `shouldReturn` answered "bad_request" 400
    posted "/api/code/check" "{}" `shouldReturn` answered "bad_request" 400
    badgesShell <- parseRequest ("http://127.0.0.1:" <> show port <> "/") >>= (`httpLbs` mgr)
    (statusCode $ responseStatus badgesShell, responseBody badgesShell) `shouldBe` (200, LB.fromStrict shell)
    poll t >>= (`shouldSatisfy` isNothing)

testNamesBootPriceFloor :: HasCallStack => TestParams -> IO ()
testNamesBootPriceFloor ps = withFakeEthNode $ \node@FakeEthNode {fnUrl} -> do
  svc <- prepareGroupService ps
  let controller = address $ encodeUtf8 controllerAddr
      registrar = address $ encodeUtf8 registrarAddr
  forM_ [selector "ens()", selector "GRACE_PERIOD()", nameExpiresCall] $ \call -> answerCall node registrar call $ Returns $ uint256 (0 :: Int)
  answerCall node controller (selector "namePolicy()") $ Returns $ addressWord fakeNamePolicy
  answerCall node fakeNamePolicy reservedNamesCall $ Returns $ uint256 (0 :: Int)
  linkTermsOnNode node
  answerCall node controller (rentPriceCall 11) $ Returns $ rentPriceAnswer 15000
  (t, cc, _, port) <- startGroupService svc {gsIniSections = "" : namesSection <> ["min_price_cents_per_year = 7500"] <> withRpcUrl (T.pack fnUrl)} Nothing
  flip finally (stopGroupService t cc) $ do
    pollUntilTrue (listening port)
    mgr <- newManager defaultManagerSettings
    let searched = do
          req <- parseRequest $ "http://127.0.0.1:" <> show port <> "/api/name/" <> T.unpack aliceHex <> "?length=11"
          statusCode . responseStatus <$> httpLbs req mgr
    searched `shouldReturn` 200
    answerCall node controller (rentPriceCall 11) $ Returns $ rentPriceAnswer 14999
    searched `shouldReturn` 503
    poll t >>= (`shouldSatisfy` isNothing)

-- | [names] with codes_only on, as an investors' launch runs.
codesOnlyNames :: [Text]
codesOnlyNames = namesSection <> ["codes_only = on"]

namesWorkerBootTests :: SpecWith TestParams
namesWorkerBootTests = it "starts the workers and the terms fetch with the service, sending the commit of an order queued after the start and storing both terms texts" testWorkerBoot

testWorkerBoot :: HasCallStack => TestParams -> IO ()
testWorkerBoot ps = withFakeEthNode $ \node@FakeEthNode {fnUrl} -> do
  svc <- prepareGroupService ps
  let onFakeNode line = if "rpc_url" `T.isPrefixOf` line then "rpc_url = " <> T.pack fnUrl else line
      controller = either error id $ strDecode $ encodeUtf8 controllerAddr
  -- the order's commitment, digestFixture 1, not yet committed
  forM_ [("minCommitmentAge()", "", 60 :: Int), ("maxCommitmentAge()", "", 86400), ("commitments(bytes32)", digestFixture 1, 0)] $ \(signature, args, value) ->
    answerCall node controller (selector signature <> args) $ Returns $ uint256 value
  answerCall node controller (selector "namePolicy()") $ Returns $ addressWord fakeNamePolicy
  linkTermsOnNode node
  (t, cc, _, port) <- startGroupService svc {gsIniSections = "" : namesSection <> map onFakeNode chainSection} Nothing
  flip finally (stopGroupService t cc) $ do
    pollUntilTrue (listening port)
    let st = chatStore cc
    code <- issueNameRaw cc "name 8"
    row <- fromMaybe (error "no prepaid code row") <$> withTransaction st (`getPrepaidCode` namePrepaidCodeHash code)
    placeCode st "r1" ownerA (order "alicebakery" 1) row `shouldReturn` Right ()
    timeout waitLimit (awaitSent node 1) >>= (`shouldSatisfy` isJust)
    pollUntilTrue $ (== 2) <$> countRows cc "sx_badge_service_name_terms"
    poll t >>= (`shouldSatisfy` isNothing)

namesCodesChatTests :: SpecWith TestParams
namesCodesChatTests = describe "StoreService names: prepaid codes" $ do
  it "issues a prepaid code on /issue name for 2 years by default and for the years given, answering the usage to /bulk name" testGroupNameCodes
  it "issues at most 10 name codes a day in the group, not counting console codes" testGroupNameCodesPerDay
  it "issues prepaid codes on //issue name, checks them through the names router and revokes them on //revoke" testOperatorNameCodes
  it "revokes a prepaid code in the code's turn, after a reservation holding it" testRevokeTakesCodeTurn

testGroupNameCodes :: HasCallStack => TestParams -> IO ()
testGroupNameCodes ps = do
  svc <- prepareGroupService ps
  runWithOwner svc {gsIniSections = "" : codesOnlyNames <> chainSection} $ \cc _ _ alice -> do
    one <- prepaidIn . T.stripPrefix "Code: " =<< replyTo cc alice "/issue name 8"
    namePrepaidCodeLength one `shouldBe` 8
    prepaidTerms cc one `shouldReturn` Just (8, 2, CPSFree)
    five <- prepaidIn . T.stripPrefix "Code: " =<< replyTo cc alice "/issue name 7 years 5"
    prepaidTerms cc five `shouldReturn` Just (7, 5, CPSFree)
    replyTo cc alice "/bulk name 6 count 3" `shouldReturn` bulkUsage
    replyTo cc alice "/issue name 8 years 11" `shouldReturn` issueUsage
    countRows cc "sx_badge_service_name_prepaid_codes" `shouldReturn` 2

testGroupNameCodesPerDay :: HasCallStack => TestParams -> IO ()
testGroupNameCodesPerDay ps = do
  svc <- prepareGroupService ps
  runWithOwner svc {gsIniSections = "" : codesOnlyNames <> chainSection} $ \cc _ _ alice -> do
    replicateM_ 2 $ issueNameRaw cc "name 6"
    replicateM_ 10 $ T.isPrefixOf "Code: " <$> replyTo cc alice "/issue name 6" `shouldReturn` True
    replyTo cc alice "/issue name 6" `shouldReturn` "At most 10 name codes are issued in a day, 10 so far. Issue more with //issue name in --run-cli."
    countRows cc "sx_badge_service_name_prepaid_codes" `shouldReturn` 12
    countRows cc "sx_badge_service_name_prepaid_codes WHERE issued_in_group = 1" `shouldReturn` 10

testOperatorNameCodes :: HasCallStack => TestParams -> IO ()
testOperatorNameCodes ps = do
  svc <- prepareGroupService ps
  (t, cc, _, port) <- startGroupService svc {gsIniSections = "" : codesOnlyNames <> chainSection} Nothing
  flip finally (stopGroupService t cc) $ do
    pollUntilTrue (listening port)
    forM_ ["name", "name 5", "name 64", "name 07", "name 7 years 1", "name 7 years 11", "name 7 gratis", "name 6 unpaid"] $ \args ->
      (args,) <$> issueRaw cc args `shouldReturn` (args, Left badgeCmdUsage)
    forM_ [(5, 2), (64, 2), (7, 1), (7, 11)] $ \(len, years) ->
      ((len, years),) . isLeft <$> issueNameCode cc IssuedInConsole len years CPSFree `shouldReturn` ((len, years), True)
    countRows cc "sx_badge_service_name_prepaid_codes" `shouldReturn` 0
    paid <- issueNameRaw cc "name 7 years 3 paid"
    prepaidTerms cc paid `shouldReturn` Just (7, 3, CPSPaid)
    let check code = checkPrepaid port (formatNameCode (NCPrepaid code))
        coverage :: Int -> Int -> Text -> (Int, Maybe J.Value)
        coverage minLength years state = (200, Just $ J.object ["minLength" J..= minLength, "years" J..= years, "state" J..= state])
        revoke = revokeRaw cc . formatNameCode . NCPrepaid
    check paid `shouldReturn` coverage 7 3 "unused"
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
    withTransaction st (\db -> spendLandedPrepaidCode db spentId (RegistrationId "r2") spentAt) `shouldReturn` True
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
  it "answers an owner's /balance, /record_refund and /close in the group, worded for the group, refusing as the console does" testGroupMoneyCmds
  it "closes a failed paid order by its invoice id, recording the refund of its price, and refuses an unknown reference and an order no longer failed" testCloseRecorded
  it "refuses //close without [names], and answers the usage to a malformed one" testCloseNamesOff

balanceRaw :: HasCallStack => ChatController -> Text -> IO (Either Text Text)
balanceRaw cc args = operatorCmd cc ("//balance " <> args)

recordRefundRaw :: HasCallStack => ChatController -> Text -> IO (Either Text Text)
recordRefundRaw cc args = operatorCmd cc ("//record_refund " <> args)

closeRaw :: HasCallStack => ChatController -> Text -> IO (Either Text Text)
closeRaw cc args = operatorCmd cc ("//close " <> args)

testCloseRecorded :: HasCallStack => TestParams -> IO ()
testCloseRecorded ps = withFakeEthNode $ \node@FakeEthNode {fnUrl} -> do
  svc <- prepareGroupService ps
  let registrar = address $ encodeUtf8 registrarAddr
  (t, cc, _, port) <- startGroupService svc {gsIniSections = "" : namesSection <> withRpcUrl (T.pack fnUrl)} Nothing
  flip finally (stopGroupService t cc) $ do
    pollUntilTrue (listening port)
    let st = chatStore cc
        paidBy = paidByBtc "f"
    now <- getCurrentTime
    Balances.placeOrderRow st "f" False 10000 (addUTCTime 3600 now)
    unpaid <- reg st "f"
    write st unpaid {nrPhase = PFailed, nrFailedAt = Just t0} `shouldReturn` Written 2
    answerCall node registrar (ownerOfSelector <> labelBytes "f") $ Reverts ""
    closeRaw cc "nothing" `shouldReturn` Left "Nothing recorded: no order has the reference nothing."
    logs <- capturingStderr $
      closeRaw cc "inv-f"
        `shouldReturn` Right
          ( "Closed order f, failed at " <> minuteText t0 <> ", as taken and recorded a refund of $100.00. Check at the provider that it was not refunded by hand before; if not, refund it now, by: "
              <> paidBy
              <> "."
          )
    logs `shouldSatisfy` T.isInfixOf "names: closed order f and recorded a refund of 10000 cents"
    closeRaw cc "f" `shouldReturn` Left "Nothing recorded: the order of f is not a failed order paid by invoice or balance."
    Balances.figures st "f" t0 `shouldReturn` Just (0, 10000, 10000, Nothing)
    balanceRaw cc "f" `shouldReturn` Right ("Order f: balance $0.00 (credited $100.00, refunded $100.00). Paid by: " <> paidBy <> ". Nothing to refund.")
    Balances.placeOrderRow st "g" False 10000 (addUTCTime 3600 now)
    unpaidG <- reg st "g"
    write st unpaidG {nrPhase = PFailed, nrFailedAt = Just t0} `shouldReturn` Written 2
    answerCall node registrar (ownerOfSelector <> labelBytes "g") $ Returns $ addressWord ownerA
    answerCall node registrar (selector "nameExpires(uint256)" <> labelBytes "g") $ Returns $ uint256 (2000000000 :: Int)
    registeredLogs <- capturingStderr $ closeRaw cc "inv-g" `shouldReturn` Left "Nothing refunded: the register of order g went through, so the order is registered now."
    registeredLogs `shouldSatisfy` T.isInfixOf "names: order g set registered by //close, as its register went through"
    Balances.figures st "g" t0 `shouldReturn` Nothing

testGroupMoneyCmds :: HasCallStack => TestParams -> IO ()
testGroupMoneyCmds ps = withFakeEthNode $ \node@FakeEthNode {fnUrl} -> do
  svc <- prepareGroupService ps
  let registrar = address $ encodeUtf8 registrarAddr
  runWithOwner svc {gsIniSections = "" : namesSection <> withRpcUrl (T.pack fnUrl)} $ \cc _ _ alice -> do
    let st = chatStore cc
    now <- getCurrentTime
    Balances.placeOrderRow st "s" False 10000 (addUTCTime 3600 now)
    unpaid <- reg st "s"
    write st unpaid {nrPhase = PFailed, nrFailedAt = Just t0} `shouldReturn` Written 2
    answerCall node registrar (ownerOfSelector <> labelBytes "s") $ Reverts ""
    replyTo cc alice "/close nothing" `shouldReturn` "Nothing recorded: no order has the reference nothing."
    replyTo cc alice "/close inv-s"
      `shouldReturn` ("Closed order s, failed at " <> minuteText t0 <> ", as taken and recorded a refund of $100.00. Check at the provider that it was not refunded by hand before; if not, refund it now, by: " <> paidByBtc "s" <> ".")
    Balances.credit st "s" 2505
    replyTo cc alice "/balance s"
      `shouldReturn` ("Order s: balance $25.05 (credited $125.05, refunded $100.00). Paid by: " <> paidByBtc "s" <> ". To record its refund: /record_refund s 2505")
    replyTo cc alice "/record_refund inv-s 2500" `shouldReturn` "Nothing recorded: the balance is $25.05 now, not $25.00."
    replyTo cc alice "/record_refund inv-s 2505" `shouldReturn` ("Recorded a refund of $25.05 for order s. Refund it now, by: " <> paidByBtc "s" <> ".")
    Balances.figures st "s" t0 `shouldReturn` Just (0, 12505, 12505, Nothing)

testCloseNamesOff :: HasCallStack => TestParams -> IO ()
testCloseNamesOff ps = withStoreService ps $ \_ _ cc -> do
  Balances.source (chatStore cc) "s" 10000
  closeRaw cc "s" `shouldReturn` Left "Names are not enabled on this service."
  forM_ ["", "s junk"] $ \args -> (args,) <$> closeRaw cc args `shouldReturn` (args, Left badgeCmdUsage)
  Balances.figures (chatStore cc) "s" t0 `shouldReturn` Just (10000, 10000, 0, Nothing)

minuteText :: UTCTime -> Text
minuteText = T.pack . formatTime defaultTimeLocale "%Y-%m-%d %H:%M UTC"

testRefundRecorded :: HasCallStack => TestParams -> IO ()
testRefundRecorded ps = withStoreService ps $ \_ _ cc -> do
  let st = chatStore cc
      paidBy = paidByBtc "s"
  Balances.source st "s" 10000
  balanceRaw cc "inv-s" `shouldReturn` Right ("Order s: balance $100.00 (credited $100.00, refunded $0.00). Paid by: " <> paidBy <> ". To record its refund: //record_refund inv-s 10000")
  logs <- capturingStderr $ recordRefundRaw cc "inv-s 10000" `shouldReturn` Right ("Recorded a refund of $100.00 for order s. Refund it now, by: " <> paidBy <> ".")
  logs `shouldSatisfy` T.isInfixOf "names: recorded a refund of 10000 cents for order s"
  Balances.figures st "s" t0 `shouldReturn` Just (0, 10000, 10000, Nothing)
  balanceRaw cc "inv-s" `shouldReturn` Right ("Order s: balance $0.00 (credited $100.00, refunded $100.00). Paid by: " <> paidBy <> ". Nothing to refund.")
  Balances.credit st "s" 2505
  recordRefundRaw cc "s 2505" `shouldReturn` Right ("Recorded a refund of $25.05 for order s. Refund it now, by: " <> paidBy <> ".")
  Balances.figures st "s" t0 `shouldReturn` Just (0, 12505, 12505, Nothing)

testRefundRefusals :: HasCallStack => TestParams -> IO ()
testRefundRefusals ps = withStoreService ps $ \_ _ cc -> do
  let st = chatStore cc
  Balances.placeOrderRow st "plain" False 20000 t0
  Balances.source st "s" 10000
  balanceRaw cc "nothing" `shouldReturn` Left "No order has the reference nothing."
  recordRefundRaw cc "nothing 10000" `shouldReturn` Left "Nothing recorded: no order has the reference nothing."
  balanceRaw cc "inv-plain" `shouldReturn` Left "Order plain has no balance."
  recordRefundRaw cc "inv-plain 20000" `shouldReturn` Left "Nothing recorded: the order of inv-plain has no balance."
  recordRefundRaw cc "s 9000" `shouldReturn` Left "Nothing recorded: the balance is $100.00 now, not $90.00."
  recordRefundRaw cc "s 10001" `shouldReturn` Left "Nothing recorded: the balance is $100.00 now, not $100.01."
  forM_ ["", "s", "s 0", "s -5", "s 100.00", "s 10000 junk", "s 99999999999999999999"] $ \args ->
    (args,) <$> recordRefundRaw cc args `shouldReturn` (args, Left badgeCmdUsage)
  forM_ ["", "s 10000"] $ \args -> (args,) <$> balanceRaw cc args `shouldReturn` (args, Left badgeCmdUsage)
  operatorCmd cc "//refund s 10000" `shouldReturn` Left badgeCmdUsage
  Balances.figures st "s" t0 `shouldReturn` Just (10000, 10000, 0, Nothing)

-- | The source with a balance of $100.00 and the holder holding it, whose invoice expires at the given time.
heldSource :: ChatController -> Text -> Text -> UTCTime -> IO ()
heldSource cc src holder expiry = do
  Balances.source (chatStore cc) src 10000
  Balances.holder (chatStore cc) holder src 10000 expiry `shouldReturn` Right ()

heldRefusals :: HasCallStack => ChatController -> Text -> Text -> IO ()
heldRefusals cc src held = do
  balanceRaw cc src `shouldReturn` Right ("Order " <> src <> ": balance $100.00 (credited $100.00, refunded $0.00). Paid by: " <> paidByBtc src <> ". It cannot be refunded now: " <> held <> ".")
  recordRefundRaw cc ("inv-" <> src <> " 10000") `shouldReturn` Left ("Nothing recorded: " <> held <> ".")
  Balances.figures (chatStore cc) src t0 >>= (`shouldSatisfy` \case Just (10000, 10000, 0, Just _) -> True; _ -> False)

recordedS :: Either Text Text
recordedS = Right ("Recorded a refund of $100.00 for order s. Refund it now, by: " <> paidByBtc "s" <> ".")

paidByBtc :: Text -> Text
paidByBtc r = "order " <> r <> " of " <> addressText ownerA <> ": $100.00 by btc on invoice inv-" <> r <> " (provider reference ref-inv-" <> r <> ")"

testRefundOpenHolder :: HasCallStack => TestParams -> IO ()
testRefundOpenHolder ps = withStoreService ps $ \_ _ cc -> do
  now <- getCurrentTime
  let expiry = addUTCTime 3600 now
  heldSource cc "s" "h" expiry
  heldRefusals cc "s" $ "held by order h, its invoice open until " <> minuteText expiry <> "; run the command again then"
  waiters <- newWaiters
  cancelOpenInvoice (chatStore cc) waiters Nothing (InvoiceId "inv-h") now `shouldReturn` True
  recordRefundRaw cc "s 10000" `shouldReturn` recordedS

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
    -- | settings added to [listener]
    gsListener :: [Text],
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
      pure GroupSvc {gsCfg, gsKey, gsStaticDir, gsIniSections = [], gsListener = [], gsPs = ps}

runGroupService :: HasCallStack => GroupSvc -> (ChatController -> ServiceState -> Int64 -> IO a) -> IO a
runGroupService svc = runGroupServiceAs svc (Just testGroupConfig)

runGroupServiceAs :: HasCallStack => GroupSvc -> Maybe GroupConfig -> (ChatController -> ServiceState -> Int64 -> IO a) -> IO a
runGroupServiceAs svc groupCfg action = do
  (t, cc, env, port) <- startGroupService svc groupCfg
  -- The web listener starts after group start-up, so the stop cannot land inside start-up's database calls.
  let started = pollUntilTrue (listening port) >> pollUntil (getStoredGroup cc)
      -- A stop inside a lane's database call leaves a statement open, and closing the store then fails.
      settle gid = when (isJust groupCfg) $ settleServingLane cc env gid
  (started >>= \ManagedGroup {mgGroupId} -> action cc env mgGroupId <* settle mgGroupId) `finally` stopGroupService t cc

settleServingLane :: HasCallStack => ChatController -> ServiceState -> Int64 -> IO ()
settleServingLane cc env gid = do
  status <- memberStatus cc gid badgeBotName
  forM_ status $ \s -> unless (s `elem` ["removed", "left", "deleted"]) $ settleLane cc env gid

-- It returns once chat has started, while group start-up may still run; the Int is its web listener's port.
startGroupService :: HasCallStack => GroupSvc -> Maybe GroupConfig -> IO (Async (), ChatController, ServiceState, Int)
startGroupService svc@GroupSvc {gsCfg} groupCfg = do
  (opts, serviceCfg, port) <- groupServiceOpts svc groupCfg
  env <- newServiceState
  t <- async $ storeService opts (Just serviceCfg) gsCfg env
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

-- It writes the service's ini on free ports, the names site's too with [names]; Nothing omits the [group] section.
groupServiceOpts :: GroupSvc -> Maybe GroupConfig -> IO (StoreServiceOpts, ServiceConfig, Int)
groupServiceOpts GroupSvc {gsKey = BadgeIssuerKey {secretKey}, gsStaticDir, gsIniSections, gsListener, gsPs = ps@TestParams {tmpPath}} groupCfg = do
  (port, sock) <- openFreePort
  (namesPort, namesSock) <- openFreePort
  close sock >> close namesSock
  let iniPath = tmpPath </> "badge_group.ini"
      namesSite
        | "[names]" `elem` gsIniSections = ["names_static_dir = " <> gsStaticDir, "names_port = " <> show namesPort]
        | otherwise = []
  B.writeFile iniPath . encodeUtf8 . T.pack $
    unlines $
      [ "[listener]",
        "host = 127.0.0.1",
        "badges_port = " <> show port,
        "badges_static_dir = " <> gsStaticDir
      ]
        <> map T.unpack gsListener
        <> namesSite
        <> maybe [] groupSection groupCfg
        <> map T.unpack gsIniSections
  Right serviceCfg <- readServiceConfig iniPath
  pure ((mkStoreServiceOpts ps secretKey) {noAddress = True}, serviceCfg, port)
  where
    groupSection GroupConfig {gDisplayName, gDescription} =
      ["", "[group]", "display_name = " <> T.unpack gDisplayName]
        <> maybe [] (\d -> ["description = " <> T.unpack d]) gDescription

withGroupOwner :: HasCallStack => TestParams -> (BadgeIssuerKey -> ChatController -> ServiceState -> TestCC -> IO ()) -> IO ()
withGroupOwner ps action = do
  svc@GroupSvc {gsKey} <- prepareGroupService ps
  runWithOwner svc $ \cc env _ -> action gsKey cc env

runWithOwner :: HasCallStack => GroupSvc -> (ChatController -> ServiceState -> Int64 -> TestCC -> IO a) -> IO a
runWithOwner svc@GroupSvc {gsPs} action = runGroupService svc $ \cc env gid -> withOwnerJoined gsPs cc env gid $ action cc env gid

withOwnerJoined :: HasCallStack => TestParams -> ChatController -> ServiceState -> Int64 -> (TestCC -> IO a) -> IO a
withOwnerJoined ps cc env gid action =
  withNewTestChat ps "alice" aliceProfile $ \alice -> do
    joinGroup cc alice
    waitMemberRole cc gid "alice" "owner"
    drainUntil alice ["#" <> groupName <> ": " <> botName <> " changed your role from member to owner", "changed member admission rules"]
    joined <- lastMessageId cc
    r <- action alice
    settleServingLane cc env gid
    awaitBroadcasts cc joined "alice" alice
    drainConsole alice
    pure r

-- Bob joins after the first member and is connected to her.
withMemberJoined :: HasCallStack => TestParams -> ChatController -> ServiceState -> Int64 -> TestCC -> IO () -> (TestCC -> IO a) -> IO a
withMemberJoined ps cc env gid alice admit action =
  withNewTestChat ps "bob" bobProfile $ \bob -> do
    joinGroup cc bob
    drainUntil alice ["#" <> groupName <> ": new member bob is connected"]
    admit
    joined <- lastMessageId cc
    r <- action bob
    settleServingLane cc env gid
    awaitBroadcasts cc joined "bob" bob
    drainConsole bob
    pure r

joinGroup :: HasCallStack => ChatController -> TestCC -> IO ()
joinGroup cc member = groupLink cc >>= send member . ("/c " <>)

admitMember :: HasCallStack => ChatController -> Int64 -> TestCC -> Text -> IO ()
admitMember cc gid moderator name = do
  memberStatus cc gid name `shouldReturn` Just "pending_review"
  send moderator ("/accept member #" <> groupName <> " " <> T.unpack name)
  drainUntil moderator ["#" <> groupName <> ": " <> T.unpack name <> " accepted"]
  pollUntilTrue $ (`elem` [Just "connected", Just "complete"]) <$> memberStatus cc gid name

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
  -- The lane drains only after the group is resolved, so a handled command proves the start-up sync has run.
  runGroupService svc $ \cc env gid -> do
    awaitLane cc env
    profileUpdatedAt cc gid `shouldReturn` advertisedAt

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
    clearMemberAdmission cc gid
    withMemberJoined ps cc env gid alice (pure ()) $ \bob -> do
      waitMemberRole cc gid "alice" "owner"
      awaitLaneOnConsoles cc env [alice, bob]
      memberRoles cc gid `shouldReturn` [("alice", "owner"), ("bob", "member")]
      profileMemberAdmission cc gid `shouldReturn` Just reviewAll

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
  (opts, serviceCfg, _) <- groupServiceOpts svc (Just testGroupConfig)
  env <- newServiceState
  -- Falling through to create would keep the service running, so the timeout would fire instead.
  ended <- timeout waitLimit (try $ storeService opts (Just serviceCfg) gsCfg env)
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
    withMemberJoined ps cc env gid alice (admitMember cc gid alice "bob") $ \bob -> do
      -- The lane must handle bob's join before the flag is cleared, or it would act on the cleared flag in this run.
      awaitLaneOnConsoles cc env [alice, bob]
      action cc gid alice bob
      executeSql cc "UPDATE sx_badge_service_group SET owner_bootstrapped = 0"

testFirstJoinerPromoted :: HasCallStack => TestParams -> IO ()
testFirstJoinerPromoted ps = do
  svc <- prepareGroupService ps
  runWithOwner svc $ \cc env gid alice ->
    withMemberJoined ps cc env gid alice (admitMember cc gid alice "bob") $ \bob -> do
      waitMemberRole cc gid "bob" "member"
      -- The bot queues bob's join before it introduces him, so alice's code proves the join was handled.
      sendGroupCmd alice "/issue supporter"
      waitCodeOfType cc "supporter"
      memberRoles cc gid `shouldReturn` [("alice", "owner"), ("bob", "member")]
      let codeReply = "#" <> groupName <> " " <> botName <> "> Code: SB-"
      drainUntil alice [codeReply]
      drainUntil bob [codeReply, "#" <> groupName <> ": member alice"]

testMemberReviewRequired :: HasCallStack => TestParams -> IO ()
testMemberReviewRequired ps = do
  svc <- prepareGroupService ps
  runGroupService svc $ \cc env gid -> do
    awaitLane cc env
    profileMemberAdmission cc gid `shouldReturn` Nothing
  runWithOwner svc $ \cc _ gid _ -> do
    profileMemberAdmission cc gid `shouldReturn` Just reviewAll
    clearMemberAdmission cc gid
  runGroupService svc $ \cc _ gid -> pollUntilTrue $ (== Just reviewAll) <$> profileMemberAdmission cc gid

testRejoinerReviewed :: HasCallStack => TestParams -> IO ()
testRejoinerReviewed ps = do
  svc <- prepareGroupService ps
  runWithOwner svc $ \cc env gid alice ->
    withMemberJoined ps cc env gid alice (admitMember cc gid alice "bob") $ \bob -> do
      send alice ("/rm #" <> groupName <> " bob")
      drainUntil bob ["#" <> groupName <> ": alice removed you from the group"]
      pollUntilTrue $ (== Just "removed") <$> memberStatus cc gid "bob"
      joinGroup cc bob
      drainUntil bob ["connecting to group moderators for admission to group", "(Alice) is connected"]
      drainUntil alice ["is connected and pending review"]
      memberStatus cc gid "bob_1" `shouldReturn` Just "pending_review"

testJoinerAcceptance :: HasCallStack => TestParams -> IO ()
testJoinerAcceptance ps = do
  svc <- prepareGroupService ps
  runGroupService svc $ \cc _ gid -> do
    startedCC <- newTMVarIO cc
    managed <- groupInfoOf cc gid
    other <-
      sendChatCmdStr cc "/g othergroup" >>= \case
        Right (CRGroupCreated _ g) -> pure g
        r -> error $ "group not created: " <> show r
    let acceptance g = joinerAcceptance startedCC g GroupLinkInfo {groupId = gid, memberRole = GRMember} bobProfile
        accepted = Right (GAAccepted, GRMember)
        held = Right (GAPendingReview, GRMember)
    mapM acceptance [managed, other] `shouldReturn` [accepted, accepted]
    executeSql cc "UPDATE sx_badge_service_group SET owner_bootstrapped = 1"
    profileMemberAdmission cc gid `shouldReturn` Nothing
    mapM acceptance [managed, other] `shouldReturn` [held, accepted]
    executeSql cc "ALTER TABLE sx_badge_service_group RENAME TO sx_badge_service_group_hidden"
    (acceptance other `shouldReturn` held) `finally` executeSql cc "ALTER TABLE sx_badge_service_group_hidden RENAME TO sx_badge_service_group"

testReviewRestored :: HasCallStack => TestParams -> IO ()
testReviewRestored ps = do
  svc <- prepareGroupService ps
  runWithOwner svc $ \cc _ gid alice -> do
    send alice ("/set admission review #" <> groupName <> " off")
    drainUntil alice [botName <> " updated group #" <> groupName]
    profileMemberAdmission cc gid `shouldReturn` Just reviewAll

testReviewRepairedOnJoin :: HasCallStack => TestParams -> IO ()
testReviewRepairedOnJoin ps = do
  svc <- prepareGroupService ps
  runWithOwner svc $ \cc env gid alice -> do
    clearMemberAdmission cc gid
    withMemberJoined ps cc env gid alice (admitMember cc gid alice "bob") $ \_ ->
      pollUntilTrue $ (== Just reviewAll) <$> profileMemberAdmission cc gid

testModeratorAdmitsPendingMember :: HasCallStack => TestParams -> IO ()
testModeratorAdmitsPendingMember ps = do
  svc <- prepareGroupService ps
  runWithOwner svc $ \cc env gid alice ->
    withMemberJoined ps cc env gid alice (admitMember cc gid alice "bob") $ \bob ->
      withNewTestChat ps "cath" cathProfile $ \cath -> do
        send alice ("/mr #" <> groupName <> " bob moderator")
        waitMemberRole cc gid "bob" "moderator"
        drainUntil bob ["#" <> groupName <> ": alice changed your role from member to moderator"]
        joinGroup cc cath
        drainUntil bob ["#" <> groupName <> ": new member cath is connected and pending review"]
        code <- extractCode <$> replyTo cc alice "/issue supporter"
        awaitOnConsole bob ("Code: " <> code)
        receivedItemsWithCode cath code `shouldReturn` 0
        admitMember cc gid bob "cath"
        pollUntilTrue $ (`elem` [Just "connected", Just ("complete" :: Text)]) <$> queryFirst (chatController cath) "SELECT member_status FROM group_members WHERE member_category = 'user'"
        receivedItemsWithCode cath code `shouldReturn` 0
        drainConsole cath
  where
    receivedItemsWithCode member code = countRows (chatController member) ("chat_items WHERE item_sent = 0 AND item_text LIKE '%" <> T.unpack code <> "%'")

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
      withNewTestChat ps "bob" bobProfile $ \bob -> do
        joinGroup cc bob
        drainUntil bob ["connecting to group moderators for admission to group"]
        pollUntilTrue $ (== Just (1 :: Int)) <$> queryFirst cc ("SELECT members_require_attention FROM groups WHERE group_id = " <> show gid)
        memberStatus cc gid "bob" `shouldReturn` Just "pending_review"
        awaitLaneOnConsoles cc env [alice]
        drainConsole bob
      memberRoles cc gid `shouldReturn` [("alice", "member"), ("bob", "member")]
      drainConsole alice

testRoleGatedIssue :: HasCallStack => TestParams -> IO ()
testRoleGatedIssue ps = do
  svc <- prepareGroupService ps
  runWithOwner svc $ \cc _ gid alice ->
    withNewTestChat ps "bob" bobProfile $ \bob ->
      withNewTestChat ps "cath" cathProfile $ \cath -> do
        let preMember name = "#" <> groupName <> ": member " <> name
            newMember name = "#" <> groupName <> ": new member " <> name <> " is connected"
            usageReply = "#" <> groupName <> " " <> botName <> "> Usage:"
        joinGroup cc bob
        drainUntil alice [newMember "bob"]
        admitMember cc gid alice "bob"
        joinGroup cc cath
        drainUntil alice [newMember "cath"]
        admitMember cc gid alice "cath"
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
        waitStoredItem cc issueUsage
        codeCount cc `shouldReturn` 2
        drainUntil alice [usageReply]
        drainUntil bob [usageReply, preMember "alice", newMember "cath"]
        drainUntil cath [usageReply]
        drainConsole bob
        drainConsole cath

testBlockedMemberCommandIgnored :: HasCallStack => TestParams -> IO ()
testBlockedMemberCommandIgnored ps = do
  svc <- prepareGroupService ps
  runWithOwner svc $ \cc env gid alice ->
    withMemberJoined ps cc env gid alice (admitMember cc gid alice "bob") $ \bob -> do
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

testGroupNameCodesNamesOff :: HasCallStack => TestParams -> IO ()
testGroupNameCodesNamesOff ps =
  withGroupOwner ps $ \_ cc _ alice -> do
    replyTo cc alice "/issue name 8" `shouldReturn` namesOffReply
    countRows cc "sx_badge_service_name_prepaid_codes" `shouldReturn` 0
    unissued <- formatNameCode . NCPrepaid <$> randomNamePrepaidCode (random cc) 8
    revokeInGroup cc alice unissued "No such code."
  where
    namesOffReply = "Names are not enabled on this service, so name codes cannot be issued."

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

issueUsage :: Text
issueUsage = "Usage: /issue <kind> [<length>] [years <Y> | months <M>] [uses <N>]\nname <length>: length 6–63, years 2–10\nsupporter, legend, investor: months 1–255, uses 1–1000\ne.g. /issue name 6 years 5 · /issue supporter months 12"

bulkUsage :: Text
bulkUsage = "Usage: /bulk <kind> [months <M>] count <N>\nsupporter, legend, investor: months 1–255, count 1–100\ne.g. /bulk legend months 3 count 10"

-- The list is spelled out rather than taken from the service, so a change there fails here.
expectedCommands :: [ChatBotCommand]
expectedCommands =
  [ CBCCommand "issue" "Generate a code" (Just "<name <length>|supporter|legend|investor> [years|months <N>] [uses <N>]"),
    CBCCommand "bulk" "Generate many codes" (Just "<supporter|legend|investor> [months <N>] count <N>"),
    CBCCommand "revoke" "Revoke a code" (Just "<code>"),
    CBCCommand "balance" "Show an order's balance" (Just "<reference>"),
    CBCCommand "record_refund" "Record a balance refund" (Just "<reference> <cents>"),
    CBCCommand "close" "Close a failed order" (Just "<reference>")
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

profileMemberAdmission :: HasCallStack => ChatController -> Int64 -> IO (Maybe GroupMemberAdmission)
profileMemberAdmission cc gid =
  queryOne cc "no group profile to read member admission from" (groupProfileSql "p.member_admission" gid)

groupInfoOf :: HasCallStack => ChatController -> Int64 -> IO GroupInfo
groupInfoOf cc gid =
  sendChatCmd cc (APIGroupInfo gid) >>= \case
    Right (CRGroupInfo _ g) -> pure g
    r -> error $ "group info failed: " <> show r

reviewAll :: GroupMemberAdmission
reviewAll = GroupMemberAdmission {review = Just MCAll}

clearMemberAdmission :: ChatController -> Int64 -> IO ()
clearMemberAdmission cc gid =
  withTransaction (chatStore cc) $ \db ->
    DB.execute db "UPDATE group_profiles SET member_admission = NULL WHERE group_profile_id IN (SELECT group_profile_id FROM groups WHERE group_id = ?)" (Only gid)

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

lastMessageId :: ChatController -> IO Int64
lastMessageId cc = fromMaybe 0 <$> queryFirst cc "SELECT COALESCE(MAX(message_id), 0) FROM messages"

awaitBroadcasts :: HasCallStack => ChatController -> Int64 -> Text -> TestCC -> IO ()
awaitBroadcasts cc joined name TestCC {chatController = memberCC} = do
  lastBroadcast <- listToMaybe <$> queryRows cc lastBroadcastSql
  forM_ lastBroadcast $ \(gid, sharedMsgId) -> do
    status <- memberStatus cc gid name
    when (maybe False (`notElem` ["removed", "left", "deleted"]) status) $
      pollUntilTrue $ isJust <$> (queryFirst memberCC (receivedSql sharedMsgId) :: IO (Maybe Int64))
  where
    lastBroadcastSql =
      "SELECT group_id, " <> hexSharedMsgId <> " FROM messages WHERE msg_sent = 1 AND message_id > " <> show joined
        <> " AND chat_msg_event IN ('x.msg.new', 'x.msg.update', 'x.msg.del') ORDER BY message_id DESC LIMIT 1"
    receivedSql sharedMsgId = "SELECT message_id FROM messages WHERE msg_sent = 0 AND " <> hexSharedMsgId <> " = '" <> T.unpack sharedMsgId <> "'"

hexSharedMsgId :: String
#if defined(dbPostgres)
hexSharedMsgId = "encode(shared_msg_id, 'hex')"
#else
hexSharedMsgId = "hex(shared_msg_id)"
#endif

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

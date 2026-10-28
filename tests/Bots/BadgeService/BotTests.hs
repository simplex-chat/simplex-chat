{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PostfixOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -fno-warn-ambiguous-fields #-}

module Bots.BadgeService.BotTests where

import BadgeService.Codes (issueOneCode)
import BadgeService.Config (AppleStoreConfig (..), BadgeIssuerKey (..), PlayStoreConfig (..), ServiceConfig (..), readServiceConfig)
import BadgeService.Group (GroupEvent)
import BadgeService.StoreReceipts.Apple (appleSubscriptionChecker, verifyAppleTransaction)
import Bots.BadgeService.ConfigTests (withIssuer)
import Bots.BadgeService.FakeAppStore (FakeAppStore (..), withFakeAppStore)
import Bots.BadgeService.FakePlay (FakePlay (..), PlayAnswer (..), answerPurchase, withFakePlay)
import Bots.BadgeService.FakeStore
import Bots.BadgeService.StoreVerifierTests (TestChain (..), basePlan, lineItem, ourBundleId, purchaseRecord, signedBy, subscriptionPayload, subscriptionRecord, transactionPayload, trustedChain)
import BadgeService.Options
import BadgeService.Service
import BadgeService.Store (IssuedCode (..), getBadgeCode)
import BadgeService.Store.Invoices (markCodePaid, truncateToSecond)
import BadgeService.StoreReceipts (StoreCharge (..), StoreChecks (..), StoreRefusal (..), StoreVerifier (..), SubscriptionCheck (..), SubscriptionRef (..), checkStoreSubscription, noStoreVerifier)
import BadgeService.StoreReceipts.Mock (newMockStoreVerifier)
import Simplex.Messaging.Agent.Store.DB (Binary (..))
import qualified Simplex.Messaging.Agent.Store.DB as DB
import ChatClient
import ChatTests.DBUtils
import ChatTests.Utils
import Control.Concurrent (newEmptyMVar, putMVar, takeMVar, threadDelay)
import Control.Concurrent.Async (async, cancel)
import Control.Concurrent.STM (TQueue, atomically, readTMVar, readTQueue, readTVarIO)
import Control.Monad (forM, forM_, replicateM, void, when)
import Control.Monad.Except (runExceptT)
import Control.Exception (finally)
import qualified Data.Aeson as J
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as LB
import Data.Char (toLower)
import Data.Either (isLeft, isRight)
import Data.Int (Int64)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.List (isInfixOf, isPrefixOf, stripPrefix)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map.Strict as M
import Data.Maybe (isJust, isNothing)
import Data.String (fromString)
import System.Timeout (timeout)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Calendar (addDays, fromGregorian, toGregorian)
import Data.Time.Clock (NominalDiffTime, UTCTime (..), addUTCTime, diffUTCTime, getCurrentTime, nominalDay)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.Word (Word32)
import Simplex.Chat.Badges (BadgeCredential (..), BadgeInfo (..), BadgeMasterKey, BadgeType (..), badgeGraceInterval, generateMasterKey)
import Simplex.Chat.Badges.Code (BadgeCode, badgeCodeHash, badgeCodeText, formatBadgeCode, parseBadgeCode, randomBadgeCode)
import Simplex.Chat.Badges.Ledger (addMonths, balanceChecked, creditTypeTag, debitTypeTag, emptyEntry, endOfMondayAfter)
import qualified Simplex.Chat.Badges.Ledger as L
import Simplex.Chat.Badges.Service
import Simplex.Chat.Badges.Types (BadgeCodePaymentStatus (..))
import qualified Simplex.Chat.Badges.Types as BT
import Simplex.Chat.Bot.Store (withDB')
import Simplex.Chat.Controller (ChatConfig (..), ChatController (..), ChatError (..), ChatErrorType (..), ChatResponse (CRBadgeState, CRCustomChatResponse))
import Simplex.Chat.Core (sendChatCmdStr)
import Simplex.Chat.Library.Commands (heldChargeAttempts, resumeHoldLimit, subscriptionRecoveryWindow)
import Simplex.Chat.Options (ChatOpts (..), CoreChatOpts (..))
import Simplex.Chat.Options.DB
import Simplex.Chat.PaymentService (ServicePayment (..), googlePurchaseRef)
import Simplex.Chat.PaymentService.Types (InvoiceId (..), PaymentProvider (PPGoogle), StoreTransactionRef (StoreTransactionRef))
import Simplex.Chat.Store.Badges (StoreHoldKind (..), clearShownBadge, getLatestIssuedCredential, getNextHeldStoreReceipt, holdStoreReceipt, holdStoreTransfer, reholdStoreReceipt, storeReceiptHeldFor)
import Simplex.Chat.Store.Profiles (getUser, setUserBadge)
import Simplex.Messaging.Agent (disposeAgentClient)
import Simplex.Messaging.Agent.Env.SQLite (AgentConfig (..))
import Simplex.Messaging.Agent.RetryInterval (RetryInterval (..))
import Simplex.Messaging.Agent.Store.Common (DBStore, withTransaction)
import Simplex.Messaging.Agent.Store.DB (BoolInt (..))
import Simplex.Chat.Types (ChatPeerType (..), Profile (..))
import qualified Simplex.Messaging.Crypto as C
import Simplex.Messaging.Crypto.BBS (BBSSecretKey, bbsKeyGen)
import Simplex.Messaging.Encoding.String (strDecode, strEncode, textEncode)
import Simplex.Messaging.Util (safeDecodeUtf8, tshow)
import Data.X509 (encodeSignedObject)
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory, (</>))
import UnliftIO.Temporary (withTempDirectory)
#if defined(dbPostgres)
import Database.PostgreSQL.Simple (Only (..))
#else
import Database.SQLite.Simple (Only (..))
#endif
import Test.Hspec hiding (it)

badgeServiceTests :: SpecWith TestParams
badgeServiceTests = do
  it "should answer unsupported_version to unsupported command" testBadgeServiceUnsupported
  it "should redeem an issued code into a badge a contact sees" testRedeemBadgeCode
  it "should return the same badge when the same code is redeemed twice" testRedeemBadgeCodeTwice
  it "should answer code_invalid to an unknown code, indistinguishably from a malformed one" testRedeemUnknownCode
  it "should tell a second profile redeeming the same code that it is used" testRedeemSameCodeOtherProfile
  it "should refuse a second code while a badge is held, leaving it unspent" testRedeemSecondCode
  it "should refuse a code that has not been paid for" testRedeemUnpaidCode
  it "should refuse a badge code past its redemption deadline" testExpiredCode
  it "should keep answering a code redeemed before its deadline" testRedeemedBeforeTheDeadline
  it "should refuse a revoked badge code, and report a second revoke as already revoked" testRevokedCode
  it "should refuse to revoke a code that was redeemed, and keep its badge" testRevokeRedeemedCode
  it "should answer revoking an unknown code as no such code" testRevokeUnknownCode
  it "should answer code_invalid to a code that is both unpaid and revoked" testRevokedUnpaidCode
  it "should refuse a revoke with trailing input, leaving the code live" testRevokeRejectsTrailingInput
  it "should refuse to issue a code with an unknown badge type or a nonsense month count" testIssueRejectsBadArguments
  it "should refuse a request whose purchaseKey is not the verified signer" testPurchaseKeyMismatch
  it "should refuse to start unless the issuer secret is the key trusted at its index" testIssuerKeyMustMatchConfig
  it "should refuse to start when the [issuer] key is not one clients trust" testIssuerIniKeyMustBeTrusted
  it "should credit a code's months and issue one credential per month" testCodeMonthsRenew
  it "should return the stored credential for a repeat inside an issued period" testRepeatInsideIssuedPeriod
  it "should redeem a multi-use code to its limit with no group to track it" testMultiUseWithoutGroup
  it "should not spend a multi-use code again when its holder redeems it after the badge ended" testMultiUseRepeatAfterExpiry
  it "should answer internal, spending no use, when a holder's stored credential is unreadable" testUnreadableCredentialIsInternal
  it "should lapse only the months that elapsed while the client was away" testLapseWhileAway
  it "should round the last month's expiry up to the end of the Monday after it" testLastMonthExpiryRounds
  it "should sign a renewal with the master key stored on the purchase" testRenewalSignsWithStoredMasterKey
  it "should leave the client holding the same ledger rows as the service" testClientReplicatesLedger
  it "should renew a badge whose credential is lapsing, with no command" testWorkerRenews
  it "should take the newest issued credential by its ledger row, not by the period it ends" testLatestIssuedByRow
  it "should keep answering and renewing a holder after its partly used multi-use code is revoked" testRevokedMultiUseHolderRenews
  it "should return a multi-use holder's renewed credential on a repeat, apart from a later holder" testMultiUseHoldersRenewApart
  it "should request from the wake it set a day before the credential lapses" testRequestWakeFires
  it "should present from the wake it set at the credential's expiry" testPresentWakeFires
  it "should renew a badge whose newest ledger row is of an unknown type" testRenewsAfterUnknownEntry
  it "should catch up the months that lapsed while the client was stopped" testRenewsAfterRestart
  it "should stop showing a badge whose balance ran out, and tell contacts" testWorkerRetiresExpired
  it "should retire when entitlement ends, not when the credential expires" testRetiresWhenEntitlementEnds
  it "should alert that support ended, survive a restart, and go silent once acknowledged" testEndedAlert
  it "should raise a snoozed alert once more when the snooze lapses" testSnoozedAlertReturns
  it "should alert that renewal failed when the service refuses it" testIssueFailedAlert
  it "should wait for the shown credential to lapse before alerting on a failure that can clear" testIssueFailedWaitsForExpiry
  it "should silence an acknowledged run of failures, and alert again on the next run" testIssueFailedAckAndNewRun
  it "should record no failure when the service issues nothing because the months ran out" testNoCredentialMonthsRanOut
  it "should record a failure when the service issues nothing though months are left" testNoCredentialMonthsLeft
  it "should renew a badge on a profile that is not active, without switching to it" testRenewalKeepsActiveProfile
  it "should broadcast the current profile when a renewal presents a badge" testRenewalKeepsProfileEdits
  it "should present the month already issued when a previous pass did not" testPresentationCatchesUp
  describe "store purchases" $ do
    describe "what the store answers" $ do
      it "should credit the months a store receipt paid for and issue the first" testStorePurchase
      it "should refuse a receipt another key was credited with" testStoreReceiptUsed
      it "should refuse a receipt the store does not vouch for, with no retry" testStoreReceiptInvalid
      it "should credit a purchase the store calls a test only with credit_test_purchases on" testStoreTestPurchaseCredited
      it "should write nothing for a pending purchase, and credit it once when it settles" testStorePending
      it "should acknowledge a Play purchase before crediting it, and credit nothing when acknowledging fails" testStoreAcknowledgesBeforeCrediting
      it "should refuse a product that grants no badge" testStoreUnknownProduct
      it "should refuse a store purchase that carries an upgrade" testStoreUpgradeRefused
      it "should grant the badge of the product the receipt proves" testStoreBadgeTypeFromProduct
      it "should keep refusing invoice and receipt funding" testNonStoreFundingRefused
      it "should replay a receipt to its own key while the store is down, and to no other" testStoreReplayWhileStoreDown
      it "should answer a throwing Apple verifier as internal, and a failing or hanging Google one as retryable" testStoreVerifierFailures
    describe "claim races" $ do
      it "should answer a request that lost the claim race with the credential the winner was given" testStorePurchaseRace
      it "should refuse as receipt_used a key that lost the claim race to another key, crediting nothing" testStorePurchaseRaceOtherKey
    describe "the service's own checks" $ do
      it "should refuse a store with no verifier with no retry, and the client should hold the receipt, showing why it is not credited" testPurchaseWithNoVerifier
      it "should credit nothing when the verified transaction is not the one the evidence names" testStoreVerifiedOtherTransaction
      it "should refuse a verified quantity other than one as internal, writing nothing" testStoreQuantityRefused
      it "should refuse a store purchase whose purchaseKey is not the verified signer" testStorePurchaseKeyMismatch
      it "should refuse a Play product id it will not send to Play, without asking Play" testStoreUnsentProductId
    describe "the hand-over" $ do
      it "should hold a Play receipt until the worker credits it, and answer it as credited to its owner under any profile" testPurchaseBadge
      it "should refuse an App Store receipt that names no transaction, recording nothing" testPurchaseNamesNoTransaction
      it "should keep a receipt credited to another key as refused" testPurchaseStashReceiptUsed
      it "should hold a Play token the service will not send to Play" testPurchaseUnsentPlayToken
      it "should credit a purchase first handed over under another profile to that profile" testPurchaseStrandedUnderOtherProfile
      it "should credit a purchase to a hidden profile without naming it" testPurchaseDeliveredToHiddenProfile
      it "should record a listed transaction only into a record it already has, and a Restore hand-over as its own" testListedReceiptNotRecorded
      it "should refuse a code or an invoice while a held receipt may be credited or is an Apple subscription, and not otherwise" testRedeemRefusedWhileReceiptHeld
      it "should stop a held Apple subscription blocking an invoice or a code, or listing open, a day after its charge expired, keeping its record" testLapsedAppleChargeStopsBlocking
      it "should refuse an invoice while a held receipt's first attempt has not answered" testInvoiceRefusedWhileFirstAttemptOpen
    describe "invoice first" $ do
      it "should credit a receipt to the profile that created its invoice however late it arrives, and answer as that profile" testInvoiceOtherProfile
      it "should credit a receipt naming an unknown invoice to the presenting profile" testInvoiceUnknown
      it "should record a receipt echoing an invoice another record holds by its transaction alone, and credit it" testInvoiceHeldByAnotherRecord
      it "should list a record however old until a receipt reaching it is settled" testInvoiceAgedOut
      it "should refuse an invoice with no service configured or while a badge is held, creating no record" testInvoiceRefusedBeforeCharge
      it "should hold a receipt for an invoice while a badge is held, without attempting it" testInvoiceWhileBadgeHeld
      it "should list only the asking profile's open store purchases" testInvoiceStateOtherProfile
    describe "the receipt worker" $ do
      it "should retry a receipt the service could not verify, and credit it with no trigger at all" testStoreReceiptRetried
      it "should credit a second receipt while an earlier one waits for the store" testStoreReceiptNotBlocked
      it "should answer a refused receipt from the record, sending nothing, and not list it" testStoreRefusalKept
      it "should refuse a test purchase for good, and answer it from the record" testStoreTestPurchaseRefused
      it "should announce a settlement to an owner that is not active, and print nothing for a hidden one" testStoreSettledOtherProfile
      it "should resolve one new transaction handed over twice at once to one record" testStoreReceiptTwoAtOnce
      it "should delete a month-old record that will fund no badge, and keep held and credited ones" testStoreReceiptCleanup
      it "should keep a receipt whose attempt throws" testStoreReceiptAttemptThrows
      it "should credit a held receipt after the app restarts" testStoreReceiptAfterRestart
    describe "the store lane" $ do
      it "should answer other requests while a Play receipt is with the store, and credit it once Play answers" testPlayHoldsNoOtherRequest
      it "should route each request to its lane, judging a minute ahead" testStoreRouting
      it "should check the store at every request only with check_store_during_coverage beside the mock or credit_test_purchases, and credit test purchases only from a real store" testStoreChecksConfigured
      it "should answer other requests while a renewal check is with the store, and the renewal once the store answers" testRenewalHoldsNoOtherRequest
      it "should answer a renewal check while a Play purchase is with the store" testPurchaseHoldsNoRenewal
      it "should answer other requests while an App Store subscription is with the store, and credit it once the store answers" testAppleSubscriptionHoldsNoOtherRequest
    describe "transfers" $ do
      it "should move a store subscription to a new key, ending the old key's share, and renew it for the new key" testStoreRebind
      it "should move a payment again only once the credential of the key it last moved from has expired" testStoreRebindCapped
      it "should fund the new key from a charge the service never recorded, ending the idle old key at zero months, and grant that charge once" testStoreRebindNewCharge
      it "should answer the old key ended with no credential, without asking the store, and its purchase receipt_used" testStoreRebindOldKeyEnded
      it "should move an App Store subscription only with its latest transaction, writing nothing for an older one" testStoreRebindStaleAppleTransaction
      it "should credit a transfer of a payment credited nowhere as a first purchase" testTransferUncredited
      it "should answer a repeated transfer from the record, without asking the store or limiting it" testTransferReplayAnswered
      it "should refuse a transfer to a key that holds a purchase, without asking the store" testTransferKnownKeyRefused
      it "should answer a transfer of a one-time purchase credited to another key receipt_used, moving nothing" testTransferOneTimeRefused
    describe "restore" $ do
      it "should move a subscription to another device on Restore right after its purchase, retire it on the old one, and credit one credited nowhere" testRestoreOnNewDevice
      it "should retire a subscription Restore moved away when the old device asks within its issued month, recording no issue failure" testRestoreMovedOutAskedInMonth
      it "should hold a second Restore within the hour of the first without asking the store, and still ask once for a purchase presented in it" testRestoreHeldPerPurpose
      it "should move a subscription to another profile on Restore, and refuse a move on before the first key's credential expires, once" testRestoreOnOtherProfile
      it "should turn a held or receipt_used hand-over into a transfer on Restore" testRestoreAfterUnfinishedPass
      it "should leave a resume held for one profile unchanged when a Restore on another profile finds it" testRestoreLeavesResumeHold
      it "should move a subscription back to the device it left once the first key's credential expired" testRestoreMovesBack
      it "should refuse Restore while a badge is shown, recording nothing, and answer a limited move's next Restore with the limit, holding it again" testRestoreLimitedOnNewDevice
      it "should leave a tier change held for one profile unchanged when a Restore on another profile finds it" testRestoreLeavesTierChangeHold
      it "should no longer find a held record held for its owner once a Restore on another profile takes it" testStoreReceiptHeldForAfterRestore
      it "should hold a tier change at once after a move of its subscription was refused transfer_limited" testRefusedMoveKeepsTierChange
      it "should give a move held for a deleted profile back to the profile whose purchase it funds" testRestoreHeldProfileDeleted
      it "should take a purchase hand-over held for one profile to the profile that restores it, and credit it there" testRestoreTakesHeldPurchase
      it "should move a subscription on Restore on the same profile when its purchase is neither shown nor the profile's newest" testRestoreOnSameProfile
  describe "subscriptions" $ do
    it "should credit a subscription's charge from the store's period start, and answer replays from the record" testSubscriptionPurchase
    it "should credit nothing for a subscription with no charge it can grant, retrying as the charge may still come" testSubscriptionPurchaseNotGrantable
    it "should grant each store charge once, and answer a repeat inside the issued month without asking the store" testSubscriptionChargeGrantedOnce
    it "should ask the store only once coverage has ended, and at most once an hour, failed checks included" testSubscriptionStoreChecksBounded
    it "should record and not grant a revoked, offer, unstarted or ended charge" testSubscriptionChargeNotGranted
    it "should grant a store charge of another tier at renewal, on both sides, and back again" testSubscriptionTierChangeAtRenewal
    it "should grant an App Store upgrade during coverage once the store charges it, answering pending until then and asking at most hourly" testSubscriptionUpgradeNow
    it "should answer the upgraded tier's month after an upgrade late in the month, and nothing once it ends, though the old tier's month ends later" testSubscriptionUpgradeLateInMonth
    it "should grant the old tier's renewal from an upgrade's store check, answering the upgrade pending until its own tier is charged" testUpgradeCheckGrantsRenewal
    it "should refuse an upgrade from another key, of another subscription, of a prepaid purchase or from Play, without asking the store" testUpgradeRequestRefused
    it "should not grant a Play charge whose month from its clipped period start has already run out" testClippedPlayPeriodNotGranted
    it "should fund exactly one month from each Play charge of a monthly plan anchored on the 31st, at most the grace behind Play's period end" testPlayMonthEndWalk
    it "should fund each month of an annual Play plan anchored on the 31st and twelve from its renewal, at most the grace behind Play's period end" testPlayAnnualMonthEndWalk
    it "should answer an unreachable store with the check interval, writing no charge or ledger row" testSubscriptionStoreUnreachable
    it "should renew an App Store subscription by its original id, granting a charge first recorded before its period started" testAppleSubscriptionRenews
    it "should keep asking at zero months, and present the renewed credential when the old one expires" testSubscriptionRenews
    it "should keep the badge and ask again a day later while the store has not charged, and send nothing in between, then issue from the period start" testSubscriptionAwaitsCharge
    it "should send a due request at once when the stored wake is further than a stall away" testFarWakeHoldsNothing
    it "should wait a day, not the hour the service answered, after a renewal whose store check failed" testStoreUnreachableWaitsStall
    it "should keep a request's wait past a snooze that ends sooner, and raise the alert again without a request" testSnoozeKeepsRequestWait
    it "should wait from a late request until the next daily moment of the shown credential's expiry, not a day after it" testStallRejoinsSharedMoment
    it "should send no dev-interval request within the interval and one after it, and none for a retired purchase" testDevIntervalRequests
    it "should send no request during coverage without a dev interval" testNoDevIntervalRequests
    it "should retry a failed dev-interval request within the interval" testDevIntervalRetries
    it "should retire a subscription badge after asking, once the window has passed since the first answer that renewed nothing, whatever the service answers" testSubscriptionRetiredAfterRecoveryWindow
    it "should count the window from the first answer that renewed nothing, not from the coverage it held, and retire a store never reached" testUnrenewedRunStartsAtFirstAnswer
    it "should end the run of answers that renewed nothing at a renewal, and start a new one at the next" testRenewalEndsUnrenewedRun
    it "should start a new run at the first answer after the app was not opened for longer than the window" testAbsenceRestartsUnrenewedRun
    it "should start a new run at the first answer after the service did not answer for longer than the window" testOutageRestartsUnrenewedRun
    it "should keep a subscription badge whose ledger still holds months, past the recovery window from its shown credential" testBankedSubscriptionNotRetired
    it "should ask about a subscription not opened for longer than the recovery window, and keep it when the store kept charging" testIdleSubscriptionKeptPastWindow
    it "should keep a subscription badge past the recovery window while the service does not answer" testSubscriptionNotRetiredWithoutAnswer
    it "should retire the badge and alert once the service says the subscription ended, and stop asking" testSubscriptionEnds
    it "should refund the months not issued of a subscription found ended within the hour, without asking the store, retire its badge, and credit a new subscription after it" testSubscriptionEndsWhileCovered
    it "should refund the months not issued of an annual App Store subscription revoked in its second month, and retire its badge as ended" testAppleRevokedWhileCovered
    it "should refund the months not issued of an annual Play subscription that expired in its second month, and retire its badge as ended" testPlayExpiredWhileCovered
    it "should refund an annual subscription whose granted charge the store revoked, and not for a revoked charge it was never granted" testRevokedChargeRefunded
    it "should ask the store before each month of an annual plan, on the renewal lane, and issue it while the subscription renews" testAnnualAsksBeforeEachMonth
    it "should issue a month of an annual plan whose store check fails, and ask the store again before the next one" testAnnualIssuesWhenStoreFails
    it "should retire a subscription badge the device reports ended once its ledger stops covering, without asking, and credit a one-time purchase after it" testDeviceEndedRetires
    it "should send a badge request at once only in a dev build, keep a covered badge, and retire one the answer says ended" testRequestBadgeNow
    it "should keep a subscription badge when a due request's answer renews it, though the device reported it ended" testFreshAnswerOverridesDeviceEnded
    it "should keep the state the service answered when it credited and when it renewed past paid coverage, and announce a change" testSubscriptionStateKept
    it "should keep the state from the credit through an issuance the service answers from its stored state" testAnnualStateNotFromStoredAnswer
    it "should keep the device store's view of a credited subscription, except renewing over billing retry, and nothing for any other transaction" testSubscriptionStateFromStore
    it "should alert a cancelled subscription until acknowledged, and not remind a monthly plan of its renewal" testSubscriptionAlerts
    it "should remind an annual plan three days before it renews, from a wake of its own, until acknowledged" testRenewalReminder
    it "should resolve an Apple renewal to the subscription's record, and credit a held record that adopted it once the subscription's hourly check allows" testAppleRenewalResolvesToPurchase
    it "should adopt into a held Apple record only another transaction bought later and not revoked" testAppleAdoptionRules
    it "should present an adopted Apple transaction at once though the attempt with the one before it failed, and credit it once the subscription's hourly check allows" testAdoptedReceiptNotOverwritten
    it "should resume a retired Apple subscription on the same purchase when its store charges it again" testAppleResubscribeResumes
    it "should resume an Apple subscription retired within its coverage by a refund, once its store charges it again" testAppleResubscribeResumesWithinCoverage
    it "should resume a Play subscription retired past the recovery window when its token is handed over while Play charges it" testPlaySubscriptionResumes
    it "should set aside without a refusal a Play resume after its held attempts, and hold it again at its next hand-over" testPlayResumeAttemptsEnd
    it "should set aside after one answer a Play resume whose subscription Restore moved to another device" testMovedOutPlayResumeSetAside
    it "should not hold a Play subscription moved to another device as a resume of the purchase it left" testMovedOutPlayNotResumed
    it "should derive no alert for an Apple purchase Restore moved to another device, and not hold a later charge as a resume of it" testMovedOutAppleNoAlertNoResume
    it "should hold an Apple charge of another tier while its badge is shown, grant it through the worker and show it at once, and send nothing for one of the same tier" testAppleTierChangeHandOver
    it "should tell the apps a granted Apple tier change and show it to a contact at once" testTierChangeReachesContacts
    it "should keep an Apple tier change held while the store reports the old tier, without failing the renewal, and grant it once the store reports the new one" testAppleTierChangeStoreLags
    it "should settle an Apple tier change with no change and no request once its transaction has expired" testAppleTierChangeExpiredSettles
    it "should hold and grant an Apple tier change handed over two days after it was bought" testAppleTierChangeHandedOverLate
    it "should set aside an Apple tier change after its held attempts, asking no more, and hold it again at its next hand-over" testAppleTierChangeAttemptsEnd
    it "should show the purchase a tier change credits, though a retire cleared it while the change was with the service" testAppleTierChangeKeepsPurchaseShown
    it "should refuse in silence an Apple tier change the store does not vouch for, keeping the badge shown, and answer it again from the record" testAppleTierChangeRefused
    it "should hold and credit an Apple tier change while another receipt of the owner is held" testAppleTierChangeBesideHeldReceipt
    it "should keep renewing a tier change's purchase that the device reported ended before the change" testAppleTierChangeClearsEnded
    it "should clear the payment problem a device reported once a tier change is charged" testAppleTierChangeClearsBillingRetry
    it "should record on the receipt, not the purchase, a tier change whose credential does not verify" testAppleTierChangeCredentialUnverified
    it "should send nothing for an Apple charge of a lower tier while its badge is shown, and grant it at renewal, shown when the old credential expires" testAppleTierChangeLowerWaitsForRenewal
    it "should settle with no request a tier change held for a lower tier, as a build that held every change held it" testAppleTierChangeHeldLowerSettles
    it "should answer an Apple resubscription from the record when it cannot resume, refuse a held one that stops being resumable or that the service does not grant within its hold, and keep one behind another held receipt unfinished" testAppleResubscribeBounded
    it "should ask the App Store about a subscription claim at most once an hour, whoever presents it and as any of its transactions" testApplePurchaseChecksBounded
    it "should ask Play about a subscription token at most once an hour, whoever presents it, credited to another key or not" testPlayPurchaseChecksBounded
    it "should keep at most the maximum of purchase checks, asking without recording once full, and drop expired ones" testPurchaseChecksBounded
    it "should allow the purchase path at most the maximum of Play reads in a minute, counted from its first read" testPlayReadsBounded
    it "should answer a Play subscription rate_limited without asking Play once the minute's reads are used, and not hold it off" testPlayPurchaseReadsLimited
    it "should refuse an App Store Sandbox subscription as a test purchase without asking Apple" testAppleSandboxSubscriptionNotAsked
    it "should answer an App Store subscription provider_not_configured every time without an API key, and credit a one-time purchase" testAppleSubscriptionWithoutApiKey
    it "should acknowledge a Play subscription as a subscription before crediting it, and credit nothing when acknowledging fails" testPlaySubscriptionAcknowledgedBeforeCrediting
    it "should credit a Play subscription Play reports, beside a one-time purchase, and answer provider_not_configured without [google]" testPlaySubscriptionThroughPlay
    it "should have the dev mock credit and renew the plan a subscription names, end one it does not know, and still vouch for a one-time purchase" testMockSubscriptions
    it "should bank a charge the dev mock reports during coverage, issue it once coverage reaches it, and refund it, answering ended, after the mock restarts" testStoreChecksDuringCoverage
    it "should not bank again during coverage a charge the ledger already granted" testStoreChecksDuringCoverageGrantedOnce
    it "should announce a month a request banks during coverage, though it issues nothing" testBankedMonthAnnounced

badgeBotName :: Text
badgeBotName = "SimpleX Badges"

badgeProfile :: Profile
badgeProfile = Profile {displayName = badgeBotName, fullName = "", shortDescr = Nothing, description = Nothing, image = Nothing, contactLink = Nothing, peerType = Just CPTBot, preferences = Nothing, badge = Nothing, contactDomain = Nothing}

serviceDbPrefix :: FilePath
serviceDbPrefix = "badge_service"

testIssuerKeyIdx :: Int
testIssuerKeyIdx = 1

mkBadgeServiceOpts :: TestParams -> BBSSecretKey -> BadgeServiceOpts
mkBadgeServiceOpts ps secretKey =
  BadgeServiceOpts
    { coreOptions =
        coreOpts
          { dbOptions =
              (dbOptions testCoreOpts)
#if defined(dbPostgres)
                {dbSchemaPrefix = testSchemaPrefix ps serviceDbPrefix}
#else
                {dbFilePrefix = tmpPath ps </> serviceDbPrefix}
#endif
          },
      serviceName = badgeBotName,
      clientService = True,
      noAddress = False,
      runCLI = False,
      serviceConfigFile = Nothing,
      issuerKey = Right (Just BadgeIssuerKey {keyIdx = testIssuerKeyIdx, secretKey}),
      testing = True
    }
  where
    (_, ChatOpts {coreOptions = coreOpts}) = testPortsCfg ps testCfg testOpts

-- | The clock tracks real time plus a test-controlled offset rather than freezing it, so a sleeping worker still waits the correct real duration.
newtype TestClock = TestClock (IORef NominalDiffTime)

newTestClock :: IO TestClock
newTestClock = TestClock <$> newIORef 0

testClockTime :: TestClock -> IO UTCTime
testClockTime (TestClock r) = do
  offset <- readIORef r
  addUTCTime offset <$> getCurrentTime

setClockAt :: TestClock -> UTCTime -> IO ()
setClockAt (TestClock r) t = getCurrentTime >>= \real -> writeIORef r (diffUTCTime t real)

data BadgeServiceEnv = BadgeServiceEnv
  { bsIssuerKey :: BadgeIssuerKey,
    bsClock :: TestClock,
    bsClientCfg :: ChatConfig,
    bsAddress :: String,
    bsController :: ChatController,
    bsStore :: FakeStore,
    bsVerifier :: StoreVerifier,
    bsPurchaseChecks :: PurchaseChecks
  }

-- | Stop the service for good: requests sent after it go unanswered until they time out. Stopping
-- chat unsubscribes its queues, where killing the thread could still let a request arrive mid-teardown.
stopBadgeService :: ChatController -> IO ()
stopBadgeService cc = void $ sendChatCmdStr cc "/_stop"

withBadgeService :: HasCallStack => TestParams -> (ChatConfig -> String -> ChatController -> IO ()) -> IO ()
withBadgeService ps test =
  withBadgeServiceEnv ps $ \BadgeServiceEnv {bsClientCfg, bsAddress, bsController} -> test bsClientCfg bsAddress bsController

withBadgeServiceEnv :: HasCallStack => TestParams -> (BadgeServiceEnv -> IO ()) -> IO ()
withBadgeServiceEnv ps = withBadgeServiceVerifier ps fakeVerifier

withBadgeServiceVerifier :: HasCallStack => TestParams -> (FakeStore -> StoreVerifier) -> (BadgeServiceEnv -> IO ()) -> IO ()
withBadgeServiceVerifier ps verifierOf = withBadgeServiceVerifierIO ps (\_ store -> pure $ verifierOf store)

-- | For a verifier that reads the service's clock, as the dev mock does.
withBadgeServiceVerifierIO :: HasCallStack => TestParams -> (TestClock -> FakeStore -> IO StoreVerifier) -> (BadgeServiceEnv -> IO ()) -> IO ()
withBadgeServiceVerifierIO ps verifierOf test = do
  Right (pk, sk) <- bbsKeyGen
  clock <- newTestClock
  store <- newFakeStore
  verifier <- verifierOf clock store
  purchaseChecks <- newPurchaseChecks
  let opts = mkBadgeServiceOpts ps sk
      svcCfg = testCfg {badgePublicKeys = M.singleton testIssuerKeyIdx pk, badgeCurrentTime = testClockTime clock}
  withNewTestChatCfg ps testCfg serviceDbPrefix badgeProfile $ \_ -> pure ()
  -- First start: badge service takes the CreateMyAddress branch.
  runBadgeService ps verifier svcCfg opts $ \_ -> pure ()
  -- Reopen the DB to read the link the service created.
  bsLink <- withTestChat ps serviceDbPrefix $ \bs -> do
    bs <## "subscribed 1 connections on server localhost"
    bs ##> "/sa"
    -- getContactLinks gives the old-clients line only half a second, which a loaded run can miss.
    sLink <- getContactLink_ bs False
    bs <##. "The contact link for old clients: "
    bs <## "auto_accept off"
    pure sLink
  let clientCfg =
        svcCfg {badgeServiceAddress = Just $ either (error . ("bad badge service address: " <>)) id $ strDecode (B.pack bsLink)}
  -- Second start: badge service takes the ShowMyAddress branch, then serves the test body.
  runBadgeService ps verifier svcCfg opts $ \env -> do
    cc <- atomically $ readTMVar $ serviceCC env
    test BadgeServiceEnv {bsIssuerKey = BadgeIssuerKey {keyIdx = testIssuerKeyIdx, secretKey = sk}, bsClock = clock, bsClientCfg = clientCfg, bsAddress = bsLink, bsController = cc, bsStore = store, bsVerifier = verifier, bsPurchaseChecks = purchaseChecks}

issueCode :: HasCallStack => ChatController -> BadgeType -> Int -> IO BadgeCode
issueCode cc badgeType months = issueCodeAs cc badgeType months "free"

revokeCodeAs :: HasCallStack => ChatController -> BadgeCode -> IO (Either T.Text T.Text)
revokeCodeAs cc code = revokeRaw cc (formatBadgeCode code)

-- | Left is a refusal or a failure the service answers as a command error.
revokeRaw :: HasCallStack => ChatController -> T.Text -> IO (Either T.Text T.Text)
revokeRaw cc args =
  sendChatCmdStr cc ("//revoke " <> T.unpack args) >>= \case
    Right (CRCustomChatResponse _ response) -> pure (Right response)
    Left (ChatError (CECommandError e)) -> pure (Left (T.pack e))
    r -> error $ "revoke failed: " <> show (() <$ r)

issueCodeAs :: HasCallStack => ChatController -> BadgeType -> Int -> String -> IO BadgeCode
issueCodeAs cc badgeType months status =
  sendChatCmdStr cc ("//issue " <> T.unpack (textEncode badgeType) <> " " <> show months <> " " <> status) >>= \case
    Right (CRCustomChatResponse _ response) -> case T.stripPrefix "Code: " response of
      Just c | Just code <- parseBadgeCode c -> pure code
      _ -> error $ "unexpected issue response: " <> T.unpack response
    r -> error $ "issue failed: " <> show (() <$ r)

-- | The post-start hook fills serviceCC once the address exists, so waiting on it is the service
-- being ready. A fixed delay here raced with startup and left the address output of one start
-- arriving during the next test.
runBadgeService :: TestParams -> StoreVerifier -> ChatConfig -> BadgeServiceOpts -> (ServiceState -> IO ()) -> IO ()
runBadgeService ps verifier cfg opts action = do
  env <- (\s -> s {storeVerifier = verifier}) <$> newServiceState
  t <- async $ badgeService opts (fst $ testPortsCfg ps cfg testOpts) env
  ready <- timeout 30000000 $ atomically $ readTMVar $ serviceCC env
  when (isNothing ready) $ cancel t >> error "badge service did not start"
  action env `finally` (cancel t >> atomically (readTMVar $ serviceCC env) >>= disposeAgentClient . smpAgent)

codeArg :: BadgeCode -> String
codeArg = T.unpack . formatBadgeCode

testBadgeServiceUnsupported :: HasCallStack => TestParams -> IO ()
testBadgeServiceUnsupported ps =
  withBadgeService ps $ \clientCfg bsLink _ ->
    withNewTestChatCfg ps clientCfg "client" bobProfile $ \client -> do
      let req = "{\"version\":1,\"request\":{\"type\":\"pauseBadge\"}}"
      client ##> ("/_service_request 1 " <> bsLink <> " " <> req)
      client <## "service response: {\"code\":\"unsupported_version\",\"type\":\"error\"}"

testRedeemBadgeCode :: HasCallStack => TestParams -> IO ()
testRedeemBadgeCode ps =
  withBadgeService ps $ \clientCfg _ cc ->
    withNewTestChatCfg ps clientCfg "alice" aliceProfile $ \alice ->
      withNewTestChatCfg ps clientCfg "bob" bobProfile $ \bob -> do
        connectUsers alice bob
        code <- issueCode cc BTSupporter 1
        alice ##> ("/_redeem_badge_code 1 " <> codeArg code)
        alice <## "badge redeemed"
        alice <## "supporter badge - active"
        alice <##. "expires "
        alice ##> "/p"
        alice <## "user profile: alice (Alice, * supporter)"
        alice <## "use /p <name> [<bio>] to change it"
        alice #> "@bob hi"
        bob <# "alice *> hi"
        bob ##> "/i alice"
        bob <## "contact ID: 2"
        bob <## "supporter badge - active"
        bob <##. "expires "
        bob <## "receiving messages via: localhost"
        bob <## "sending messages via: localhost"
        bob <## "you've shared main profile with this contact"
        bob <## "connection not verified, use /code command to see security code"
        bob <## "quantum resistant end-to-end encryption"
        bob <## currentChatVRangeInfo

testRedeemBadgeCodeTwice :: HasCallStack => TestParams -> IO ()
testRedeemBadgeCodeTwice ps =
  withBadgeService ps $ \clientCfg _ cc ->
    withNewTestChatCfg ps clientCfg "alice" aliceProfile $ \alice -> do
      code <- issueCode cc BTSupporter 1
      alice ##> ("/_redeem_badge_code 1 " <> codeArg code)
      alice <## "badge redeemed"
      alice <## "supporter badge - active"
      alice <##. "expires "
      alice ##> ("/_redeem_badge_code 1 " <> map toLower (T.unpack $ badgeCodeText code))
      alice <## "badge already redeemed"
      alice ##> "/p"
      alice <## "user profile: alice (Alice, * supporter)"
      alice <## "use /p <name> [<bio>] to change it"

testRedeemUnknownCode :: HasCallStack => TestParams -> IO ()
testRedeemUnknownCode ps =
  withBadgeService ps $ \clientCfg bsLink _ ->
    withNewTestChatCfg ps clientCfg "alice" aliceProfile $ \alice -> do
      g <- C.newRandom
      unknown <- randomBadgeCode g
      alice ##> ("/_redeem_badge_code 1 " <> codeArg unknown)
      alice <## "cannot get badge: badge service error: code_invalid"
      -- a failed check character is refused before anything leaves the device
      alice ##> "/_redeem_badge_code 1 SB-00000-00000-00000-00001"
      alice <## "cannot get badge: invalid code"
      -- sent straight to the service, past the client's own check, the two are one answer
      (_, redeemPriv) <- atomically $ C.generateKeyPair g :: IO (C.KeyPair 'C.Ed25519)
      redeemDirect alice bsLink redeemPriv (T.unpack $ badgeCodeText unknown)
      alice <## "service response: {\"code\":\"code_invalid\",\"type\":\"error\"}"
      redeemDirect alice bsLink redeemPriv "SB-00000-00000-00000-00001"
      alice <## "service response: {\"code\":\"code_invalid\",\"type\":\"error\"}"

redeemDirect :: HasCallStack => TestCC -> String -> C.PrivateKeyEd25519 -> String -> IO ()
redeemDirect cc bsLink signPriv code = do
  let purchaseKey = B.unpack $ strEncode $ C.publicKey signPriv
      signKey = B.unpack $ strEncode (C.StoredPrivateKey signPriv)
      req =
        "{\"version\":1,\"purchaseKey\":\"" <> purchaseKey
          <> "\",\"request\":{\"type\":\"redeemBadgeCode\",\"masterKey\":\"" <> testMasterKeyB64
          <> "\",\"code\":\"" <> code <> "\"}}"
  cc ##> ("/_service_request 1 " <> bsLink <> " sign_key=" <> signKey <> " " <> req)

-- These requests never reach signing, so the master key can be any 32 bytes.
testMasterKeyB64 :: String
testMasterKeyB64 = "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA="

testIssueRejectsBadArguments :: HasCallStack => TestParams -> IO ()
testIssueRejectsBadArguments ps =
  withBadgeService ps $ \_ _ cc -> do
    let refuses arg = issueRaw cc arg >>= (`shouldSatisfy` isLeft)
    refuses "suporter"
    refuses "supporter 0"
    refuses "supporter 256"
    refuses "supporter 18446744073709551617"
    refuses "supporter 1 gratis"
    refuses ""
    issueRaw cc "supporter 255 paid" >>= (`shouldSatisfy` isRight)

-- | Left is a refusal or a failure the service answers as a command error.
issueRaw :: HasCallStack => ChatController -> String -> IO (Either T.Text ())
issueRaw cc args =
  sendChatCmdStr cc ("//issue " <> args) >>= \case
    Right CRCustomChatResponse {} -> pure $ Right ()
    Left (ChatError (CECommandError e)) -> pure (Left (T.pack e))
    r -> error $ "issue failed: " <> show (() <$ r)

testRedeemSecondCode :: HasCallStack => TestParams -> IO ()
testRedeemSecondCode ps =
  withBadgeService ps $ \clientCfg _ cc ->
    withNewTestChatCfg ps clientCfg "alice" aliceProfile $ \alice -> do
      supporter <- issueCode cc BTSupporter 1
      legend <- issueCode cc BTLegend 1
      alice ##> ("/_redeem_badge_code 1 " <> codeArg supporter)
      alice <## "badge redeemed"
      alice <## "supporter badge - active"
      alice <##. "expires "
      alice ##> ("/_redeem_badge_code 1 " <> codeArg legend)
      alice <## "cannot get badge: badge already active"
      alice ##> "/p"
      showActiveUser alice "alice (Alice, * supporter)"
      alice ##> "/create user alisa"
      showActiveUser alice "alisa"
      alice ##> ("/_redeem_badge_code 2 " <> codeArg legend)
      alice <## "badge redeemed"
      alice <## "legend badge - active"
      alice <##. "expires "

testRedeemSameCodeOtherProfile :: HasCallStack => TestParams -> IO ()
testRedeemSameCodeOtherProfile ps =
  withBadgeService ps $ \clientCfg _ cc ->
    withNewTestChatCfg ps clientCfg "alice" aliceProfile $ \alice -> do
      code <- issueCode cc BTSupporter 1
      alice ##> ("/_redeem_badge_code 1 " <> codeArg code)
      alice <## "badge redeemed"
      alice <## "supporter badge - active"
      alice <##. "expires "
      alice ##> "/create user alisa"
      showActiveUser alice "alisa"
      alice ##> ("/_redeem_badge_code 2 " <> codeArg code)
      alice <## "cannot get badge: badge service error: code_used"
      alice ##> "/p"
      showActiveUser alice "alisa"
      alice ##> "/user alice"
      showActiveUser alice "alice (Alice, * supporter)"

serviceCmd :: HasCallStack => BadgeServiceEnv -> C.PublicKeyEd25519 -> BadgeServiceCommand -> IO BadgeServiceResponse
serviceCmd BadgeServiceEnv {bsIssuerKey, bsController, bsVerifier, bsPurchaseChecks} purchaseKey request =
  badgeServiceResponse bsIssuerKey bsVerifier bsPurchaseChecks bsController Nothing (Just purchaseKey) (requestObject purchaseKey request)

serviceCmdWith :: HasCallStack => BadgeIssuerKey -> ChatController -> Maybe (TQueue GroupEvent) -> C.PublicKeyEd25519 -> BadgeServiceCommand -> IO BadgeServiceResponse
serviceCmdWith key cc trackerQ_ purchaseKey request = do
  checks <- newPurchaseChecks
  badgeServiceResponse key noStoreVerifier checks cc trackerQ_ (Just purchaseKey) (requestObject purchaseKey request)

requestObject :: HasCallStack => C.PublicKeyEd25519 -> BadgeServiceCommand -> J.Object
requestObject purchaseKey request = case J.toJSON BadgeServiceRequest {version = currentBadgeServiceVersion, purchaseKey = Just purchaseKey, request} of
  J.Object o -> o
  _ -> error "badge service request must encode as an object"

entryOf :: StatementEntry -> (Int, Int, UTCTime)
entryOf StatementEntry {changeMonths, balanceMonths, balanceStartTs} = (changeMonths, balanceMonths, balanceStartTs)

anchorOf :: StatementEntry -> UTCTime
anchorOf StatementEntry {balanceAnchorTs} = balanceAnchorTs

entryTag :: StatementEntry -> Text
entryTag StatementEntry {entryType} = case entryType of
  SECredit c -> creditTypeTag c
  SEDebit d -> debitTypeTag d

statementOf :: HasCallStack => BadgeServiceResponse -> ([StatementEntry], Maybe Text)
statementOf = \case
  BSPBadgeCredential {statement = BadgeStatement {entries, previousEntryId}} -> (entries, previousEntryId)
  r -> error $ "expected badgeCredential, got " <> show (J.toJSON r)

credentialOf :: HasCallStack => BadgeServiceResponse -> Maybe BadgeCredential
credentialOf = \case
  BSPBadgeCredential {credential} -> credential
  r -> error $ "expected badgeCredential, got " <> show (J.toJSON r)

shouldAnswerError :: HasCallStack => BadgeServiceResponse -> BadgeServiceErrorCode -> IO ()
shouldAnswerError r expected = case r of
  BSPError {code = ec} -> ec `shouldBe` expected
  _ -> expectationFailure $ "expected " <> show expected <> ", got: " <> show (J.toJSON r)

codeCounts :: DBStore -> ByteString -> IO (Maybe (Int, Int))
codeCounts st codeHash = fmap (\IssuedCode {redeemLimit, redeemCount} -> (redeemLimit, redeemCount)) <$> withTransaction st (`getBadgeCode` codeHash)

codeUses :: ChatController -> BadgeCode -> IO (Maybe (Int, Int))
codeUses cc code = codeCounts (chatStore cc) (badgeCodeHash code)

nextDue :: [StatementEntry] -> UTCTime
nextDue entries = let (_, _, start) = entryOf (last entries) in start

newPurchaseKeys :: IO (C.PublicKeyEd25519, BadgeMasterKey)
newPurchaseKeys = do
  g <- C.newRandom
  (purchaseKey, _) <- atomically $ C.generateKeyPair g :: IO (C.KeyPair 'C.Ed25519)
  (purchaseKey,) <$> generateMasterKey g

redeemAsNewPurchase :: HasCallStack => BadgeServiceEnv -> BadgeCode -> IO BadgeServiceResponse
redeemAsNewPurchase BadgeServiceEnv {bsIssuerKey, bsController} = redeemWithQueue bsIssuerKey bsController Nothing . badgeCodeText

redeemWithKeys :: HasCallStack => BadgeServiceEnv -> (C.PublicKeyEd25519, BadgeMasterKey) -> BadgeCode -> IO BadgeServiceResponse
redeemWithKeys env (purchaseKey, masterKey) code = serviceCmd env purchaseKey BSCRedeemBadgeCode {masterKey, code = badgeCodeText code}

redeemWithQueue :: HasCallStack => BadgeIssuerKey -> ChatController -> Maybe (TQueue GroupEvent) -> Text -> IO BadgeServiceResponse
redeemWithQueue key cc trackerQ_ codeText = do
  (purchaseKey, masterKey) <- newPurchaseKeys
  serviceCmdWith key cc trackerQ_ purchaseKey BSCRedeemBadgeCode {masterKey, code = codeText}

assertBalance :: HasCallStack => BadgeServiceEnv -> C.PublicKeyEd25519 -> StatementEntry -> IO BadgeServiceResponse
assertBalance env purchaseKey lastEntry =
  serviceCmd env purchaseKey BSCIssueBadge {balance = BadgeBalance {lastEntry}}

expiryOf :: HasCallStack => BadgeServiceResponse -> Maybe UTCTime
expiryOf r = (\(BadgeCredential _ _ _ BadgeInfo {badgeExpiry}) -> badgeExpiry) <$> credentialOf r

badgeTypeOf :: HasCallStack => BadgeServiceResponse -> Maybe BadgeType
badgeTypeOf r = (\(BadgeCredential _ _ _ BadgeInfo {badgeType}) -> badgeType) <$> credentialOf r

masterKeyOf :: HasCallStack => BadgeServiceResponse -> Maybe BadgeMasterKey
masterKeyOf r = (\(BadgeCredential _ mk _ _) -> mk) <$> credentialOf r

testCodeMonthsRenew :: HasCallStack => TestParams -> IO ()
testCodeMonthsRenew ps =
  withBadgeServiceEnv ps $ \env@BadgeServiceEnv {bsClock, bsController = cc} -> do
    code <- issueCode cc BTSupporter 3
    keys@(purchaseKey, _) <- newPurchaseKeys
    redeemed <- redeemWithKeys env keys code
    let (entries, previousEntryId) = statementOf redeemed
    previousEntryId `shouldBe` Nothing
    map entryTag entries `shouldBe` ["code", "badge"]
    map (\e -> let (c, m, _) = entryOf e in (c, m)) entries `shouldBe` [(3, 3), (-1, 2)]
    credentialOf redeemed `shouldSatisfy` isJust
    let firstDue = nextDue entries
    r2 <- assertBalance env purchaseKey (last entries)
    map entryTag (fst $ statementOf r2) `shouldBe` []
    setClockAt bsClock firstDue
    r3 <- assertBalance env purchaseKey (last entries)
    let (entries3, prev3) = statementOf r3
    prev3 `shouldBe` Just (entryIdOf $ last entries)
    map entryTag entries3 `shouldBe` ["badge"]
    map (\e -> let (c, m, _) = entryOf e in (c, m)) entries3 `shouldBe` [(-1, 1)]
    credentialOf r3 `shouldSatisfy` isJust
    credentialOf r3 `shouldNotBe` credentialOf redeemed
  where
    entryIdOf StatementEntry {entryId} = entryId

testRepeatInsideIssuedPeriod :: HasCallStack => TestParams -> IO ()
testRepeatInsideIssuedPeriod ps =
  withBadgeServiceEnv ps $ \env@BadgeServiceEnv {bsController = cc} -> do
    code <- issueCode cc BTSupporter 2
    keys@(purchaseKey, _) <- newPurchaseKeys
    redeemed <- redeemWithKeys env keys code
    let (entries, _) = statementOf redeemed
    repeated <- assertBalance env purchaseKey (last entries)
    map entryTag (fst $ statementOf repeated) `shouldBe` []
    credentialOf repeated `shouldBe` credentialOf redeemed

testMultiUseRepeatAfterExpiry :: HasCallStack => TestParams -> IO ()
testMultiUseRepeatAfterExpiry ps =
  withBadgeServiceEnv ps $ \env@BadgeServiceEnv {bsClock, bsClientCfg, bsController = cc} ->
    withNewTestChatCfg ps bsClientCfg "alice" aliceProfile $ \alice -> do
      code <- issueMultiUseCode cc BTSupporter 1 2
      redeemFirstBadge alice code
      rows <- ledgerRows (chatController alice) "badge_ledger"
      setClockAt bsClock $ dueAtOf rows
      alice `send` "/_app activate"
      alice
        <### [ "/_app activate",
               "ok",
               StartsWith "badge alert: support_ended ",
               StartsWith "1: supporter",
               StartsWith "badge alert: support_ended "
             ]
      waitShownBadge (chatController alice) Nothing
      -- The repeat uses the same purchase key, so the service returns the credential it already issued.
      alice ##> ("/_redeem_badge_code 1 " <> codeArg code)
      alice <## "badge already redeemed"
      waitShownBadge (chatController alice) Nothing
      codeUses cc code `shouldReturn` Just (2, 1)
      withNewTestChatCfg ps bsClientCfg "bob" bobProfile $ \bob -> redeemFirstBadge bob code
      redeemAsNewPurchase env code >>= (`shouldAnswerError` BSECodeUsed)

testMultiUseWithoutGroup :: HasCallStack => TestParams -> IO ()
testMultiUseWithoutGroup ps =
  withBadgeServiceEnv ps $ \env@BadgeServiceEnv {bsController = cc} -> do
    code <- issueMultiUseCode cc BTLegend 2 2
    forM_ [1 :: Int, 2] $ \claimed -> do
      keys@(_, masterKey) <- newPurchaseKeys
      redeemed <- redeemWithKeys env keys code
      let (entries, _) = statementOf redeemed
      map (\e -> let (c, m, _) = entryOf e in (c, m)) entries `shouldBe` [(2, 2), (-1, 1)]
      badgeTypeOf redeemed `shouldBe` Just BTLegend
      expiryOf redeemed `shouldBe` Just (endOfMondayAfter (nextDue entries))
      masterKeyOf redeemed `shouldBe` Just masterKey
      codeUses cc code `shouldReturn` Just (2, claimed)
    redeemAsNewPurchase env code >>= (`shouldAnswerError` BSECodeUsed)

testUnreadableCredentialIsInternal :: HasCallStack => TestParams -> IO ()
testUnreadableCredentialIsInternal ps =
  withBadgeServiceEnv ps $ \env@BadgeServiceEnv {bsController = cc} -> do
    code <- issueMultiUseCode cc BTSupporter 1 2
    let redeem keys = redeemWithKeys env keys code
    holder <- newPurchaseKeys
    redeem holder >>= (`shouldSatisfy` isJust) . credentialOf
    withTransaction (chatStore cc) $ \db ->
      DB.execute db "UPDATE sx_badge_service_badge_issuances SET credential = ?" (Only (Binary ("not a credential" :: ByteString)))
    redeem holder >>= (`shouldAnswerError` BSEInternal)
    codeUses cc code `shouldReturn` Just (2, 1)
    newPurchaseKeys >>= redeem >>= (`shouldSatisfy` isJust) . credentialOf
    codeUses cc code `shouldReturn` Just (2, 2)
    redeem holder >>= (`shouldAnswerError` BSEInternal)
    codeUses cc code `shouldReturn` Just (2, 2)

-- Multi-use codes come from the group command, and this service has no group.
issueMultiUseCode :: HasCallStack => ChatController -> BadgeType -> Int -> Int -> IO BadgeCode
issueMultiUseCode cc badgeType months uses =
  issueOneCode cc badgeType months CPSFree uses >>= \case
    Right (code, _) -> pure code
    Left e -> error $ "issuing a multi-use code failed: " <> e

testLapseWhileAway :: HasCallStack => TestParams -> IO ()
testLapseWhileAway ps =
  withBadgeServiceEnv ps $ \env@BadgeServiceEnv {bsClock, bsController = cc} -> do
    code <- issueCode cc BTSupporter 6
    keys@(purchaseKey, _) <- newPurchaseKeys
    redeemed <- redeemWithKeys env keys code
    let (entries, _) = statementOf redeemed
    -- The clock is moved from the anchor because adding months to an already clipped due date would miss the boundary.
    setClockAt bsClock (addMonths 4 (anchorOf (last entries)))
    away <- assertBalance env purchaseKey (last entries)
    let (entries', _) = statementOf away
    map entryTag entries' `shouldBe` ["lapse", "badge"]
    map (\e -> let (c, m, _) = entryOf e in (c, m)) entries' `shouldBe` [(-3, 2), (-1, 1)]
    credentialOf away `shouldSatisfy` isJust

testLastMonthExpiryRounds :: HasCallStack => TestParams -> IO ()
testLastMonthExpiryRounds ps =
  withBadgeServiceEnv ps $ \env@BadgeServiceEnv {bsClock, bsController = cc} -> do
    code <- issueCode cc BTSupporter 2
    keys@(purchaseKey, _) <- newPurchaseKeys
    redeemed <- redeemWithKeys env keys code
    let (entries, _) = statementOf redeemed
    setClockAt bsClock (nextDue entries)
    renewed <- assertBalance env purchaseKey (last entries)
    let (entries', _) = statementOf renewed
    map entryTag entries' `shouldBe` ["badge"]
    map (\e -> let (c, m, _) = entryOf e in (c, m)) entries' `shouldBe` [(-1, 0)]
    expiryOf renewed `shouldBe` Just (endOfMondayAfter (nextDue entries'))

testRenewalSignsWithStoredMasterKey :: HasCallStack => TestParams -> IO ()
testRenewalSignsWithStoredMasterKey ps =
  withBadgeServiceEnv ps $ \env@BadgeServiceEnv {bsClock, bsController = cc} -> do
    code <- issueCode cc BTSupporter 2
    keys@(purchaseKey, masterKey) <- newPurchaseKeys
    redeemed <- redeemWithKeys env keys code
    masterKeyOf redeemed `shouldBe` Just masterKey
    let (entries, _) = statementOf redeemed
    setClockAt bsClock (nextDue entries)
    renewed <- assertBalance env purchaseKey (last entries)
    masterKeyOf renewed `shouldBe` Just masterKey

-- This type omits service_created_at and created_at because the client records when it stored a row, not when the service wrote it, so those columns never match.
type ReplicatedRow = (Text, Int, Int, UTCTime, Text, Maybe Text)

replicatedColumns :: String
replicatedColumns =
  "entry_uuid, change_months, balance_months, balance_start_ts, balance_badge_type, "
    <> "COALESCE(entry_credit_type, entry_debit_type)"

ledgerRows :: ChatController -> String -> IO [ReplicatedRow]
ledgerRows ChatController {chatStore} table =
  withTransaction chatStore $ \db ->
    DB.query_ db . fromString $
      "SELECT " <> replicatedColumns <> " FROM " <> table <> " ORDER BY entry_id"

purchaseLedgerRows :: ChatController -> Text -> IO [ReplicatedRow]
purchaseLedgerRows ChatController {chatStore} entryUuid =
  withTransaction chatStore $ \db ->
    DB.query
      db
      ( fromString $
          "SELECT "
            <> replicatedColumns
            <> " FROM sx_badge_service_badge_ledger "
            <> "WHERE badge_purchase_id = (SELECT badge_purchase_id FROM sx_badge_service_badge_ledger WHERE entry_uuid = ?) "
            <> "ORDER BY entry_id"
      )
      (Only entryUuid)

-- the two dates the CLI prints for each row
ledgerTimes :: ChatController -> IO [(UTCTime, UTCTime)]
ledgerTimes ChatController {chatStore} =
  withTransaction chatStore $ \db ->
    DB.query_ db "SELECT service_created_at, balance_start_ts FROM badge_ledger ORDER BY entry_id"

-- | The client's verdict on each row, in ledger order. The service has no such column: it computes
-- the rows rather than checking what someone else computed.
balanceChecks :: ChatController -> IO [Maybe Bool]
balanceChecks ChatController {chatStore} =
  withTransaction chatStore $ \db ->
    map (fmap unBI . fromOnly)
      <$> DB.query_ db "SELECT balance_checked FROM badge_ledger ORDER BY entry_id"

testClientReplicatesLedger :: HasCallStack => TestParams -> IO ()
testClientReplicatesLedger ps =
  withBadgeServiceEnv ps $ \BadgeServiceEnv {bsClientCfg, bsController = cc} ->
    withNewTestChatCfg ps bsClientCfg "alice" aliceProfile $ \alice -> do
      code <- issueCode cc BTSupporter 3
      alice ##> ("/_redeem_badge_code 1 " <> codeArg code)
      alice <## "badge redeemed"
      alice <## "supporter badge - active"
      alice <##. "expires "
      serviceLedger <- ledgerRows cc "sx_badge_service_badge_ledger"
      clientLedger <- ledgerRows (chatController alice) "badge_ledger"
      map (\(_, ch, m, _, _, t) -> (ch, m, t)) serviceLedger `shouldBe` [(3, 3, Just "code"), (-1, 2, Just "badge")]
      clientLedger `shouldBe` serviceLedger
      checks <- balanceChecks (chatController alice)
      checks `shouldBe` [Just True, Just True]
      alice ##> ("/_redeem_badge_code 1 " <> codeArg code)
      alice <## "badge already redeemed"
      clientLedger' <- ledgerRows (chatController alice) "badge_ledger"
      clientLedger' `shouldBe` serviceLedger
      expiries <- issuedExpiries (chatController alice)
      length expiries `shouldBe` 1
      -- the CLI lists the rows oldest first, with the dates they carry
      times <- ledgerTimes (chatController alice)
      alice ##> "/_badge ledger 1 1"
      forM_ (zip times [("code", "+3", "3"), ("badge", "-1", "2")]) $ \((createdAt, from), (kind, change, balance)) ->
        alice <## (day createdAt <> " " <> kind <> " " <> change <> " -> " <> balance <> ", from " <> day from)
      -- and nothing for a purchase that is another profile's
      alice ##> "/create user alisa"
      showActiveUser alice "alisa"
      alice ##> "/_badge ledger 2 1"
      alice <## "no ledger entries"
  where
    day = formatTime defaultTimeLocale "%Y-%m-%d"

dueAtOf :: [ReplicatedRow] -> UTCTime
dueAtOf rows = let (_, _, _, start, _, _) = last rows in start

renewalMoments :: [ReplicatedRow] -> (UTCTime, UTCTime)
renewalMoments rows =
  let expiry = endOfMondayAfter $ dueAtOf rows
   in (addUTCTime (-nominalDay) expiry, expiry)

-- | The margin must be long enough for the arming pass to finish first, because a pass that overruns does the work itself and the test then passes without arming any wake.
badgeWakeMargin :: NominalDiffTime
badgeWakeMargin = 3

-- | Standing the clock just short of t before signalling makes the worker arm the wake at t rather than do the work in this pass, so whatever follows is produced by that wake.
armWakeAt :: HasCallStack => TestCC -> TestClock -> UTCTime -> IO ()
armWakeAt cc clock t = do
  setClockAt clock $ addUTCTime (negate badgeWakeMargin) t
  cc ##> "/_app activate"
  cc <## "ok"

issuedExpiries :: ChatController -> IO [UTCTime]
issuedExpiries ChatController {chatStore} = do
  rows :: [(UTCTime, Int64)] <-
    withTransaction chatStore $ \db ->
      DB.query_ db "SELECT expiry, badge_purchase_id FROM badge_issuances ORDER BY period_end"
  pure $ map fst rows

peerBadgeExpiry :: ChatController -> IO (Maybe UTCTime)
peerBadgeExpiry ChatController {chatStore} = do
  rows :: [(Maybe UTCTime, Int64)] <-
    withTransaction chatStore $ \db ->
      DB.query_ db "SELECT badge_expiry, contact_profile_id FROM contact_profiles WHERE badge_proof IS NOT NULL ORDER BY contact_profile_id"
  pure $ case rows of
    ((t, _) : _) -> t
    [] -> Nothing

peerBadgeTypes :: ChatController -> IO [Text]
peerBadgeTypes ChatController {chatStore} =
  withTransaction chatStore $ \db ->
    map fromOnly <$> DB.query_ db "SELECT badge_type FROM contact_profiles WHERE badge_proof IS NOT NULL ORDER BY contact_profile_id"

setBadgeExpiry :: ChatController -> String -> UTCTime -> IO ()
setBadgeExpiry ChatController {chatStore} whichBadge t =
  withTransaction chatStore $ \db ->
    DB.execute db (fromString $ "UPDATE contact_profiles SET badge_expiry = ? WHERE " <> whichBadge <> " IS NOT NULL") (Only t)

insertUnknownLedgerEntry :: ChatController -> IO ()
insertUnknownLedgerEntry ChatController {chatStore} =
  withTransaction chatStore $ \db ->
    DB.execute_ db . fromString $
      "INSERT INTO badge_ledger"
        <> " (entry_uuid, badge_purchase_id, change_months, balance_months, balance_start_ts, balance_anchor_ts,"
        <> "  balance_badge_type, service_created_at, created_at, entry_type, entry_credit_type,"
        <> "  entry_type_unknown, entry_type_value)"
        <> " SELECT 'unknown-entry', badge_purchase_id, 0, balance_months, balance_start_ts, balance_anchor_ts,"
        <> "  balance_badge_type, service_created_at, created_at, 'credit', 'grant', 1, '{\"type\":\"grant\"}'"
        <> " FROM badge_ledger ORDER BY entry_id DESC LIMIT 1"

shownAndIssuedExpiry :: ChatController -> IO (Maybe UTCTime, Maybe UTCTime)
shownAndIssuedExpiry ChatController {chatStore} = withTransaction chatStore $ \db -> do
  shown :: [(Maybe UTCTime, Int64)] <-
    DB.query_ db "SELECT badge_expiry, contact_profile_id FROM contact_profiles WHERE badge_signature IS NOT NULL ORDER BY contact_profile_id"
  issued :: [(Maybe UTCTime, Int64)] <-
    DB.query_ db "SELECT expiry, badge_purchase_id FROM badge_issuances ORDER BY period_end DESC LIMIT 1"
  pure (firstOf shown, firstOf issued)
  where
    firstOf = \case
      ((t, _) : _) -> t
      [] -> Nothing

waitShownIssued :: HasCallStack => ChatController -> IO ()
waitShownIssued cc = loop (100 :: Int)
  where
    -- Both values being absent would compare equal, so the presence of a badge is asserted separately.
    loop 0 = shownAndIssuedExpiry cc >>= \(shown, issued) -> do
      shown `shouldSatisfy` isJust
      shown `shouldBe` issued
    loop i =
      shownAndIssuedExpiry cc >>= \(shown, issued) ->
        if isJust shown && shown == issued then pure () else threadDelay 50000 >> loop (i - 1)

waitLedgerRows :: HasCallStack => ChatController -> Int -> IO [ReplicatedRow]
waitLedgerRows cc n = loop (100 :: Int)
  where
    loop 0 = ledgerRows cc "badge_ledger" >>= \rows -> error $ "expected " <> show n <> " ledger rows, got " <> show (length rows)
    loop i = do
      rows <- ledgerRows cc "badge_ledger"
      if length rows >= n then pure rows else threadDelay 50000 >> loop (i - 1)

shownBadgeId :: HasCallStack => ChatController -> IO (Maybe Int64)
shownBadgeId ChatController {chatStore} = do
  -- Selecting two columns rather than one lets the row type avoid a backend-specific Only wrapper.
  rows :: [(Maybe Int64, Int64)] <-
    withTransaction chatStore $ \db ->
      DB.query_ db "SELECT shown_badge_id, user_id FROM users WHERE user_id = 1"
  -- An unexpected row shape raises an error rather than returning Nothing, which would let a wait for Nothing pass without reading the row.
  pure $ case rows of
    [(i, _)] -> i
    _ -> error $ "expected one users row, got " <> show rows

ackedEpisode :: HasCallStack => ChatController -> IO (Maybe Text, Maybe Text)
ackedEpisode ChatController {chatStore} = do
  rows :: [(Maybe Text, Maybe Text)] <-
    withTransaction chatStore $ \db ->
      DB.query_ db "SELECT alert_acked_kind, alert_acked_episode FROM badge_purchases"
  pure $ case rows of
    [r] -> r
    _ -> error $ "expected one badge purchase, got " <> show rows

waitShownBadge :: HasCallStack => ChatController -> Maybe Int64 -> IO ()
waitShownBadge cc expected = loop (100 :: Int)
  where
    loop 0 = shownBadgeId cc >>= \actual -> actual `shouldBe` expected
    loop i =
      shownBadgeId cc >>= \actual ->
        if actual == expected then pure () else threadDelay 50000 >> loop (i - 1)

redeemFirstBadge :: HasCallStack => TestCC -> BadgeCode -> IO ()
redeemFirstBadge alice code = do
  alice ##> ("/_redeem_badge_code 1 " <> codeArg code)
  alice <## "badge redeemed"
  alice <## "supporter badge - active"
  alice <##. "expires "
  waitWakeArmed (chatController alice)

waitWakeArmed :: HasCallStack => ChatController -> IO ()
waitWakeArmed ChatController {chatStore} = loop (100 :: Int)
  where
    loop i = do
      rows :: [(Maybe UTCTime, Int64)] <-
        withTransaction chatStore $ \db ->
          DB.query_ db "SELECT next_wake_at, badge_purchase_id FROM badge_purchases"
      case rows of
        [(Just _, _)] -> pure ()
        _
          | i == 0 -> error $ "expected one badge purchase with a wake, got " <> show rows
          | otherwise -> threadDelay 50000 >> loop (i - 1)

testWorkerRenews :: HasCallStack => TestParams -> IO ()
testWorkerRenews ps =
  withBadgeServiceEnv ps $ \BadgeServiceEnv {bsClock, bsClientCfg, bsController = cc} ->
    withNewTestChatCfg ps bsClientCfg "alice" aliceProfile $ \alice -> do
      code <- issueCode cc BTSupporter 3
      redeemFirstBadge alice code
      redeemed <- ledgerRows (chatController alice) "badge_ledger"
      map (\(_, ch, m, _, _, t) -> (ch, m, t)) redeemed `shouldBe` [(3, 3, Just "code"), (-1, 2, Just "badge")]
      let (requestAt, presentAt) = renewalMoments redeemed
      setClockAt bsClock requestAt
      alice ##> "/_app activate"
      alice <## "ok"
      renewed <- waitLedgerRows (chatController alice) 3
      alice <##. "1: supporter"
      map (\(_, ch, m, _, _, t) -> (ch, m, t)) renewed `shouldBe` [(3, 3, Just "code"), (-1, 2, Just "badge"), (-1, 1, Just "badge")]
      (shownEarly, issuedEarly) <- shownAndIssuedExpiry (chatController alice)
      shownEarly `shouldNotBe` issuedEarly
      setClockAt bsClock presentAt
      alice ##> "/_app activate"
      alice <## "ok"
      waitShownIssued (chatController alice)
      let (requestAt2, presentAt2) = renewalMoments renewed
      setClockAt bsClock requestAt2
      alice ##> "/_app activate"
      alice <## "ok"
      alice <##. "1: supporter"
      renewed2 <- waitLedgerRows (chatController alice) 4
      map (\(_, ch, m, _, _, t) -> (ch, m, t)) renewed2
        `shouldBe` [(3, 3, Just "code"), (-1, 2, Just "badge"), (-1, 1, Just "badge"), (-1, 0, Just "badge")]
      serviceLedger <- ledgerRows cc "sx_badge_service_badge_ledger"
      renewed2 `shouldBe` serviceLedger
      setClockAt bsClock presentAt2
      alice ##> "/_app activate"
      alice <## "ok"
      waitShownIssued (chatController alice)

testLatestIssuedByRow :: HasCallStack => TestParams -> IO ()
testLatestIssuedByRow ps =
  withBadgeServiceEnv ps $ \BadgeServiceEnv {bsClock, bsClientCfg, bsController = cc} ->
    withNewTestChatCfg ps bsClientCfg "alice" aliceProfile $ \alice -> do
      code <- issueCode cc BTSupporter 3
      redeemFirstBadge alice code
      redeemed <- ledgerRows (chatController alice) "badge_ledger"
      setClockAt bsClock $ fst $ renewalMoments redeemed
      alice ##> "/_app activate"
      alice <## "ok"
      _ <- waitLedgerRows (chatController alice) 3
      alice <##. "1: supporter"
      withTransaction (chatStore $ chatController alice) $ \db -> do
        -- as a tier change leaves them: the newer issuance ends before the older one
        [(purchaseId, olderId, olderEnd), (_, newerId, newerEnd)] :: [(Int64, Int64, UTCTime)] <-
          DB.query_ db "SELECT badge_purchase_id, entry_id, period_end FROM badge_issuances ORDER BY entry_id"
        DB.execute db "UPDATE badge_issuances SET period_end = ? WHERE entry_id = ?" (newerEnd, olderId)
        DB.execute db "UPDATE badge_issuances SET period_end = ? WHERE entry_id = ?" (olderEnd, newerId)
        [Only (Binary newest)] <- DB.query db "SELECT credential FROM badge_issuances WHERE entry_id = ?" (Only newerId)
        getLatestIssuedCredential db purchaseId `shouldReturn` J.decodeStrict' newest

testRevokedMultiUseHolderRenews :: HasCallStack => TestParams -> IO ()
testRevokedMultiUseHolderRenews ps =
  withBadgeServiceEnv ps $ \env@BadgeServiceEnv {bsClock, bsClientCfg, bsController = cc} ->
    withNewTestChatCfg ps bsClientCfg "alice" aliceProfile $ \alice -> do
      code <- issueMultiUseCode cc BTSupporter 3 2
      redeemFirstBadge alice code
      redeemed <- ledgerRows (chatController alice) "badge_ledger"
      revokeCodeAs cc code `shouldReturn` Right "Revoked."
      redeemAsNewPurchase env code >>= (`shouldAnswerError` BSECodeInvalid)
      codeUses cc code `shouldReturn` Just (2, 1)
      -- A holder whose reply was lost retries with the same key and still gets its badge.
      alice ##> ("/_redeem_badge_code 1 " <> codeArg code)
      alice <## "badge already redeemed"
      codeUses cc code `shouldReturn` Just (2, 1)
      let (requestAt, presentAt) = renewalMoments redeemed
      setClockAt bsClock requestAt
      alice ##> "/_app activate"
      alice <## "ok"
      renewed <- waitLedgerRows (chatController alice) 3
      alice <##. "1: supporter"
      map (\(_, ch, m, _, _, t) -> (ch, m, t)) renewed `shouldBe` [(3, 3, Just "code"), (-1, 2, Just "badge"), (-1, 1, Just "badge")]
      ledgerRows cc "sx_badge_service_badge_ledger" `shouldReturn` renewed
      setClockAt bsClock presentAt
      alice ##> "/_app activate"
      alice <## "ok"
      waitShownIssued (chatController alice)

testMultiUseHoldersRenewApart :: HasCallStack => TestParams -> IO ()
testMultiUseHoldersRenewApart ps =
  withBadgeServiceEnv ps $ \env@BadgeServiceEnv {bsClock, bsClientCfg, bsController = cc} ->
    withNewTestChatCfg ps bsClientCfg "alice" aliceProfile $ \alice -> do
      code <- issueMultiUseCode cc BTSupporter 3 2
      redeemFirstBadge alice code
      redeemed <- ledgerRows (chatController alice) "badge_ledger"
      let requestAt = fst $ renewalMoments redeemed
      setClockAt bsClock requestAt
      alice ##> "/_app activate"
      alice <## "ok"
      renewed <- waitLedgerRows (chatController alice) 3
      alice <##. "1: supporter"
      map (\(_, ch, m, _, _, t) -> (ch, m, t)) renewed `shouldBe` [(3, 3, Just "code"), (-1, 2, Just "badge"), (-1, 1, Just "badge")]
      bobRows <- withNewTestChatCfg ps bsClientCfg "bob" bobProfile $ \bob -> do
        redeemFirstBadge bob code
        ledgerRows (chatController bob) "badge_ledger"
      map (\(_, ch, m, _, _, t) -> (ch, m, t)) bobRows `shouldBe` [(3, 3, Just "code"), (-1, 2, Just "badge")]
      let (bobFirstUuid, _, _, bobRedeemedAt, _, _) = head bobRows
      bobRedeemedAt `shouldSatisfy` (>= requestAt)
      purchaseLedgerRows cc bobFirstUuid `shouldReturn` bobRows
      issued <- issuedExpiries (chatController alice)
      length issued `shouldBe` 2
      alice ##> ("/_redeem_badge_code 1 " <> codeArg code)
      alice <## "badge already redeemed"
      codeUses cc code `shouldReturn` Just (2, 2)
      let (aliceFirstUuid, _, _, _, _, _) = head renewed
      ledgerRows (chatController alice) "badge_ledger" `shouldReturn` renewed
      purchaseLedgerRows cc aliceFirstUuid `shouldReturn` renewed
      issuedExpiries (chatController alice) `shouldReturn` issued
      alicePurchaseKey <- codeRedemptionKey (chatController alice)
      (_, masterKey) <- newPurchaseKeys
      repeated <- redeemWithKeys env (alicePurchaseKey, masterKey) code
      expiryOf repeated `shouldBe` Just (last issued)
      codeUses cc code `shouldReturn` Just (2, 2)

codeRedemptionKey :: HasCallStack => ChatController -> IO C.PublicKeyEd25519
codeRedemptionKey ChatController {chatStore} = do
  rows :: [Only C.PublicKeyEd25519] <-
    withTransaction chatStore $ \db ->
      DB.query_ db "SELECT purchase_key FROM badge_code_redemptions"
  case rows of
    [Only k] -> pure k
    _ -> error $ "expected one code redemption, got " <> show (length rows)

testRequestWakeFires :: HasCallStack => TestParams -> IO ()
testRequestWakeFires ps =
  withBadgeServiceEnv ps $ \BadgeServiceEnv {bsClock, bsClientCfg, bsController = cc} ->
    withNewTestChatCfg ps bsClientCfg "alice" aliceProfile $ \alice -> do
      code <- issueCode cc BTSupporter 3
      redeemFirstBadge alice code
      redeemed <- ledgerRows (chatController alice) "badge_ledger"
      armWakeAt alice bsClock $ fst $ renewalMoments redeemed
      ledgerRows (chatController alice) "badge_ledger" >>= (`shouldBe` redeemed)
      renewed <- waitLedgerRows (chatController alice) 3
      alice <##. "1: supporter"
      map (\(_, ch, m, _, _, t) -> (ch, m, t)) renewed
        `shouldBe` [(3, 3, Just "code"), (-1, 2, Just "badge"), (-1, 1, Just "badge")]

testPresentWakeFires :: HasCallStack => TestParams -> IO ()
testPresentWakeFires ps =
  withBadgeServiceEnv ps $ \BadgeServiceEnv {bsClock, bsClientCfg, bsController = cc} ->
    withNewTestChatCfg ps bsClientCfg "alice" aliceProfile $ \alice -> do
      code <- issueCode cc BTSupporter 3
      redeemFirstBadge alice code
      redeemed <- ledgerRows (chatController alice) "badge_ledger"
      let (requestAt, presentAt) = renewalMoments redeemed
      setClockAt bsClock requestAt
      alice ##> "/_app activate"
      alice <## "ok"
      void $ waitLedgerRows (chatController alice) 3
      alice <##. "1: supporter"
      armWakeAt alice bsClock presentAt
      shownAndIssuedExpiry (chatController alice) >>= \(shown, issued) -> shown `shouldNotBe` issued
      waitShownIssued (chatController alice)

testRenewsAfterUnknownEntry :: HasCallStack => TestParams -> IO ()
testRenewsAfterUnknownEntry ps =
  withBadgeServiceEnv ps $ \BadgeServiceEnv {bsClock, bsClientCfg, bsController = cc} ->
    withNewTestChatCfg ps bsClientCfg "alice" aliceProfile $ \alice -> do
      code <- issueCode cc BTSupporter 3
      redeemFirstBadge alice code
      rows <- ledgerRows (chatController alice) "badge_ledger"
      insertUnknownLedgerEntry (chatController alice)
      setClockAt bsClock $ fst $ renewalMoments rows
      alice ##> "/_app activate"
      alice <## "ok"
      renewed <- waitLedgerRows (chatController alice) 4
      alice <##. "1: supporter"
      map (\(_, ch, m, _, _, t) -> (ch, m, t)) renewed
        `shouldBe` [(3, 3, Just "code"), (-1, 2, Just "badge"), (0, 2, Just "grant"), (-1, 1, Just "badge")]
      checks <- balanceChecks (chatController alice)
      checks `shouldBe` [Just True, Just True, Nothing, Just True]

testRenewsAfterRestart :: HasCallStack => TestParams -> IO ()
testRenewsAfterRestart ps =
  withBadgeServiceEnv ps $ \BadgeServiceEnv {bsClock, bsClientCfg, bsController = cc} -> do
    rows <- withNewTestChatCfg ps bsClientCfg "alice" aliceProfile $ \alice -> do
      code <- issueCode cc BTSupporter 6
      redeemFirstBadge alice code
      ledgerRows (chatController alice) "badge_ledger"
    let (_, _, _, anchor, _, _) = head rows
    setClockAt bsClock $ addMonths 4 anchor
    withTestChatCfg ps bsClientCfg "alice" $ \alice -> do
      renewed <- waitLedgerRows (chatController alice) 4
      alice <##. "1: supporter"
      map (\(_, ch, m, _, _, t) -> (ch, m, t)) renewed
        `shouldBe` [(6, 6, Just "code"), (-1, 5, Just "badge"), (-3, 2, Just "lapse"), (-1, 1, Just "badge")]
      serviceLedger <- ledgerRows cc "sx_badge_service_badge_ledger"
      renewed `shouldBe` serviceLedger
      checks <- balanceChecks (chatController alice)
      checks `shouldBe` replicate 4 (Just True)
      waitShownIssued (chatController alice)

testWorkerRetiresExpired :: HasCallStack => TestParams -> IO ()
testWorkerRetiresExpired ps =
  withBadgeServiceEnv ps $ \BadgeServiceEnv {bsClock, bsClientCfg, bsController = cc} ->
    withNewTestChatCfg ps bsClientCfg "alice" aliceProfile $ \alice ->
      withNewTestChatCfg ps bsClientCfg "bob" bobProfile $ \bob -> do
        connectUsers alice bob
        code <- issueCode cc BTSupporter 1
        redeemFirstBadge alice code
        alice #> "@bob hi"
        bob <# "alice *> hi"
        bob ##> "/i alice"
        bob <## "contact ID: 2"
        bob <## "supporter badge - active"
        bob <##. "expires "
        bob <## "receiving messages via: localhost"
        bob <## "sending messages via: localhost"
        bob <## "you've shared main profile with this contact"
        bob <## "connection not verified, use /code command to see security code"
        bob <## "quantum resistant end-to-end encryption"
        bob <## currentChatVRangeInfo
        rows <- ledgerRows (chatController alice) "badge_ledger"
        setClockAt bsClock $ dueAtOf rows
        alice `send` "/_app activate"
        alice
          <### [ "/_app activate",
                 "ok",
                 StartsWith "badge alert: support_ended ",
                 StartsWith "1: supporter",
                 StartsWith "badge alert: support_ended "
               ]
        waitShownBadge (chatController alice) Nothing
        alice ##> "/p"
        alice <## "user profile: alice (Alice)"
        alice <## "use /p <name> [<bio>] to change it"
        -- A profile update prints nothing when only the badge changed, because viewContactUpdated compares names and links, so the next message is what confirms the removal reached bob.
        alice #> "@bob after"
        bob <# "alice> after"
        bob ##> "/i alice"
        bob <## "contact ID: 2"
        bob <## "receiving messages via: localhost"
        bob <## "sending messages via: localhost"
        bob <## "you've shared main profile with this contact"
        bob <## "connection not verified, use /code command to see security code"
        bob <## "quantum resistant end-to-end encryption"
        bob <## currentChatVRangeInfo

testRetiresWhenEntitlementEnds :: HasCallStack => TestParams -> IO ()
testRetiresWhenEntitlementEnds ps =
  withBadgeServiceEnv ps $ \BadgeServiceEnv {bsClock, bsClientCfg, bsController = cc} ->
    withNewTestChatCfg ps bsClientCfg "alice" aliceProfile $ \alice -> do
      code <- issueCode cc BTSupporter 1
      redeemFirstBadge alice code
      rows <- ledgerRows (chatController alice) "badge_ledger"
      armWakeAt alice bsClock $ dueAtOf rows
      alice ##> "/p"
      alice <## "user profile: alice (Alice, * supporter)"
      alice <## "use /p <name> [<bio>] to change it"
      alice <##. "badge alert: support_ended "
      alice <##. "1: supporter"
      alice <##. "badge alert: support_ended "
      waitShownBadge (chatController alice) Nothing

testEndedAlert :: HasCallStack => TestParams -> IO ()
testEndedAlert ps =
  withBadgeServiceEnv ps $ \BadgeServiceEnv {bsClock, bsClientCfg, bsController = cc} -> do
    endsAt <- withNewTestChatCfg ps bsClientCfg "alice" aliceProfile $ \alice -> do
      code <- issueCode cc BTSupporter 1
      redeemFirstBadge alice code
      rows <- ledgerRows (chatController alice) "badge_ledger"
      let endsAt = dueAtOf rows
      setClockAt bsClock endsAt
      alice `send` "/_app activate"
      alice
        <### [ "/_app activate",
               "ok",
               StartsWith "badge alert: support_ended ",
               StartsWith "1: supporter",
               StartsWith "badge alert: support_ended "
             ]
      pure endsAt
    withTestChatCfg ps bsClientCfg "alice" $ \alice -> do
      alice <##. "badge alert: support_ended "
      alice ##> ("/_badge ack 1 1 support_ended off " <> T.unpack (safeDecodeUtf8 $ strEncode endsAt))
      alice <##. "1: supporter"
      ackedEpisode (chatController alice) `shouldReturn` (Just "support_ended", Just (safeDecodeUtf8 $ strEncode endsAt))
      alice ##> "/_badge state 1"
      alice <##. "1: supporter"
      alice ##> "/_app activate"
      alice <## "ok"
      alice ##> "/p"
      alice <## "user profile: alice (Alice)"
      alice <## "use /p <name> [<bio>] to change it"

-- A refusal the service will not take back is worth telling the user at once: the badge is still
-- worn and will start showing as expired.
testIssueFailedAlert :: HasCallStack => TestParams -> IO ()
testIssueFailedAlert ps =
  withBadgeServiceEnv ps $ \BadgeServiceEnv {bsClock, bsClientCfg, bsController = cc} ->
    withNewTestChatCfg ps bsClientCfg "alice" aliceProfile $ \alice -> do
      code <- issueCode cc BTSupporter 3
      redeemFirstBadge alice code
      rows <- ledgerRows (chatController alice) "badge_ledger"
      -- the service no longer knows this purchase, so it refuses with no retryAfter: terminal
      setServicePurchaseKey cc "not the purchase key"
      setClockAt bsClock $ fst $ renewalMoments rows
      alice ##> "/_app activate"
      alice <## "ok"
      alice <## badgeServiceRefused
      alice <##. "badge alert: issue_failed "
      -- the state reports the failure, when the next attempt is due, and the alert it raised
      alice <##. "1: supporter"
      alice <##. "renewal failing since "
      alice <##. "badge alert: issue_failed "
      issueErrorRow (chatController alice) >>= \(_, reason) ->
        reason `shouldBe` Just "service_error final unknown_purchase_key"

-- A failure that can clear on its own is not worth a word while contacts still see the badge as
-- valid: neither the alert nor the state shows it until the shown credential lapses, which is when
-- they stop. It is recorded from the first attempt, so the run's start is not lost.
testIssueFailedWaitsForExpiry :: HasCallStack => TestParams -> IO ()
testIssueFailedWaitsForExpiry ps =
  withBadgeServiceEnv ps $ \BadgeServiceEnv {bsClock, bsClientCfg, bsController = cc} -> do
    -- the redemption runs against the service, so it keeps the ordinary request timeout
    (requestAt, presentAt) <- withNewTestChatCfg ps bsClientCfg "alice" aliceProfile $ \alice -> do
      code <- issueCode cc BTSupporter 3
      redeemFirstBadge alice code
      renewalMoments <$> ledgerRows (chatController alice) "badge_ledger"
    stopBadgeService cc
    let cfg = failingServiceCfg bsClientCfg
    -- the request goes unanswered and times out, which is a failure that can clear on its own
    setClockAt bsClock requestAt
    (failedSince, errorAt, wake) <- withTestChatCfg ps cfg "alice" $ \alice -> do
      alice <##. "1: supporter"
      -- the state carries no failure and no alert: /p prints only its own output
      alice ##> "/p"
      alice <## "user profile: alice (Alice, * supporter)"
      alice <## "use /p <name> [<bio>] to change it"
      (since, reason) <- issueErrorRow (chatController alice)
      reason `shouldBe` Just "service_timeout"
      (since,,) <$> issueErrorAt (chatController alice) <*> nextWakes (chatController alice)
    -- restarted before the wake that failure set, it waits for it: a request sent would fail after 2s and be recorded
    setClockAt bsClock $ addUTCTime 1800 requestAt
    withTestChatCfg ps cfg "alice" $ \alice -> do
      threadDelay 3000000
      (alice </)
      issueErrorAt (chatController alice) `shouldReturn` errorAt
      nextWakes (chatController alice) `shouldReturn` wake
    -- the shown credential lapses: from here contacts see the badge as expired, and it is worth a word
    setClockAt bsClock presentAt
    withTestChatCfg ps cfg "alice" $ \alice -> do
      alice <##. "1: supporter"
      alice <##. "renewal failing since "
      alice <##. "badge alert: issue_failed "
      -- still the one run: the alert's episode is when it started, not this pass
      issueErrorRow (chatController alice) >>= \(since, _) -> since `shouldBe` failedSince

-- Acknowledging answers the run that is failing, not the failure: the error stays on the badge
-- screen while the alert goes quiet, and a later run raises it again under a new episode.
testIssueFailedAckAndNewRun :: HasCallStack => TestParams -> IO ()
testIssueFailedAckAndNewRun ps =
  withBadgeServiceEnv ps $ \BadgeServiceEnv {bsClock, bsClientCfg, bsController = cc} ->
    withNewTestChatCfg ps bsClientCfg "alice" aliceProfile $ \alice -> do
      code <- issueCode cc BTSupporter 3
      redeemFirstBadge alice code
      rows <- ledgerRows (chatController alice) "badge_ledger"
      purchaseKey <- servicePurchaseKey cc
      setServicePurchaseKey cc "not the purchase key"
      let (requestAt, presentAt) = renewalMoments rows
      setClockAt bsClock requestAt
      alice ##> "/_app activate"
      alice <## "ok"
      alice <## badgeServiceRefused
      alice <##. "badge alert: issue_failed "
      alice <##. "1: supporter"
      alice <##. "renewal failing since "
      alice <##. "badge alert: issue_failed "
      episode <- episodeOf . fst <$> issueErrorRow (chatController alice)
      errorAt <- issueErrorAt (chatController alice)
      alice ##> ("/_badge ack 1 1 issue_failed off " <> T.unpack episode)
      -- the state still carries the error, and no alert with it
      alice <##. "1: supporter"
      alice <##. "renewal failing since "
      -- the ack signalled the worker, and the pass it ran waited for the wake the refusal set, sending nothing
      threadDelay 2000000
      (alice </)
      issueErrorAt (chatController alice) `shouldReturn` errorAt
      ackedEpisode (chatController alice) `shouldReturn` (Just "issue_failed", Just episode)
      -- and so does opening the app before that wake
      setClockAt bsClock $ addUTCTime (nominalDay / 2) requestAt
      alice ##> "/_app activate"
      alice <## "ok"
      alice ##> "/_badge state 1"
      alice <##. "1: supporter"
      alice <##. "renewal failing since "
      threadDelay 2000000
      (alice </)
      issueErrorAt (chatController alice) `shouldReturn` errorAt
      -- the service is back: at the wake the month it owes is issued and presented, which ends the run,
      -- and only then is the next renewal asked for - so the run that fails next is a new one
      setServicePurchaseKey cc purchaseKey
      setClockAt bsClock presentAt
      alice ##> "/_app activate"
      alice <## "ok"
      renewed <- waitLedgerRows (chatController alice) 3
      alice <##. "1: supporter"
      waitIssueErrorCleared (chatController alice)
      waitShownIssued (chatController alice)
      setServicePurchaseKey cc "not the purchase key"
      setClockAt bsClock $ fst $ renewalMoments renewed
      alice ##> "/_app activate"
      alice <## "ok"
      alice <## badgeServiceRefused
      alice <##. "badge alert: issue_failed "
      alice <##. "1: supporter"
      alice <##. "renewal failing since "
      alice <##. "badge alert: issue_failed "
      -- a new run, so acknowledging the first one does not silence this one
      newEpisode <- episodeOf . fst <$> issueErrorRow (chatController alice)
      newEpisode `shouldNotBe` episode

-- The service issues nothing when the months it holds have run out - here through a debit the
-- client had not seen. That is support ending, not a failed renewal: the statement brings the
-- balance to nothing and the usual alert follows, where a recorded failure would name a
-- credential that was never issued.
testNoCredentialMonthsRanOut :: HasCallStack => TestParams -> IO ()
testNoCredentialMonthsRanOut ps =
  withBadgeServiceEnv ps $ \BadgeServiceEnv {bsClock, bsClientCfg, bsController = cc} ->
    withNewTestChatCfg ps bsClientCfg "alice" aliceProfile $ \alice -> do
      code <- issueCode cc BTSupporter 3
      redeemFirstBadge alice code
      rows <- ledgerRows (chatController alice) "badge_ledger"
      insertServiceSupportDebit cc
      -- at the credential's expiry the issued period has ended, so the service holds no current credential either
      setClockAt bsClock $ snd $ renewalMoments rows
      alice ##> "/_app activate"
      alice <## "ok"
      alice <##. "badge alert: support_ended "
      alice <##. "1: supporter, 0 months left"
      alice <##. "badge alert: support_ended "
      issueErrorRow (chatController alice) `shouldReturn` (Nothing, Nothing)
      -- the debit is the client's now, and the next pass retires the badge on it
      rows' <- ledgerRows (chatController alice) "badge_ledger"
      map (\(_, ch, m, _, _, t) -> (ch, m, t)) rows' `shouldBe` [(3, 3, Just "code"), (-1, 2, Just "badge"), (-2, 0, Just "support")]
      alice ##> "/_app activate"
      alice <### ["ok", StartsWith "1: supporter", StartsWith "badge alert: support_ended "]
      waitShownBadge (chatController alice) Nothing

-- The service issuing nothing while the ledger still owes a month is a fault the client cannot
-- resolve, so it is recorded and told at once.
testNoCredentialMonthsLeft :: HasCallStack => TestParams -> IO ()
testNoCredentialMonthsLeft ps =
  withBadgeServiceEnv ps $ \BadgeServiceEnv {bsClock, bsClientCfg, bsController = cc} ->
    withNewTestChatCfg ps bsClientCfg "alice" aliceProfile $ \alice -> do
      code <- issueCode cc BTSupporter 3
      redeemFirstBadge alice code
      rows <- ledgerRows (chatController alice) "badge_ledger"
      zeroServiceLedgerTip cc
      setClockAt bsClock $ snd $ renewalMoments rows
      alice ##> "/_app activate"
      alice <## "ok"
      alice <## "internal chat error: badge service issued no credential"
      alice <##. "badge alert: issue_failed "
      alice <##. "1: supporter"
      alice <##. "renewal failing since "
      alice <##. "badge alert: issue_failed "
      (_, reason) <- issueErrorRow (chatController alice)
      reason `shouldBe` Just "unexpected badge service issued no credential"

-- | A debit the service wrote on its own, taking back the months the purchase had: copied from
-- the tip so that it follows it in every column the client checks.
insertServiceSupportDebit :: ChatController -> IO ()
insertServiceSupportDebit ChatController {chatStore} =
  withTransaction chatStore $ \db ->
    DB.execute_ db . fromString $
      "INSERT INTO sx_badge_service_badge_ledger"
        <> " (entry_uuid, badge_purchase_id, change_months, balance_months, balance_start_ts, balance_anchor_ts,"
        <> "  balance_badge_type, service_created_at, created_at, entry_type, entry_debit_type)"
        <> " SELECT 'support-debit', badge_purchase_id, -balance_months, 0, balance_start_ts, balance_anchor_ts,"
        <> "  balance_badge_type, service_created_at, created_at, 'debit', 'support'"
        <> " FROM sx_badge_service_badge_ledger ORDER BY entry_id DESC LIMIT 1"

-- | The service's tip with its months struck out: nothing to issue, and no row restating the ledger.
zeroServiceLedgerTip :: ChatController -> IO ()
zeroServiceLedgerTip ChatController {chatStore} =
  withTransaction chatStore $ \db ->
    DB.execute_ db "UPDATE sx_badge_service_badge_ledger SET balance_months = 0 WHERE entry_id = (SELECT MAX(entry_id) FROM sx_badge_service_badge_ledger)"

badgeServiceRefused :: String
badgeServiceRefused = "bad chat command: badge service error: unknown_purchase_key"

-- | The alert's episode is the start of the run of failures, as the ack command spells it.
episodeOf :: HasCallStack => Maybe UTCTime -> Text
episodeOf = maybe (error "no failure recorded") (safeDecodeUtf8 . strEncode)

-- | A client that gives up on an unanswered service request in seconds rather than half a minute,
-- and does not retry a failed pass on its own - so every pass is one the test asked for.
failingServiceCfg :: ChatConfig -> ChatConfig
failingServiceCfg cfg =
  cfg
    { agentConfig = (agentConfig cfg) {serviceRequestTimeout = 2},
      badgeRetryInterval = RetryInterval {initialInterval = 3600000000, increaseAfter = 0, maxInterval = 3600000000}
    }

-- | The service reaches a purchase by the verified signer's key and no other way, so changing the
-- key it holds makes it answer unknown_purchase_key, and putting it back makes renewals work again.
servicePurchaseKey :: HasCallStack => ChatController -> IO ByteString
servicePurchaseKey ChatController {chatStore} = do
  rows :: [(Binary ByteString, Int64)] <-
    withTransaction chatStore $ \db ->
      DB.query_ db "SELECT purchase_key, badge_purchase_id FROM sx_badge_service_badge_purchases"
  pure $ case rows of
    [(Binary k, _)] -> k
    _ -> error $ "expected one service purchase, got " <> show (length rows)

setServicePurchaseKey :: ChatController -> ByteString -> IO ()
setServicePurchaseKey ChatController {chatStore} k =
  withTransaction chatStore $ \db ->
    DB.execute db "UPDATE sx_badge_service_badge_purchases SET purchase_key = ?" (Only (Binary k))

-- the run of failed renewals the purchase carries: when it started, and the last failure as stored
issueErrorRow :: HasCallStack => ChatController -> IO (Maybe UTCTime, Maybe Text)
issueErrorRow ChatController {chatStore} = do
  rows :: [(Maybe UTCTime, Maybe Text)] <-
    withTransaction chatStore $ \db ->
      DB.query_ db "SELECT issue_failed_since, issue_error FROM badge_purchases"
  pure $ case rows of
    [r] -> r
    _ -> error $ "expected one badge purchase, got " <> show (length rows)

issueErrorAt :: ChatController -> IO [Maybe UTCTime]
issueErrorAt ChatController {chatStore} =
  withTransaction chatStore $ \db -> map fromOnly <$> DB.query_ db "SELECT issue_error_at FROM badge_purchases"

-- the clearing is written by the pass that stored the issuance, which the test waits for
waitIssueErrorCleared :: HasCallStack => ChatController -> IO ()
waitIssueErrorCleared cc = loop (100 :: Int)
  where
    loop 0 = issueErrorRow cc >>= (`shouldBe` (Nothing, Nothing))
    loop i =
      issueErrorRow cc >>= \r ->
        if r == (Nothing, Nothing) then pure () else threadDelay 50000 >> loop (i - 1)

testRenewalKeepsActiveProfile :: HasCallStack => TestParams -> IO ()
testRenewalKeepsActiveProfile ps =
  withBadgeServiceEnv ps $ \BadgeServiceEnv {bsClock, bsClientCfg, bsController = cc} ->
    withNewTestChatCfg ps bsClientCfg "alice" aliceProfile $ \alice -> do
      code <- issueCode cc BTSupporter 3
      redeemFirstBadge alice code
      rows <- ledgerRows (chatController alice) "badge_ledger"
      alice ##> "/create user alisa"
      showActiveUser alice "alisa"
      let (requestAt, presentAt) = renewalMoments rows
      setClockAt bsClock requestAt
      alice ##> "/_app activate"
      alice <## "ok"
      renewed <- waitLedgerRows (chatController alice) 3
      map (\(_, ch, m, _, _, t) -> (ch, m, t)) renewed
        `shouldBe` [(3, 3, Just "code"), (-1, 2, Just "badge"), (-1, 1, Just "badge")]
      alice <##. "[user: alice] 1: supporter"
      setClockAt bsClock presentAt
      alice ##> "/_app activate"
      alice <## "ok"
      waitShownIssued (chatController alice)
      alice ##> "/p"
      showActiveUser alice "alisa"

testSnoozedAlertReturns :: HasCallStack => TestParams -> IO ()
testSnoozedAlertReturns ps =
  withBadgeServiceEnv ps $ \BadgeServiceEnv {bsClock, bsClientCfg, bsController = cc} ->
    withNewTestChatCfg ps bsClientCfg "alice" aliceProfile $ \alice -> do
      code <- issueCode cc BTSupporter 1
      redeemFirstBadge alice code
      rows <- ledgerRows (chatController alice) "badge_ledger"
      let endsAt = dueAtOf rows
      setClockAt bsClock endsAt
      alice `send` "/_app activate"
      alice
        <### [ "/_app activate",
               "ok",
               StartsWith "badge alert: support_ended ",
               StartsWith "1: supporter",
               StartsWith "badge alert: support_ended "
             ]
      alice ##> ("/_badge ack 1 1 support_ended on " <> T.unpack (safeDecodeUtf8 $ strEncode endsAt))
      alice <##. "1: supporter"
      -- no request waits, so the snooze's end, a day after the ack, is the wake
      let snoozeWake = \case
            [Just w] -> w >= addUTCTime nominalDay endsAt && w < addUTCTime (nominalDay + 60) endsAt
            _ -> False
      (snoozeWake <$> nextWakes (chatController alice)) `shouldEventuallyReturn` True
      alice ##> "/p"
      alice <## "user profile: alice (Alice)"
      alice <## "use /p <name> [<bio>] to change it"
      setClockAt bsClock $ addUTCTime (nominalDay + 60) endsAt
      alice `send` "/_app activate"
      alice <### ["/_app activate", "ok", StartsWith "badge alert: support_ended "]

testRenewalKeepsProfileEdits :: HasCallStack => TestParams -> IO ()
testRenewalKeepsProfileEdits ps =
  withBadgeServiceEnv ps $ \BadgeServiceEnv {bsClock, bsClientCfg, bsController = cc} ->
    withNewTestChatCfg ps bsClientCfg "alice" aliceProfile $ \alice ->
      withNewTestChatCfg ps bsClientCfg "bob" bobProfile $ \bob -> do
        connectUsers alice bob
        code <- issueCode cc BTSupporter 3
        redeemFirstBadge alice code
        alice ##> "/p alice Alice Jones"
        concurrentlyN_
          [ alice <## "user bio changed to Alice Jones (your 1 contacts are notified)",
            bob <## "contact alice updated bio: Alice Jones"
          ]
        rows <- ledgerRows (chatController alice) "badge_ledger"
        let (requestAt, presentAt) = renewalMoments rows
        setClockAt bsClock requestAt
        alice ##> "/_app activate"
        alice <## "ok"
        alice <##. "1: supporter"
        renewed <- waitLedgerRows (chatController alice) 3
        map (\(_, ch, m, _, _, t) -> (ch, m, t)) renewed
          `shouldBe` [(3, 3, Just "code"), (-1, 2, Just "badge"), (-1, 1, Just "badge")]
        setClockAt bsClock presentAt
        alice ##> "/_app activate"
        alice <## "ok"
        waitShownIssued (chatController alice)
        -- Had the renewal carried the profile the worker started with, bob would print a bio change back to "Alice" before the next message, so that message is what asserts the edited bio survived.
        alice #> "@bob after renewal"
        bob <# "alice *> after renewal"
        bob ##> "/i alice"
        bob <## "contact ID: 2"
        bob <## "supporter badge - active"
        bob <##. "expires "
        bob <## "receiving messages via: localhost"
        bob <## "sending messages via: localhost"
        bob <## "you've shared main profile with this contact"
        bob <## "connection not verified, use /code command to see security code"
        bob <## "quantum resistant end-to-end encryption"
        bob <## currentChatVRangeInfo

testPresentationCatchesUp :: HasCallStack => TestParams -> IO ()
testPresentationCatchesUp ps =
  withBadgeServiceEnv ps $ \BadgeServiceEnv {bsClock, bsClientCfg, bsController = cc} ->
    withNewTestChatCfg ps bsClientCfg "alice" aliceProfile $ \alice ->
      withNewTestChatCfg ps bsClientCfg "bob" bobProfile $ \bob -> do
        connectUsers alice bob
        code <- issueCode cc BTSupporter 3
        redeemFirstBadge alice code
        alice #> "@bob hi"
        bob <# "alice *> hi"
        rows <- ledgerRows (chatController alice) "badge_ledger"
        let (requestAt, presentAt) = renewalMoments rows
        setClockAt bsClock requestAt
        alice ##> "/_app activate"
        alice <## "ok"
        alice <##. "1: supporter"
        void $ waitLedgerRows (chatController alice) 3
        setClockAt bsClock presentAt
        alice ##> "/_app activate"
        alice <## "ok"
        waitShownIssued (chatController alice)
        expiries <- issuedExpiries (chatController alice)
        length expiries `shouldBe` 2
        let firstMonth = head expiries
            latestMonth = last expiries
        -- The test waits here first so that bob does not receive the presentation after the updates below and keep it.
        waitPeerBadgeExpiry (chatController bob) latestMonth
        setBadgeExpiry (chatController alice) "badge_signature" firstMonth
        setBadgeExpiry (chatController bob) "badge_proof" firstMonth
        alice ##> "/_app activate"
        alice <## "ok"
        waitPeerBadgeExpiry (chatController bob) latestMonth
        (shown, issued) <- shownAndIssuedExpiry (chatController alice)
        shown `shouldBe` Just latestMonth
        issued `shouldBe` Just latestMonth
        alice #> "@bob after repair"
        bob <# "alice *> after repair"

waitPeerBadgeExpiry :: HasCallStack => ChatController -> UTCTime -> IO ()
waitPeerBadgeExpiry cc expected = loop (100 :: Int)
  where
    loop 0 = peerBadgeExpiry cc >>= \actual -> actual `shouldBe` Just expected
    loop i =
      peerBadgeExpiry cc >>= \actual ->
        if actual == Just expected then pure () else threadDelay 50000 >> loop (i - 1)

testPurchaseKeyMismatch :: HasCallStack => TestParams -> IO ()
testPurchaseKeyMismatch ps =
  withBadgeService ps $ \clientCfg bsLink _ ->
    withNewTestChatCfg ps clientCfg "alice" aliceProfile $ \alice -> do
      g <- C.newRandom
      (_, signPriv) <- atomically $ C.generateKeyPair g :: IO (C.KeyPair 'C.Ed25519)
      (claimedPub, _) <- atomically $ C.generateKeyPair g :: IO (C.KeyPair 'C.Ed25519)
      let signKey = B.unpack $ strEncode (C.StoredPrivateKey signPriv)
          claimed = B.unpack $ strEncode claimedPub
          req = "{\"version\":1,\"purchaseKey\":\"" <> claimed <> "\",\"request\":{\"type\":\"pauseBadge\"}}"
      alice ##> ("/_service_request 1 " <> bsLink <> " sign_key=" <> signKey <> " " <> req)
      alice <## "service response: {\"code\":\"bad_request\",\"type\":\"error\"}"

testIssuerKeyMustMatchConfig :: HasCallStack => TestParams -> IO ()
testIssuerKeyMustMatchConfig ps = do
  Right (pk, sk) <- bbsKeyGen
  Right (_, otherSk) <- bbsKeyGen
  let optsFor sk' = mkBadgeServiceOpts ps sk'
      cfg = testCfg {badgePublicKeys = M.singleton testIssuerKeyIdx pk}
  checkIssuerKey (optsFor sk) Nothing cfg >>= (`shouldSatisfy` isRight)
  checkIssuerKey (optsFor otherSk) Nothing cfg >>= (`shouldSatisfy` isLeft)
  checkIssuerKey (optsFor sk) Nothing testCfg {badgePublicKeys = M.empty} >>= (`shouldSatisfy` isLeft)
  let halfGiven = (optsFor sk) {issuerKey = Left "--issuer-key-idx and --issuer-secret are given together or not at all"}
  checkIssuerKey halfGiven Nothing cfg >>= (`shouldSatisfy` isLeft)

testIssuerIniKeyMustBeTrusted :: HasCallStack => TestParams -> IO ()
testIssuerIniKeyMustBeTrusted ps = do
  Right (pk, sk) <- bbsKeyGen
  Right (_, untrusted) <- bbsKeyGen
  let cfg = testCfg {badgePublicKeys = M.singleton testIssuerKeyIdx pk}
      fromCli = mkBadgeServiceOpts ps sk
      fromIni = fromCli {issuerKey = Right Nothing}
      iniWith idx k = withIssuer ["index = " <> tshow idx, "private_key = " <> safeDecodeUtf8 (strEncode k)] readServiceConfig
  Right trusted <- iniWith testIssuerKeyIdx sk
  checkIssuerKey fromIni (Just trusted) cfg `shouldReturn` Right (BadgeIssuerKey testIssuerKeyIdx sk)

  Right wrongSecret <- iniWith testIssuerKeyIdx untrusted
  checkIssuerKey fromIni (Just wrongSecret) cfg >>= (`shouldSatisfy` isLeft)
  checkIssuerKey fromCli (Just wrongSecret) cfg `shouldReturn` Right (BadgeIssuerKey testIssuerKeyIdx sk)

  Right unknownIndex <- iniWith (testIssuerKeyIdx + 8) sk
  checkIssuerKey fromIni (Just unknownIndex) cfg
    `shouldReturn` Left ("no configured badge key at index " <> show (testIssuerKeyIdx + 8) <> ", clients could not verify what this service signs")

testRedeemUnpaidCode :: HasCallStack => TestParams -> IO ()
testRedeemUnpaidCode ps =
  withBadgeService ps $ \clientCfg _ cc ->
    withNewTestChatCfg ps clientCfg "alice" aliceProfile $ \alice -> do
      unpaid <- issueCodeAs cc BTSupporter 1 "unpaid"
      alice ##> ("/_redeem_badge_code 1 " <> codeArg unpaid)
      alice <## "cannot get badge: badge service error: payment_pending"
      paid <- issueCodeAs cc BTSupporter 1 "paid"
      alice ##> ("/_redeem_badge_code 1 " <> codeArg paid)
      alice <## "badge redeemed"
      alice <## "supporter badge - active"
      alice <##. "expires "

-- | A redemption deadline is a year out and cannot be waited for, so the code is issued unpaid and then marked paid with a deadline already in the past.
testExpiredCode :: HasCallStack => TestParams -> IO ()
testExpiredCode ps =
  withBadgeService ps $ \clientCfg _ cc ->
    withNewTestChatCfg ps clientCfg "alice" aliceProfile $ \alice -> do
      code <- issueCodeAs cc BTSupporter 1 "unpaid"
      now <- getCurrentTime
      withDB' "markCodePaid" cc (\db -> markCodePaid db (badgeCodeHash code) (addUTCTime (-60) now))
        `shouldReturn` Right ()
      alice ##> ("/_redeem_badge_code 1 " <> codeArg code)
      alice <## "cannot get badge: badge service error: code_expired"

testRedeemedBeforeTheDeadline :: HasCallStack => TestParams -> IO ()
testRedeemedBeforeTheDeadline ps =
  withBadgeService ps $ \clientCfg _ cc ->
    withNewTestChatCfg ps clientCfg "alice" aliceProfile $ \alice -> do
      code <- issueCodeAs cc BTSupporter 1 "unpaid"
      now <- getCurrentTime
      withDB' "markCodePaid" cc (\db -> markCodePaid db (badgeCodeHash code) (addUTCTime 3600 now))
        `shouldReturn` Right ()
      alice ##> ("/_redeem_badge_code 1 " <> codeArg code)
      alice <## "badge redeemed"
      alice <## "supporter badge - active"
      alice <##. "expires "

      -- The expiry is set with direct SQL because markCodePaid writes it only on the unpaid-to-paid transition, which this code has already made.
      withDB' "expireCode" cc (\db ->
        DB.execute
          db
          "UPDATE sx_badge_service_badge_codes SET expires_at = ? WHERE code_hash = ?"
          (addUTCTime (-60) now, Binary (badgeCodeHash code)))
        `shouldReturn` Right ()
      alice ##> ("/_redeem_badge_code 1 " <> codeArg code)
      alice <## "badge already redeemed"

testRevokeRedeemedCode :: HasCallStack => TestParams -> IO ()
testRevokeRedeemedCode ps =
  withBadgeService ps $ \clientCfg _ cc ->
    withNewTestChatCfg ps clientCfg "alice" aliceProfile $ \alice -> do
      code <- issueCodeAs cc BTSupporter 1 "paid"
      alice ##> ("/_redeem_badge_code 1 " <> codeArg code)
      alice <## "badge redeemed"
      alice <## "supporter badge - active"
      alice <##. "expires "
      refused <- revokeCodeAs cc code
      refused `shouldBe` Left "Fully redeemed. It cannot be revoked."
      alice ##> ("/_redeem_badge_code 1 " <> codeArg code)
      alice <## "badge already redeemed"

testRevokeUnknownCode :: HasCallStack => TestParams -> IO ()
testRevokeUnknownCode ps =
  withBadgeService ps $ \_ _ cc -> do
    g <- C.newRandom
    code <- randomBadgeCode g
    unknown <- revokeCodeAs cc code
    unknown `shouldBe` Left "No such code."

testRevokedCode :: HasCallStack => TestParams -> IO ()
testRevokedCode ps =
  withBadgeService ps $ \clientCfg _ cc ->
    withNewTestChatCfg ps clientCfg "alice" aliceProfile $ \alice -> do
      paid <- issueCodeAs cc BTSupporter 1 "paid"
      revokeCodeAs cc paid `shouldReturn` Right "Revoked."
      alice ##> ("/_redeem_badge_code 1 " <> codeArg paid)
      alice <## "cannot get badge: badge service error: code_invalid"
      revokeCodeAs cc paid `shouldReturn` Right "Already revoked."

testRevokedUnpaidCode :: HasCallStack => TestParams -> IO ()
testRevokedUnpaidCode ps =
  withBadgeServiceEnv ps $ \env@BadgeServiceEnv {bsController = cc} -> do
    code <- issueCodeAs cc BTSupporter 1 "unpaid"
    revokeCodeAs cc code `shouldReturn` Right "Revoked."
    redeemAsNewPurchase env code >>= (`shouldAnswerError` BSECodeInvalid)

testRevokeRejectsTrailingInput :: HasCallStack => TestParams -> IO ()
testRevokeRejectsTrailingInput ps =
  withBadgeService ps $ \_ _ cc -> do
    code <- issueCodeAs cc BTSupporter 1 "paid"
    refused <- revokeRaw cc (formatBadgeCode code <> " junk")
    -- If the refused call revoked the code anyway, this fails with "Already revoked.".
    revokeCodeAs cc code `shouldReturn` Right "Revoked."
    refused `shouldBe` Left badgeCmdUsage

-- The text is spelled out rather than taken from the service, so a change there fails here.
badgeCmdUsage :: Text
badgeCmdUsage = "Usage: //issue supporter|legend|investor [months 1-255] [paid|unpaid|free], or //revoke <code>"

purchaseCmd :: BadgeMasterKey -> ServicePayment -> BadgeServiceCommand
purchaseCmd masterKey payment = BSCPurchaseBadge {masterKey, payment, upgrade = Nothing}

supporterPlay :: ServicePayment
supporterPlay = googlePayment "badge_supporter_01" googleSupporterToken

paymentArg :: ServicePayment -> String
paymentArg = LB.unpack . J.encode

refusalOf :: HasCallStack => BadgeServiceResponse -> (BadgeServiceErrorCode, Maybe Word32)
refusalOf = \case
  BSPError {code, retryAfter} -> (code, retryAfter)
  r -> error $ "expected an error, got " <> show (J.toJSON r)

rowCount :: ChatController -> String -> IO Int
rowCount ChatController {chatStore} table =
  withTransaction chatStore $ \db -> do
    [Only n] <- DB.query_ db $ fromString $ "SELECT COUNT(*) FROM " <> table
    pure n

-- | Each store payment's provider, amount and currency, and the ledger rows that name it.
storePayments :: ChatController -> IO [(Text, Maybe Int, Maybe Text, Int)]
storePayments ChatController {chatStore} =
  withTransaction chatStore $ \db ->
    DB.query_ db . fromString $
      "SELECT pay.provider, pay.amount, pay.currency, COUNT(l.entry_id) FROM sx_badge_service_payments pay "
        <> "LEFT JOIN sx_badge_service_badge_ledger l ON l.payment_id = pay.payment_id "
        <> "GROUP BY pay.payment_id, pay.provider, pay.amount, pay.currency ORDER BY pay.provider"

nothingPurchased :: HasCallStack => ChatController -> IO ()
nothingPurchased cc = do
  rowCount cc "sx_badge_service_payments" `shouldReturn` 0
  rowCount cc "sx_badge_service_badge_purchases" `shouldReturn` 0

testStorePurchase :: HasCallStack => TestParams -> IO ()
testStorePurchase ps =
  withBadgeServiceEnv ps $ \env@BadgeServiceEnv {bsController = cc} -> do
    (purchaseKey, masterKey) <- newPurchaseKeys
    purchased <- serviceCmd env purchaseKey $ purchaseCmd masterKey supporterPlay
    let (entries, previousEntryId) = statementOf purchased
    previousEntryId `shouldBe` Nothing
    map entryTag entries `shouldBe` ["payment", "badge"]
    map (\e -> let (c, m, _) = entryOf e in (c, m)) entries `shouldBe` [(1, 1), (-1, 0)]
    badgeTypeOf purchased `shouldBe` Just BTSupporter
    masterKeyOf purchased `shouldBe` Just masterKey
    map (\(_, c, m, _, _, t) -> (c, m, t)) <$> ledgerRows cc "sx_badge_service_badge_ledger" `shouldReturn` [(1, 1, Just "payment"), (-1, 0, Just "badge")]
    -- Play reports no price, and only the credit names the payment
    storePayments cc `shouldReturn` [("google", Nothing, Nothing, 1)]

testStoreReceiptUsed :: HasCallStack => TestParams -> IO ()
testStoreReceiptUsed ps =
  withBadgeServiceEnv ps $ \env@BadgeServiceEnv {bsController = cc} -> do
    (firstKey, firstMasterKey) <- newPurchaseKeys
    _ <- serviceCmd env firstKey $ purchaseCmd firstMasterKey supporterPlay
    ledger <- ledgerRows cc "sx_badge_service_badge_ledger"
    (otherKey, otherMasterKey) <- newPurchaseKeys
    used <- serviceCmd env otherKey $ purchaseCmd otherMasterKey supporterPlay
    refusalOf used `shouldBe` (BSEReceiptUsed, Nothing)
    ledgerRows cc "sx_badge_service_badge_ledger" `shouldReturn` ledger
    rowCount cc "sx_badge_service_badge_purchases" `shouldReturn` 1

testStoreReceiptInvalid :: HasCallStack => TestParams -> IO ()
testStoreReceiptInvalid ps =
  withBadgeServiceEnv ps $ \env@BadgeServiceEnv {bsController = cc, bsStore = FakeStore {appleSandboxJWS}} -> do
    (purchaseKey, masterKey) <- newPurchaseKeys
    let refused payment = refusalOf <$> serviceCmd env purchaseKey (purchaseCmd masterKey payment)
    refused (googlePayment "badge_supporter_01" "not-a-purchase") `shouldReturn` (BSEReceiptInvalid, Nothing)
    refused SPApple {jws = "not.a.jws"} `shouldReturn` (BSEReceiptInvalid, Nothing)
    refused SPApple {jws = unsignedJWS "{\"transactionId\":\"2000000812345699\",\"productId\":\"BADGE_SUPPORTER_01\"}"} `shouldReturn` (BSEReceiptInvalid, Nothing)
    -- named rather than lumped in with a forgery, since the device that bought it already knows
    refused SPApple {jws = appleSandboxJWS} `shouldReturn` (BSETestPurchase, Nothing)
    nothingPurchased cc

testStoreTestPurchaseCredited :: HasCallStack => TestParams -> IO ()
testStoreTestPurchaseCredited ps =
  withBadgeServiceEnv ps $ \env@BadgeServiceEnv {bsClock, bsController = cc, bsStore = store@FakeStore {appleSandboxJWS, appleSandboxSubscriptionJWS}} -> do
    now <- serviceNow env
    let crediting = env {bsVerifier = (fakeVerifier store) {creditTestPurchases = True}}
        testToken = "fake-play-token-subscription-test.AO-J1Os2q5djX0pRm6"
        testCheck planId = SubscriptionCheck {subscriptionState = SSRenewing, testSubscription = True, latestCharge = Just $ storeCharge planId (addUTCTime (-3600) now) 1}
        receipts = [SPApple {jws = appleSandboxJWS}, googlePayment "subscr_badge_supporter_01" testToken, SPApple {jws = appleSandboxSubscriptionJWS}]
        purchaseUnderNewKey verifierEnv payment = do
          (purchaseKey, masterKey) <- newPurchaseKeys
          serviceCmd verifierEnv purchaseKey $ purchaseCmd masterKey payment
    setSubscription store (SubscriptionGoogle testToken) $ testCheck "subscr-badge-supporter-month-02"
    setSubscription store (SubscriptionApple appleSubscriptionOriginalId) $ testCheck "SUBSCR_BADGE_SUPPORTER_MONTH_01"
    credited <- mapM (purchaseUnderNewKey crediting) receipts
    map (map entryTag . fst . statementOf) credited `shouldBe` [["payment", "badge"], ["charge", "badge"], ["charge", "badge"]]
    map badgeTypeOf credited `shouldBe` [Just BTLegend, Just BTSupporter, Just BTSupporter]
    -- past the hour the Play claim is held for since it was credited, so Play is asked again
    setClockAt bsClock $ addUTCTime 3700 now
    refused <- mapM (purchaseUnderNewKey env) receipts
    map refusalOf refused `shouldBe` replicate 3 (BSETestPurchase, Nothing)
    rowCount cc "sx_badge_service_badge_purchases" `shouldReturn` 3

testStorePending :: HasCallStack => TestParams -> IO ()
testStorePending ps =
  withBadgeServiceEnv ps $ \env@BadgeServiceEnv {bsController = cc, bsStore = store} -> do
    (purchaseKey, masterKey) <- newPurchaseKeys
    let unsettled = purchaseCmd masterKey $ googlePayment "badge_supporter_01" googlePendingToken
    waiting <- serviceCmd env purchaseKey unsettled
    refusalOf waiting `shouldSatisfy` \(code, retryAfter) -> code == BSEPaymentPending && isJust retryAfter
    nothingPurchased cc
    settlePending store
    settled <- serviceCmd env purchaseKey unsettled
    badgeTypeOf settled `shouldBe` Just BTSupporter
    again <- serviceCmd env purchaseKey unsettled
    credentialOf again `shouldBe` credentialOf settled
    rowCount cc "sx_badge_service_badge_purchases" `shouldReturn` 1
    storePayments cc `shouldReturn` [("google", Nothing, Nothing, 1)]

testStoreAcknowledgesBeforeCrediting :: HasCallStack => TestParams -> IO ()
testStoreAcknowledgesBeforeCrediting ps = do
  acknowledged <- newIORef []
  acknowledgement <- newIORef $ Right ()
  let acknowledging store = (fakeVerifier store) {acknowledgeGoogle = Just $ \_ token -> modifyIORef' acknowledged (token :) >> readIORef acknowledgement}
  withBadgeServiceVerifier ps acknowledging $ \env@BadgeServiceEnv {bsController = cc} -> do
    (purchaseKey, masterKey) <- newPurchaseKeys
    let purchase payment = serviceCmd env purchaseKey $ purchaseCmd masterKey payment
    unsettled <- purchase $ googlePayment "badge_supporter_01" googlePendingToken
    fst (refusalOf unsettled) `shouldBe` BSEPaymentPending
    unpriced <- purchase $ googlePayment "badge_unknown_01" googleUnknownProductToken
    refusalOf unpriced `shouldBe` (BSEProductUnavailable, Nothing)
    readIORef acknowledged `shouldReturn` []
    writeIORef acknowledgement $ Left $ SRUnreachable "acknowledging: fake store is down"
    unacknowledged <- purchase supporterPlay
    refusalOf unacknowledged `shouldSatisfy` \(code, retryAfter) -> code == BSEProviderUnavailable && isJust retryAfter
    writeIORef acknowledgement $ Left $ SRVerifierFailed "acknowledging: the service account lacks the Play Console permission for this call"
    forbidden <- purchase supporterPlay
    refusalOf forbidden `shouldBe` (BSEInternal, Nothing)
    nothingPurchased cc
    writeIORef acknowledgement $ Right ()
    purchased <- purchase supporterPlay
    badgeTypeOf purchased `shouldBe` Just BTSupporter
    replayed <- purchase supporterPlay
    credentialOf replayed `shouldBe` credentialOf purchased
    readIORef acknowledged `shouldReturn` replicate 3 googleSupporterToken
    rowCount cc "sx_badge_service_badge_purchases" `shouldReturn` 1

testStoreUnknownProduct :: HasCallStack => TestParams -> IO ()
testStoreUnknownProduct ps =
  withBadgeServiceEnv ps $ \env@BadgeServiceEnv {bsController = cc} -> do
    (purchaseKey, masterKey) <- newPurchaseKeys
    -- a real purchase the store vouches for, of a product this service does not price
    unpriced <- serviceCmd env purchaseKey $ purchaseCmd masterKey $ googlePayment "badge_unknown_01" googleUnknownProductToken
    refusalOf unpriced `shouldBe` (BSEProductUnavailable, Nothing)
    nothingPurchased cc

testStoreUpgradeRefused :: HasCallStack => TestParams -> IO ()
testStoreUpgradeRefused ps =
  withBadgeServiceEnv ps $ \env@BadgeServiceEnv {bsController = cc} -> do
    (purchaseKey, masterKey) <- newPurchaseKeys
    g <- C.newRandom
    (fromPurchaseKey, fromPriv) <- atomically $ C.generateKeyPair g :: IO (C.KeyPair 'C.Ed25519)
    now <- getCurrentTime
    let upgrade = BadgeUpgrade {fromPurchaseKey, receipt = "receipt", receiptSignature = C.sign' fromPriv "receipt", balance = BadgeBalance {lastEntry = emptyEntry now BTSupporter}}
    upgraded <- serviceCmd env purchaseKey BSCPurchaseBadge {masterKey, payment = supporterPlay, upgrade = Just upgrade}
    refusalOf upgraded `shouldBe` (BSEBadRequest, Nothing)
    nothingPurchased cc

testStoreBadgeTypeFromProduct :: HasCallStack => TestParams -> IO ()
testStoreBadgeTypeFromProduct ps =
  withBadgeServiceEnv ps $ \env@BadgeServiceEnv {bsController = cc, bsStore = FakeStore {appleLegendJWS}} -> do
    (purchaseKey, masterKey) <- newPurchaseKeys
    legend <- serviceCmd env purchaseKey $ purchaseCmd masterKey SPApple {jws = appleLegendJWS}
    badgeTypeOf legend `shouldBe` Just BTLegend
    map (\StatementEntry {balanceBadgeType} -> balanceBadgeType) (fst $ statementOf legend) `shouldBe` [BTLegend, BTLegend]
    storePayments cc `shouldReturn` [("apple", Just 7000, Just "USD", 1)]

testNonStoreFundingRefused :: HasCallStack => TestParams -> IO ()
testNonStoreFundingRefused ps =
  withBadgeServiceEnv ps $ \env@BadgeServiceEnv {bsController = cc} -> do
    (purchaseKey, masterKey) <- newPurchaseKeys
    invoice <- serviceCmd env purchaseKey $ purchaseCmd masterKey SPInvoice {invoiceId = InvoiceId "inv"}
    refusalOf invoice `shouldBe` (BSEUnknownPurchaseKey, Nothing)
    transfer <- serviceCmd env purchaseKey $ purchaseCmd masterKey SPReceipt {receipt = "receipt"}
    refusalOf transfer `shouldBe` (BSEUnknownPurchaseKey, Nothing)
    nothingPurchased cc

testStoreReplayWhileStoreDown :: HasCallStack => TestParams -> IO ()
testStoreReplayWhileStoreDown ps =
  withBadgeServiceEnv ps $ \env@BadgeServiceEnv {bsController = cc, bsStore = store} -> do
    (purchaseKey, masterKey) <- newPurchaseKeys
    purchased <- serviceCmd env purchaseKey $ purchaseCmd masterKey supporterPlay
    ledger <- ledgerRows cc "sx_badge_service_badge_ledger"
    setGoogleDown store True
    -- the key it was credited to is answered from the record
    replayed <- serviceCmd env purchaseKey $ purchaseCmd masterKey supporterPlay
    credentialOf replayed `shouldBe` credentialOf purchased
    map entryTag (fst $ statementOf replayed) `shouldBe` map entryTag (fst $ statementOf purchased)
    -- any other key waits for the store, or the answer would tell it which purchases were credited
    (otherKey, otherMasterKey) <- newPurchaseKeys
    other <- serviceCmd env otherKey $ purchaseCmd otherMasterKey supporterPlay
    fst (refusalOf other) `shouldBe` BSEProviderUnavailable
    ledgerRows cc "sx_badge_service_badge_ledger" `shouldReturn` ledger
    storePayments cc `shouldReturn` [("google", Nothing, Nothing, 1)]
    rowCount cc "sx_badge_service_badge_purchases" `shouldReturn` 1

testStoreVerifierFailures :: HasCallStack => TestParams -> IO ()
testStoreVerifierFailures ps =
  withBadgeServiceEnv ps $ \env@BadgeServiceEnv {bsController = cc, bsStore = FakeStore {appleThrowingJWS}} -> do
    (purchaseKey, masterKey) <- newPurchaseKeys
    let answer payment = refusalOf <$> serviceCmd env purchaseKey (purchaseCmd masterKey payment)
    -- Apple is checked offline, so a verifier that throws was answered by nothing but its own bug
    answer SPApple {jws = appleThrowingJWS} `shouldReturn` (BSEInternal, Nothing)
    answer (googlePayment "badge_supporter_01" googleThrowingToken) >>= (`shouldSatisfy` \(code, retryAfter) -> code == BSEProviderUnavailable && isJust retryAfter)
    answer (googlePayment "badge_supporter_01" googleHangingToken) >>= (`shouldSatisfy` \(code, retryAfter) -> code == BSEProviderUnavailable && isJust retryAfter)
    nothingPurchased cc

testStoreVerifiedOtherTransaction :: HasCallStack => TestParams -> IO ()
testStoreVerifiedOtherTransaction ps =
  withBadgeServiceEnv ps $ \env@BadgeServiceEnv {bsController = cc, bsStore = FakeStore {appleMisnamedJWS}} -> do
    (purchaseKey, masterKey) <- newPurchaseKeys
    refusalOf <$> serviceCmd env purchaseKey (purchaseCmd masterKey SPApple {jws = appleMisnamedJWS}) `shouldReturn` (BSEInternal, Nothing)
    nothingPurchased cc

testStoreQuantityRefused :: HasCallStack => TestParams -> IO ()
testStoreQuantityRefused ps =
  withBadgeServiceEnv ps $ \env@BadgeServiceEnv {bsController = cc, bsStore = FakeStore {appleQuantityJWS}} -> do
    (purchaseKey, masterKey) <- newPurchaseKeys
    refusalOf <$> serviceCmd env purchaseKey (purchaseCmd masterKey SPApple {jws = appleQuantityJWS}) `shouldReturn` (BSEInternal, Nothing)
    nothingPurchased cc

testPurchaseWithNoVerifier :: HasCallStack => TestParams -> IO ()
testPurchaseWithNoVerifier ps =
  withBadgeServiceVerifier ps (const noStoreVerifier) $ \env@BadgeServiceEnv {bsClientCfg, bsController = cc} -> do
    (purchaseKey, masterKey) <- newPurchaseKeys
    refusalOf <$> serviceCmd env purchaseKey (purchaseCmd masterKey supporterPlay) `shouldReturn` (BSEProviderNotConfigured, Nothing)
    nothingPurchased cc
    withNewTestChatCfg ps bsClientCfg "alice" aliceProfile $ \alice -> do
      alice ##> ("/_badge purchase 1 " <> paymentArg supporterPlay)
      storePurchaseOpen alice ""
      -- not yet rather than never, so the receipt stays held for the retry once a verifier is deployed
      heldStoreReceiptErrors (chatController alice) `shouldEventuallyReturn` ["service_error final provider_not_configured"]
      heldStoreReceipts (chatController alice) `shouldReturn` 1
      alice ##> "/_badge state 1"
      alice .<## ", not credited: service_error final provider_not_configured"

testStorePurchaseRace :: HasCallStack => TestParams -> IO ()
testStorePurchaseRace ps = do
  hook <- newIORef (pure ())
  withBadgeServiceVerifier ps (googleVerifierWithHook hook) $ \env@BadgeServiceEnv {bsController = cc} -> do
    (purchaseKey, masterKey) <- newPurchaseKeys
    winner <- newIORef Nothing
    -- the same purchase is presented again while the first is with the store, and credited first
    writeIORef hook $ serviceCmd env purchaseKey (purchaseCmd masterKey supporterPlay) >>= writeIORef winner . Just
    raced <- serviceCmd env purchaseKey $ purchaseCmd masterKey supporterPlay
    Just credited <- readIORef winner
    credentialOf credited `shouldSatisfy` isJust
    credentialOf raced `shouldBe` credentialOf credited
    storePayments cc `shouldReturn` [("google", Nothing, Nothing, 1)]
    rowCount cc "sx_badge_service_badge_purchases" `shouldReturn` 1

testStorePurchaseRaceOtherKey :: HasCallStack => TestParams -> IO ()
testStorePurchaseRaceOtherKey ps = do
  hook <- newIORef (pure ())
  withBadgeServiceVerifier ps (googleVerifierWithHook hook) $ \env@BadgeServiceEnv {bsController = cc} -> do
    (purchaseKey, masterKey) <- newPurchaseKeys
    (otherKey, otherMasterKey) <- newPurchaseKeys
    winner <- newIORef Nothing
    -- another key presents the same purchase while the first is with the store, and is credited first
    writeIORef hook $ serviceCmd env otherKey (purchaseCmd otherMasterKey supporterPlay) >>= writeIORef winner . Just
    raced <- serviceCmd env purchaseKey $ purchaseCmd masterKey supporterPlay
    refusalOf raced `shouldBe` (BSEReceiptUsed, Nothing)
    Just credited <- readIORef winner
    masterKeyOf credited `shouldBe` Just otherMasterKey
    ledger <- ledgerRows cc "sx_badge_service_badge_ledger"
    replayed <- serviceCmd env otherKey $ purchaseCmd otherMasterKey supporterPlay
    credentialOf replayed `shouldBe` credentialOf credited
    ledgerRows cc "sx_badge_service_badge_ledger" `shouldReturn` ledger
    storePayments cc `shouldReturn` [("google", Nothing, Nothing, 1)]
    rowCount cc "sx_badge_service_badge_purchases" `shouldReturn` 1

testStorePurchaseKeyMismatch :: HasCallStack => TestParams -> IO ()
testStorePurchaseKeyMismatch ps =
  withBadgeService ps $ \clientCfg bsLink cc ->
    withNewTestChatCfg ps clientCfg "alice" aliceProfile $ \alice -> do
      g <- C.newRandom
      (_, signPriv) <- atomically $ C.generateKeyPair g :: IO (C.KeyPair 'C.Ed25519)
      (claimedPub, _) <- atomically $ C.generateKeyPair g :: IO (C.KeyPair 'C.Ed25519)
      let signKey = B.unpack $ strEncode (C.StoredPrivateKey signPriv)
          claimed = B.unpack $ strEncode claimedPub
          req =
            "{\"version\":1,\"purchaseKey\":\"" <> claimed
              <> "\",\"request\":{\"type\":\"purchaseBadge\",\"masterKey\":\"" <> testMasterKeyB64
              <> "\",\"payment\":" <> paymentArg supporterPlay <> "}}"
      alice ##> ("/_service_request 1 " <> bsLink <> " sign_key=" <> signKey <> " " <> req)
      alice <## "service response: {\"code\":\"bad_request\",\"type\":\"error\"}"
      nothingPurchased cc

testStoreUnsentProductId :: HasCallStack => TestParams -> IO ()
testStoreUnsentProductId ps = do
  hook <- newIORef (pure ())
  verifications <- countGoogleVerifications hook
  withBadgeServiceVerifier ps (googleVerifierWithHook hook) $ \env@BadgeServiceEnv {bsController = cc} -> do
    (purchaseKey, masterKey) <- newPurchaseKeys
    refusalOf <$> serviceCmd env purchaseKey (purchaseCmd masterKey $ googlePayment "badge_supporter_01/../other" googleSupporterToken) `shouldReturn` (BSEReceiptInvalid, Nothing)
    verifications `shouldReturn` 0
    nothingPurchased cc

subscriptionPlay :: ServicePayment
subscriptionPlay = googlePayment "subscr_badge_supporter_01" googleSubscriptionToken

subscriptionCheck :: SubscriptionState -> Maybe StoreCharge -> SubscriptionCheck
subscriptionCheck subscriptionState latestCharge = SubscriptionCheck {subscriptionState, testSubscription = False, latestCharge}

storeCharge :: Text -> UTCTime -> Integer -> StoreCharge
storeCharge planId periodStart months =
  StoreCharge {chargeRef = "GPA.3301-1234-5678-90123", planId, periodStart, periodEnd = addMonths months periodStart, chargePaid = Nothing, revoked = False, offer = False}

subscriptionOf :: HasCallStack => BadgeServiceResponse -> Maybe SubscriptionState
subscriptionOf = \case
  BSPBadgeCredential {subscription} -> subscription
  r -> error $ "expected badgeCredential, got " <> show (J.toJSON r)

createdAtOf :: StatementEntry -> UTCTime
createdAtOf StatementEntry {createdAt} = createdAt

-- | The ledger stores whole seconds.
serviceNow :: BadgeServiceEnv -> IO UTCTime
serviceNow BadgeServiceEnv {bsClock} = truncateToSecond <$> testClockTime bsClock

subscriptionPayments :: ChatController -> IO [(Text, Text, Maybe Text, Maybe Text)]
subscriptionPayments ChatController {chatStore} =
  withTransaction chatStore $ \db ->
    DB.query_ db "SELECT provider, provider_ref, subscription_state, provider_token FROM sx_badge_service_payments ORDER BY provider"

testSubscriptionPurchase :: HasCallStack => TestParams -> IO ()
testSubscriptionPurchase ps =
  withBadgeServiceEnv ps $ \env@BadgeServiceEnv {bsClock, bsController = cc, bsStore = store@FakeStore {appleSubscriptionJWS, appleSandboxSubscriptionJWS}} -> do
    now <- serviceNow env
    let periodStart = addUTCTime (-3600) now
        changes = map (\e -> let (c, m, _) = entryOf e in (c, m)) . fst . statementOf
    setSubscription store (SubscriptionGoogle googleSubscriptionToken) $ subscriptionCheck SSRenewing $ Just $ storeCharge "subscr-badge-supporter-month-02" periodStart 1
    (purchaseKey, masterKey) <- newPurchaseKeys
    purchased <- serviceCmd env purchaseKey $ purchaseCmd masterKey subscriptionPlay
    map entryTag (fst $ statementOf purchased) `shouldBe` ["charge", "badge"]
    changes purchased `shouldBe` [(1, 1), (-1, 0)]
    -- the month is granted at the store's period start, not when the receipt arrived
    map createdAtOf (take 1 $ fst $ statementOf purchased) `shouldBe` [periodStart]
    badgeTypeOf purchased `shouldBe` Just BTSupporter
    subscriptionOf purchased `shouldBe` Just SSRenewing
    rowCount cc "sx_badge_service_subscription_charges" `shouldReturn` 1
    rowCount cc "sx_badge_service_badge_ledger WHERE charge_id IS NOT NULL" `shouldReturn` 1
    subscriptionPayments cc `shouldReturn` [("google", googlePurchaseRef googleSubscriptionToken, Just "renewing", Just googleSubscriptionToken)]
    checks <- subscriptionChecks store
    replayed <- serviceCmd env purchaseKey $ purchaseCmd masterKey subscriptionPlay
    credentialOf replayed `shouldBe` credentialOf purchased
    subscriptionOf replayed `shouldBe` Just SSRenewing
    subscriptionChecks store `shouldReturn` checks
    (otherKey, otherMasterKey) <- newPurchaseKeys
    -- another key is told once Play is asked again, after the hour the token is held for
    setClockAt bsClock $ addUTCTime 3700 now
    refusalOf <$> serviceCmd env otherKey (purchaseCmd otherMasterKey subscriptionPlay) `shouldReturn` (BSEReceiptUsed, Nothing)
    -- an annual Apple subscription is claimed by its original id, which the store is asked about
    setSubscription store (SubscriptionApple appleSubscriptionOriginalId) $ subscriptionCheck SSRenewing $ Just $ storeCharge "SUBSCR_BADGE_SUPPORTER_YEAR_01" periodStart 12
    (appleKey, appleMasterKey) <- newPurchaseKeys
    apple <- serviceCmd env appleKey $ purchaseCmd appleMasterKey SPApple {jws = appleSubscriptionJWS}
    changes apple `shouldBe` [(12, 12), (-1, 11)]
    subscriptionOf apple `shouldBe` Just SSRenewing
    take 1 <$> subscriptionPayments cc `shouldReturn` [("apple", appleSubscriptionOriginalId, Just "renewing", Nothing)]
    -- a test purchase is named, on the store's word for Play, and before the store is asked for Apple
    let testToken = "fake-play-token-subscription-test.AO-J1Os2q5djX0pRm6"
    setSubscription store (SubscriptionGoogle testToken) SubscriptionCheck {subscriptionState = SSRenewing, testSubscription = True, latestCharge = Just $ storeCharge "subscr-badge-supporter-month-02" periodStart 1}
    (testKey, testMasterKey) <- newPurchaseKeys
    refusalOf <$> serviceCmd env testKey (purchaseCmd testMasterKey $ googlePayment "subscr_badge_supporter_01" testToken) `shouldReturn` (BSETestPurchase, Nothing)
    sandboxChecks <- subscriptionChecks store
    refusalOf <$> serviceCmd env testKey (purchaseCmd testMasterKey SPApple {jws = appleSandboxSubscriptionJWS}) `shouldReturn` (BSETestPurchase, Nothing)
    subscriptionChecks store `shouldReturn` sandboxChecks
    rowCount cc "sx_badge_service_badge_purchases" `shouldReturn` 2

testSubscriptionPurchaseNotGrantable :: HasCallStack => TestParams -> IO ()
testSubscriptionPurchaseNotGrantable ps =
  withBadgeServiceEnv ps $ \env@BadgeServiceEnv {bsClock, bsController = cc, bsStore = store@FakeStore {appleSubscriptionJWS}} -> do
    now <- serviceNow env
    (purchaseKey, masterKey) <- newPurchaseKeys
    let monthlyFrom start = storeCharge "subscr-badge-supporter-month-02" start 1
        monthly = monthlyFrom (addUTCTime (-3600) now)
        -- one token presented again and again, so each presentation comes after the hour the one before holds it for
        answer ref payment checked = do
          setSubscription store ref checked
          refusalOf <$> serviceCmd env purchaseKey (purchaseCmd masterKey payment) <* nextHour
        nextHour = testClockTime bsClock >>= setClockAt bsClock . addUTCTime 3700
        play = answer (SubscriptionGoogle googleSubscriptionToken) subscriptionPlay
        aDay = Just 86400
    play (subscriptionCheck SSRenewing $ Just $ monthlyFrom $ addUTCTime 3600 now) `shouldReturn` (BSEPaymentPending, Just 3600)
    -- Play gives a subscription bought again a new token, so this one can never be credited
    play (subscriptionCheck SSEnded $ Just monthly) `shouldReturn` (BSEReceiptInvalid, Nothing)
    -- a later charge could still credit this receipt
    play (subscriptionCheck SSRenewing $ Just monthly {revoked = True}) `shouldReturn` (BSEPaymentPending, aDay)
    play (subscriptionCheck SSRenewing $ Just monthly {offer = True}) `shouldReturn` (BSEPaymentPending, aDay)
    play (subscriptionCheck SSBillingRetry Nothing) `shouldReturn` (BSEPaymentPending, aDay)
    play (subscriptionCheck SSBillingRetry $ Just $ monthlyFrom $ addMonths (-2) now) `shouldReturn` (BSEPaymentPending, aDay)
    play (subscriptionCheck SSRenewing $ Just $ storeCharge "subscr-badge-supporter-month-99" (addUTCTime (-3600) now) 1) `shouldReturn` (BSEPaymentPending, aDay)
    -- Play gives a plan it does not know no length, so its charge has an empty period, and is still only an unsold plan
    let unsoldAt t = (storeCharge "subscr-badge-supporter-month-99" t 1) {periodEnd = t}
    play (subscriptionCheck SSRenewing $ Just $ unsoldAt $ addMonths 1 now) `shouldReturn` (BSEPaymentPending, aDay)
    -- an Apple resubscription keeps the original id, so an ended subscription may yet be credited
    answer (SubscriptionApple appleSubscriptionOriginalId) SPApple {jws = appleSubscriptionJWS} (subscriptionCheck SSEnded $ Just $ storeCharge "SUBSCR_BADGE_SUPPORTER_MONTH_01" (addUTCTime (-3600) now) 1)
      `shouldReturn` (BSEPaymentPending, aDay)
    play (subscriptionCheck SSRenewing $ Just monthly {periodEnd = addUTCTime (-7200) now}) `shouldReturn` (BSEInternal, Nothing)
    nothingPurchased cc
    rowCount cc "sx_badge_service_subscription_charges" `shouldReturn` 0

-- | A monthly Play subscription bought an hour into its first period: its key, that period's start and the statement it was answered.
subscribedMonthly :: HasCallStack => BadgeServiceEnv -> IO (C.PublicKeyEd25519, UTCTime, [StatementEntry])
subscribedMonthly env = subscribedTo env "subscr-badge-supporter-month-02" 1

-- | The same for the Play plan given, of that many months.
subscribedTo :: HasCallStack => BadgeServiceEnv -> Text -> Integer -> IO (C.PublicKeyEd25519, UTCTime, [StatementEntry])
subscribedTo env@BadgeServiceEnv {bsStore} planId months = do
  firstStart <- addUTCTime (-3600) <$> serviceNow env
  setSubscription bsStore (SubscriptionGoogle googleSubscriptionToken) $ subscriptionCheck SSRenewing $ Just $ storeCharge planId firstStart months
  (purchaseKey, masterKey) <- newPurchaseKeys
  purchased <- serviceCmd env purchaseKey $ purchaseCmd masterKey subscriptionPlay
  pure (purchaseKey, firstStart, fst $ statementOf purchased)

-- | The store's charge n months after the first, for the plan given.
monthlyCharge :: Text -> UTCTime -> Integer -> StoreCharge
monthlyCharge planId firstStart n = (storeCharge planId (addMonths n firstStart) 1) {chargeRef = "GPA.renewal." <> tshow n}

storeReports :: FakeStore -> SubscriptionCheck -> IO ()
storeReports store = setSubscription store (SubscriptionGoogle googleSubscriptionToken)

subscriptionCheckedAt :: ChatController -> IO [Maybe UTCTime]
subscriptionCheckedAt ChatController {chatStore} =
  withTransaction chatStore $ \db ->
    map fromOnly <$> DB.query_ db "SELECT subscription_checked_at FROM sx_badge_service_payments"

testSubscriptionChargeGrantedOnce :: HasCallStack => TestParams -> IO ()
testSubscriptionChargeGrantedOnce ps =
  withBadgeServiceEnv ps $ \env@BadgeServiceEnv {bsClock, bsController = cc, bsStore = store} -> do
    (purchaseKey, firstStart, purchased) <- subscribedMonthly env
    let renewalStart = addMonths 1 firstStart
        -- the store's period runs past the month granted, so the charge is still current once that month is spent
        renewal = (monthlyCharge "subscr-badge-supporter-month-02" firstStart 1) {periodEnd = addUTCTime (10 * nominalDay) (addMonths 2 firstStart)}
    storeReports store $ subscriptionCheck SSRenewing $ Just renewal
    setClockAt bsClock $ addUTCTime (3 * nominalDay) renewalStart
    renewed <- assertBalance env purchaseKey (last purchased)
    map entryTag (fst $ statementOf renewed) `shouldBe` ["charge", "badge"]
    map createdAtOf (take 1 $ fst $ statementOf renewed) `shouldBe` [renewalStart]
    subscriptionOf renewed `shouldBe` Just SSRenewing
    credentialOf renewed `shouldSatisfy` isJust
    -- inside the issued month a repeat gets the same credential, and the store is not asked
    checks <- subscriptionChecks store
    repeated <- assertBalance env purchaseKey (last purchased)
    credentialOf repeated `shouldBe` credentialOf renewed
    subscriptionOf repeated `shouldBe` Just SSRenewing
    subscriptionChecks store `shouldReturn` checks
    -- the store still reports that charge once the month has run out, and it is not granted again
    ledger <- ledgerRows cc "sx_badge_service_badge_ledger"
    setClockAt bsClock $ addUTCTime (3 * nominalDay) (addMonths 2 firstStart)
    reported <- assertBalance env purchaseKey (last $ fst $ statementOf renewed)
    subscriptionChecks store `shouldReturn` (checks + 1)
    map entryTag (fst $ statementOf reported) `shouldBe` []
    credentialOf reported `shouldBe` Nothing
    ledgerRows cc "sx_badge_service_badge_ledger" `shouldReturn` ledger
    rowCount cc "sx_badge_service_subscription_charges" `shouldReturn` 2

testSubscriptionStoreChecksBounded :: HasCallStack => TestParams -> IO ()
testSubscriptionStoreChecksBounded ps =
  withBadgeServiceEnv ps $ \env@BadgeServiceEnv {bsClock, bsStore = store} -> do
    (purchaseKey, firstStart, purchased) <- subscribedMonthly env
    let request = assertBalance env purchaseKey (last purchased)
        ended = addUTCTime (3 * nominalDay) (addMonths 1 firstStart)
    checks <- subscriptionChecks store
    setClockAt bsClock $ addUTCTime (10 * nominalDay) firstStart
    _ <- request
    subscriptionChecks store `shouldReturn` checks
    -- once the month has run out the store is asked, and not again within the hour
    storeReports store $ subscriptionCheck SSBillingRetry Nothing
    setClockAt bsClock ended
    waiting <- request
    subscriptionOf waiting `shouldBe` Just SSBillingRetry
    credentialOf waiting `shouldBe` Nothing
    setClockAt bsClock $ addUTCTime 1800 ended
    subscriptionOf <$> request `shouldReturn` Just SSBillingRetry
    subscriptionChecks store `shouldReturn` (checks + 1)
    -- an hour on it is asked again, and a check that fails holds the next one off as long
    setGoogleDown store True
    setClockAt bsClock $ addUTCTime 3700 ended
    refusalOf <$> request `shouldReturn` (BSEProviderUnavailable, Just 3600)
    setGoogleDown store False
    setClockAt bsClock $ addUTCTime (3700 + 1800) ended
    subscriptionOf <$> request `shouldReturn` Just SSBillingRetry
    subscriptionChecks store `shouldReturn` (checks + 2)

testSubscriptionChargeNotGranted :: HasCallStack => TestParams -> IO ()
testSubscriptionChargeNotGranted ps =
  withBadgeServiceEnv ps $ \env@BadgeServiceEnv {bsClock, bsController = cc, bsStore = store} -> do
    (purchaseKey, firstStart, purchased) <- subscribedMonthly env
    ledger <- ledgerRows cc "sx_badge_service_badge_ledger"
    let monthly = "subscr-badge-supporter-month-02"
        checkedAt i = addUTCTime (3 * nominalDay + fromIntegral (i :: Int) * 3700) (addMonths 1 firstStart)
        notGranted =
          [ (monthlyCharge monthly firstStart 1) {revoked = True},
            (monthlyCharge monthly firstStart 1) {offer = True, chargeRef = "GPA.offer"},
            (monthlyCharge monthly firstStart 4) {periodStart = addUTCTime nominalDay (checkedAt 3), periodEnd = addMonths 1 (addUTCTime nominalDay (checkedAt 3))},
            (monthlyCharge monthly firstStart 5) {periodStart = firstStart, periodEnd = addMonths 1 firstStart},
            -- an unsold Play plan, given an empty period
            (monthlyCharge "subscr-badge-supporter-month-99" firstStart 6) {periodEnd = addMonths 6 firstStart}
          ]
    forM_ (zip [0 ..] notGranted) $ \(i, charge) -> do
      storeReports store $ subscriptionCheck SSRenewing $ Just charge
      setClockAt bsClock $ checkedAt i
      answered <- assertBalance env purchaseKey (last purchased)
      credentialOf answered `shouldBe` Nothing
      map entryTag (fst $ statementOf answered) `shouldBe` []
    -- each charge is recorded, and none funds a month
    rowCount cc "sx_badge_service_subscription_charges" `shouldReturn` 1 + length notGranted
    ledgerRows cc "sx_badge_service_badge_ledger" `shouldReturn` ledger

testClippedPlayPeriodNotGranted :: HasCallStack => TestParams -> IO ()
testClippedPlayPeriodNotGranted ps =
  withBadgeServiceEnv ps $ \env@BadgeServiceEnv {bsClock, bsController = cc, bsStore = store} -> do
    (purchaseKey, firstStart, purchased) <- subscribedMonthly env
    ledger <- ledgerRows cc "sx_badge_service_badge_ledger"
    -- Play dates the period ending on 31 March from the last day of February, and the ledger's month from then ends before 31 March
    let (year, _, _) = toGregorian (utctDay firstStart)
        clippedEnd = UTCTime (fromGregorian (year + 1) 3 31) 43200
        clipped = (storeCharge "subscr-badge-supporter-month-02" (addMonths (-1) clippedEnd) 1) {periodEnd = clippedEnd, chargeRef = "GPA.clipped"}
    storeReports store $ subscriptionCheck SSRenewing $ Just clipped
    setClockAt bsClock $ addUTCTime (-nominalDay) clippedEnd
    renewal <- assertBalance env purchaseKey (last purchased)
    credentialOf renewal `shouldBe` Nothing
    map entryTag (fst $ statementOf renewal) `shouldBe` []
    ledgerRows cc "sx_badge_service_badge_ledger" `shouldReturn` ledger
    rowCount cc "sx_badge_service_subscription_charges" `shouldReturn` 2
    (otherKey, otherMasterKey) <- newPurchaseKeys
    let otherToken = "fake-play-token-clipped.AO-J1Op9n4cjW9qQl5"
    setSubscription store (SubscriptionGoogle otherToken) $ subscriptionCheck SSRenewing $ Just clipped
    refusalOf <$> serviceCmd env otherKey (purchaseCmd otherMasterKey $ googlePayment "subscr_badge_supporter_01" otherToken) `shouldReturn` (BSEPaymentPending, Just 86400)

testPlayMonthEndWalk :: HasCallStack => TestParams -> IO ()
testPlayMonthEndWalk ps = walkPlayMonthEnd ps "subscr-badge-supporter-month-02" 1 5

testPlayAnnualMonthEndWalk :: HasCallStack => TestParams -> IO ()
testPlayAnnualMonthEndWalk ps = walkPlayMonthEnd ps "subscr-badge-supporter-year-01" 12 12

-- | A Play subscription bought an hour into a period starting on 31 January, through the service as deployed, and then
-- as many more months asked for at the device's moments. Play's periods end on the 31st clipped at month ends, and it
-- reports at each moment the period that moment is in.
walkPlayMonthEnd :: HasCallStack => TestParams -> Text -> Integer -> Int -> IO ()
walkPlayMonthEnd ps planId planMonths months =
  withFakePlay $ \fake@FakePlay {fpConfig} -> do
    let ini = takeDirectory (gServiceAccountFile fpConfig) </> "badge_service.ini"
    writeFile ini "[listener]\nstatic_dir = /srv/badges\n"
    Right cfg <- readServiceConfig ini
    ServiceState {storeVerifier} <- newServiceState >>= configureStoreVerifier (Just cfg {playStore = Just fpConfig})
    withBadgeServiceVerifier ps (const storeVerifier) $ \env@BadgeServiceEnv {bsClock, bsController = cc} -> do
      (purchaseKey, masterKey) <- newPurchaseKeys
      let anchor = UTCTime (fromGregorian 2027 1 31) 39600
          periodEnd k = addMonths ((k + 1) * planMonths) anchor
          periodAt t = toInteger $ length $ takeWhile (<= t) $ map periodEnd [0 ..]
          askAt t = do
            let k = periodAt t
                item = lineItem [("expiryTime", J.toJSON $ periodEnd k), ("latestSuccessfulOrderId", J.String $ "GPA.1234-5678-9012-34567.." <> tshow k), basePlan planId] []
            answerPurchase fake googleSubscriptionToken $ PlayRecord $ subscriptionRecord "SUBSCRIPTION_STATE_ACTIVE" [] [item]
            setClockAt bsClock t
          monthIssued :: UTCTime -> Int -> BadgeServiceResponse -> IO (StatementEntry, UTCTime, UTCTime)
          monthIssued t monthsHeld answer = do
            let rows = fst $ statementOf answer
                expected
                  | monthsHeld == 0 = [("charge", fromInteger planMonths), ("badge", -1)]
                  | otherwise = [("badge", -1)]
                lag = diffUTCTime (periodEnd $ periodAt t) (L.paidThrough $ last rows)
            map (\e -> let (change, _, _) = entryOf e in (entryTag e, change)) rows `shouldBe` expected
            lag `shouldSatisfy` \l -> l >= 0 && l <= badgeGraceInterval
            Just expiry <- pure $ expiryOf answer
            pure (last rows, expiry, t)
          renew (tip, expiry, _) = askFrom [addUTCTime (fromInteger d * nominalDay) expiry | d <- [-1 .. 8]]
            where
              (_, held, _) = entryOf tip
              askFrom = \case
                [] -> error $ "no month issued after the credential expiring " <> show expiry
                t : ts -> do
                  askAt t
                  answer <- assertBalance env purchaseKey tip
                  if null (fst $ statementOf answer) then askFrom ts else monthIssued t held answer
          walk :: Int -> (StatementEntry, UTCTime, UTCTime) -> IO UTCTime
          walk 0 (_, _, t) = pure t
          walk n issued = renew issued >>= walk (n - 1)
          purchasedAt = addUTCTime 3600 anchor
      askAt purchasedAt
      purchased <- serviceCmd env purchaseKey $ purchaseCmd masterKey subscriptionPlay
      lastAskedAt <- monthIssued purchasedAt 0 purchased >>= walk months
      rowCount cc "sx_badge_service_subscription_charges" `shouldReturn` fromInteger (periodAt lastAskedAt + 1)
      rowCount cc "sx_badge_service_subscription_charges c WHERE NOT EXISTS (SELECT 1 FROM sx_badge_service_badge_ledger l WHERE l.charge_id = c.charge_id)" `shouldReturn` 0

testSubscriptionStoreUnreachable :: HasCallStack => TestParams -> IO ()
testSubscriptionStoreUnreachable ps =
  withBadgeServiceEnv ps $ \env@BadgeServiceEnv {bsClock, bsController = cc, bsStore = store} -> do
    (purchaseKey, firstStart, purchased) <- subscribedMonthly env
    ledger <- ledgerRows cc "sx_badge_service_badge_ledger"
    let ended = addUTCTime (3 * nominalDay) (addMonths 1 firstStart)
    storeReports store $ subscriptionCheck SSRenewing $ Just $ monthlyCharge "subscr-badge-supporter-month-02" firstStart 1
    setGoogleDown store True
    setClockAt bsClock ended
    refusalOf <$> assertBalance env purchaseKey (last purchased) `shouldReturn` (BSEProviderUnavailable, Just 3600)
    subscriptionCheckedAt cc >>= (`shouldSatisfy` \case [Just t] -> abs (diffUTCTime t ended) < 60; _ -> False)
    ledgerRows cc "sx_badge_service_badge_ledger" `shouldReturn` ledger
    rowCount cc "sx_badge_service_subscription_charges" `shouldReturn` 1

testAppleSubscriptionRenews :: HasCallStack => TestParams -> IO ()
testAppleSubscriptionRenews ps =
  withBadgeServiceEnv ps $ \env@BadgeServiceEnv {bsClock, bsController = cc@ChatController {chatStore}, bsStore = store@FakeStore {appleSubscriptionJWS}} -> do
    firstStart <- addUTCTime (-3600) <$> serviceNow env
    let monthly = "SUBSCR_BADGE_SUPPORTER_MONTH_01"
        appleReports = setSubscription store (SubscriptionApple appleSubscriptionOriginalId) . subscriptionCheck SSRenewing . Just
        firstCheck = addUTCTime (3 * nominalDay) (addMonths 1 firstStart)
        renewalStart = addUTCTime nominalDay firstCheck
        renewal = (storeCharge monthly renewalStart 1) {chargeRef = "2000000812345690"}
        recordedIds :: IO [Text]
        recordedIds = withTransaction chatStore $ \db -> map fromOnly <$> DB.query db "SELECT charge_id FROM sx_badge_service_subscription_charges WHERE provider_charge_ref = ?" (Only ("2000000812345690" :: Text))
        grantsOf :: Text -> IO [Int]
        grantsOf chargeId = withTransaction chatStore $ \db -> map fromOnly <$> DB.query db "SELECT COUNT(*) FROM sx_badge_service_badge_ledger WHERE charge_id = ?" (Only chargeId)
    appleReports $ storeCharge monthly firstStart 1
    (purchaseKey, masterKey) <- newPurchaseKeys
    purchased <- serviceCmd env purchaseKey $ purchaseCmd masterKey SPApple {jws = appleSubscriptionJWS}
    subscriptionOf purchased `shouldBe` Just SSRenewing
    ledger <- ledgerRows cc "sx_badge_service_badge_ledger"
    -- the next charge is reported before its period starts: recorded, and nothing granted or issued
    appleReports renewal
    setClockAt bsClock firstCheck
    waiting <- assertBalance env purchaseKey (last $ fst $ statementOf purchased)
    subscriptionOf waiting `shouldBe` Just SSRenewing
    credentialOf waiting `shouldBe` Nothing
    map entryTag (fst $ statementOf waiting) `shouldBe` []
    ledgerRows cc "sx_badge_service_badge_ledger" `shouldReturn` ledger
    [recorded] <- recordedIds
    grantsOf recorded `shouldReturn` [0]
    -- once its period has started, a later check grants that charge under the id it was recorded with
    setClockAt bsClock $ addUTCTime 3600 renewalStart
    renewed <- assertBalance env purchaseKey (last $ fst $ statementOf purchased)
    map entryTag (fst $ statementOf renewed) `shouldBe` ["charge", "badge"]
    map createdAtOf (take 1 $ fst $ statementOf renewed) `shouldBe` [renewalStart]
    subscriptionOf renewed `shouldBe` Just SSRenewing
    expiryOf renewed `shouldSatisfy` (> expiryOf purchased)
    recordedIds `shouldReturn` [recorded]
    grantsOf recorded `shouldReturn` [1]
    rowCount cc "sx_badge_service_subscription_charges" `shouldReturn` 2

-- | The client buys a monthly Play subscription an hour into its first period: that period's start, and its ledger then.
clientSubscribedMonthly :: HasCallStack => BadgeServiceEnv -> TestCC -> IO (UTCTime, [ReplicatedRow])
clientSubscribedMonthly env@BadgeServiceEnv {bsStore} alice = do
  firstStart <- addUTCTime (-3600) <$> serviceNow env
  storeReports bsStore $ subscriptionCheck SSRenewing $ Just $ storeCharge "subscr-badge-supporter-month-02" firstStart 1
  alice ##> ("/_badge purchase 1 " <> paymentArg subscriptionPlay)
  inAnyOrder alice [[openPurchase ""], ["1: supporter"]]
  waitWakeArmed (chatController alice)
  rows <- ledgerRows (chatController alice) "badge_ledger"
  map (\(_, ch, m, _, _, t) -> (ch, m, t)) rows `shouldBe` [(1, 1, Just "charge"), (-1, 0, Just "badge")]
  pure (firstStart, rows)

-- | Waits for the worker to arm a wake no earlier than t, which is how a pass that issued nothing shows it has ended.
waitWakeFrom :: HasCallStack => ChatController -> UTCTime -> IO ()
waitWakeFrom ChatController {chatStore} t = loop (100 :: Int)
  where
    loop i = do
      rows :: [(Maybe UTCTime, Int64)] <-
        withTransaction chatStore $ \db ->
          DB.query_ db "SELECT next_wake_at, badge_purchase_id FROM badge_purchases"
      case rows of
        [(Just w, _)] | w >= t -> pure ()
        _
          | i == 0 -> error $ "expected a wake from " <> show t <> ", got " <> show rows
          | otherwise -> threadDelay 50000 >> loop (i - 1)

rowChanges :: [ReplicatedRow] -> [(Int, Int, UTCTime, Maybe Text)]
rowChanges = map (\(_, ch, m, start, _, t) -> (ch, m, start, t))

testSubscriptionRenews :: HasCallStack => TestParams -> IO ()
testSubscriptionRenews ps =
  withBadgeServiceEnv ps $ \env@BadgeServiceEnv {bsClock, bsClientCfg, bsStore = store} ->
    withNewTestChatCfg ps bsClientCfg "alice" aliceProfile $ \alice -> do
      (firstStart, rows) <- clientSubscribedMonthly env alice
      let (requestAt, presentAt) = renewalMoments rows
          renewalStart = addMonths 1 firstStart
      storeReports store $ subscriptionCheck SSRenewing $ Just $ monthlyCharge "subscr-badge-supporter-month-02" firstStart 1
      -- asked at zero months, past paidThrough, with the badge still shown
      requestAt `shouldSatisfy` (> renewalStart)
      setClockAt bsClock requestAt
      alice ##> "/_app activate"
      alice <## "ok"
      renewed <- waitLedgerRows (chatController alice) 4
      alice <##. "1: supporter"
      drop 2 (rowChanges renewed) `shouldBe` [(1, 1, renewalStart, Just "charge"), (-1, 0, addMonths 1 renewalStart, Just "badge")]
      balanceChecks (chatController alice) `shouldReturn` replicate 4 (Just True)
      issueErrorRow (chatController alice) `shouldReturn` (Nothing, Nothing)
      shownBadgeId (chatController alice) >>= (`shouldSatisfy` isJust)
      -- and the new credential replaces the old one at its expiry
      shownAndIssuedExpiry (chatController alice) >>= \(shown, issued) -> shown `shouldNotBe` issued
      setClockAt bsClock presentAt
      alice ##> "/_app activate"
      alice <## "ok"
      waitShownIssued (chatController alice)

testSubscriptionTierChangeAtRenewal :: HasCallStack => TestParams -> IO ()
testSubscriptionTierChangeAtRenewal ps =
  withBadgeServiceEnv ps $ \env@BadgeServiceEnv {bsClock, bsClientCfg, bsController = cc, bsStore = store@FakeStore {appleSubscriptionJWS}} -> do
    withNewTestChatCfg ps bsClientCfg "alice" aliceProfile $ \alice -> do
      firstStart <- addUTCTime (-3600) <$> serviceNow env
      let monthly plan n = (storeCharge plan (addMonths n firstStart) 1) {chargeRef = "apple.renewal." <> tshow n}
          tiersOf = map (\(_, ch, m, start, tier, t) -> (ch, m, start, tier, t))
          renewAs plan n = do
            rows <- ledgerRows (chatController alice) "badge_ledger"
            let (requestAt, _) = renewalMoments rows
            appleStoreReports store $ subscriptionCheck SSRenewing $ Just $ monthly plan n
            setClockAt bsClock requestAt
            alice ##> "/_app activate"
            alice <## "ok"
            renewed <- waitLedgerRows (chatController alice) (length rows + 2)
            pure $ drop (length rows) renewed
      appleStoreReports store $ subscriptionCheck SSRenewing $ Just $ storeCharge "SUBSCR_BADGE_SUPPORTER_MONTH_01" firstStart 1
      alice ##> ("/_badge purchase 1 " <> paymentArg SPApple {jws = appleSubscriptionJWS})
      inAnyOrder alice [[openPurchase ""], ["1: supporter"]]
      waitWakeArmed (chatController alice)
      -- the store charges Legend at the renewal: a new run from its period start, on both sides
      upgraded <- renewAs "SUBSCR_BADGE_LEGEND_MONTH_01" 1
      alice <##. "1: legend"
      let legendStart = addMonths 1 firstStart
      tiersOf upgraded `shouldBe` [(1, 1, legendStart, "legend", Just "charge"), (-1, 0, addMonths 1 legendStart, "legend", Just "badge")]
      balanceChecks (chatController alice) `shouldReturn` replicate 4 (Just True)
      purchaseBadgeTypes (chatController alice) "badge_purchases" `shouldReturn` ["legend"]
      purchaseBadgeTypes cc "sx_badge_service_badge_purchases" `shouldReturn` ["legend"]
      issuedBadgeTypes (chatController alice) `shouldReturn` ["supporter", "legend"]
      -- the raised tier is shown without waiting for the Supporter credential to expire
      waitShownIssued (chatController alice)
      -- and back to Supporter at the next renewal
      downgraded <- renewAs "SUBSCR_BADGE_SUPPORTER_MONTH_01" 2
      alice <##. "1: supporter"
      tiersOf downgraded `shouldBe` [(1, 1, addMonths 2 firstStart, "supporter", Just "charge"), (-1, 0, addMonths 3 firstStart, "supporter", Just "badge")]
      balanceChecks (chatController alice) `shouldReturn` replicate 6 (Just True)
      purchaseBadgeTypes (chatController alice) "badge_purchases" `shouldReturn` ["supporter"]
      purchaseBadgeTypes cc "sx_badge_service_badge_purchases" `shouldReturn` ["supporter"]
    -- an annual Supporter upgraded to Legend in its third month, found only once its year ended, after a month
    -- issued since the upgrade: the new run starts at that issue, and every row still follows the one before it
    yearStart <- addUTCTime (-3600) <$> serviceNow env
    storeReports store $ subscriptionCheck SSRenewing $ Just $ storeCharge "subscr-badge-supporter-year-01" yearStart 12
    (purchaseKey, masterKey) <- newPurchaseKeys
    purchased <- serviceCmd env purchaseKey $ purchaseCmd masterKey subscriptionPlay
    setClockAt bsClock $ addUTCTime 600 (addMonths 11 yearStart)
    lastMonth <- assertBalance env purchaseKey (last $ fst $ statementOf purchased)
    let tip = last $ fst $ statementOf lastMonth
        upgradedAt = addUTCTime (10 * nominalDay) (addMonths 2 yearStart)
    storeReports store $ subscriptionCheck SSRenewing $ Just (storeCharge "subscr-badge-legend-year-01" upgradedAt 12) {chargeRef = "GPA.legend"}
    setClockAt bsClock $ addUTCTime (3 * nominalDay) (addMonths 12 yearStart)
    found <- assertBalance env purchaseKey tip
    let rows = fst $ statementOf found
    map entryTag rows `shouldBe` ["charge", "lapse", "badge"]
    map createdAtOf (take 1 rows) `shouldBe` [createdAtOf tip]
    map (\StatementEntry {balanceBadgeType} -> balanceBadgeType) rows `shouldBe` [BTLegend, BTLegend, BTLegend]
    badgeTypeOf found `shouldBe` Just BTLegend
    now <- serviceNow env
    map snd (balanceChecked now BTSupporter (Just tip) rows) `shouldBe` [Just True, Just True, Just True]

purchaseBadgeTypes :: ChatController -> String -> IO [Text]
purchaseBadgeTypes ChatController {chatStore} table =
  withTransaction chatStore $ \db ->
    map fromOnly <$> DB.query_ db (fromString $ "SELECT current_badge_type FROM " <> table <> " ORDER BY badge_purchase_id")

issuedBadgeTypes :: ChatController -> IO [Text]
issuedBadgeTypes ChatController {chatStore} =
  withTransaction chatStore $ \db -> map fromOnly <$> DB.query_ db "SELECT badge_type FROM badge_issuances ORDER BY period_end"

upgradeCmdWith :: Text -> StatementEntry -> BadgeServiceCommand
upgradeCmdWith jws lastEntry = BSCUpgradeBadgeSubscription {payment = SPApple {jws}, balance = BadgeBalance {lastEntry}}

testSubscriptionUpgradeNow :: HasCallStack => TestParams -> IO ()
testSubscriptionUpgradeNow ps =
  withBadgeServiceEnv ps $ \env@BadgeServiceEnv {bsClock, bsController = cc, bsStore = store@FakeStore {appleSubscriptionJWS, appleLegendSubscriptionJWS}} -> do
    firstStart <- addUTCTime (-3600) <$> serviceNow env
    appleStoreReports store $ subscriptionCheck SSRenewing $ Just $ storeCharge "SUBSCR_BADGE_SUPPORTER_YEAR_01" firstStart 12
    (purchaseKey, masterKey) <- newPurchaseKeys
    purchased <- serviceCmd env purchaseKey $ purchaseCmd masterKey SPApple {jws = appleSubscriptionJWS}
    setClockAt bsClock $ addUTCTime 600 (addMonths 1 firstStart)
    second <- assertBalance env purchaseKey (last $ fst $ statementOf purchased)
    setClockAt bsClock $ addUTCTime 600 (addMonths 2 firstStart)
    third <- assertBalance env purchaseKey (last $ fst $ statementOf second)
    let tip = last $ fst $ statementOf third
        upgradedAt = addUTCTime (10 * nominalDay) (addMonths 2 firstStart)
        upgrade jws = serviceCmd env purchaseKey $ upgradeCmdWith jws tip
    setClockAt bsClock upgradedAt
    checks <- subscriptionChecks store
    ledger <- ledgerRows cc "sx_badge_service_badge_ledger"
    -- a transaction of the tier the ledger holds is answered from the record
    sameTier <- upgrade appleSubscriptionJWS
    badgeTypeOf sameTier `shouldBe` Just BTSupporter
    subscriptionChecks store `shouldReturn` checks
    ledgerRows cc "sx_badge_service_badge_ledger" `shouldReturn` ledger
    -- while the store still reports the Supporter plan, nothing is granted and the Supporter credential is not given
    refusalOf <$> upgrade appleLegendSubscriptionJWS `shouldReturn` (BSEPaymentPending, Just 3600)
    subscriptionChecks store `shouldReturn` checks + 1
    subscriptionCheckedAt cc >>= (`shouldSatisfy` \case [Just t] -> abs (diffUTCTime t upgradedAt) < 60; _ -> False)
    ledgerRows cc "sx_badge_service_badge_ledger" `shouldReturn` ledger
    -- the store now reports Legend, but the hour since it was asked holds
    appleStoreReports store $ subscriptionCheck SSRenewing $ Just (storeCharge "SUBSCR_BADGE_LEGEND_YEAR_01" upgradedAt 12) {chargeRef = "apple.legend"}
    setClockAt bsClock $ addUTCTime 1800 upgradedAt
    refusalOf <$> upgrade appleLegendSubscriptionJWS >>= (`shouldSatisfy` \case (BSEPaymentPending, Just s) -> s > 1740 && s <= 1801; _ -> False)
    subscriptionChecks store `shouldReturn` checks + 1
    -- past it, the unissued Supporter months are debited and the Legend year starts at the store's period
    setClockAt bsClock $ addUTCTime 3700 upgradedAt
    upgraded <- upgrade appleLegendSubscriptionJWS
    subscriptionChecks store `shouldReturn` checks + 2
    let rows = fst $ statementOf upgraded
    map entryTag rows `shouldBe` ["upgrade", "charge", "badge"]
    map createdAtOf (take 2 rows) `shouldBe` [upgradedAt, upgradedAt]
    badgeTypeOf upgraded `shouldBe` Just BTLegend
    now <- serviceNow env
    map snd (balanceChecked now BTSupporter (Just tip) rows) `shouldBe` [Just True, Just True, Just True]
    purchaseBadgeTypes cc "sx_badge_service_badge_purchases" `shouldReturn` ["legend"]
    -- asked again, it is answered from the record with the Legend credential
    again <- upgrade appleLegendSubscriptionJWS
    credentialOf again `shouldBe` credentialOf upgraded
    subscriptionChecks store `shouldReturn` checks + 2

testSubscriptionUpgradeLateInMonth :: HasCallStack => TestParams -> IO ()
testSubscriptionUpgradeLateInMonth ps =
  withBadgeServiceEnv ps $ \env@BadgeServiceEnv {bsClock, bsStore = store@FakeStore {appleSubscriptionJWS, appleLegendSubscriptionJWS}} -> do
    let anchor = UTCTime (fromGregorian 2027 1 31) 36000
        upgradedAt = UTCTime (fromGregorian 2027 2 28) 54000
        legendEnd = addMonths 1 upgradedAt
    appleStoreReports store $ subscriptionCheck SSRenewing $ Just $ storeCharge "SUBSCR_BADGE_SUPPORTER_YEAR_01" anchor 12
    (purchaseKey, masterKey) <- newPurchaseKeys
    setClockAt bsClock $ addUTCTime 300 anchor
    purchased <- serviceCmd env purchaseKey $ purchaseCmd masterKey SPApple {jws = appleSubscriptionJWS}
    setClockAt bsClock $ addUTCTime 600 (addMonths 1 anchor)
    second <- assertBalance env purchaseKey (last $ fst $ statementOf purchased)
    -- the Supporter month issued on 28 February runs to 31 March, past the end of the Legend month it is upgraded to
    nextDue (fst $ statementOf second) `shouldBe` addMonths 2 anchor
    let tip = last $ fst $ statementOf second
        upgrade = serviceCmd env purchaseKey $ upgradeCmdWith appleLegendSubscriptionJWS tip
    appleStoreReports store $ subscriptionCheck SSRenewing $ Just (storeCharge "SUBSCR_BADGE_LEGEND_MONTH_01" upgradedAt 1) {chargeRef = "apple.legend"}
    setClockAt bsClock upgradedAt
    upgraded <- upgrade
    map entryTag (fst $ statementOf upgraded) `shouldBe` ["upgrade", "charge", "badge"]
    nextDue (fst $ statementOf upgraded) `shouldBe` legendEnd
    badgeTypeOf upgraded `shouldBe` Just BTLegend
    replayed <- upgrade
    credentialOf replayed `shouldBe` credentialOf upgraded
    -- the purchase replayed is answered with the newest issuance, though the Supporter month ends later
    repurchased <- serviceCmd env purchaseKey $ purchaseCmd masterKey SPApple {jws = appleSubscriptionJWS}
    badgeTypeOf repurchased `shouldBe` Just BTLegend
    let legendTip = last $ fst $ statementOf upgraded
    setClockAt bsClock $ addUTCTime (-3600) legendEnd
    beforeEnd <- assertBalance env purchaseKey legendTip
    credentialOf beforeEnd `shouldBe` credentialOf upgraded
    -- the Legend month has ended and its next charge is not granted: nothing is current, though the Supporter month runs on
    setClockAt bsClock $ addUTCTime 3600 legendEnd
    afterEnd <- assertBalance env purchaseKey legendTip
    credentialOf afterEnd `shouldBe` Nothing

testUpgradeCheckGrantsRenewal :: HasCallStack => TestParams -> IO ()
testUpgradeCheckGrantsRenewal ps =
  withBadgeServiceEnv ps $ \env@BadgeServiceEnv {bsClock, bsController = cc, bsStore = store@FakeStore {appleSubscriptionJWS, appleLegendSubscriptionJWS}} -> do
    firstStart <- addUTCTime (-3600) <$> serviceNow env
    appleStoreReports store $ subscriptionCheck SSRenewing $ Just $ storeCharge "SUBSCR_BADGE_SUPPORTER_MONTH_01" firstStart 1
    (purchaseKey, masterKey) <- newPurchaseKeys
    purchased <- serviceCmd env purchaseKey $ purchaseCmd masterKey SPApple {jws = appleSubscriptionJWS}
    let tip = last $ fst $ statementOf purchased
        renewalStart = addMonths 1 firstStart
        upgradedAt = addUTCTime 600 renewalStart
    -- coverage has ended and Apple charged the Supporter renewal, while its status still shows Supporter
    appleStoreReports store $ subscriptionCheck SSRenewing $ Just (storeCharge "SUBSCR_BADGE_SUPPORTER_MONTH_01" renewalStart 1) {chargeRef = "apple.renewal"}
    setClockAt bsClock upgradedAt
    checks <- subscriptionChecks store
    refusalOf <$> serviceCmd env purchaseKey (upgradeCmdWith appleLegendSubscriptionJWS tip) `shouldReturn` (BSEPaymentPending, Just 3600)
    subscriptionChecks store `shouldReturn` checks + 1
    -- the check granted the renewal, so the hour's requests get its month without asking the store
    renewed <- assertBalance env purchaseKey tip
    map entryTag (fst $ statementOf renewed) `shouldBe` ["charge", "badge"]
    badgeTypeOf renewed `shouldBe` Just BTSupporter
    subscriptionChecks store `shouldReturn` checks + 1
    purchaseBadgeTypes cc "sx_badge_service_badge_purchases" `shouldReturn` ["supporter"]
    -- Apple reports Legend, and past the hour the upgrade grants it
    appleStoreReports store $ subscriptionCheck SSRenewing $ Just (storeCharge "SUBSCR_BADGE_LEGEND_MONTH_01" (addUTCTime 1800 upgradedAt) 1) {chargeRef = "apple.legend"}
    setClockAt bsClock $ addUTCTime 3700 upgradedAt
    upgraded <- serviceCmd env purchaseKey $ upgradeCmdWith appleLegendSubscriptionJWS $ last $ fst $ statementOf renewed
    badgeTypeOf upgraded `shouldBe` Just BTLegend
    subscriptionChecks store `shouldReturn` checks + 2
    purchaseBadgeTypes cc "sx_badge_service_badge_purchases" `shouldReturn` ["legend"]

testUpgradeRequestRefused :: HasCallStack => TestParams -> IO ()
testUpgradeRequestRefused ps =
  withBadgeServiceEnv ps $ \env@BadgeServiceEnv {bsIssuerKey, bsVerifier, bsPurchaseChecks, bsController = cc, bsStore = store@FakeStore {appleSubscriptionJWS, appleLegendSubscriptionJWS, appleSecondSubscriptionJWS, appleSupporterJWS}} -> do
    now <- serviceNow env
    appleStoreReports store $ subscriptionCheck SSRenewing $ Just $ storeCharge "SUBSCR_BADGE_SUPPORTER_MONTH_01" (addUTCTime (-3600) now) 1
    (appleKey, appleMasterKey) <- newPurchaseKeys
    apple <- serviceCmd env appleKey $ purchaseCmd appleMasterKey SPApple {jws = appleSubscriptionJWS}
    (playKey, _, played) <- subscribedMonthly env
    (prepaidKey, prepaidMasterKey) <- newPurchaseKeys
    prepaid <- serviceCmd env prepaidKey $ purchaseCmd prepaidMasterKey SPApple {jws = appleSupporterJWS}
    (unknownKey, _) <- newPurchaseKeys
    checks <- subscriptionChecks store
    ledger <- ledgerRows cc "sx_badge_service_badge_ledger"
    let appleTip = last $ fst $ statementOf apple
        refused key cmd = refusalOf <$> serviceCmd env key cmd
    -- signed by another key than the purchase it names
    refusalOf <$> badgeServiceResponse bsIssuerKey bsVerifier bsPurchaseChecks cc Nothing (Just playKey) (requestObject appleKey $ upgradeCmdWith appleLegendSubscriptionJWS appleTip)
      `shouldReturn` (BSEBadRequest, Nothing)
    refused unknownKey (upgradeCmdWith appleLegendSubscriptionJWS appleTip) `shouldReturn` (BSEUnknownPurchaseKey, Nothing)
    -- another purchase's subscription, and another subscription for this purchase
    refused playKey (upgradeCmdWith appleLegendSubscriptionJWS $ last played) `shouldReturn` (BSEBadRequest, Nothing)
    refused appleKey (upgradeCmdWith appleSecondSubscriptionJWS appleTip) `shouldReturn` (BSEBadRequest, Nothing)
    refused prepaidKey (upgradeCmdWith appleLegendSubscriptionJWS $ last $ fst $ statementOf prepaid) `shouldReturn` (BSEBadRequest, Nothing)
    -- a transaction the verifier does not vouch for, one of no subscription product, and Play
    refused appleKey (upgradeCmdWith (subscriptionJWS appleSubscriptionOriginalId "2000000812345689" []) appleTip) `shouldReturn` (BSEReceiptInvalid, Nothing)
    refused appleKey (upgradeCmdWith appleSupporterJWS appleTip) `shouldReturn` (BSEBadRequest, Nothing)
    refused playKey BSCUpgradeBadgeSubscription {payment = subscriptionPlay, balance = BadgeBalance {lastEntry = last played}} `shouldReturn` (BSEBadRequest, Nothing)
    subscriptionChecks store `shouldReturn` checks
    ledgerRows cc "sx_badge_service_badge_ledger" `shouldReturn` ledger

transferCmd :: BadgeMasterKey -> ServicePayment -> BadgeServiceCommand
transferCmd masterKey payment = BSCTransferBadge {masterKey, payment}

transferWithNewKey :: HasCallStack => BadgeServiceEnv -> ServicePayment -> IO (C.PublicKeyEd25519, BadgeServiceResponse)
transferWithNewKey env payment = do
  (purchaseKey, masterKey) <- newPurchaseKeys
  (purchaseKey,) <$> serviceCmd env purchaseKey (transferCmd masterKey payment)

-- | Past the hour a claim's store check holds the next one off.
nextClaimCheck :: BadgeServiceEnv -> IO ()
nextClaimCheck BadgeServiceEnv {bsClock} = testClockTime bsClock >>= setClockAt bsClock . addUTCTime 3700

movedOutTo :: StatementEntry -> Maybe C.PublicKeyEd25519
movedOutTo StatementEntry {entryType = SEDebit (SDTransferOut k)} = Just k
movedOutTo _ = Nothing

reboundPayments :: ChatController -> IO Int
reboundPayments cc = rowCount cc "sx_badge_service_payments WHERE rebound_at IS NOT NULL"

testStoreRebind :: HasCallStack => TestParams -> IO ()
testStoreRebind ps =
  withBadgeServiceEnv ps $ \env@BadgeServiceEnv {bsClock, bsController = cc, bsStore = store@FakeStore {appleSubscriptionJWS}} -> do
    let changes = map (\e -> let (c, m, _) = entryOf e in (c, m))
        verified badgeType rows = verifiedAfter badgeType Nothing rows
        verifiedAfter badgeType tip_ rows = do
          now <- serviceNow env
          map snd (balanceChecked now badgeType tip_ rows) `shouldBe` map (const $ Just True) rows
    (playKey, firstStart, purchased) <- subscribedMonthly env
    setClockAt bsClock $ addUTCTime (10 * nominalDay) firstStart
    (movedKey, moved) <- transferWithNewKey env subscriptionPlay
    let movedRows = fst $ statementOf moved
    map entryTag movedRows `shouldBe` ["charge", "badge"]
    map createdAtOf (take 1 movedRows) `shouldBe` [firstStart]
    subscriptionOf moved `shouldBe` Just SSRenewing
    credentialOf moved `shouldSatisfy` isJust
    verified BTSupporter movedRows
    reboundPayments cc `shouldReturn` 1
    ended <- assertBalance env playKey (last purchased)
    map entryTag (fst $ statementOf ended) `shouldBe` ["transferOut"]
    map movedOutTo (fst $ statementOf ended) `shouldBe` [Just movedKey]
    verifiedAfter BTSupporter (Just $ last purchased) (fst $ statementOf ended)
    -- the next charge renews the new key's purchase
    storeReports store $ subscriptionCheck SSRenewing $ Just $ monthlyCharge "subscr-badge-supporter-month-02" firstStart 1
    setClockAt bsClock $ addUTCTime (3 * nominalDay) (addMonths 1 firstStart)
    renewed <- assertBalance env movedKey (last movedRows)
    map entryTag (fst $ statementOf renewed) `shouldBe` ["charge", "badge"]
    credentialOf renewed `shouldSatisfy` isJust
    -- an annual App Store subscription two months in: the old key's passed month lapses before the rest moves
    appleStart <- addUTCTime (-3600) <$> serviceNow env
    appleStoreReports store $ subscriptionCheck SSRenewing $ Just (storeCharge "SUBSCR_BADGE_SUPPORTER_YEAR_01" appleStart 12) {chargeRef = "2000000812345681"}
    (appleKey, appleMasterKey) <- newPurchaseKeys
    applePurchased <- serviceCmd env appleKey $ purchaseCmd appleMasterKey SPApple {jws = appleSubscriptionJWS}
    setClockAt bsClock $ addUTCTime (10 * nominalDay) (addMonths 2 appleStart)
    (appleMovedKey, appleMoved) <- transferWithNewKey env SPApple {jws = appleSubscriptionJWS}
    let appleMovedRows = fst $ statementOf appleMoved
    map entryTag appleMovedRows `shouldBe` ["charge", "lapse", "badge"]
    changes appleMovedRows `shouldBe` [(12, 12), (-2, 10), (-1, 9)]
    verified BTSupporter appleMovedRows
    appleEnded <- fst . statementOf <$> assertBalance env appleKey (last $ fst $ statementOf applePurchased)
    map entryTag appleEnded `shouldBe` ["lapse", "transferOut"]
    changes appleEnded `shouldBe` [(-1, 10), (-10, 0)]
    map movedOutTo appleEnded `shouldBe` [Nothing, Just appleMovedKey]
    verifiedAfter BTSupporter (Just $ last $ fst $ statementOf applePurchased) appleEnded
    reboundPayments cc `shouldReturn` 2

testStoreRebindNewCharge :: HasCallStack => TestParams -> IO ()
testStoreRebindNewCharge ps =
  withBadgeServiceEnv ps $ \env@BadgeServiceEnv {bsClock, bsController = cc, bsStore = store} -> do
    (oldKey, firstStart, purchased) <- subscribedMonthly env
    let chargeStart = addMonths 2 firstStart
        changes = map (\e -> let (c, m, _) = entryOf e in (c, m))
        verifiedAfter tip_ rows = do
          now <- serviceNow env
          map snd (balanceChecked now BTSupporter tip_ rows) `shouldBe` map (const $ Just True) rows
    -- the old key went idle after its first month, and the store has since charged a month the service never recorded;
    -- its period runs past the month the move funds, so the charge is still current once that month is spent
    storeReports store $ subscriptionCheck SSRenewing $ Just (monthlyCharge "subscr-badge-supporter-month-02" firstStart 2) {periodEnd = addUTCTime (10 * nominalDay) (addMonths 1 chargeStart)}
    setClockAt bsClock $ addUTCTime (3 * nominalDay) chargeStart
    (newKey, moved) <- transferWithNewKey env subscriptionPlay
    let movedRows = fst $ statementOf moved
    map entryTag movedRows `shouldBe` ["charge", "badge"]
    map createdAtOf (take 1 movedRows) `shouldBe` [chargeStart]
    verifiedAfter Nothing movedRows
    ended <- fst . statementOf <$> assertBalance env oldKey (last purchased)
    map entryTag ended `shouldBe` ["transferOut"]
    changes ended `shouldBe` [(0, 0)]
    verifiedAfter (Just $ last purchased) ended
    -- asked again in the same period, the new key is granted nothing more
    ledger <- ledgerRows cc "sx_badge_service_badge_ledger"
    again <- assertBalance env newKey (last movedRows)
    map entryTag (fst $ statementOf again) `shouldBe` []
    credentialOf again `shouldBe` credentialOf moved
    ledgerRows cc "sx_badge_service_badge_ledger" `shouldReturn` ledger
    -- the store still reports that charge once the month it funded has run out, and it is not granted again
    checks <- subscriptionChecks store
    setClockAt bsClock $ addUTCTime (3 * nominalDay) (addMonths 1 chargeStart)
    reported <- assertBalance env newKey (last movedRows)
    subscriptionChecks store `shouldReturn` checks + 1
    map entryTag (fst $ statementOf reported) `shouldBe` []
    credentialOf reported `shouldBe` Nothing
    ledgerRows cc "sx_badge_service_badge_ledger" `shouldReturn` ledger

testStoreRebindCapped :: HasCallStack => TestParams -> IO ()
testStoreRebindCapped ps =
  withBadgeServiceEnv ps $ \env@BadgeServiceEnv {bsClock, bsStore = store} -> do
    (_, firstStart, _) <- subscribedMonthly env
    let firstExpiry = endOfMondayAfter (addMonths 1 firstStart)
    nextClaimCheck env
    (firstMoveKey, firstMove) <- transferWithNewKey env subscriptionPlay
    credentialOf firstMove `shouldSatisfy` isJust
    expiryOf firstMove `shouldBe` Just firstExpiry
    nextClaimCheck env
    checks <- subscriptionChecks store
    refusalOf . snd <$> transferWithNewKey env subscriptionPlay `shouldReturn` (BSETransferLimited, Nothing)
    subscriptionChecks store `shouldReturn` checks + 1
    -- past the first key's credential the first move's key renews, into a credential that outlives the first
    storeReports store $ subscriptionCheck SSRenewing $ Just $ monthlyCharge "subscr-badge-supporter-month-02" firstStart 1
    setClockAt bsClock $ addUTCTime 3600 firstExpiry
    renewed <- assertBalance env firstMoveKey (last $ fst $ statementOf firstMove)
    expiryOf renewed `shouldBe` Just (endOfMondayAfter $ addMonths 2 firstStart)
    (_, secondMove) <- transferWithNewKey env subscriptionPlay
    credentialOf secondMove `shouldSatisfy` isJust
    nextClaimCheck env
    refusalOf . snd <$> transferWithNewKey env subscriptionPlay `shouldReturn` (BSETransferLimited, Nothing)

testStoreRebindOldKeyEnded :: HasCallStack => TestParams -> IO ()
testStoreRebindOldKeyEnded ps =
  withBadgeServiceEnv ps $ \env@BadgeServiceEnv {bsStore = store} -> do
    (purchaseKey, _, purchased) <- subscribedMonthly env
    nextClaimCheck env
    (_, moved) <- transferWithNewKey env subscriptionPlay
    credentialOf moved `shouldSatisfy` isJust
    checks <- subscriptionChecks store
    ended <- assertBalance env purchaseKey (last purchased)
    subscriptionOf ended `shouldBe` Just SSEnded
    credentialOf ended `shouldBe` Nothing
    subscriptionChecks store `shouldReturn` checks
    nextClaimCheck env
    (_, masterKey) <- newPurchaseKeys
    refusalOf <$> serviceCmd env purchaseKey (purchaseCmd masterKey subscriptionPlay) `shouldReturn` (BSEReceiptUsed, Nothing)

testStoreRebindStaleAppleTransaction :: HasCallStack => TestParams -> IO ()
testStoreRebindStaleAppleTransaction ps =
  withBadgeServiceEnv ps $ \env@BadgeServiceEnv {bsController = cc, bsStore = store@FakeStore {appleSubscriptionJWS, appleRenewalSubscriptionJWS}} -> do
    now <- serviceNow env
    appleStoreReports store $ subscriptionCheck SSRenewing $ Just (storeCharge "SUBSCR_BADGE_SUPPORTER_MONTH_01" (addUTCTime (-3600) now) 1) {chargeRef = "2000000812345683"}
    (purchaseKey, masterKey) <- newPurchaseKeys
    _ <- serviceCmd env purchaseKey $ purchaseCmd masterKey SPApple {jws = appleSubscriptionJWS}
    nextClaimCheck env
    ledger <- ledgerRows cc "sx_badge_service_badge_ledger"
    checks <- subscriptionChecks store
    refusalOf . snd <$> transferWithNewKey env SPApple {jws = appleSubscriptionJWS} `shouldReturn` (BSEPaymentPending, Just 3600)
    subscriptionChecks store `shouldReturn` checks + 1
    ledgerRows cc "sx_badge_service_badge_ledger" `shouldReturn` ledger
    rowCount cc "sx_badge_service_badge_purchases" `shouldReturn` 1
    reboundPayments cc `shouldReturn` 0
    nextClaimCheck env
    credentialOf . snd <$> transferWithNewKey env SPApple {jws = appleRenewalSubscriptionJWS} >>= (`shouldSatisfy` isJust)
    reboundPayments cc `shouldReturn` 1

testTransferUncredited :: HasCallStack => TestParams -> IO ()
testTransferUncredited ps =
  withBadgeServiceEnv ps $ \env@BadgeServiceEnv {bsController = cc, bsStore = store} -> do
    periodStart <- addUTCTime (-3600) <$> serviceNow env
    storeReports store $ subscriptionCheck SSRenewing $ Just $ storeCharge "subscr-badge-supporter-month-02" periodStart 1
    (_, transferred) <- transferWithNewKey env subscriptionPlay
    map entryTag (fst $ statementOf transferred) `shouldBe` ["charge", "badge"]
    map createdAtOf (take 1 $ fst $ statementOf transferred) `shouldBe` [periodStart]
    subscriptionOf transferred `shouldBe` Just SSRenewing
    credentialOf transferred `shouldSatisfy` isJust
    rowCount cc "sx_badge_service_badge_purchases" `shouldReturn` 1
    reboundPayments cc `shouldReturn` 0

testTransferReplayAnswered :: HasCallStack => TestParams -> IO ()
testTransferReplayAnswered ps =
  withBadgeServiceEnv ps $ \env@BadgeServiceEnv {bsStore = store} -> do
    _ <- subscribedMonthly env
    nextClaimCheck env
    (movedKey, movedMasterKey) <- newPurchaseKeys
    moved <- serviceCmd env movedKey $ transferCmd movedMasterKey subscriptionPlay
    checks <- subscriptionChecks store
    replayed <- serviceCmd env movedKey $ transferCmd movedMasterKey subscriptionPlay
    credentialOf replayed `shouldBe` credentialOf moved
    credentialOf replayed `shouldSatisfy` isJust
    subscriptionChecks store `shouldReturn` checks

testTransferKnownKeyRefused :: HasCallStack => TestParams -> IO ()
testTransferKnownKeyRefused ps =
  withBadgeServiceEnv ps $ \env@BadgeServiceEnv {bsController = cc, bsStore = store} -> do
    _ <- subscribedMonthly env
    (heldKey, heldMasterKey) <- newPurchaseKeys
    _ <- serviceCmd env heldKey $ purchaseCmd heldMasterKey supporterPlay
    nextClaimCheck env
    checks <- subscriptionChecks store
    ledger <- ledgerRows cc "sx_badge_service_badge_ledger"
    refusalOf <$> serviceCmd env heldKey (transferCmd heldMasterKey subscriptionPlay) `shouldReturn` (BSEBadRequest, Nothing)
    subscriptionChecks store `shouldReturn` checks
    ledgerRows cc "sx_badge_service_badge_ledger" `shouldReturn` ledger
    rowCount cc "sx_badge_service_badge_purchases" `shouldReturn` 2
    reboundPayments cc `shouldReturn` 0

-- | Restore presents every transaction it finds as a transfer, a one-time purchase bought on another device included.
testTransferOneTimeRefused :: HasCallStack => TestParams -> IO ()
testTransferOneTimeRefused ps =
  withBadgeServiceEnv ps $ \env@BadgeServiceEnv {bsController = cc, bsStore = store@FakeStore {appleSupporterJWS}} -> do
    let oneTime = [supporterPlay, SPApple {jws = appleSupporterJWS}]
    owned <- forM oneTime $ \payment -> do
      (ownerKey, ownerMasterKey) <- newPurchaseKeys
      purchased <- serviceCmd env ownerKey $ purchaseCmd ownerMasterKey payment
      pure (ownerKey, ownerMasterKey, payment, purchased)
    checks <- subscriptionChecks store
    ledger <- ledgerRows cc "sx_badge_service_badge_ledger"
    forM_ oneTime $ \payment -> refusalOf . snd <$> transferWithNewKey env payment `shouldReturn` (BSEReceiptUsed, Nothing)
    subscriptionChecks store `shouldReturn` checks
    ledgerRows cc "sx_badge_service_badge_ledger" `shouldReturn` ledger
    rowCount cc "sx_badge_service_badge_purchases" `shouldReturn` 2
    reboundPayments cc `shouldReturn` 0
    forM_ owned $ \(ownerKey, ownerMasterKey, payment, purchased) ->
      credentialOf <$> serviceCmd env ownerKey (purchaseCmd ownerMasterKey payment) `shouldReturn` credentialOf purchased

restoredCmd :: Int64 -> ServicePayment -> String
restoredCmd userId payment = "/_badge purchase " <> show userId <> " restored " <> paymentArg payment

shownBadges :: ChatController -> IO [(Int64, Maybe Int64)]
shownBadges ChatController {chatStore} =
  withTransaction chatStore $ \db -> DB.query_ db "SELECT user_id, shown_badge_id FROM users ORDER BY user_id"

-- | Each purchase's owner, and whether it still links the store record, through which its subscription's transaction is read.
purchaseReceiptLinks :: ChatController -> IO [(Int64, Bool)]
purchaseReceiptLinks ChatController {chatStore} =
  withTransaction chatStore $ \db -> DB.query_ db "SELECT user_id, badge_store_receipt_id IS NOT NULL FROM badge_purchases ORDER BY badge_purchase_id"

ledgerTags :: ChatController -> IO [Maybe Text]
ledgerTags cc = map (\(_, _, _, _, _, t) -> t) <$> ledgerRows cc "badge_ledger"

restoreToken :: Text -> Text
restoreToken n = "fake-play-token-restore-" <> n <> ".AO-J1Om8l3biV8pPk4"

storeReceiptHolds :: ChatController -> IO [(Int64, Text)]
storeReceiptHolds ChatController {chatStore} =
  withTransaction chatStore $ \db -> DB.query_ db "SELECT user_id, hold_kind FROM badge_store_receipts ORDER BY badge_store_receipt_id"

-- | For clients checked through their databases: what they printed is read and dropped, until a second passes with nothing.
dropOutput :: TestCC -> IO ()
dropOutput cc = timeout 1000000 (atomically $ readTQueue $ termQ cc) >>= mapM_ (\_ -> dropOutput cc)

testRestoreOnNewDevice :: HasCallStack => TestParams -> IO ()
testRestoreOnNewDevice ps =
  withBadgeServiceEnv ps $ \env@BadgeServiceEnv {bsClock, bsClientCfg, bsController = cc, bsStore = store} ->
    withNewTestChatCfg ps bsClientCfg "alice" aliceProfile $ \alice ->
      withNewTestChatCfg ps bsClientCfg "bob" bobProfile $ \bob -> do
        firstStart <- addUTCTime (-3600) <$> serviceNow env
        storeReports store $ subscriptionCheck SSRenewing $ Just $ storeCharge "subscr-badge-supporter-month-02" firstStart 1
        alice `send` ("/_badge purchase 1 " <> paymentArg subscriptionPlay)
        waitShownBadge (chatController alice) (Just 1)
        aliceRows <- ledgerRows (chatController alice) "badge_ledger"
        -- within the hour of the purchase's store check, the Restore asks the store itself
        checks <- subscriptionChecks store
        bob `send` restoredCmd 1 subscriptionPlay
        waitShownBadge (chatController bob) (Just 1)
        ledgerTags (chatController bob) `shouldReturn` [Just "charge", Just "badge"]
        reboundPayments cc `shouldReturn` 1
        subscriptionChecks store `shouldReturn` checks + 1
        -- the old device's badge retires at its next request, which the service answers ended
        setClockAt bsClock $ fst $ renewalMoments aliceRows
        alice `send` "/_app activate"
        waitShownBadge (chatController alice) Nothing
        withNewTestChatCfg ps bsClientCfg "cath" cathProfile $ \cath -> do
          creditedNowhere <- addUTCTime (-3600) <$> serviceNow env
          setSubscription store (SubscriptionGoogle $ restoreToken "1") $ subscriptionCheck SSRenewing $ Just $ storeCharge "subscr-badge-supporter-month-02" creditedNowhere 1
          cath `send` restoredCmd 1 (googlePayment "subscr_badge_supporter_01" $ restoreToken "1")
          waitShownBadge (chatController cath) (Just 1)
          ledgerTags (chatController cath) `shouldReturn` [Just "charge", Just "badge"]
          reboundPayments cc `shouldReturn` 1
          mapM_ dropOutput [alice, bob, cath]

testRestoreMovedOutAskedInMonth :: HasCallStack => TestParams -> IO ()
testRestoreMovedOutAskedInMonth ps =
  withBadgeServiceEnv ps $ \env@BadgeServiceEnv {bsClientCfg, bsStore = store} ->
    withNewTestChatCfg ps bsClientCfg {badgeDevRequests = True} "alice" aliceProfile $ \alice ->
      withNewTestChatCfg ps bsClientCfg "bob" bobProfile $ \bob -> do
        firstStart <- addUTCTime (-3600) <$> serviceNow env
        storeReports store $ subscriptionCheck SSRenewing $ Just $ storeCharge "subscr-badge-supporter-month-02" firstStart 1
        alice `send` ("/_badge purchase 1 " <> paymentArg subscriptionPlay)
        waitShownBadge (chatController alice) (Just 1)
        bob `send` restoredCmd 1 subscriptionPlay
        waitShownBadge (chatController bob) (Just 1)
        -- the answer is ended with no credential while the issued month still runs past now
        alice `send` "/_badge request 1"
        waitShownBadge (chatController alice) Nothing
        issueErrorRow (chatController alice) `shouldReturn` (Nothing, Nothing)
        mapM_ dropOutput [alice, bob]

testRestoreHeldPerPurpose :: HasCallStack => TestParams -> IO ()
testRestoreHeldPerPurpose ps =
  withBadgeServiceEnv ps $ \env@BadgeServiceEnv {bsClock, bsStore = store} -> do
    let heldFor s (code, retryAfter) = code == BSEPaymentPending && maybe False (\t -> t > s - 60 && t <= s + 60) retryAfter
    _ <- subscribedMonthly env
    purchasedAt <- testClockTime bsClock
    setClockAt bsClock $ addUTCTime 1800 purchasedAt
    checks <- subscriptionChecks store
    (_, moved) <- transferWithNewKey env subscriptionPlay
    credentialOf moved `shouldSatisfy` isJust
    subscriptionChecks store `shouldReturn` checks + 1
    -- past the purchase's hour and inside the Restore's
    setClockAt bsClock $ addUTCTime 3700 purchasedAt
    refusalOf . snd <$> transferWithNewKey env subscriptionPlay >>= (`shouldSatisfy` heldFor 1700)
    subscriptionChecks store `shouldReturn` checks + 1
    (otherKey, otherMasterKey) <- newPurchaseKeys
    let presented = refusalOf <$> serviceCmd env otherKey (purchaseCmd otherMasterKey subscriptionPlay)
    presented `shouldReturn` (BSEReceiptUsed, Nothing)
    subscriptionChecks store `shouldReturn` checks + 2
    presented >>= (`shouldSatisfy` heldFor 3600)
    subscriptionChecks store `shouldReturn` checks + 2

testRestoreOnOtherProfile :: HasCallStack => TestParams -> IO ()
testRestoreOnOtherProfile ps =
  withBadgeServiceEnv ps $ \env@BadgeServiceEnv {bsClock, bsClientCfg, bsController = cc, bsStore = store} ->
    withNewTestChatCfg ps bsClientCfg "alice" aliceProfile $ \alice -> do
      let a = chatController alice
      firstStart <- addUTCTime (-3600) <$> serviceNow env
      storeReports store $ subscriptionCheck SSRenewing $ Just $ storeCharge "subscr-badge-supporter-month-02" firstStart 1
      alice `send` ("/_badge purchase 1 " <> paymentArg subscriptionPlay)
      waitShownBadge a (Just 1)
      aliceRows <- ledgerRows a "badge_ledger"
      alice `send` "/create user alisa"
      shownBadges a `shouldEventuallyReturn` [(1, Just 1), (2, Nothing)]
      nextClaimCheck env
      alice `send` restoredCmd 2 subscriptionPlay
      shownBadges a `shouldEventuallyReturn` [(1, Just 1), (2, Just 2)]
      purchaseReceiptLinks a `shouldReturn` [(1, False), (2, True)]
      storeReceiptRows a `shouldReturn` [(2, Nothing, True)]
      -- the move settles the record, so it is not sent as a transfer again
      storeReceiptHolds a `shouldReturn` [(2, "purchase")]
      setClockAt bsClock $ fst $ renewalMoments aliceRows
      alice `send` "/_app activate"
      shownBadges a `shouldEventuallyReturn` [(1, Nothing), (2, Just 2)]
      -- before the first key's credential expires, a move on is refused once and not retried, and the record returns to its owner
      alice `send` "/create user alisa2"
      shownBadges a `shouldEventuallyReturn` [(1, Nothing), (2, Just 2), (3, Nothing)]
      purchases <- rowCount cc "sx_badge_service_badge_purchases"
      alice `send` restoredCmd 3 subscriptionPlay
      storeReceiptCreditErrors a `shouldEventuallyReturn` ["service_error final transfer_limited"]
      storeReceiptRows a `shouldReturn` [(2, Nothing, True)]
      heldStoreReceipts a `shouldReturn` 0
      setClockAt bsClock $ addUTCTime 7200 (fst $ renewalMoments aliceRows)
      threadDelay 1000000
      heldStoreReceipts a `shouldReturn` 0
      rowCount cc "sx_badge_service_badge_purchases" `shouldReturn` purchases
      -- a later Restore says why, and holds the record for this profile again; the worker's attempt is refused again
      let restoreLimited = either show (const "answered without a refusal") <$> sendChatCmdStr a (restoredCmd 3 subscriptionPlay) >>= (`shouldSatisfy` ("TransferLimited" `isInfixOf`))
      restoreLimited
      storeReceiptHolds a `shouldEventuallyReturn` [(2, "purchase")]
      -- within the hour of that attempt's store check the next one waits, so the record stays this profile's move
      restoreLimited
      storeReceiptHolds a `shouldEventuallyReturn` [(3, "transfer")]
      heldStoreReceiptErrors a `shouldEventuallyReturn` ["service_error retry payment_pending"]
      shownBadges a `shouldReturn` [(1, Nothing), (2, Just 2), (3, Nothing)]
      dropOutput alice

testRestoreMovesBack :: HasCallStack => TestParams -> IO ()
testRestoreMovesBack ps =
  withBadgeServiceEnv ps $ \env@BadgeServiceEnv {bsClock, bsClientCfg, bsStore = store} ->
    withNewTestChatCfg ps bsClientCfg "alice" aliceProfile $ \alice ->
      withNewTestChatCfg ps bsClientCfg "bob" bobProfile $ \bob -> do
        firstStart <- addUTCTime (-3600) <$> serviceNow env
        storeReports store $ subscriptionCheck SSRenewing $ Just $ storeCharge "subscr-badge-supporter-month-02" firstStart 1
        alice `send` ("/_badge purchase 1 " <> paymentArg subscriptionPlay)
        waitShownBadge (chatController alice) (Just 1)
        (renewalAt, firstExpiry) <- renewalMoments <$> ledgerRows (chatController alice) "badge_ledger"
        nextClaimCheck env
        bob `send` restoredCmd 1 subscriptionPlay
        waitShownBadge (chatController bob) (Just 1)
        setClockAt bsClock renewalAt
        alice `send` "/_app activate"
        waitShownBadge (chatController alice) Nothing
        -- past the first key's credential, the subscription moves back to the purchase it left, as a new purchase
        storeReports store $ subscriptionCheck SSRenewing $ Just $ monthlyCharge "subscr-badge-supporter-month-02" firstStart 1
        setClockAt bsClock $ addUTCTime 3600 firstExpiry
        alice `send` restoredCmd 1 subscriptionPlay
        waitShownBadge (chatController alice) (Just 2)
        purchaseReceiptLinks (chatController alice) `shouldReturn` [(1, False), (1, True)]
        storeReceiptHolds (chatController alice) `shouldReturn` [(1, "purchase")]
        bob `send` "/_app activate"
        waitShownBadge (chatController bob) Nothing
        mapM_ dropOutput [alice, bob]

testRestoreLimitedOnNewDevice :: HasCallStack => TestParams -> IO ()
testRestoreLimitedOnNewDevice ps =
  withBadgeServiceEnv ps $ \env@BadgeServiceEnv {bsClientCfg, bsStore = store} ->
    withNewTestChatCfg ps bsClientCfg "alice" aliceProfile $ \alice ->
      withNewTestChatCfg ps bsClientCfg "bob" bobProfile $ \bob ->
        withNewTestChatCfg ps bsClientCfg "cath" cathProfile $ \cath -> do
          let refusedWith err cc cmd = either show (const "answered without a refusal") <$> sendChatCmdStr cc cmd >>= (`shouldSatisfy` (err `isInfixOf`))
          firstStart <- addUTCTime (-3600) <$> serviceNow env
          storeReports store $ subscriptionCheck SSRenewing $ Just $ storeCharge "subscr-badge-supporter-month-02" firstStart 1
          alice `send` ("/_badge purchase 1 " <> paymentArg subscriptionPlay)
          waitShownBadge (chatController alice) (Just 1)
          nextClaimCheck env
          bob `send` restoredCmd 1 subscriptionPlay
          waitShownBadge (chatController bob) (Just 1)
          -- a Restore on a profile showing a badge is refused, recording nothing
          refusedWith "BadgeActive" (chatController bob) $ restoredCmd 1 $ googlePayment "subscr_badge_supporter_01" $ restoreToken "3"
          storeReceiptRows (chatController bob) `shouldReturn` [(1, Nothing, True)]
          -- before the first key's credential expires a third device's move is refused, and its next Restore says why and holds it again
          nextClaimCheck env
          cath `send` restoredCmd 1 subscriptionPlay
          storeReceiptCreditErrors (chatController cath) `shouldEventuallyReturn` ["service_error final transfer_limited"]
          heldStoreReceipts (chatController cath) `shouldReturn` 0
          refusedWith "TransferLimited" (chatController cath) $ restoredCmd 1 subscriptionPlay
          heldStoreReceiptErrors (chatController cath) `shouldEventuallyReturn` ["service_error retry payment_pending"]
          storeReceiptHolds (chatController cath) `shouldReturn` [(1, "transfer")]
          mapM_ dropOutput [alice, bob, cath]

testRestoreLeavesTierChangeHold :: HasCallStack => TestParams -> IO ()
testRestoreLeavesTierChangeHold ps =
  withBadgeServiceEnv ps $ \env@BadgeServiceEnv {bsClientCfg, bsStore = FakeStore {appleLegendSubscriptionJWS}} ->
    withNewTestChatCfg ps bsClientCfg "alice" aliceProfile $ \alice -> do
      let a = chatController alice
          legend = SPApple {jws = appleLegendSubscriptionJWS}
      appleSupporterBeforeLegend env alice
      -- the store still reports Supporter, so the tier change stays held
      alice `send` ("/_badge purchase 1 " <> paymentArg legend)
      heldStoreReceiptErrors a `shouldEventuallyReturn` ["service_error retry payment_pending"]
      alice `send` "/create user alisa"
      shownBadges a `shouldEventuallyReturn` [(1, Just 1), (2, Nothing)]
      isRight <$> sendChatCmdStr a (restoredCmd 2 legend) `shouldReturn` True
      storeReceiptHolds a `shouldReturn` [(1, "tier_change")]
      heldStoreReceipts a `shouldReturn` 1
      dropOutput alice

-- | A Restore takes a held record without its owner's lock, so the owner's worker checks it again under the lock.
testStoreReceiptHeldForAfterRestore :: HasCallStack => TestParams -> IO ()
testStoreReceiptHeldForAfterRestore ps =
  withBadgeServiceEnv ps $ \env@BadgeServiceEnv {bsClientCfg} ->
    withNewTestChatCfg ps bsClientCfg "alice" aliceProfile $ \alice -> do
      alice ##> "/create user alisa"
      showActiveUser alice "alisa"
      g <- C.newRandom
      now <- serviceNow env
      let token = restoreToken "held-for"
          payment = googlePayment "subscr_badge_supporter_01" token
          txRef = StoreTransactionRef PPGoogle (googlePurchaseRef token)
      withTransaction (chatStore $ chatController alice) $ \db -> do
        Right owner <- runExceptT $ getUser db 1
        Right restorer <- runExceptT $ getUser db 2
        void $ holdStoreReceipt db g owner BT.SROPurchased Nothing txRef payment now
        [Only receiptId] <- DB.query_ db "SELECT badge_store_receipt_id FROM badge_store_receipts"
        storeReceiptHeldFor db receiptId 1 SHKPurchase `shouldReturn` True
        holdStoreTransfer db g restorer txRef payment now `shouldReturn` False
        storeReceiptHeldFor db receiptId 1 SHKPurchase `shouldReturn` False
        storeReceiptHeldFor db receiptId 2 SHKTransfer `shouldReturn` True

testRefusedMoveKeepsTierChange :: HasCallStack => TestParams -> IO ()
testRefusedMoveKeepsTierChange ps =
  withBadgeServiceEnv ps $ \env@BadgeServiceEnv {bsStore = store@FakeStore {appleSubscriptionJWS, appleLegendSubscriptionJWS}, bsClientCfg} ->
    withNewTestChatCfg ps bsClientCfg "bob" bobProfile $ \bob ->
      withNewTestChatCfg ps bsClientCfg "alice" aliceProfile $ \alice -> do
        let a = chatController alice
            supporter = SPApple {jws = appleSubscriptionJWS}
        appleSupporterBeforeLegend env bob
        -- the status names the transaction presented as the latest, so it moves
        appleStoreReports store $ subscriptionCheck SSRenewing $ Just (storeCharge "SUBSCR_BADGE_SUPPORTER_MONTH_01" (addUTCTime (-5 * nominalDay) legendBoughtAt) 1) {chargeRef = "2000000812345681"}
        alice `send` restoredCmd 1 supporter
        waitShownBadge a (Just 1)
        alice `send` "/create user alisa"
        shownBadges a `shouldEventuallyReturn` [(1, Just 1), (2, Nothing)]
        nextClaimCheck env
        alice `send` restoredCmd 2 supporter
        storeReceiptCreditErrors a `shouldEventuallyReturn` ["service_error final transfer_limited"]
        storeReceiptHolds a `shouldReturn` [(1, "purchase")]
        -- the refused move is not the purchase's own refused change, so a tier change of it is held at once
        isRight <$> sendChatCmdStr a "/user alice" `shouldReturn` True
        either show (const "answered") <$> sendChatCmdStr a ("/_badge purchase 1 " <> paymentArg SPApple {jws = appleLegendSubscriptionJWS}) `shouldReturn` "answered"
        storeReceiptHolds a `shouldReturn` [(1, "tier_change")]
        heldStoreReceipts a `shouldReturn` 1
        mapM_ dropOutput [alice, bob]

testRestoreHeldProfileDeleted :: HasCallStack => TestParams -> IO ()
testRestoreHeldProfileDeleted ps =
  withBadgeServiceEnv ps $ \env@BadgeServiceEnv {bsClientCfg, bsStore = store} ->
    withNewTestChatCfg ps bsClientCfg "alice" aliceProfile $ \alice -> do
      let a = chatController alice
      firstStart <- addUTCTime (-3600) <$> serviceNow env
      storeReports store $ subscriptionCheck SSRenewing $ Just $ storeCharge "subscr-badge-supporter-month-02" firstStart 1
      alice `send` ("/_badge purchase 1 " <> paymentArg subscriptionPlay)
      waitShownBadge a (Just 1)
      alice `send` "/create user alisa"
      shownBadges a `shouldEventuallyReturn` [(1, Just 1), (2, Nothing)]
      -- with no charge to grant the move waits, held for the second profile
      storeReports store $ subscriptionCheck SSRenewing Nothing
      alice `send` restoredCmd 2 subscriptionPlay
      heldStoreReceiptErrors a `shouldEventuallyReturn` ["service_error retry payment_pending"]
      storeReceiptHolds a `shouldReturn` [(2, "transfer")]
      isRight <$> sendChatCmdStr a "/user alice" `shouldReturn` True
      isRight <$> sendChatCmdStr a "/_delete user 2 del_smp=off" `shouldReturn` True
      storeReceiptHolds a `shouldReturn` [(1, "purchase")]
      -- the move's failures are not the owner's refused resume or tier change
      storeReceiptCreditErrors a `shouldReturn` []
      heldStoreReceipts a `shouldReturn` 0
      purchaseReceiptLinks a `shouldReturn` [(1, True)]
      -- handed over again, it is answered from the record
      isRight <$> sendChatCmdStr a ("/_badge purchase 1 " <> paymentArg subscriptionPlay) `shouldReturn` True
      heldStoreReceipts a `shouldReturn` 0
      dropOutput alice

testRestoreTakesHeldPurchase :: HasCallStack => TestParams -> IO ()
testRestoreTakesHeldPurchase ps =
  withBadgeServiceEnv ps $ \env@BadgeServiceEnv {bsClientCfg, bsStore = store} ->
    withNewTestChatCfg ps bsClientCfg "bob" bobProfile $ \bob ->
      withNewTestChatCfg ps bsClientCfg "alice" aliceProfile $ \alice -> do
        let a = chatController alice
        firstStart <- addUTCTime (-3600) <$> serviceNow env
        storeReports store $ subscriptionCheck SSRenewing $ Just $ storeCharge "subscr-badge-supporter-month-02" firstStart 1
        bob `send` ("/_badge purchase 1 " <> paymentArg subscriptionPlay)
        waitShownBadge (chatController bob) (Just 1)
        -- a hand-over on profile 1 within the hour of the purchase's store check is held
        alice `send` ("/_badge purchase 1 " <> paymentArg subscriptionPlay)
        heldStoreReceiptErrors a `shouldEventuallyReturn` ["service_error retry payment_pending"]
        storeReceiptHolds a `shouldReturn` [(1, "purchase")]
        alice `send` "/create user alisa"
        shownBadges a `shouldEventuallyReturn` [(1, Nothing), (2, Nothing)]
        isRight <$> sendChatCmdStr a (restoredCmd 2 subscriptionPlay) `shouldReturn` True
        storeReceiptHolds a `shouldReturn` [(2, "transfer")]
        nextClaimCheck env
        isRight <$> sendChatCmdStr a (restoredCmd 2 subscriptionPlay) `shouldReturn` True
        shownBadges a `shouldEventuallyReturn` [(1, Nothing), (2, Just 1)]
        purchaseReceiptLinks a `shouldReturn` [(2, True)]
        mapM_ dropOutput [alice, bob]

testRestoreOnSameProfile :: HasCallStack => TestParams -> IO ()
testRestoreOnSameProfile ps =
  withBadgeServiceEnv ps $ \env@BadgeServiceEnv {bsClock, bsClientCfg, bsController = cc, bsStore = store} ->
    withNewTestChatCfg ps bsClientCfg "alice" aliceProfile $ \alice -> do
      let a = chatController alice
      (_, rows) <- clientSubscribedMonthly env alice
      -- the subscription lapses: retired once the recovery window passed while Play retried the payment
      storeReports store $ subscriptionCheck SSBillingRetry Nothing
      t0 <- firstUnrenewedAnswer alice bsClock rows []
      -- an answer within the window keeps the run going, so the one past it retires the badge
      let beforeEnd = addUTCTime (subscriptionRecoveryWindow - 3600) t0
      activateAt alice bsClock beforeEnd
      fst <$> answeredRun a beforeEnd `shouldReturn` t0
      [Just wake] <- nextWakes a
      setClockAt bsClock $ max wake $ addUTCTime (subscriptionRecoveryWindow + 3600) t0
      activateAnswered alice ["badge alert: subscription_ended "]
      waitShownBadge a Nothing
      -- a code's badge, now the profile's newest purchase, which then ends too, as Restore is refused while a badge shows
      code <- issueCode cc BTSupporter 1
      dropOutput alice
      alice ##> ("/_redeem_badge_code 1 " <> codeArg code)
      expectLinesWithin 30 alice ["badge redeemed", "supporter badge - active", "expires "] ["1: supporter", "2: supporter", "badge alert: "]
      waitShownBadge a (Just 2)
      codeEnds <- dueAtOf <$> ledgerRows a "badge_ledger"
      setClockAt bsClock $ addUTCTime 3600 codeEnds
      alice `send` "/_app activate"
      waitShownBadge a Nothing
      dropOutput alice
      -- Play charges the subscription again, and Restore on the same profile moves it to fresh keys
      resubscribedAt <- serviceNow env
      storeReports store $ subscriptionCheck SSRenewing $ Just (storeCharge "subscr-badge-supporter-month-02" (addUTCTime (-3600) resubscribedAt) 1) {chargeRef = "GPA.resubscribed"}
      alice `send` restoredCmd 1 subscriptionPlay
      waitShownBadge a (Just 3)
      storeReceiptCreditErrors a `shouldReturn` []
      purchaseReceiptLinks a `shouldReturn` [(1, False), (1, False), (1, True)]
      dropOutput alice

testRestoreLeavesResumeHold :: HasCallStack => TestParams -> IO ()
testRestoreLeavesResumeHold ps =
  withBadgeServiceEnv ps $ \env@BadgeServiceEnv {bsClock, bsClientCfg, bsStore = store} ->
    withNewTestChatCfg ps bsClientCfg "dan" danProfile $ \dan -> do
      requestAt <- appleSubscriptionRetired env dan
      let resubscribedAt = addUTCTime 7200 requestAt
          resubscription = SPApple {jws = appleTransactionDated "2000000812345695" resubscribedAt (addMonths 1 resubscribedAt)}
      setClockAt bsClock resubscribedAt
      appleStoreReports store $ subscriptionCheck SSRenewing $ Just (storeCharge "SUBSCR_BADGE_SUPPORTER_MONTH_01" resubscribedAt 1) {chargeRef = "apple.revoked", revoked = True}
      dan `send` ("/_badge purchase 1 " <> paymentArg resubscription)
      heldStoreReceiptErrors (chatController dan) `shouldEventuallyReturn` ["service_error retry payment_pending"]
      dan `send` "/create user daniel"
      shownBadges (chatController dan) `shouldEventuallyReturn` [(1, Nothing), (2, Nothing)]
      isRight <$> sendChatCmdStr (chatController dan) (restoredCmd 2 resubscription) `shouldReturn` True
      storeReceiptHolds (chatController dan) `shouldReturn` [(1, "purchase")]
      heldStoreReceipts (chatController dan) `shouldReturn` 1
      dropOutput dan

testRestoreAfterUnfinishedPass :: HasCallStack => TestParams -> IO ()
testRestoreAfterUnfinishedPass ps =
  withBadgeServiceEnv ps $ \env@BadgeServiceEnv {bsClientCfg, bsStore = store} ->
    withNewTestChatCfg ps bsClientCfg "alice" aliceProfile $ \alice ->
      withNewTestChatCfg ps bsClientCfg "bob" bobProfile $ \bob ->
        withNewTestChatCfg ps bsClientCfg "cath" cathProfile $ \cath -> do
          firstStart <- addUTCTime (-3600) <$> serviceNow env
          let other = googlePayment "subscr_badge_supporter_01" $ restoreToken "2"
          storeReports store $ subscriptionCheck SSRenewing $ Just $ storeCharge "subscr-badge-supporter-month-02" firstStart 1
          setSubscription store (SubscriptionGoogle $ restoreToken "2") $ subscriptionCheck SSRenewing $ Just $ storeCharge "subscr-badge-supporter-month-02" firstStart 1
          alice `send` ("/_badge purchase 1 " <> paymentArg subscriptionPlay)
          waitShownBadge (chatController alice) (Just 1)
          -- a sweep's hand-over, held while the claim's hour holds the store check off, becomes a transfer on Restore
          bob `send` ("/_badge purchase 1 " <> paymentArg subscriptionPlay)
          heldStoreReceiptErrors (chatController bob) `shouldEventuallyReturn` ["service_error retry payment_pending"]
          nextClaimCheck env
          bob `send` restoredCmd 1 subscriptionPlay
          waitShownBadge (chatController bob) (Just 1)
          heldStoreReceipts (chatController bob) `shouldReturn` 0
          -- a hand-over refused receipt_used is held again as a transfer
          alice `send` "/create user alisa"
          shownBadges (chatController alice) `shouldEventuallyReturn` [(1, Just 1), (2, Nothing)]
          alice `send` ("/_badge purchase 2 " <> paymentArg other)
          shownBadges (chatController alice) `shouldEventuallyReturn` [(1, Just 1), (2, Just 2)]
          nextClaimCheck env
          cath `send` ("/_badge purchase 1 " <> paymentArg other)
          storeReceiptCreditErrors (chatController cath) `shouldEventuallyReturn` ["service_error final receipt_used"]
          nextClaimCheck env
          cath `send` restoredCmd 1 other
          waitShownBadge (chatController cath) (Just 1)
          storeReceiptCreditErrors (chatController cath) `shouldReturn` []
          mapM_ dropOutput [alice, bob, cath]

testSubscriptionAwaitsCharge :: HasCallStack => TestParams -> IO ()
testSubscriptionAwaitsCharge ps =
  withBadgeServiceEnv ps $ \env@BadgeServiceEnv {bsClock, bsClientCfg, bsController = cc, bsStore = store} ->
    withNewTestChatCfg ps bsClientCfg "alice" aliceProfile $ \alice -> do
      (_, rows) <- clientSubscribedMonthly env alice
      let (requestAt, presentAt) = renewalMoments rows
          recoveredAt = addUTCTime (nominalDay / 2) requestAt
      storeReports store $ subscriptionCheck SSBillingRetry Nothing
      checks <- subscriptionChecks store
      setClockAt bsClock requestAt
      alice `send` "/_app activate"
      alice
        <### [ "/_app activate",
               "ok",
               paymentIssue,
               Predicate (\l -> "1: supporter" `isPrefixOf` l && ", subscription billingRetry" `isInfixOf` l),
               paymentIssue
             ]
      -- nothing issued and no failure: the badge stays, and the next request is a day on
      waitWakeFrom (chatController alice) (addUTCTime (nominalDay - 60) requestAt)
      subscriptionChecks store `shouldReturn` checks + 1
      issueErrorRow (chatController alice) `shouldReturn` (Nothing, Nothing)
      ledgerRows (chatController alice) "badge_ledger" `shouldReturn` rows
      ledgerRows cc "sx_badge_service_badge_ledger" `shouldReturn` rows
      shownBadgeId (chatController alice) >>= (`shouldSatisfy` isJust)
      -- opening the app before that wake sends nothing, though the service would ask the store again after an hour
      checkedAt <- subscriptionCheckedAt cc
      wake <- nextWakes (chatController alice)
      forM_ [2 * 3600, nominalDay / 2] $ \sinceRequest -> do
        setClockAt bsClock $ addUTCTime sinceRequest requestAt
        alice ##> "/_app activate"
        alice <## "ok"
        alice ##> "/_badge state 1"
        alice <### [StartsWith "1: supporter", paymentIssue]
        threadDelay 2000000
        subscriptionChecks store `shouldReturn` checks + 1
        subscriptionCheckedAt cc `shouldReturn` checkedAt
        nextWakes (chatController alice) `shouldReturn` wake
      -- the charge lands, its period starting when the store recovered it
      storeReports store $ subscriptionCheck SSRenewing $ Just (storeCharge "subscr-badge-supporter-month-02" recoveredAt 1) {chargeRef = "GPA.recovered"}
      setClockAt bsClock presentAt
      alice ##> "/_app activate"
      alice <## "ok"
      renewed <- waitLedgerRows (chatController alice) 4
      alice <##. "1: supporter"
      drop 2 (rowChanges renewed) `shouldBe` [(1, 1, recoveredAt, Just "charge"), (-1, 0, addMonths 1 recoveredAt, Just "badge")]
      issueErrorRow (chatController alice) `shouldReturn` (Nothing, Nothing)
      waitShownIssued (chatController alice)

testFarWakeHoldsNothing :: HasCallStack => TestParams -> IO ()
testFarWakeHoldsNothing ps =
  withBadgeServiceEnv ps $ \env@BadgeServiceEnv {bsClock, bsClientCfg, bsStore = store} ->
    withNewTestChatCfg ps bsClientCfg "alice" aliceProfile $ \alice -> do
      (_, rows) <- clientSubscribedMonthly env alice
      let (requestAt, presentAt) = renewalMoments rows
      storeReports store $ subscriptionCheck SSBillingRetry Nothing
      setClockAt bsClock requestAt
      alice `send` "/_app activate"
      alice
        <### [ "/_app activate",
               "ok",
               paymentIssue,
               Predicate (\l -> "1: supporter" `isPrefixOf` l && ", subscription billingRetry" `isInfixOf` l),
               paymentIssue
             ]
      waitWakeFrom (chatController alice) (addUTCTime (nominalDay - 60) requestAt)
      checks <- subscriptionChecks store
      -- no pass stores a wake this far ahead while a request is due: a device clock set back since left it
      setNextWake (chatController alice) $ addUTCTime (2 * nominalDay) requestAt
      setClockAt bsClock $ addUTCTime 7200 requestAt
      alice ##> "/_app activate"
      alice <## "ok"
      nextWakes (chatController alice) `shouldEventuallyReturn` [Just presentAt]
      subscriptionChecks store `shouldReturn` checks + 1

testStoreUnreachableWaitsStall :: HasCallStack => TestParams -> IO ()
testStoreUnreachableWaitsStall ps =
  withBadgeServiceEnv ps $ \env@BadgeServiceEnv {bsClock, bsClientCfg, bsController = cc, bsStore = store} ->
    withNewTestChatCfg ps bsClientCfg "alice" aliceProfile $ \alice -> do
      (firstStart, rows) <- clientSubscribedMonthly env alice
      let (requestAt, presentAt) = renewalMoments rows
          renewalStart = addMonths 1 firstStart
      storeReports store $ subscriptionCheck SSRenewing $ Just $ monthlyCharge "subscr-badge-supporter-month-02" firstStart 1
      setGoogleDown store True
      setClockAt bsClock requestAt
      alice `send` "/_app activate"
      alice
        <### [ "/_app activate",
               "ok",
               "bad chat command: badge service error: provider_unavailable",
               StartsWith "1: supporter"
             ]
      waitWakeFrom (chatController alice) (addUTCTime (nominalDay - 60) requestAt)
      checkedAt <- subscriptionCheckedAt cc
      errorAt <- issueErrorAt (chatController alice)
      wake <- nextWakes (chatController alice)
      forM_ [2 * 3600, nominalDay / 2] $ \sinceRequest -> do
        setClockAt bsClock $ addUTCTime sinceRequest requestAt
        alice ##> "/_app activate"
        alice <## "ok"
        alice ##> "/_badge state 1"
        alice <##. "1: supporter"
        threadDelay 2000000
        subscriptionCheckedAt cc `shouldReturn` checkedAt
        issueErrorAt (chatController alice) `shouldReturn` errorAt
        nextWakes (chatController alice) `shouldReturn` wake
      setGoogleDown store False
      setClockAt bsClock presentAt
      alice ##> "/_app activate"
      alice <## "ok"
      renewed <- waitLedgerRows (chatController alice) 4
      alice <##. "1: supporter"
      drop 2 (rowChanges renewed) `shouldBe` [(1, 1, renewalStart, Just "charge"), (-1, 0, addMonths 1 renewalStart, Just "badge")]
      waitShownIssued (chatController alice)
      waitIssueErrorCleared (chatController alice)

testSnoozeKeepsRequestWait :: HasCallStack => TestParams -> IO ()
testSnoozeKeepsRequestWait ps =
  withBadgeServiceEnv ps $ \env@BadgeServiceEnv {bsClock, bsClientCfg, bsController = cc, bsStore = store} ->
    withNewTestChatCfg ps bsClientCfg "alice" aliceProfile $ \alice -> do
      (firstStart, rows) <- clientSubscribedMonthly env alice
      let (requestAt, presentAt) = renewalMoments rows
          episode = safeDecodeUtf8 $ strEncode $ addMonths 1 firstStart
      storeReports store $ subscriptionCheck SSBillingRetry Nothing
      setClockAt bsClock requestAt
      alice `send` "/_app activate"
      alice
        <### [ "/_app activate",
               "ok",
               paymentIssue,
               Predicate (\l -> "1: supporter" `isPrefixOf` l && ", subscription billingRetry" `isInfixOf` l),
               paymentIssue
             ]
      waitWakeFrom (chatController alice) (addUTCTime (nominalDay - 60) requestAt)
      setClockAt bsClock $ addUTCTime 7200 requestAt
      alice ##> ("/_badge ack 1 1 payment_issue on " <> T.unpack episode)
      alice <##. "1: supporter"
      ackedEpisode (chatController alice) `shouldReturn` (Just "payment_issue", Just episode)
      nextWakes (chatController alice) `shouldEventuallyReturn` [Just presentAt]
      checks <- subscriptionChecks store
      -- the second request, while the alert is snoozed until two hours after it
      setClockAt bsClock presentAt
      alice ##> "/_app activate"
      alice <## "ok"
      subscriptionChecks store `shouldEventuallyReturn` (checks + 1)
      waitWakeFrom (chatController alice) (addUTCTime (nominalDay - 60) presentAt)
      checkedAt <- subscriptionCheckedAt cc
      wake <- nextWakes (chatController alice)
      setClockAt bsClock $ addUTCTime (3 * 3600) presentAt
      alice `send` "/_app activate"
      alice <### ["/_app activate", "ok", paymentIssue]
      threadDelay 2000000
      subscriptionChecks store `shouldReturn` checks + 1
      subscriptionCheckedAt cc `shouldReturn` checkedAt
      nextWakes (chatController alice) `shouldReturn` wake

testStallRejoinsSharedMoment :: HasCallStack => TestParams -> IO ()
testStallRejoinsSharedMoment ps =
  withBadgeServiceEnv ps $ \env@BadgeServiceEnv {bsClock, bsClientCfg, bsStore = store} ->
    withNewTestChatCfg ps bsClientCfg "alice" aliceProfile $ \alice -> do
      (_, rows) <- clientSubscribedMonthly env alice
      let (requestAt, presentAt) = renewalMoments rows
          requestedAt t = do
            checks <- subscriptionChecks store
            setClockAt bsClock t
            alice ##> "/_app activate"
            alice <## "ok"
            subscriptionChecks store `shouldEventuallyReturn` (checks + 1)
      storeReports store $ subscriptionCheck SSBillingRetry Nothing
      setClockAt bsClock requestAt
      alice `send` "/_app activate"
      alice
        <### [ "/_app activate",
               "ok",
               paymentIssue,
               Predicate (\l -> "1: supporter" `isPrefixOf` l && ", subscription billingRetry" `isInfixOf` l),
               paymentIssue
             ]
      waitWakeFrom (chatController alice) (addUTCTime (nominalDay - 60) requestAt)
      -- the app is not running at the expiry, and an activation three hours later sends the request
      requestedAt $ addUTCTime (3 * 3600) presentAt
      nextWakes (chatController alice) `shouldEventuallyReturn` [Just $ addUTCTime nominalDay presentAt]
      requestedAt $ addUTCTime nominalDay presentAt
      nextWakes (chatController alice) `shouldEventuallyReturn` [Just $ addUTCTime (2 * nominalDay) presentAt]

-- | A service that asks its store at every issue request, so the store's check count counts the client's requests.
withRequestCountingService :: HasCallStack => TestParams -> (BadgeServiceEnv -> IO ()) -> IO ()
withRequestCountingService ps = withBadgeServiceVerifier ps $ \store -> (fakeVerifier store) {storeChecks = ChecksAtEveryRequest}

-- | An hour past a midnight UTC three days before paidThrough: inside coverage, with the next daily moment of the
-- shown expiry 23 hours on and every ledger moment days away, so only the dev interval can bring a request.
devIntervalStart :: [ReplicatedRow] -> UTCTime
devIntervalStart rows = UTCTime (addDays (-3) (utctDay (dueAtOf rows))) 3600

activateAt :: HasCallStack => TestCC -> TestClock -> UTCTime -> IO ()
activateAt cc clock t = do
  setClockAt clock t
  cc ##> "/_app activate"
  cc <## "ok"

testDevIntervalRequests :: HasCallStack => TestParams -> IO ()
testDevIntervalRequests ps =
  withRequestCountingService ps $ \env@BadgeServiceEnv {bsClock, bsClientCfg, bsStore = store} ->
    withNewTestChatCfg ps bsClientCfg {badgeDevRequestInterval = Just 600} "alice" aliceProfile $ \alice -> do
      (_, rows) <- clientSubscribedMonthly env alice
      let t1 = devIntervalStart rows
      checks <- subscriptionChecks store
      activateAt alice bsClock t1
      subscriptionChecks store `shouldEventuallyReturn` (checks + 1)
      waitWakeFrom (chatController alice) (addUTCTime 600 t1)
      [Just wake] <- nextWakes (chatController alice)
      wake `shouldSatisfy` (< addUTCTime 610 t1)
      activateAt alice bsClock $ addUTCTime 300 t1
      threadDelay 2000000
      subscriptionChecks store `shouldReturn` checks + 1
      nextWakes (chatController alice) `shouldReturn` [Just wake]
      activateAt alice bsClock $ addUTCTime 620 t1
      subscriptionChecks store `shouldEventuallyReturn` (checks + 2)
      -- a purchase the store ended is retired by the request's answer, and not asked about again
      storeReports store $ subscriptionCheck SSEnded Nothing
      setClockAt bsClock $ addUTCTime 1240 t1
      alice `send` "/_app activate"
      waitShownBadge (chatController alice) Nothing
      subscriptionChecks store `shouldReturn` checks + 3
      dropOutput alice
      setClockAt bsClock $ addUTCTime 1860 t1
      alice `send` "/_app activate"
      threadDelay 2000000
      subscriptionChecks store `shouldReturn` checks + 3
      dropOutput alice

testNoDevIntervalRequests :: HasCallStack => TestParams -> IO ()
testNoDevIntervalRequests ps =
  withRequestCountingService ps $ \env@BadgeServiceEnv {bsClock, bsClientCfg, bsStore = store} ->
    withNewTestChatCfg ps bsClientCfg "alice" aliceProfile $ \alice -> do
      (_, rows) <- clientSubscribedMonthly env alice
      let t1 = devIntervalStart rows
      checks <- subscriptionChecks store
      wake <- nextWakes (chatController alice)
      forM_ [0, 300, 620] $ \sinceStart -> do
        activateAt alice bsClock $ addUTCTime sinceStart t1
        threadDelay 2000000
        subscriptionChecks store `shouldReturn` checks
        nextWakes (chatController alice) `shouldReturn` wake

testDevIntervalRetries :: HasCallStack => TestParams -> IO ()
testDevIntervalRetries ps =
  withRequestCountingService ps $ \env@BadgeServiceEnv {bsClock, bsClientCfg, bsController = cc} -> do
    let hourly cfg = cfg {badgeDevRequestInterval = Just 3600}
    rows <- withNewTestChatCfg ps (hourly bsClientCfg) "alice" aliceProfile $ \alice -> snd <$> clientSubscribedMonthly env alice
    stopBadgeService cc
    let t1 = devIntervalStart rows
    -- the start's pass sends the request the interval made due, which times out and is retried
    setClockAt bsClock t1
    withTestChatCfg ps (hourly $ fastRetryCfg $ failingServiceCfg bsClientCfg) "alice" $ \alice -> do
      failedAt <- replicateM 2 $ alice <##. "1: supporter" >> issueErrorAt (chatController alice)
      case failedAt of
        [[Just first], [Just second]] -> do
          second `shouldSatisfy` (> first)
          [first, second] `shouldSatisfy` all (\t -> t >= t1 && t < addUTCTime 60 t1)
        _ -> expectationFailure $ "expected two failed attempts, got " <> show failedAt

testSubscriptionRetiredAfterRecoveryWindow :: HasCallStack => TestParams -> IO ()
testSubscriptionRetiredAfterRecoveryWindow ps =
  withBadgeServiceEnv ps $ \env@BadgeServiceEnv {bsClock, bsClientCfg, bsController = cc} ->
    withNewTestChatCfg ps bsClientCfg "alice" aliceProfile $ \alice -> do
      (_, rows) <- clientSubscribedMonthly env alice
      setServicePurchaseKey cc "not the purchase key"
      t0 <- firstUnrenewedAnswer alice bsClock rows [badgeServiceRefused]
      let beforeEnd = addUTCTime (subscriptionRecoveryWindow - 3600) t0
      setClockAt bsClock beforeEnd
      activateAnswered alice [badgeServiceRefused]
      fst <$> answeredRun (chatController alice) beforeEnd `shouldReturn` t0
      shownBadgeId (chatController alice) >>= (`shouldSatisfy` isJust)
      errorAt <- issueErrorAt (chatController alice)
      -- past the window, and at or after the wake the refusal stored, so the request is sent and answered
      [Just wake] <- nextWakes (chatController alice)
      setClockAt bsClock $ max wake $ addUTCTime (subscriptionRecoveryWindow + 3600) t0
      alice `send` "/_app activate"
      alice
        <### [ "/_app activate",
               "ok",
               ConsoleString badgeServiceRefused,
               StartsWith "badge alert: subscription_ended ",
               StartsWith "1: supporter",
               StartsWith "renewal failing since ",
               StartsWith "badge alert: subscription_ended "
             ]
      waitShownBadge (chatController alice) Nothing
      retiredErrorAt <- issueErrorAt (chatController alice)
      retiredErrorAt `shouldNotBe` errorAt
      testClockTime bsClock >>= setClockAt bsClock . addUTCTime 3600
      alice ##> "/_app activate"
      alice <## "ok"
      threadDelay 2000000
      issueErrorAt (chatController alice) `shouldReturn` retiredErrorAt

testBankedSubscriptionNotRetired :: HasCallStack => TestParams -> IO ()
testBankedSubscriptionNotRetired ps =
  withBadgeServiceEnv ps $ \env@BadgeServiceEnv {bsClock, bsClientCfg, bsController = cc, bsStore = store} ->
    withNewTestChatCfg ps bsClientCfg "alice" aliceProfile $ \alice -> do
      firstStart <- addUTCTime (-3600) <$> serviceNow env
      storeReports store $ subscriptionCheck SSRenewing $ Just $ storeCharge "subscr-badge-supporter-year-01" firstStart 12
      alice ##> ("/_badge purchase 1 " <> paymentArg subscriptionPlay)
      inAnyOrder alice [[openPurchase ""], ["1: supporter"]]
      waitWakeArmed (chatController alice)
      rows <- ledgerRows (chatController alice) "badge_ledger"
      map (\(_, ch, m, _, _, t) -> (ch, m, t)) rows `shouldBe` [(12, 12, Just "charge"), (-1, 11, Just "badge")]
      setServicePurchaseKey cc "not the purchase key"
      setClockAt bsClock $ addUTCTime (subscriptionRecoveryWindow + nominalDay) $ snd $ renewalMoments rows
      alice `send` "/_app activate"
      alice
        <### [ "/_app activate",
               "ok",
               ConsoleString badgeServiceRefused,
               StartsWith "badge alert: issue_failed ",
               StartsWith "1: supporter",
               StartsWith "renewal failing since ",
               StartsWith "badge alert: issue_failed "
             ]
      shownBadgeId (chatController alice) >>= (`shouldSatisfy` isJust)

testIdleSubscriptionKeptPastWindow :: HasCallStack => TestParams -> IO ()
testIdleSubscriptionKeptPastWindow ps =
  withBadgeServiceEnv ps $ \env@BadgeServiceEnv {bsClock, bsClientCfg, bsStore = store} ->
    withNewTestChatCfg ps bsClientCfg "alice" aliceProfile $ \alice -> do
      (firstStart, rows) <- clientSubscribedMonthly env alice
      -- the store kept charging while the app was not opened: its third renewal covers the day of the first activation
      storeReports store $ subscriptionCheck SSRenewing $ Just $ monthlyCharge "subscr-badge-supporter-month-02" firstStart 3
      setClockAt bsClock $ addUTCTime (subscriptionRecoveryWindow + nominalDay) $ dueAtOf rows
      alice ##> "/_app activate"
      alice <## "ok"
      renewed <- waitLedgerRows (chatController alice) 4
      alice <##. "1: supporter"
      drop 2 (rowChanges renewed) `shouldBe` [(1, 1, addMonths 3 firstStart, Just "charge"), (-1, 0, addMonths 4 firstStart, Just "badge")]
      waitShownIssued (chatController alice)
      shownBadgeId (chatController alice) >>= (`shouldSatisfy` isJust)
      unrenewedRuns (chatController alice) `shouldReturn` [(Nothing, Nothing)]
      (alice </)

testSubscriptionNotRetiredWithoutAnswer :: HasCallStack => TestParams -> IO ()
testSubscriptionNotRetiredWithoutAnswer ps =
  withBadgeServiceEnv ps $ \env@BadgeServiceEnv {bsClock, bsClientCfg, bsController = cc} -> do
    t0 <- withNewTestChatCfg ps bsClientCfg "alice" aliceProfile $ \alice -> do
      (_, rows) <- clientSubscribedMonthly env alice
      setServicePurchaseKey cc "not the purchase key"
      firstUnrenewedAnswer alice bsClock rows [badgeServiceRefused]
    stopBadgeService cc
    setClockAt bsClock $ addUTCTime (subscriptionRecoveryWindow + 3600) t0
    withTestChatCfg ps (failingServiceCfg bsClientCfg) "alice" $ \alice -> do
      alice <##. "1: supporter"
      alice <##. "renewal failing since "
      alice <##. "badge alert: issue_failed "
      (_, reason) <- issueErrorRow (chatController alice)
      reason `shouldBe` Just "service_timeout"
      shownBadgeId (chatController alice) >>= (`shouldSatisfy` isJust)
      unrenewedRuns (chatController alice) `shouldReturn` [(Just t0, Just t0)]

testUnrenewedRunStartsAtFirstAnswer :: HasCallStack => TestParams -> IO ()
testUnrenewedRunStartsAtFirstAnswer ps =
  withBadgeServiceEnv ps $ \env@BadgeServiceEnv {bsClock, bsClientCfg, bsStore = store} ->
    withNewTestChatCfg ps bsClientCfg "alice" aliceProfile $ \alice -> do
      (_, rows) <- clientSubscribedMonthly env alice
      setGoogleDown store True
      -- not opened for longer than the window: the first answer, which grants nothing, starts the run
      let firstAnswerAt = addUTCTime (subscriptionRecoveryWindow + nominalDay) $ dueAtOf rows
          unreachable = "bad chat command: badge service error: provider_unavailable"
      setClockAt bsClock firstAnswerAt
      activateAnswered alice [unreachable]
      shownBadgeId (chatController alice) >>= (`shouldSatisfy` isJust)
      (t1, t1') <- answeredRun (chatController alice) firstAnswerAt
      t1' `shouldBe` t1
      let beforeEnd = addUTCTime (subscriptionRecoveryWindow - 3600) t1
      setClockAt bsClock beforeEnd
      activateAnswered alice [unreachable]
      fst <$> answeredRun (chatController alice) beforeEnd `shouldReturn` t1
      shownBadgeId (chatController alice) >>= (`shouldSatisfy` isJust)
      [Just wake] <- nextWakes (chatController alice)
      setClockAt bsClock $ max wake $ addUTCTime (subscriptionRecoveryWindow + 3600) t1
      activateAnswered alice [unreachable, "badge alert: subscription_ended "]
      waitShownBadge (chatController alice) Nothing

testRenewalEndsUnrenewedRun :: HasCallStack => TestParams -> IO ()
testRenewalEndsUnrenewedRun ps =
  withBadgeServiceEnv ps $ \env@BadgeServiceEnv {bsClock, bsClientCfg, bsStore = store} ->
    withNewTestChatCfg ps bsClientCfg "alice" aliceProfile $ \alice -> do
      (firstStart, rows) <- clientSubscribedMonthly env alice
      storeReports store $ subscriptionCheck SSBillingRetry Nothing
      t0 <- firstUnrenewedAnswer alice bsClock rows []
      storeReports store $ subscriptionCheck SSRenewing $ Just $ monthlyCharge "subscr-badge-supporter-month-02" firstStart 1
      [Just renewalWake] <- nextWakes (chatController alice)
      setClockAt bsClock renewalWake
      alice ##> "/_app activate"
      alice <## "ok"
      renewed <- waitLedgerRows (chatController alice) 4
      alice <##. "1: supporter"
      unrenewedRuns (chatController alice) `shouldEventuallyReturn` [(Nothing, Nothing)]
      waitShownIssued (chatController alice)
      storeReports store $ subscriptionCheck SSBillingRetry Nothing
      t2 <- firstUnrenewedAnswer alice bsClock renewed []
      t2 `shouldSatisfy` (> t0)
      [Just wake] <- nextWakes (chatController alice)
      checks <- subscriptionChecks store
      let pastFirstWindow = max wake $ addUTCTime (subscriptionRecoveryWindow + 3600) t0
      setClockAt bsClock pastFirstWindow
      alice ##> "/_app activate"
      alice <## "ok"
      subscriptionChecks store `shouldEventuallyReturn` (checks + 1)
      fst <$> answeredRun (chatController alice) pastFirstWindow `shouldReturn` t2
      waitWakeFrom (chatController alice) (addUTCTime 1 pastFirstWindow)
      shownBadgeId (chatController alice) >>= (`shouldSatisfy` isJust)
      expectLinesWithin 5 alice [] ["1: supporter", "renewal failing since ", "badge alert: "]

testAbsenceRestartsUnrenewedRun :: HasCallStack => TestParams -> IO ()
testAbsenceRestartsUnrenewedRun ps =
  withBadgeServiceEnv ps $ \env@BadgeServiceEnv {bsClock, bsClientCfg, bsController = cc} ->
    withNewTestChatCfg ps bsClientCfg "alice" aliceProfile $ \alice -> do
      (_, rows) <- clientSubscribedMonthly env alice
      setServicePurchaseKey cc "not the purchase key"
      t0 <- firstUnrenewedAnswer alice bsClock rows [badgeServiceRefused]
      -- held by the wake the refusal stored, so nothing is sent and the run stays
      setClockAt bsClock $ addUTCTime 3600 t0
      alice ##> "/_app activate"
      alice <## "ok"
      threadDelay 2000000
      unrenewedRuns (chatController alice) `shouldReturn` [(Just t0, Just t0)]
      let returnAt = addUTCTime (subscriptionRecoveryWindow + 2 * nominalDay) t0
      setClockAt bsClock returnAt
      activateAnswered alice [badgeServiceRefused]
      (since, lastAt) <- answeredRun (chatController alice) returnAt
      since `shouldBe` lastAt
      shownBadgeId (chatController alice) >>= (`shouldSatisfy` isJust)

testOutageRestartsUnrenewedRun :: HasCallStack => TestParams -> IO ()
testOutageRestartsUnrenewedRun ps = do
  hook <- newIORef (pure ())
  withBadgeServiceVerifier ps (subscriptionCheckWithHook hook) $ \env@BadgeServiceEnv {bsClock, bsClientCfg, bsStore = store} -> do
    t0 <- withNewTestChatCfg ps bsClientCfg "alice" aliceProfile $ \alice -> do
      (_, rows) <- clientSubscribedMonthly env alice
      storeReports store $ subscriptionCheck SSBillingRetry Nothing
      firstUnrenewedAnswer alice bsClock rows []
    -- the store answers after the client gives up, so the pass throws without an answer and leaves the run as it was
    writeIORef hook $ threadDelay 5000000
    let outageAt = addUTCTime (subscriptionRecoveryWindow + 3600) t0
    setClockAt bsClock outageAt
    withTestChatCfg ps (failingServiceCfg bsClientCfg) "alice" $ \alice -> do
      expectLinesWithin 30 alice ["1: supporter", "renewal failing since "] ["badge alert: "]
      (_, reason) <- issueErrorRow (chatController alice)
      reason `shouldBe` Just "service_timeout"
      unrenewedRuns (chatController alice) `shouldReturn` [(Just t0, Just t0)]
      shownBadgeId (chatController alice) >>= (`shouldSatisfy` isJust)
    let answeredAt = addUTCTime (2 * nominalDay) outageAt
    setClockAt bsClock answeredAt
    withTestChatCfg ps bsClientCfg "alice" $ \alice -> do
      (since, lastAt) <- answeredRun (chatController alice) answeredAt
      since `shouldBe` lastAt
      waitWakeFrom (chatController alice) (addUTCTime 1 answeredAt)
      shownBadgeId (chatController alice) >>= (`shouldSatisfy` isJust)
      expectLinesWithin 5 alice [] ["1: supporter", "renewal failing since ", "badge alert: ", "subscribed "]

setNextWake :: ChatController -> UTCTime -> IO ()
setNextWake ChatController {chatStore} at =
  withTransaction chatStore $ \db -> DB.execute db "UPDATE badge_purchases SET next_wake_at = ?" (Only at)

testSubscriptionEnds :: HasCallStack => TestParams -> IO ()
testSubscriptionEnds ps =
  withBadgeServiceEnv ps $ \env@BadgeServiceEnv {bsClock, bsClientCfg, bsStore = store} ->
    withNewTestChatCfg ps bsClientCfg "alice" aliceProfile $ \alice -> do
      (_, rows) <- clientSubscribedMonthly env alice
      let (requestAt, _) = renewalMoments rows
      storeReports store $ subscriptionCheck SSEnded Nothing
      setClockAt bsClock requestAt
      alice `send` "/_app activate"
      alice
        <### [ "/_app activate",
               "ok",
               StartsWith "badge alert: subscription_ended ",
               StartsWith "1: supporter",
               StartsWith "badge alert: subscription_ended "
             ]
      waitShownBadge (chatController alice) Nothing
      -- a retired purchase is not asked about again
      checks <- subscriptionChecks store
      setClockAt bsClock $ addUTCTime (2 * nominalDay) requestAt
      alice ##> "/_app activate"
      alice <## "ok"
      threadDelay 2000000
      subscriptionChecks store `shouldReturn` checks
      -- the device's store still lists it as renewing, which does not undo the service's word that it ended
      subscriptionStatuses (chatController alice) `shouldReturn` [Just "ended"]
      alice ##> ("/_badge subscription renewing " <> paymentArg subscriptionPlay)
      alice <## "ok"
      subscriptionStatuses (chatController alice) `shouldReturn` [Just "ended"]

testSubscriptionEndsWhileCovered :: HasCallStack => TestParams -> IO ()
testSubscriptionEndsWhileCovered ps =
  withBadgeServiceEnv ps $ \env@BadgeServiceEnv {bsClock, bsClientCfg, bsController = ChatController {chatStore}, bsStore = store} ->
    withNewTestChatCfg ps bsClientCfg "alice" aliceProfile $ \alice -> do
      firstStart <- addUTCTime (-3600) <$> serviceNow env
      storeReports store $ subscriptionCheck SSRenewing $ Just $ storeCharge "subscr-badge-supporter-year-01" firstStart 12
      alice ##> ("/_badge purchase 1 " <> paymentArg subscriptionPlay)
      inAnyOrder alice [[openPurchase ""], ["1: supporter"]]
      waitWakeArmed (chatController alice)
      rows <- ledgerRows (chatController alice) "badge_ledger"
      let requestAt = fst $ renewalMoments rows
      -- stands in for a check within the hour that found it ended, as an upgrade's check records one
      withTransaction chatStore $ \db -> DB.execute db "UPDATE sx_badge_service_payments SET subscription_state = 'ended', subscription_checked_at = ?" (Only requestAt)
      checks <- subscriptionChecks store
      setClockAt bsClock requestAt
      alice `send` "/_app activate"
      alice
        <### [ "/_app activate",
               "ok",
               StartsWith "badge alert: subscription_ended ",
               Predicate (\l -> "1: supporter, 0 months left" `isPrefixOf` l && ", subscription ended" `isInfixOf` l),
               StartsWith "badge alert: subscription_ended "
             ]
      waitShownBadge (chatController alice) Nothing
      ledgerTags (chatController alice) `shouldReturn` [Just "charge", Just "badge", Just "refund"]
      subscriptionStatuses (chatController alice) `shouldReturn` [Just "ended"]
      issueErrorRow (chatController alice) `shouldReturn` (Nothing, Nothing)
      subscriptionChecks store `shouldReturn` checks
      now <- serviceNow env
      let resubscribedToken = "fake-play-token-resubscribed.AO-J1Or2p4bgV3sPn6"
      setSubscription store (SubscriptionGoogle resubscribedToken) $ subscriptionCheck SSRenewing $ Just (storeCharge "subscr-badge-supporter-month-02" (addUTCTime (-3600) now) 1) {chargeRef = "GPA.resubscribed"}
      alice ##> ("/_badge purchase 1 " <> paymentArg (googlePayment "subscr_badge_supporter_01" resubscribedToken))
      inAnyOrder alice [["1: supporter", "badge alert: subscription_ended ", openPurchase ""], ["2: supporter"]]
      waitShownBadge (chatController alice) (Just 2)
      alice ##> "/_badge state 1"
      alice <##. "2: supporter"

testAppleRevokedWhileCovered :: HasCallStack => TestParams -> IO ()
testAppleRevokedWhileCovered ps =
  -- Apple answers a refunded subscription as revoked, which its verifier maps to ended
  endedInSecondMonth ps appleStoreReports (\FakeStore {appleSubscriptionJWS} -> SPApple {jws = appleSubscriptionJWS}) "SUBSCR_BADGE_SUPPORTER_YEAR_01" $ \charge ->
    subscriptionCheck SSEnded $ Just charge {revoked = True}

testPlayExpiredWhileCovered :: HasCallStack => TestParams -> IO ()
testPlayExpiredWhileCovered ps =
  endedInSecondMonth ps storeReports (const subscriptionPlay) "subscr-badge-supporter-year-01" $ subscriptionCheck SSEnded . Just

-- | An annual subscription the client bought, whose store reports at the second month's request what endedWith makes of its charge.
endedInSecondMonth :: HasCallStack => TestParams -> (FakeStore -> SubscriptionCheck -> IO ()) -> (FakeStore -> ServicePayment) -> Text -> (StoreCharge -> SubscriptionCheck) -> IO ()
endedInSecondMonth ps reports paymentOf planId endedWith =
  withBadgeServiceEnv ps $ \env@BadgeServiceEnv {bsClock, bsClientCfg, bsController = cc, bsStore = store} ->
    withNewTestChatCfg ps bsClientCfg "alice" aliceProfile $ \alice -> do
      firstStart <- addUTCTime (-3600) <$> serviceNow env
      let charge = storeCharge planId firstStart 12
      reports store $ subscriptionCheck SSRenewing $ Just charge
      alice ##> ("/_badge purchase 1 " <> paymentArg (paymentOf store))
      inAnyOrder alice [[openPurchase ""], ["1: supporter"]]
      waitWakeArmed (chatController alice)
      rows <- ledgerRows (chatController alice) "badge_ledger"
      reports store $ endedWith charge
      checks <- subscriptionChecks store
      setClockAt bsClock $ fst $ renewalMoments rows
      alice `send` "/_app activate"
      alice
        <### [ "/_app activate",
               "ok",
               StartsWith "badge alert: subscription_ended ",
               Predicate (\l -> "1: supporter, 0 months left" `isPrefixOf` l && ", subscription ended" `isInfixOf` l),
               StartsWith "badge alert: subscription_ended "
             ]
      waitShownBadge (chatController alice) Nothing
      subscriptionChecks store `shouldReturn` checks + 1
      ledgerTags (chatController alice) `shouldReturn` [Just "charge", Just "badge", Just "refund"]
      map (\(_, ch, m, _, _, t) -> (ch, m, t)) <$> ledgerRows cc "sx_badge_service_badge_ledger" `shouldReturn` [(12, 12, Just "charge"), (-1, 11, Just "badge"), (-11, 0, Just "refund")]
      subscriptionStatuses (chatController alice) `shouldReturn` [Just "ended"]
      issueErrorRow (chatController alice) `shouldReturn` (Nothing, Nothing)

testRevokedChargeRefunded :: HasCallStack => TestParams -> IO ()
testRevokedChargeRefunded ps =
  withBadgeServiceEnv ps $ \env@BadgeServiceEnv {bsClock, bsStore = store} -> do
    (purchaseKey, firstStart, purchased) <- subscribedTo env "subscr-badge-supporter-year-01" 12
    let granted = storeCharge "subscr-badge-supporter-year-01" firstStart 12
        changes = map (\e -> let (c, m, _) = entryOf e in (c, m)) . fst . statementOf
    storeReports store $ subscriptionCheck SSRenewing $ Just granted {chargeRef = "GPA.never-granted", revoked = True}
    setClockAt bsClock $ addUTCTime 600 (addMonths 1 firstStart)
    second <- assertBalance env purchaseKey (last purchased)
    map entryTag (fst $ statementOf second) `shouldBe` ["badge"]
    credentialOf second `shouldSatisfy` isJust
    storeReports store $ subscriptionCheck SSRenewing $ Just granted {revoked = True}
    setClockAt bsClock $ addUTCTime 600 (addMonths 2 firstStart)
    refunded <- assertBalance env purchaseKey (last $ fst $ statementOf second)
    map entryTag (fst $ statementOf refunded) `shouldBe` ["refund"]
    changes refunded `shouldBe` [(-10, 0)]
    (credentialOf refunded, subscriptionOf refunded) `shouldBe` (Nothing, Just SSEnded)
    -- what follows within the hour is answered ended from the record
    checks <- subscriptionChecks store
    again <- assertBalance env purchaseKey (last $ fst $ statementOf refunded)
    (credentialOf again, subscriptionOf again) `shouldBe` (Nothing, Just SSEnded)
    subscriptionChecks store `shouldReturn` checks

testAnnualAsksBeforeEachMonth :: HasCallStack => TestParams -> IO ()
testAnnualAsksBeforeEachMonth ps =
  withBadgeServiceEnv ps $ \env@BadgeServiceEnv {bsClock, bsController = cc, bsStore = store} -> do
    (purchaseKey, firstStart, purchased) <- subscribedTo env "subscr-badge-supporter-year-01" 12
    let issue = BSCIssueBadge {balance = BadgeBalance {lastEntry = last purchased}}
        routedAt t = setClockAt bsClock t >> requestLane ChecksAfterCoverage cc (Just purchaseKey) (requestObject purchaseKey issue)
    checks <- subscriptionChecks store
    -- inside the issued month no month is due, so the store is not asked
    routedAt (addUTCTime (10 * nominalDay) firstStart) `shouldReturn` MainLane
    credentialOf <$> assertBalance env purchaseKey (last purchased) >>= (`shouldSatisfy` isJust)
    subscriptionChecks store `shouldReturn` checks
    routedAt (addUTCTime 600 (addMonths 1 firstStart)) `shouldReturn` RenewalLane
    second <- assertBalance env purchaseKey (last purchased)
    map entryTag (fst $ statementOf second) `shouldBe` ["badge"]
    (isJust $ credentialOf second, subscriptionOf second) `shouldBe` (True, Just SSRenewing)
    subscriptionChecks store `shouldReturn` checks + 1
    repeated <- assertBalance env purchaseKey (last purchased)
    credentialOf repeated `shouldBe` credentialOf second
    subscriptionChecks store `shouldReturn` checks + 1

testAnnualIssuesWhenStoreFails :: HasCallStack => TestParams -> IO ()
testAnnualIssuesWhenStoreFails ps =
  withBadgeServiceEnv ps $ \env@BadgeServiceEnv {bsClock, bsStore = store} -> do
    (purchaseKey, firstStart, purchased) <- subscribedTo env "subscr-badge-supporter-year-01" 12
    setGoogleDown store True
    checks <- subscriptionChecks store
    setClockAt bsClock $ addUTCTime 600 (addMonths 1 firstStart)
    second <- assertBalance env purchaseKey (last purchased)
    map entryTag (fst $ statementOf second) `shouldBe` ["badge"]
    (isJust $ credentialOf second, subscriptionOf second) `shouldBe` (True, Just SSRenewing)
    subscriptionChecks store `shouldReturn` checks + 1
    setGoogleDown store False
    setClockAt bsClock $ addUTCTime 600 (addMonths 2 firstStart)
    third <- assertBalance env purchaseKey (last $ fst $ statementOf second)
    map entryTag (fst $ statementOf third) `shouldBe` ["badge"]
    subscriptionChecks store `shouldReturn` checks + 2

testDeviceEndedRetires :: HasCallStack => TestParams -> IO ()
testDeviceEndedRetires ps =
  withBadgeServiceEnv ps $ \env@BadgeServiceEnv {bsClock, bsClientCfg, bsStore = store} ->
    withNewTestChatCfg ps bsClientCfg "alice" aliceProfile $ \alice -> do
      (_, rows) <- clientSubscribedMonthly env alice
      checks <- subscriptionChecks store
      let ended = Predicate (\l -> "1: supporter, 0 months left" `isPrefixOf` l && ", subscription ended" `isInfixOf` l)
      alice ##> ("/_badge subscription ended " <> paymentArg subscriptionPlay)
      alice <### ["ok", ended]
      threadDelay 2000000
      shownBadgeId (chatController alice) >>= (`shouldSatisfy` isJust)
      subscriptionStatuses (chatController alice) `shouldReturn` [Just "ended"]
      -- past paidThrough, and before the shown credential's request moment, so nothing is asked
      let endsAt = dueAtOf rows
          (requestAt, _) = renewalMoments rows
      setClockAt bsClock $ addUTCTime (diffUTCTime requestAt endsAt / 2) endsAt
      alice `send` "/_app activate"
      alice <### ["/_app activate", "ok", StartsWith "badge alert: subscription_ended ", ended, StartsWith "badge alert: subscription_ended "]
      waitShownBadge (chatController alice) Nothing
      subscriptionChecks store `shouldReturn` checks
      invoiceId <- createInvoice alice 1
      alice ##> purchaseWithInvoice 1 invoiceId supporterPlay
      inAnyOrder alice [["1: supporter", "badge alert: subscription_ended ", invoicePurchase "" invoiceId], creditedLines "" invoiceId "2: supporter"]
      waitShownBadge (chatController alice) (Just 2)

testRequestBadgeNow :: HasCallStack => TestParams -> IO ()
testRequestBadgeNow ps =
  withBadgeServiceEnv ps $ \env@BadgeServiceEnv {bsClientCfg, bsController = ChatController {chatStore}, bsStore = store} -> do
    withNewTestChatCfg ps bsClientCfg {badgeDevRequests = True} "alice" aliceProfile $ \alice -> do
      (_, rows) <- clientSubscribedMonthly env alice
      let cc = chatController alice
          ended = Predicate (\l -> "1: supporter, 0 months left" `isPrefixOf` l && ", subscription ended" `isInfixOf` l)
      shown <- shownBadgeId cc
      checks <- subscriptionChecks store
      -- while covered the service answers with the credential already shown
      alice ##> "/_badge request 1"
      alice <##. "1: supporter"
      ledgerRows cc "badge_ledger" `shouldReturn` rows
      shownBadgeId cc `shouldReturn` shown
      issueErrorRow cc `shouldReturn` (Nothing, Nothing)
      subscriptionChecks store `shouldReturn` checks
      -- stands in for a store check that found it ended: only this request can carry it to the client now
      withTransaction chatStore $ \db -> DB.execute_ db "UPDATE sx_badge_service_payments SET subscription_state = 'ended'"
      alice ##> "/_badge request 1"
      alice <### [ended]
      waitShownBadge cc Nothing
      subscriptionStatuses cc `shouldReturn` [Just "ended"]
      subscriptionChecks store `shouldReturn` checks
      alice ##> "/_badge request 1"
      alice <## "bad chat command: no badge is shown"
    withNewTestChatCfg ps bsClientCfg "bob" bobProfile $ \bob -> do
      bob ##> "/_badge request 1"
      bob <## "bad chat command: /_badge request is only available in builds for a dev badge service"

testFreshAnswerOverridesDeviceEnded :: HasCallStack => TestParams -> IO ()
testFreshAnswerOverridesDeviceEnded ps =
  withBadgeServiceEnv ps $ \env@BadgeServiceEnv {bsClock, bsClientCfg, bsStore = store} ->
    withNewTestChatCfg ps bsClientCfg "alice" aliceProfile $ \alice -> do
      (firstStart, rows) <- clientSubscribedMonthly env alice
      storeReports store $ subscriptionCheck SSRenewing $ Just $ monthlyCharge "subscr-badge-supporter-month-02" firstStart 1
      setClockAt bsClock $ fst $ renewalMoments rows
      let stateLine st = Predicate (\l -> "1: supporter" `isPrefixOf` l && (", subscription " <> st) `isInfixOf` l)
      alice ##> ("/_badge subscription ended " <> paymentArg subscriptionPlay)
      alice <### ["ok", stateLine "ended", stateLine "renewing"]
      _ <- waitLedgerRows (chatController alice) 4
      subscriptionStatuses (chatController alice) `shouldEventuallyReturn` [Just "renewing"]
      shownBadgeId (chatController alice) >>= (`shouldSatisfy` isJust)

-- | Read from the API response, as the apps read it: the terminal view does not show it.
badgeSubscriptionRef :: HasCallStack => ChatController -> IO (Maybe Text)
badgeSubscriptionRef cc =
  sendChatCmdStr cc "/_badge state 1" >>= \case
    Right (CRBadgeState _ (Just BT.BadgeState {BT.subscription = Just BT.BadgeSubscription {BT.transactionRef = ref_}}) _) -> pure ref_
    r -> error $ "expected a subscription's badge state, got " <> show (() <$ r)

subscriptionStatuses :: ChatController -> IO [Maybe Text]
subscriptionStatuses ChatController {chatStore} =
  withTransaction chatStore $ \db -> map fromOnly <$> DB.query_ db "SELECT subscription_status FROM badge_purchases"

unrenewedRuns :: ChatController -> IO [(Maybe UTCTime, Maybe UTCTime)]
unrenewedRuns ChatController {chatStore} =
  withTransaction chatStore $ \db -> DB.query_ db "SELECT unrenewed_since, last_unrenewed_at FROM badge_purchases"

-- | The run once the pass answered at t has stored it, the pass's clock having run on from t.
answeredRun :: HasCallStack => ChatController -> UTCTime -> IO (UTCTime, UTCTime)
answeredRun cc t = loop (600 :: Int)
  where
    loop i =
      unrenewedRuns cc >>= \case
        [(Just since, Just lastAt)] | diffUTCTime lastAt t >= -1 && diffUTCTime lastAt t < 30 -> pure (since, lastAt)
        runs
          | i == 0 -> error $ "expected a run answered at " <> show t <> ", got " <> show runs
          | otherwise -> threadDelay 50000 >> loop (i - 1)

-- | An activation whose request is answered: the answer's lines and the badge's, its alerts among them.
activateAnswered :: HasCallStack => TestCC -> [String] -> IO ()
activateAnswered cc answer = do
  cc `send` "/_app activate"
  expectLinesWithin 30 cc (["/_app activate", "ok"] <> answer <> ["1: supporter"]) ["1: supporter", "renewal failing since ", "badge alert: "]

-- | The first request after the coverage ends, answered without renewing, which starts a run: its time.
firstUnrenewedAnswer :: HasCallStack => TestCC -> TestClock -> [ReplicatedRow] -> [String] -> IO UTCTime
firstUnrenewedAnswer cc clock rows answer = do
  -- the request moment can come before the coverage ends, and an answer while covered starts no run
  let answerAt = max (fst $ renewalMoments rows) (addUTCTime 3600 $ dueAtOf rows)
  setClockAt clock answerAt
  activateAnswered cc answer
  (since, lastAt) <- answeredRun (chatController cc) answerAt
  lastAt `shouldBe` since
  pure since

testSubscriptionStateKept :: HasCallStack => TestParams -> IO ()
testSubscriptionStateKept ps =
  withBadgeServiceEnv ps $ \env@BadgeServiceEnv {bsClock, bsClientCfg, bsStore = store} ->
    withNewTestChatCfg ps bsClientCfg "alice" aliceProfile $ \alice -> do
      (_, rows) <- clientSubscribedMonthly env alice
      subscriptionStatuses (chatController alice) `shouldReturn` [Just "renewing"]
      badgeSubscriptionRef (chatController alice) `shouldReturn` Just (googlePurchaseRef googleSubscriptionToken)
      alice ##> "/_badge state 1"
      alice <### [Predicate (\l -> "1: supporter, 0 months left" `isPrefixOf` l && ", subscription renewing" `isInfixOf` l)]
      -- the request asserts coverage that has ended, so the service asks the store for its answer
      storeReports store $ subscriptionCheck SSBillingRetry Nothing
      setClockAt bsClock $ fst $ renewalMoments rows
      alice `send` "/_app activate"
      alice <### ["/_app activate", "ok", Predicate (", subscription billingRetry" `isInfixOf`), paymentIssue, paymentIssue]
      subscriptionStatuses (chatController alice) `shouldReturn` [Just "billingRetry"]

nextWakes :: ChatController -> IO [Maybe UTCTime]
nextWakes ChatController {chatStore} =
  withTransaction chatStore $ \db -> map fromOnly <$> DB.query_ db "SELECT next_wake_at FROM badge_purchases"

-- | A monthly plan is not reminded of its renewal; a cancellation is alerted until acknowledged.
testSubscriptionAlerts :: HasCallStack => TestParams -> IO ()
testSubscriptionAlerts ps =
  withBadgeServiceEnv ps $ \env@BadgeServiceEnv {bsClock, bsClientCfg} ->
    withNewTestChatCfg ps bsClientCfg "alice" aliceProfile $ \alice -> do
      (firstStart, _) <- clientSubscribedMonthly env alice
      let paidThrough = addMonths 1 firstStart
          episode = T.unpack $ safeDecodeUtf8 $ strEncode paidThrough
      armWakeAt alice bsClock $ addUTCTime (-3 * nominalDay) paidThrough
      nextWakes (chatController alice) >>= (`shouldSatisfy` all (maybe False (>= paidThrough)))
      threadDelay 2000000
      (alice </)
      alice ##> ("/_badge subscription cancelled " <> paymentArg subscriptionPlay)
      alice <### ["ok", StartsWith "1: supporter", subscriptionCancelled, subscriptionCancelled]
      alice ##> ("/_badge ack 1 1 subscription_cancelled off " <> episode)
      alice <##. "1: supporter"
      ackedEpisode (chatController alice) `shouldReturn` (Just "subscription_cancelled", Just (T.pack episode))
      alice ##> "/_badge state 1"
      alice <##. "1: supporter"
      (alice </)

-- | An annual plan is reminded three days before it renews, by a wake of its own.
testRenewalReminder :: HasCallStack => TestParams -> IO ()
testRenewalReminder ps =
  withBadgeServiceEnv ps $ \env@BadgeServiceEnv {bsClock, bsClientCfg, bsStore = store@FakeStore {appleSubscriptionJWS}} ->
    withNewTestChatCfg ps bsClientCfg "alice" aliceProfile $ \alice -> do
      firstStart <- addUTCTime (-3600) <$> serviceNow env
      setSubscription store (SubscriptionApple appleSubscriptionOriginalId) $ subscriptionCheck SSRenewing $ Just $ storeCharge "SUBSCR_BADGE_SUPPORTER_YEAR_01" firstStart 12
      alice ##> ("/_badge purchase 1 " <> paymentArg SPApple {jws = appleSubscriptionJWS})
      inAnyOrder alice [[openPurchase ""], ["1: supporter"]]
      waitWakeArmed (chatController alice)
      let paidThrough = addMonths 12 firstStart
          remindAt = addUTCTime (-3 * nominalDay) paidThrough
          episode = T.unpack $ safeDecodeUtf8 $ strEncode paidThrough
      -- with the last month issued, the reminder is the next thing due
      setClockAt bsClock $ addUTCTime (-20 * nominalDay) paidThrough
      alice ##> "/_app activate"
      alice <## "ok"
      alice <##. "1: supporter, 0 months left"
      waitWakeFrom (chatController alice) remindAt
      nextWakes (chatController alice) `shouldReturn` [Just remindAt]
      armWakeAt alice bsClock remindAt
      alice <##. "badge alert: renewal_approaching "
      alice ##> ("/_badge ack 1 1 renewal_approaching off " <> episode)
      alice <##. "1: supporter"
      alice ##> "/_badge state 1"
      alice <##. "1: supporter"
      (alice </)

paymentIssue :: ConsoleResponse
paymentIssue = StartsWith "badge alert: payment_issue "

subscriptionCancelled :: ConsoleResponse
subscriptionCancelled = StartsWith "badge alert: subscription_cancelled "

-- | The service answers a request made within the hour of its last check from the state it stored, which a client
-- does not take while coverage runs.
testAnnualStateNotFromStoredAnswer :: HasCallStack => TestParams -> IO ()
testAnnualStateNotFromStoredAnswer ps =
  withBadgeServiceEnv ps $ \env@BadgeServiceEnv {bsClock, bsClientCfg, bsController = cc@ChatController {chatStore}, bsStore = store} ->
    withNewTestChatCfg ps bsClientCfg "alice" aliceProfile $ \alice -> do
      firstStart <- addUTCTime (-3600) <$> serviceNow env
      storeReports store $ subscriptionCheck SSCancelled $ Just $ storeCharge "subscr-badge-supporter-year-01" firstStart 12
      alice ##> ("/_badge purchase 1 " <> paymentArg subscriptionPlay)
      inAnyOrder alice [[openPurchase ""], ["1: supporter", "badge alert: subscription_cancelled "], ["badge alert: subscription_cancelled "]]
      waitWakeArmed (chatController alice)
      rows <- ledgerRows (chatController alice) "badge_ledger"
      map (\(_, ch, m, _, _, t) -> (ch, m, t)) rows `shouldBe` [(12, 12, Just "charge"), (-1, 11, Just "badge")]
      subscriptionStatuses (chatController alice) `shouldReturn` [Just "cancelled"]
      let requestAt = fst $ renewalMoments rows
      withTransaction chatStore $ \db -> DB.execute db "UPDATE sx_badge_service_payments SET subscription_state = 'renewing', subscription_checked_at = ?" (Only requestAt)
      checks <- subscriptionChecks store
      setClockAt bsClock requestAt
      alice ##> "/_app activate"
      alice <## "ok"
      _ <- waitLedgerRows (chatController alice) 3
      alice <### [Predicate (\l -> "1: supporter, 10 months left" `isPrefixOf` l && ", subscription cancelled" `isInfixOf` l), subscriptionCancelled]
      subscriptionChecks store `shouldReturn` checks
      subscriptionPayments cc `shouldReturn` [("google", googlePurchaseRef googleSubscriptionToken, Just "renewing", Just googleSubscriptionToken)]
      subscriptionStatuses (chatController alice) `shouldReturn` [Just "cancelled"]

testSubscriptionStateFromStore :: HasCallStack => TestParams -> IO ()
testSubscriptionStateFromStore ps =
  withBadgeServiceEnv ps $ \env@BadgeServiceEnv {bsClock, bsClientCfg, bsStore = store} ->
    withNewTestChatCfg ps bsClientCfg "alice" aliceProfile $ \alice -> do
      (_, rows) <- clientSubscribedMonthly env alice
      let report state payment = alice ##> ("/_badge subscription " <> state <> " " <> paymentArg payment)
      report "cancelled" subscriptionPlay
      alice <### ["ok", Predicate (", subscription cancelled" `isInfixOf`), subscriptionCancelled, subscriptionCancelled]
      subscriptionStatuses (chatController alice) `shouldReturn` [Just "cancelled"]
      report "cancelled" subscriptionPlay
      alice <## "ok"
      -- Play lists a subscription in its grace period as renewing, which does not replace the service's billing retry
      storeReports store $ subscriptionCheck SSBillingRetry Nothing
      setClockAt bsClock $ fst $ renewalMoments rows
      alice `send` "/_app activate"
      alice <### ["/_app activate", "ok", Predicate (", subscription billingRetry" `isInfixOf`), paymentIssue, paymentIssue]
      checks <- subscriptionChecks store
      report "renewing" subscriptionPlay
      alice <## "ok"
      subscriptionStatuses (chatController alice) `shouldReturn` [Just "billingRetry"]
      -- a held transaction, here a one-time purchase handed over while the badge is shown, and an unknown one change nothing
      alice ##> ("/_badge purchase 1 " <> paymentArg supporterPlay)
      alice <##. "1: supporter"
      alice <##. "badge alert: payment_issue "
      alice <##. openPurchase ""
      report "cancelled" supporterPlay
      alice <## "ok"
      report "cancelled" $ googlePayment "subscr_badge_supporter_01" "fake-play-token-unknown.AO-J1Os2q5djX0pRm6"
      alice <## "ok"
      subscriptionStatuses (chatController alice) `shouldReturn` [Just "billingRetry"]
      report "cancelled" SPApple {jws = "not.a-jws"}
      alice <## "cannot get badge: invalid store receipt"
      report "paused" subscriptionPlay
      alice <##. "bad chat command: "
      subscriptionStatuses (chatController alice) `shouldReturn` [Just "billingRetry"]
      subscriptionChecks store `shouldReturn` checks

-- | The held record's payment and when it is next attempted.
heldStoreReceiptAttempt :: ChatController -> IO [(Text, UTCTime)]
heldStoreReceiptAttempt ChatController {chatStore} =
  withTransaction chatStore $ \db -> DB.query_ db "SELECT payment, next_attempt_at FROM badge_store_receipts WHERE payment IS NOT NULL"

testAppleAdoptionRules :: HasCallStack => TestParams -> IO ()
testAppleAdoptionRules ps =
  withBadgeServiceEnv ps $ \env@BadgeServiceEnv {bsClientCfg, bsStore = store@FakeStore {appleSubscriptionJWS, appleRevokedSubscriptionJWS}} ->
    withNewTestChatCfg ps bsClientCfg "alice" aliceProfile $ \alice -> do
      now <- serviceNow env
      setSubscription store (SubscriptionApple appleSubscriptionOriginalId) $ subscriptionCheck SSRenewing $ Just $ storeCharge "SUBSCR_BADGE_SUPPORTER_MONTH_01" (addUTCTime 3600 now) 1
      let handOver jws = do
            alice ##> ("/_badge purchase 1 " <> paymentArg SPApple {jws})
            storePurchaseOpen alice ""
      handOver appleSubscriptionJWS
      heldStoreReceiptErrors (chatController alice) `shouldEventuallyReturn` ["service_error retry payment_pending"]
      held <- heldStoreReceiptAttempt (chatController alice)
      checks <- subscriptionChecks store
      let resigned = T.intercalate "." $ take 2 (T.splitOn "." appleSubscriptionJWS) <> ["c2lnbmVkIGFnYWlu"]
          earlier = subscriptionJWS appleSubscriptionOriginalId "2000000812345685" ["purchaseDate" J..= (1764547200000 :: Int64)]
          undated = subscriptionJWS appleSubscriptionOriginalId "2000000812345686" []
      forM_ [appleSubscriptionJWS, resigned, earlier, undated, appleRevokedSubscriptionJWS] $ \jws -> do
        handOver jws
        heldStoreReceiptAttempt (chatController alice) `shouldReturn` held
      subscriptionChecks store `shouldReturn` checks

testAdoptedReceiptNotOverwritten :: HasCallStack => TestParams -> IO ()
testAdoptedReceiptNotOverwritten ps = do
  entered <- newEmptyMVar
  release <- newEmptyMVar
  hook <- newIORef (pure ())
  withBadgeServiceVerifier ps (subscriptionCheckWithHook hook) $ \env@BadgeServiceEnv {bsClientCfg, bsClock, bsStore = store@FakeStore {appleSubscriptionJWS, appleRenewalSubscriptionJWS}} -> do
    now <- serviceNow env
    checks <- subscriptionChecks store
    withNewTestChatCfg ps bsClientCfg "alice" aliceProfile $ \alice -> do
      -- the first check waits for the renewal's hand-over and finds no subscription; the next finds it charged
      writeIORef hook $ do
        putMVar entered ()
        takeMVar release
        writeIORef hook $ setSubscription store (SubscriptionApple appleSubscriptionOriginalId) $ subscriptionCheck SSRenewing $ Just $ storeCharge "SUBSCR_BADGE_SUPPORTER_MONTH_01" (addUTCTime (-3600) now) 1
      let handOver jws = do
            alice ##> ("/_badge purchase 1 " <> paymentArg SPApple {jws})
            storePurchaseOpen alice ""
      handOver appleSubscriptionJWS
      timeout 30000000 (takeMVar entered) `shouldReturn` Just ()
      handOver appleRenewalSubscriptionJWS
      putMVar release ()
      -- the failed attempt sent the first transaction, so the renewal adopted meanwhile is presented at once,
      -- and held off for the rest of the hour since the subscription was asked about
      heldStoreReceiptErrors (chatController alice) `shouldEventuallyReturn` ["service_error retry payment_pending"]
      subscriptionChecks store `shouldReturn` checks + 1
    setClockAt bsClock $ addUTCTime 3700 now
    withTestChatCfg ps bsClientCfg "alice" $ \alice -> do
      expectLinesWithin 60 alice ["1: supporter"] ["subscribed 1 connections on server localhost"]
      storeReceiptRows (chatController alice) `shouldReturn` [(1, Nothing, True)]

testAppleRenewalResolvesToPurchase :: HasCallStack => TestParams -> IO ()
testAppleRenewalResolvesToPurchase ps =
  withBadgeServiceEnv ps $ \env@BadgeServiceEnv {bsClientCfg, bsClock, bsController = cc, bsStore = store@FakeStore {appleSubscriptionJWS, appleRenewalSubscriptionJWS}} -> do
    now <- serviceNow env
    let appleReports start = setSubscription store (SubscriptionApple appleSubscriptionOriginalId) $ subscriptionCheck SSRenewing $ Just $ storeCharge "SUBSCR_BADGE_SUPPORTER_MONTH_01" start 1
    withNewTestChatCfg ps bsClientCfg "alice" aliceProfile $ \alice -> do
      let handOver jws = alice ##> ("/_badge purchase 1 " <> paymentArg SPApple {jws})
      -- the first attempt finds a period that has not started, and the record waits
      appleReports $ addUTCTime 3600 now
      handOver appleSubscriptionJWS
      storePurchaseOpen alice ""
      heldStoreReceiptErrors (chatController alice) `shouldEventuallyReturn` ["service_error retry payment_pending"]
      checks <- subscriptionChecks store
      -- a later renewal joins that record, which presents it at once, inside the hour since the subscription was asked about;
      -- joining clears the record's error, so the error read again is the answer to that presentation
      appleReports $ addUTCTime (-3600) now
      handOver appleRenewalSubscriptionJWS
      storePurchaseOpen alice ""
      heldStoreReceiptErrors (chatController alice) `shouldEventuallyReturn` ["service_error retry payment_pending"]
      subscriptionChecks store `shouldReturn` checks
    setClockAt bsClock $ addUTCTime 3700 now
    withTestChatCfg ps bsClientCfg "alice" $ \alice -> do
      expectLinesWithin 60 alice ["1: supporter"] ["subscribed 1 connections on server localhost"]
      storeReceiptRows (chatController alice) `shouldReturn` [(1, Nothing, True)]
      rowCount cc "sx_badge_service_badge_purchases" `shouldReturn` 1
      -- the first transaction handed over again resolves to the credited record, and creates nothing
      alice ##> ("/_badge purchase 1 " <> paymentArg SPApple {jws = appleSubscriptionJWS})
      alice <## "badge already redeemed"
      storeReceiptRows (chatController alice) `shouldReturn` [(1, Nothing, True)]
      rowCount cc "sx_badge_service_badge_purchases" `shouldReturn` 1
      map (\(_, ch, m, _, _, t) -> (ch, m, t)) <$> ledgerRows (chatController alice) "badge_ledger" `shouldReturn` [(1, 1, Just "charge"), (-1, 0, Just "badge")]

testAppleResubscribeResumes :: HasCallStack => TestParams -> IO ()
testAppleResubscribeResumes ps =
  -- retired past its coverage on the service's answer that it ended, then charged again for the same tier
  withBadgeServiceEnv ps $ \env@BadgeServiceEnv {bsClock, bsClientCfg, bsStore = store} ->
    withNewTestChatCfg ps bsClientCfg "alice" aliceProfile $ \alice -> do
      requestAt <- appleSubscriptionRetired env alice
      -- past the hour in which the service answers from the check that found it ended
      let resubscribedAt = addUTCTime 7200 requestAt
      setClockAt bsClock resubscribedAt
      appleStoreReports store $ subscriptionCheck SSRenewing $ Just (storeCharge "SUBSCR_BADGE_SUPPORTER_MONTH_01" resubscribedAt 1) {chargeRef = "apple.resubscribed"}
      alice ##> ("/_badge purchase 1 " <> paymentArg SPApple {jws = appleResubscription "2000000812345690" resubscribedAt})
      expectLinesWithin 30 alice ["1: supporter", openPurchase "", "1: supporter"] ["badge alert: subscription_ended ", "subscribed "]
      waitShownBadge (chatController alice) (Just 1)
      heldStoreReceipts (chatController alice) `shouldReturn` 0
      storeReceiptCreditErrors (chatController alice) `shouldReturn` []
      storeReceiptRows (chatController alice) `shouldReturn` [(1, Nothing, True)]
      map (\(_, _, _, _, _, t) -> t) <$> ledgerRows (chatController alice) "badge_ledger" `shouldReturn` [Just "charge", Just "badge", Just "charge", Just "badge"]
      [retiredExpiry, resumedExpiry] <- issuedExpiries (chatController alice)
      resumedExpiry `shouldSatisfy` (> retiredExpiry)
      waitShownIssued (chatController alice)
      subscriptionStatuses (chatController alice) `shouldReturn` [Just "renewing"]

testAppleResubscribeResumesWithinCoverage :: HasCallStack => TestParams -> IO ()
testAppleResubscribeResumesWithinCoverage ps =
  -- retired within its coverage, its months not issued refunded, when the store reported it ended
  withBadgeServiceEnv ps $ \env@BadgeServiceEnv {bsClock, bsClientCfg, bsStore = store@FakeStore {appleSubscriptionJWS}} ->
    withNewTestChatCfg ps bsClientCfg "alice" aliceProfile $ \alice -> do
      firstStart <- addUTCTime (-3600) <$> serviceNow env
      let yearly = storeCharge "SUBSCR_BADGE_SUPPORTER_YEAR_01" firstStart 12
      appleStoreReports store $ subscriptionCheck SSRenewing $ Just yearly
      alice ##> ("/_badge purchase 1 " <> paymentArg SPApple {jws = appleSubscriptionJWS})
      inAnyOrder alice [[openPurchase ""], ["1: supporter"]]
      waitWakeArmed (chatController alice)
      rows <- ledgerRows (chatController alice) "badge_ledger"
      appleStoreReports store $ subscriptionCheck SSEnded $ Just yearly {revoked = True}
      let requestAt = fst $ renewalMoments rows
      setClockAt bsClock requestAt
      alice `send` "/_app activate"
      alice <### ["/_app activate", "ok", StartsWith "badge alert: subscription_ended ", Predicate (\l -> "1: supporter" `isPrefixOf` l && ", subscription ended" `isInfixOf` l), StartsWith "badge alert: subscription_ended "]
      waitShownBadge (chatController alice) Nothing
      -- past the hour in which the service answers from the check that found it ended
      let resubscribedAt = addUTCTime 7200 requestAt
      setClockAt bsClock resubscribedAt
      appleStoreReports store $ subscriptionCheck SSRenewing $ Just (storeCharge "SUBSCR_BADGE_SUPPORTER_MONTH_01" resubscribedAt 1) {chargeRef = "apple.resubscribed"}
      alice ##> ("/_badge purchase 1 " <> paymentArg SPApple {jws = appleResubscription "2000000812345691" resubscribedAt})
      expectLinesWithin 30 alice ["1: supporter", openPurchase "", "1: supporter"] ["badge alert: subscription_ended ", "subscribed "]
      waitShownBadge (chatController alice) (Just 1)
      ledgerTags (chatController alice) `shouldReturn` [Just "charge", Just "badge", Just "refund", Just "charge", Just "badge"]
      subscriptionStatuses (chatController alice) `shouldReturn` [Just "renewing"]
      alice ##> "/_app activate"
      alice <## "ok"
      threadDelay 2000000
      shownBadgeId (chatController alice) `shouldReturn` Just 1

testPlaySubscriptionResumes :: HasCallStack => TestParams -> IO ()
testPlaySubscriptionResumes ps =
  -- retired once the recovery window passed while Play retried the payment, then charged again on the same token
  withBadgeServiceEnv ps $ \env@BadgeServiceEnv {bsClock, bsClientCfg, bsStore = store} ->
    withNewTestChatCfg ps bsClientCfg "alice" aliceProfile $ \alice -> do
      playSubscriptionRetired env alice
      -- past the hour in which the service answers from the check that retired it
      resumedAt <- addUTCTime 7200 <$> serviceNow env
      setClockAt bsClock resumedAt
      storeReports store $ subscriptionCheck SSRenewing $ Just (storeCharge "subscr-badge-supporter-month-02" (addUTCTime (-3600) resumedAt) 1) {chargeRef = "GPA.resumed"}
      alice ##> ("/_badge purchase 1 " <> paymentArg subscriptionPlay)
      expectLinesWithin 30 alice ["1: supporter", openPurchase "", "1: supporter"] ["badge alert: ", "renewal failing since ", "subscribed "]
      waitShownBadge (chatController alice) (Just 1)
      heldStoreReceipts (chatController alice) `shouldReturn` 0
      storeReceiptCreditErrors (chatController alice) `shouldReturn` []
      ledgerTags (chatController alice) `shouldReturn` [Just "charge", Just "badge", Just "charge", Just "badge"]

-- | A monthly Play subscription the client bought, retired once the recovery window passed while Play retried the payment.
playSubscriptionRetired :: HasCallStack => BadgeServiceEnv -> TestCC -> IO ()
playSubscriptionRetired env@BadgeServiceEnv {bsClock, bsStore = store} alice = do
  (_, rows) <- clientSubscribedMonthly env alice
  storeReports store $ subscriptionCheck SSBillingRetry Nothing
  t0 <- firstUnrenewedAnswer alice bsClock rows []
  -- an answer within the window keeps the run going, so the one past it retires the badge
  let beforeEnd = addUTCTime (subscriptionRecoveryWindow - 3600) t0
  activateAt alice bsClock beforeEnd
  fst <$> answeredRun (chatController alice) beforeEnd `shouldReturn` t0
  [Just wake] <- nextWakes (chatController alice)
  setClockAt bsClock $ max wake $ addUTCTime (subscriptionRecoveryWindow + 3600) t0
  activateAnswered alice ["badge alert: subscription_ended "]
  waitShownBadge (chatController alice) Nothing

testPlayResumeAttemptsEnd :: HasCallStack => TestParams -> IO ()
testPlayResumeAttemptsEnd ps =
  withBadgeServiceEnv ps $ \env@BadgeServiceEnv {bsClock, bsClientCfg, bsStore = store} ->
    withNewTestChatCfg ps bsClientCfg "alice" aliceProfile $ \alice -> do
      playSubscriptionRetired env alice
      let a = chatController alice
          handOver = alice `send` ("/_badge purchase 1 " <> paymentArg subscriptionPlay)
      -- Play still retries the payment, so each attempt, an hour apart, credits nothing
      start <- addUTCTime 7200 <$> serviceNow env
      setClockAt bsClock start
      handOver
      forM_ [1 .. heldChargeAttempts - 1] $ \i -> do
        storeReceiptRetries a `shouldEventuallyReturn` [i]
        setClockAt bsClock $ addUTCTime (fromIntegral i * 3700) start
        alice `send` "/_app activate"
      -- set aside with no refusal recorded, and the next hand-over holds it again and credits it once Play charges
      heldStoreReceipts a `shouldEventuallyReturn` 0
      storeReceiptCreditErrors a `shouldReturn` []
      resumedAt <- addUTCTime 3700 <$> serviceNow env
      setClockAt bsClock resumedAt
      storeReports store $ subscriptionCheck SSRenewing $ Just (storeCharge "subscr-badge-supporter-month-02" (addUTCTime (-3600) resumedAt) 1) {chargeRef = "GPA.resumed"}
      handOver
      waitShownBadge a (Just 1)
      heldStoreReceipts a `shouldEventuallyReturn` 0
      dropOutput alice

testMovedOutPlayResumeSetAside :: HasCallStack => TestParams -> IO ()
testMovedOutPlayResumeSetAside ps =
  withBadgeServiceEnv ps $ \env@BadgeServiceEnv {bsClock, bsClientCfg, bsStore = store} ->
    withNewTestChatCfg ps bsClientCfg "alice" aliceProfile $ \alice ->
      withNewTestChatCfg ps bsClientCfg "bob" bobProfile $ \bob -> do
        playSubscriptionRetired env alice
        -- Play charges again, and bob's Restore moves the subscription before the retired device hands its token over
        resumedAt <- addUTCTime 7200 <$> serviceNow env
        setClockAt bsClock resumedAt
        storeReports store $ subscriptionCheck SSRenewing $ Just (storeCharge "subscr-badge-supporter-month-02" (addUTCTime (-3600) resumedAt) 1) {chargeRef = "GPA.resumed"}
        bob `send` restoredCmd 1 subscriptionPlay
        waitShownBadge (chatController bob) (Just 1)
        alice `send` ("/_badge purchase 1 " <> paymentArg subscriptionPlay)
        (last <$> ledgerTags (chatController alice)) `shouldEventuallyReturn` Just "transferOut"
        heldStoreReceipts (chatController alice) `shouldEventuallyReturn` 0
        storeReceiptRetries (chatController alice) `shouldReturn` [0]
        mapM_ dropOutput [alice, bob]

testMovedOutPlayNotResumed :: HasCallStack => TestParams -> IO ()
testMovedOutPlayNotResumed ps =
  withBadgeServiceEnv ps $ \env@BadgeServiceEnv {bsClock, bsClientCfg, bsStore = store} ->
    withNewTestChatCfg ps bsClientCfg "alice" aliceProfile $ \alice ->
      withNewTestChatCfg ps bsClientCfg "bob" bobProfile $ \bob -> do
        (_, rows) <- clientSubscribedMonthly env alice
        bob `send` restoredCmd 1 subscriptionPlay
        waitShownBadge (chatController bob) (Just 1)
        -- the old device's badge retires at its next request, which the service answers ended
        setClockAt bsClock $ fst $ renewalMoments rows
        alice `send` "/_app activate"
        waitShownBadge (chatController alice) Nothing
        last <$> ledgerTags (chatController alice) `shouldReturn` Just "transferOut"
        dropOutput alice
        checks <- subscriptionChecks store
        alice ##> ("/_badge purchase 1 " <> paymentArg subscriptionPlay)
        alice <## "badge already redeemed"
        heldStoreReceipts (chatController alice) `shouldReturn` 0
        subscriptionChecks store `shouldReturn` checks
        mapM_ dropOutput [alice, bob]

testMovedOutAppleNoAlertNoResume :: HasCallStack => TestParams -> IO ()
testMovedOutAppleNoAlertNoResume ps =
  withBadgeServiceEnv ps $ \env@BadgeServiceEnv {bsClock, bsClientCfg, bsStore = store@FakeStore {appleSubscriptionJWS}} ->
    withNewTestChatCfg ps bsClientCfg "alice" aliceProfile $ \alice ->
      withNewTestChatCfg ps bsClientCfg "bob" bobProfile $ \bob -> do
        firstStart <- addUTCTime (-3600) <$> serviceNow env
        -- the status names the transaction presented as the latest, so Restore moves it
        appleStoreReports store $ subscriptionCheck SSRenewing $ Just (storeCharge "SUBSCR_BADGE_SUPPORTER_MONTH_01" firstStart 1) {chargeRef = "2000000812345681"}
        alice ##> ("/_badge purchase 1 " <> paymentArg SPApple {jws = appleSubscriptionJWS})
        inAnyOrder alice [[openPurchase ""], ["1: supporter"]]
        waitWakeArmed (chatController alice)
        aliceRows <- ledgerRows (chatController alice) "badge_ledger"
        bob `send` restoredCmd 1 SPApple {jws = appleSubscriptionJWS}
        waitShownBadge (chatController bob) (Just 1)
        -- the source profile's badge retires at its next request, answered ended, and no alert says it ended there
        let requestAt = fst $ renewalMoments aliceRows
        setClockAt bsClock requestAt
        alice `send` "/_app activate"
        expectLinesWithin 30 alice ["/_app activate", "ok", "1: supporter"] ["1: supporter", "subscribed "]
        waitShownBadge (chatController alice) Nothing
        last <$> ledgerTags (chatController alice) `shouldReturn` Just "transferOut"
        alice ##> "/_badge state 1"
        alice <##. "1: supporter"
        (alice </)
        -- a later charge of the subscription is answered from the record, not held as a resume of the purchase it left
        checks <- subscriptionChecks store
        alice ##> ("/_badge purchase 1 " <> paymentArg SPApple {jws = appleResubscription "2000000812345692" requestAt})
        alice <## "badge already redeemed"
        heldStoreReceipts (chatController alice) `shouldReturn` 0
        subscriptionChecks store `shouldReturn` checks
        dropOutput bob

testAppleResubscribeBounded :: HasCallStack => TestParams -> IO ()
testAppleResubscribeBounded ps =
  withBadgeServiceEnv ps $ \env@BadgeServiceEnv {bsClock, bsClientCfg, bsStore = store} ->
    withNewTestChatCfg ps bsClientCfg "alice" aliceProfile $ \alice -> do
      requestAt <- appleSubscriptionRetired env alice
      let handOver jws = alice ##> ("/_badge purchase 1 " <> paymentArg SPApple {jws})
          answeredFromRecord jws = do
            handOver jws
            alice <## "badge already redeemed"
            heldStoreReceipts (chatController alice) `shouldReturn` 0
      -- a charge whose paid period has ended
      answeredFromRecord $ appleTransactionDated "2000000812345692" (addUTCTime (-40 * nominalDay) requestAt) (addUTCTime (-10 * nominalDay) requestAt)
      -- the store reports a revoked charge, which the service does not grant
      let resubscribedAt = addUTCTime 7200 requestAt
          handOverHeld jws = do
            handOver jws
            expectLinesWithin 30 alice ["1: supporter", "badge alert: subscription_ended ", openPurchase ""] ["subscribed "]
            heldStoreReceiptErrors (chatController alice) `shouldEventuallyReturn` ["service_error retry payment_pending"]
      setClockAt bsClock resubscribedAt
      appleStoreReports store $ subscriptionCheck SSRenewing $ Just (storeCharge "SUBSCR_BADGE_SUPPORTER_MONTH_01" resubscribedAt 1) {chargeRef = "apple.revoked", revoked = True}
      -- a charge whose paid period ends while it is held is refused before it is sent again
      handOverHeld $ appleTransactionDated "2000000812345695" resubscribedAt (addUTCTime 7200 resubscribedAt)
      checks <- subscriptionChecks store
      setClockAt bsClock $ addUTCTime 10800 resubscribedAt
      alice ##> "/_app activate"
      alice <## "ok"
      heldStoreReceipts (chatController alice) `shouldEventuallyReturn` 0
      storeReceiptCreditErrors (chatController alice) `shouldReturn` ["service_error final receipt_invalid"]
      subscriptionChecks store `shouldReturn` checks
      shownBadgeId (chatController alice) `shouldReturn` Nothing
      -- a later charge is held again though a resume was refused, while its own hold, from its purchase date, runs
      let laterCharge = appleResubscription "2000000812345693" (addUTCTime 60 resubscribedAt)
      handOverHeld laterCharge
      setClockAt bsClock $ addUTCTime (resumeHoldLimit + 120) resubscribedAt
      alice ##> "/_app activate"
      alice <## "ok"
      heldStoreReceipts (chatController alice) `shouldEventuallyReturn` 0
      storeReceiptCreditErrors (chatController alice) `shouldReturn` ["service_error final receipt_invalid"]
      shownBadgeId (chatController alice) `shouldReturn` Nothing
      answeredFromRecord laterCharge
      invoiceId <- createInvoice alice 1
      -- while another receipt of the owner is held, a resubscription is answered held and left unfinished, not held here
      setGoogleDown store True
      alice ##> purchaseWithInvoice 1 invoiceId supporterPlay
      let heldLines = expectLinesWithin 30 alice ["1: supporter", "badge alert: subscription_ended ", invoicePurchase "" invoiceId] ["subscribed "]
      heldLines
      now <- serviceNow env
      handOver $ appleResubscription "2000000812345694" now
      heldLines
      heldStoreReceipts (chatController alice) `shouldReturn` 1

-- | When appleLegendSubscriptionJWS says it was bought, which the client compares with its clock.
legendBoughtAt :: UTCTime
legendBoughtAt = posixSecondsToUTCTime 1769904000

-- | A monthly Apple Supporter subscription bought five days before the Legend transaction, shown; the clock an hour into that transaction.
appleSupporterBeforeLegend :: HasCallStack => BadgeServiceEnv -> TestCC -> IO ()
appleSupporterBeforeLegend env alice = appleSupporterBeforeLegendWith env alice Nothing

-- | The same, handed over with the invoice given, if any.
appleSupporterBeforeLegendWith :: HasCallStack => BadgeServiceEnv -> TestCC -> Maybe String -> IO ()
appleSupporterBeforeLegendWith BadgeServiceEnv {bsClock, bsStore = store@FakeStore {appleSubscriptionJWS}} alice invoiceId_ = do
  let firstStart = addUTCTime (-5 * nominalDay) legendBoughtAt
      supporter = SPApple {jws = appleSubscriptionJWS}
  setClockAt bsClock $ addUTCTime 3600 firstStart
  appleStoreReports store $ subscriptionCheck SSRenewing $ Just $ storeCharge "SUBSCR_BADGE_SUPPORTER_MONTH_01" firstStart 1
  case invoiceId_ of
    Nothing -> do
      alice ##> ("/_badge purchase 1 " <> paymentArg supporter)
      inAnyOrder alice [[openPurchase ""], ["1: supporter"]]
    Just invoiceId -> do
      alice ##> purchaseWithInvoice 1 invoiceId supporter
      inAnyOrder alice [[invoicePurchase "" invoiceId], creditedLines "" invoiceId "1: supporter"]
  waitWakeArmed (chatController alice)
  setClockAt bsClock $ addUTCTime 3600 legendBoughtAt

appleReportsLegend :: FakeStore -> IO ()
appleReportsLegend store = appleStoreReports store $ subscriptionCheck SSRenewing $ Just (storeCharge "SUBSCR_BADGE_LEGEND_YEAR_01" legendBoughtAt 12) {chargeRef = "apple.legend"}

testAppleTierChangeHandOver :: HasCallStack => TestParams -> IO ()
testAppleTierChangeHandOver ps =
  withBadgeServiceEnv ps $ \env@BadgeServiceEnv {bsClientCfg, bsStore = store@FakeStore {appleRenewalSubscriptionJWS, appleLegendSubscriptionJWS}} ->
    withNewTestChatCfg ps bsClientCfg "alice" aliceProfile $ \alice -> do
      appleSupporterBeforeLegend env alice
      let handOver jws = alice ##> ("/_badge purchase 1 " <> paymentArg SPApple {jws})
      checks <- subscriptionChecks store
      -- a charge of the tier shown is answered from the record and sends nothing
      handOver appleRenewalSubscriptionJWS
      alice <## "badge already redeemed"
      heldStoreReceipts (chatController alice) `shouldReturn` 0
      subscriptionChecks store `shouldReturn` checks
      -- a Legend charge is held, and the worker's request grants it and shows its credential at once
      appleReportsLegend store
      handOver appleLegendSubscriptionJWS
      expectLinesWithin 30 alice ["1: legend"] ["1: supporter", "store purchase open", "subscribed "]
      heldStoreReceipts (chatController alice) `shouldEventuallyReturn` 0
      subscriptionChecks store `shouldReturn` checks + 1
      waitShownIssued (chatController alice)
      issuedBadgeTypes (chatController alice) `shouldReturn` ["supporter", "legend"]
      purchaseBadgeTypes (chatController alice) "badge_purchases" `shouldReturn` ["legend"]
      balanceChecks (chatController alice) `shouldReturn` replicate 4 (Just True)
      issueErrorRow (chatController alice) `shouldReturn` (Nothing, Nothing)
      -- handed over again, it is answered from the record
      handOver appleLegendSubscriptionJWS
      alice <## "badge already redeemed"
      subscriptionChecks store `shouldReturn` checks + 1

testTierChangeReachesContacts :: HasCallStack => TestParams -> IO ()
testTierChangeReachesContacts ps = do
  legend <- legendInTwoHours
  withBadgeServiceVerifier ps (vouchingFor legend fakeVerifier) $ \env@BadgeServiceEnv {bsClientCfg} ->
    withNewTestChatCfg ps bsClientCfg "alice" aliceProfile $ \alice ->
      withNewTestChatCfg ps bsClientCfg "bob" bobProfile $ \bob -> do
        raised <- supporterRaisedToLegend env alice bob
        alice ##> ("/_badge purchase 1 " <> paymentArg SPApple {jws = legend})
        expectLinesWithin 30 alice [raised] ["1: supporter", "store purchase open", "subscribed "]
        heldStoreReceipts (chatController alice) `shouldEventuallyReturn` 0
        peerBadgeTypes (chatController bob) `shouldEventuallyReturn` ["legend"]

-- | A Legend year of the fake Apple subscription bought two hours from now, which the fake store does not list.
-- Not appleLegendSubscriptionJWS: its date is fixed, and an own badge long expired by the real clock is never presented.
legendInTwoHours :: IO Text
legendInTwoHours = do
  boughtAt <- addUTCTime 7200 <$> getCurrentTime
  pure $ subscriptionJWS appleSubscriptionOriginalId "2000000812345699" ["purchaseDate" J..= (round (utcTimeToPOSIXSeconds boughtAt * 1000) :: Int64), "productId" J..= ("SUBSCR_BADGE_LEGEND_YEAR_01" :: Text)]

-- | The verifier made of the fake store, vouching for jws as that store vouches for its own Legend year.
vouchingFor :: Text -> (FakeStore -> StoreVerifier) -> FakeStore -> StoreVerifier
vouchingFor jws verifierOf store@FakeStore {appleLegendSubscriptionJWS} = v {verifyApple = (\verify j -> verify $ if j == jws then appleLegendSubscriptionJWS else j) <$> verifyApple v}
  where
    v = verifierOf store

-- | A monthly Apple Supporter subscription bought now and shown to bob, then the clock two hours on, past the hour the
-- purchase's store check holds an upgrade off, with the store reporting a Legend year from then: what the apps are told once it is granted.
supporterRaisedToLegend :: HasCallStack => BadgeServiceEnv -> TestCC -> TestCC -> IO String
supporterRaisedToLegend env@BadgeServiceEnv {bsClock, bsStore = store@FakeStore {appleSubscriptionJWS}} alice bob = do
  connectUsers alice bob
  boughtAt <- serviceNow env
  appleStoreReports store $ subscriptionCheck SSRenewing $ Just $ storeCharge "SUBSCR_BADGE_SUPPORTER_MONTH_01" (addUTCTime (-3600) boughtAt) 1
  alice ##> ("/_badge purchase 1 " <> paymentArg SPApple {jws = appleSubscriptionJWS})
  inAnyOrder alice [[openPurchase ""], ["1: supporter"]]
  waitWakeArmed (chatController alice)
  peerBadgeTypes (chatController bob) `shouldEventuallyReturn` ["supporter"]
  let raisedAt = addUTCTime 7200 boughtAt
  setClockAt bsClock raisedAt
  appleStoreReports store $ subscriptionCheck SSRenewing $ Just (storeCharge "SUBSCR_BADGE_LEGEND_YEAR_01" raisedAt 12) {chargeRef = "2000000812345699"}
  pure $ "1: legend, 11 months left, paid through " <> formatTime defaultTimeLocale "%Y-%m-%d" (addMonths 12 raisedAt)

testAppleTierChangeStoreLags :: HasCallStack => TestParams -> IO ()
testAppleTierChangeStoreLags ps =
  withBadgeServiceEnv ps $ \env@BadgeServiceEnv {bsClock, bsClientCfg, bsStore = store@FakeStore {appleLegendSubscriptionJWS}} ->
    withNewTestChatCfg ps bsClientCfg "alice" aliceProfile $ \alice -> do
      appleSupporterBeforeLegend env alice
      -- the store still reports Supporter: the record stays held, and the purchase records no failed renewal
      alice ##> ("/_badge purchase 1 " <> paymentArg SPApple {jws = appleLegendSubscriptionJWS})
      heldStoreReceiptErrors (chatController alice) `shouldEventuallyReturn` ["service_error retry payment_pending"]
      expectLinesWithin 30 alice ["bad chat command: badge service error: payment_pending"] ["1: supporter", "store purchase open", "subscribed "]
      purchaseBadgeTypes (chatController alice) "badge_purchases" `shouldReturn` ["supporter"]
      issueErrorRow (chatController alice) `shouldReturn` (Nothing, Nothing)
      -- once the store reports Legend and the hour since it was asked has passed, the next attempt grants it
      appleReportsLegend store
      setClockAt bsClock $ addUTCTime (2 * 3600 + 120) legendBoughtAt
      alice ##> "/_app activate"
      alice <## "ok"
      expectLinesWithin 30 alice ["1: legend"] ["1: supporter", "store purchase open", "subscribed "]
      heldStoreReceipts (chatController alice) `shouldEventuallyReturn` 0
      purchaseBadgeTypes (chatController alice) "badge_purchases" `shouldReturn` ["legend"]

testAppleTierChangeExpiredSettles :: HasCallStack => TestParams -> IO ()
testAppleTierChangeExpiredSettles ps =
  withBadgeServiceVerifier ps (vouchingFor legendForADay fakeVerifier) $ \env@BadgeServiceEnv {bsClock, bsClientCfg, bsController = cc, bsStore = store} ->
    withNewTestChatCfg ps bsClientCfg "alice" aliceProfile $ \alice -> do
      appleSupporterBeforeLegend env alice
      let handOver = alice ##> ("/_badge purchase 1 " <> paymentArg SPApple {jws = legendForADay})
      handOver
      heldStoreReceiptErrors (chatController alice) `shouldEventuallyReturn` ["service_error retry payment_pending"]
      expectLinesWithin 30 alice ["bad chat command: badge service error: payment_pending"] ["1: supporter", "store purchase open", "subscribed "]
      -- once the transaction has expired the record settles with no change and no request, and the renewal check is left to grant it
      checks <- subscriptionChecks store
      ledger <- ledgerRows cc "sx_badge_service_badge_ledger"
      appleReportsLegend store
      setClockAt bsClock $ addUTCTime (nominalDay + 7200) legendBoughtAt
      alice ##> "/_app activate"
      alice <## "ok"
      heldStoreReceipts (chatController alice) `shouldEventuallyReturn` 0
      subscriptionChecks store `shouldReturn` checks
      ledgerRows cc "sx_badge_service_badge_ledger" `shouldReturn` ledger
      purchaseBadgeTypes (chatController alice) "badge_purchases" `shouldReturn` ["supporter"]
      handOver
      alice <## "badge already redeemed"
      heldStoreReceipts (chatController alice) `shouldReturn` 0
  where
    legendForADay = subscriptionJWS appleSubscriptionOriginalId "2000000812345689" ["purchaseDate" J..= epochMs legendBoughtAt, "expiresDate" J..= epochMs (addUTCTime nominalDay legendBoughtAt), "productId" J..= ("SUBSCR_BADGE_LEGEND_YEAR_01" :: Text)]
    epochMs t = round (utcTimeToPOSIXSeconds t * 1000) :: Int64

testAppleTierChangeHandedOverLate :: HasCallStack => TestParams -> IO ()
testAppleTierChangeHandedOverLate ps =
  withBadgeServiceEnv ps $ \env@BadgeServiceEnv {bsClock, bsClientCfg, bsStore = store@FakeStore {appleLegendSubscriptionJWS}} ->
    withNewTestChatCfg ps bsClientCfg "alice" aliceProfile $ \alice -> do
      appleSupporterBeforeLegend env alice
      appleReportsLegend store
      setClockAt bsClock $ addUTCTime (2 * nominalDay) legendBoughtAt
      alice ##> ("/_badge purchase 1 " <> paymentArg SPApple {jws = appleLegendSubscriptionJWS})
      expectLinesWithin 30 alice ["1: legend"] ["1: supporter", "store purchase open", "subscribed "]
      heldStoreReceipts (chatController alice) `shouldEventuallyReturn` 0
      purchaseBadgeTypes (chatController alice) "badge_purchases" `shouldReturn` ["legend"]

testAppleTierChangeAttemptsEnd :: HasCallStack => TestParams -> IO ()
testAppleTierChangeAttemptsEnd ps =
  withBadgeServiceEnv ps $ \env@BadgeServiceEnv {bsClock, bsClientCfg, bsStore = store@FakeStore {appleLegendSubscriptionJWS}} ->
    withNewTestChatCfg ps bsClientCfg "alice" aliceProfile $ \alice -> do
      appleSupporterBeforeLegend env alice
      let a = chatController alice
          handOver = alice ##> ("/_badge purchase 1 " <> paymentArg SPApple {jws = appleLegendSubscriptionJWS})
          answeredPending = expectLinesWithin 30 alice ["bad chat command: badge service error: payment_pending"] ["1: supporter", "store purchase open", "subscribed "]
          attemptAt i = do
            setClockAt bsClock $ addUTCTime (3600 + fromIntegral i * 3700) legendBoughtAt
            alice ##> "/_app activate"
            alice <## "ok"
      -- the store keeps reporting Supporter, so each attempt, an hour apart, is answered payment_pending
      handOver
      answeredPending
      forM_ [1 .. heldChargeAttempts - 1] $ \i -> do
        storeReceiptRetries a `shouldEventuallyReturn` [i]
        attemptAt i
        answeredPending
      -- set aside with no refusal recorded, and asked about no more
      heldStoreReceipts a `shouldEventuallyReturn` 0
      storeReceiptCreditErrors a `shouldReturn` []
      checks <- subscriptionChecks store
      attemptAt heldChargeAttempts
      threadDelay 2000000
      subscriptionChecks store `shouldReturn` checks
      -- the next hand-over holds it again, and it is granted once the store reports Legend
      appleReportsLegend store
      handOver
      expectLinesWithin 30 alice ["1: legend"] ["1: supporter", "store purchase open", "subscribed "]
      heldStoreReceipts a `shouldEventuallyReturn` 0
      purchaseBadgeTypes a "badge_purchases" `shouldReturn` ["legend"]

storeReceiptRetries :: ChatController -> IO [Int64]
storeReceiptRetries ChatController {chatStore} =
  withTransaction chatStore $ \db -> map fromOnly <$> DB.query_ db "SELECT retry_count FROM badge_store_receipts"

testAppleTierChangeKeepsPurchaseShown :: HasCallStack => TestParams -> IO ()
testAppleTierChangeKeepsPurchaseShown ps = do
  hook <- newIORef (pure ())
  withBadgeServiceVerifier ps (subscriptionCheckWithHook hook) $ \env@BadgeServiceEnv {bsClientCfg, bsStore = store@FakeStore {appleLegendSubscriptionJWS}} ->
    withNewTestChatCfg ps bsClientCfg "alice" aliceProfile $ \alice -> do
      appleSupporterBeforeLegend env alice
      appleReportsLegend store
      -- a retire, as retireBadge writes it, while the change is with the service
      writeIORef hook $ withTransaction (chatStore $ chatController alice) $ \db -> do
        Right user <- runExceptT $ getUser db 1
        clearShownBadge db user 1
        void $ runExceptT $ setUserBadge db user Nothing
      alice ##> ("/_badge purchase 1 " <> paymentArg SPApple {jws = appleLegendSubscriptionJWS})
      expectLinesWithin 30 alice ["1: legend"] ["1: supporter", "store purchase open", "subscribed "]
      heldStoreReceipts (chatController alice) `shouldEventuallyReturn` 0
      shownBadgeId (chatController alice) `shouldReturn` Just 1
      waitShownIssued (chatController alice)

testAppleTierChangeRefused :: HasCallStack => TestParams -> IO ()
testAppleTierChangeRefused ps =
  withBadgeServiceEnv ps $ \env@BadgeServiceEnv {bsClientCfg, bsStore = store} ->
    withNewTestChatCfg ps bsClientCfg "alice" aliceProfile $ \alice -> do
      invoiceId <- createInvoice alice 1
      appleSupporterBeforeLegendWith env alice (Just invoiceId)
      let unvouched = subscriptionJWS appleSubscriptionOriginalId "2000000812345696" ["purchaseDate" J..= (1769904000000 :: Int64), "productId" J..= ("SUBSCR_BADGE_LEGEND_YEAR_01" :: Text)]
          handOver = alice ##> ("/_badge purchase 1 " <> paymentArg SPApple {jws = unvouched})
      checks <- subscriptionChecks store
      handOver
      expectLinesWithin 30 alice ["1: supporter", "bad chat command: badge service error: receipt_invalid"] ["store purchase open", "subscribed "]
      heldStoreReceipts (chatController alice) `shouldEventuallyReturn` 0
      storeReceiptCreditErrors (chatController alice) `shouldReturn` ["service_error final receipt_invalid"]
      (alice </)
      issueErrorRow (chatController alice) `shouldReturn` (Nothing, Nothing)
      shownBadgeId (chatController alice) `shouldReturn` Just 1
      purchaseBadgeTypes (chatController alice) "badge_purchases" `shouldReturn` ["supporter"]
      handOver
      alice <## "badge already redeemed"
      heldStoreReceipts (chatController alice) `shouldReturn` 0
      subscriptionChecks store `shouldReturn` checks

testAppleTierChangeBesideHeldReceipt :: HasCallStack => TestParams -> IO ()
testAppleTierChangeBesideHeldReceipt ps =
  withBadgeServiceEnv ps $ \env@BadgeServiceEnv {bsClientCfg, bsStore = store@FakeStore {appleLegendSubscriptionJWS}} ->
    withNewTestChatCfg ps bsClientCfg "alice" aliceProfile $ \alice -> do
      appleSupporterBeforeLegend env alice
      alice ##> ("/_badge purchase 1 " <> paymentArg supporterPlay)
      expectLinesWithin 30 alice ["1: supporter", openPurchase ""] ["subscribed "]
      heldStoreReceipts (chatController alice) `shouldReturn` 1
      appleReportsLegend store
      alice ##> ("/_badge purchase 1 " <> paymentArg SPApple {jws = appleLegendSubscriptionJWS})
      expectLinesWithin 30 alice ["1: legend"] ["1: supporter", "store purchase open", "subscribed "]
      waitShownIssued (chatController alice)
      heldStoreReceipts (chatController alice) `shouldReturn` 1

testAppleTierChangeClearsEnded :: HasCallStack => TestParams -> IO ()
testAppleTierChangeClearsEnded ps =
  withBadgeServiceEnv ps $ \env@BadgeServiceEnv {bsClock, bsClientCfg, bsStore = store@FakeStore {appleSubscriptionJWS, appleLegendSubscriptionJWS}} ->
    withNewTestChatCfg ps bsClientCfg "alice" aliceProfile $ \alice -> do
      appleSupporterBeforeLegend env alice
      alice ##> ("/_badge subscription ended " <> paymentArg SPApple {jws = appleSubscriptionJWS})
      alice <### ["ok", Predicate (\l -> "1: supporter" `isPrefixOf` l && ", subscription ended" `isInfixOf` l)]
      -- a monthly Legend charge, so the month it funds ends within the test
      appleStoreReports store $ subscriptionCheck SSRenewing $ Just (storeCharge "SUBSCR_BADGE_LEGEND_MONTH_01" legendBoughtAt 1) {chargeRef = "apple.legend"}
      alice ##> ("/_badge purchase 1 " <> paymentArg SPApple {jws = appleLegendSubscriptionJWS})
      expectLinesWithin 30 alice ["1: legend"] ["1: supporter", "store purchase open", "subscribed "]
      waitShownIssued (chatController alice)
      paidThrough <- dueAtOf <$> ledgerRows (chatController alice) "badge_ledger"
      (Just expiry, _) <- shownAndIssuedExpiry (chatController alice)
      let pastPaidThrough = addUTCTime 3600 paidThrough
          requestAt = addUTCTime (-nominalDay) expiry
      pastPaidThrough `shouldSatisfy` (< requestAt)
      setClockAt bsClock pastPaidThrough
      alice ##> "/_app activate"
      alice <## "ok"
      waitWakeFrom (chatController alice) requestAt
      shownBadgeId (chatController alice) `shouldReturn` Just 1
      subscriptionStatuses (chatController alice) `shouldReturn` [Nothing]

testAppleTierChangeClearsBillingRetry :: HasCallStack => TestParams -> IO ()
testAppleTierChangeClearsBillingRetry ps =
  withBadgeServiceEnv ps $ \env@BadgeServiceEnv {bsClientCfg, bsStore = store@FakeStore {appleSubscriptionJWS, appleLegendSubscriptionJWS}} ->
    withNewTestChatCfg ps bsClientCfg "alice" aliceProfile $ \alice -> do
      appleSupporterBeforeLegend env alice
      alice ##> ("/_badge subscription billingRetry " <> paymentArg SPApple {jws = appleSubscriptionJWS})
      expectLinesWithin 30 alice ["ok", "1: supporter", "badge alert: payment_issue "] ["badge alert: payment_issue "]
      subscriptionStatuses (chatController alice) `shouldReturn` [Just "billingRetry"]
      appleReportsLegend store
      alice ##> ("/_badge purchase 1 " <> paymentArg SPApple {jws = appleLegendSubscriptionJWS})
      expectLinesWithin 30 alice ["1: legend"] ["1: supporter", "store purchase open", "subscribed ", "badge alert: payment_issue "]
      heldStoreReceipts (chatController alice) `shouldEventuallyReturn` 0
      subscriptionStatuses (chatController alice) `shouldReturn` [Nothing]
      alice ##> "/_badge state 1"
      alice <##. "1: legend"
      (alice </)

testAppleTierChangeCredentialUnverified :: HasCallStack => TestParams -> IO ()
testAppleTierChangeCredentialUnverified ps =
  withBadgeServiceEnv ps $ \env@BadgeServiceEnv {bsClientCfg, bsStore = store@FakeStore {appleLegendSubscriptionJWS}} -> do
    withNewTestChatCfg ps bsClientCfg "alice" aliceProfile $ \alice -> appleSupporterBeforeLegend env alice
    appleReportsLegend store
    -- trusting no issuer key, so the Legend credential the service issues does not verify
    withTestChatCfg ps bsClientCfg {badgePublicKeys = M.empty} "alice" $ \alice -> do
      alice ##> ("/_badge purchase 1 " <> paymentArg SPApple {jws = appleLegendSubscriptionJWS})
      expectLinesWithin 30 alice ["1: supporter", "internal chat error: issued badge credential does not verify"] ["store purchase open", "subscribed "]
      heldStoreReceiptErrors (chatController alice) `shouldEventuallyReturn` ["invalid_credential"]
      issueErrorRow (chatController alice) `shouldReturn` (Nothing, Nothing)
      shownBadgeId (chatController alice) `shouldReturn` Just 1

-- | A monthly Apple Legend subscription bought at legendBoughtAt, shown; the clock an hour into it.
appleLegendShown :: HasCallStack => BadgeServiceEnv -> TestCC -> IO ()
appleLegendShown BadgeServiceEnv {bsClock, bsStore = store@FakeStore {appleLegendSubscriptionJWS}} alice = do
  setClockAt bsClock $ addUTCTime 3600 legendBoughtAt
  appleStoreReports store $ subscriptionCheck SSRenewing $ Just $ storeCharge "SUBSCR_BADGE_LEGEND_MONTH_01" legendBoughtAt 1
  alice ##> ("/_badge purchase 1 " <> paymentArg SPApple {jws = appleLegendSubscriptionJWS})
  inAnyOrder alice [[openPurchase ""], ["1: legend"]]
  waitWakeArmed (chatController alice)

testAppleTierChangeLowerWaitsForRenewal :: HasCallStack => TestParams -> IO ()
testAppleTierChangeLowerWaitsForRenewal ps =
  withBadgeServiceEnv ps $ \env@BadgeServiceEnv {bsClock, bsClientCfg, bsController = cc, bsStore = store@FakeStore {appleRenewalSubscriptionJWS}} ->
    withNewTestChatCfg ps bsClientCfg "alice" aliceProfile $ \alice -> do
      appleLegendShown env alice
      let a = chatController alice
      checks <- subscriptionChecks store
      -- a Supporter charge of the subscription within the window is answered from the record and sends nothing
      alice ##> ("/_badge purchase 1 " <> paymentArg SPApple {jws = appleRenewalSubscriptionJWS})
      alice <## "badge already redeemed"
      (alice </)
      heldStoreReceipts a `shouldReturn` 0
      subscriptionChecks store `shouldReturn` checks
      purchaseBadgeTypes a "badge_purchases" `shouldReturn` ["legend"]
      -- the renewal check grants it, and its credential is shown once the Legend one expires
      rows <- ledgerRows a "badge_ledger"
      let (requestAt, presentAt) = renewalMoments rows
          supporterStart = addMonths 1 legendBoughtAt
      appleStoreReports store $ subscriptionCheck SSRenewing $ Just (storeCharge "SUBSCR_BADGE_SUPPORTER_MONTH_01" supporterStart 1) {chargeRef = "apple.supporter"}
      setClockAt bsClock requestAt
      alice ##> "/_app activate"
      alice <## "ok"
      renewed <- waitLedgerRows a (length rows + 2)
      alice <##. "1: supporter"
      map (\(_, ch, m, start, tier, t) -> (ch, m, start, tier, t)) (drop (length rows) renewed)
        `shouldBe` [(1, 1, supporterStart, "supporter", Just "charge"), (-1, 0, addMonths 1 supporterStart, "supporter", Just "badge")]
      purchaseBadgeTypes a "badge_purchases" `shouldReturn` ["supporter"]
      purchaseBadgeTypes cc "sx_badge_service_badge_purchases" `shouldReturn` ["supporter"]
      issuedBadgeTypes a `shouldReturn` ["legend", "supporter"]
      shownAndIssuedExpiry a >>= \(shown, issued) -> shown `shouldNotBe` issued
      setClockAt bsClock presentAt
      alice ##> "/_app activate"
      alice <## "ok"
      waitShownIssued a

testAppleTierChangeHeldLowerSettles :: HasCallStack => TestParams -> IO ()
testAppleTierChangeHeldLowerSettles ps =
  withBadgeServiceEnv ps $ \env@BadgeServiceEnv {bsClientCfg, bsStore = store@FakeStore {appleRenewalSubscriptionJWS}} ->
    withNewTestChatCfg ps bsClientCfg "alice" aliceProfile $ \alice -> do
      appleLegendShown env alice
      let a = chatController alice
      now <- serviceNow env
      -- held directly, as the hand-over no longer holds a lower tier
      withTransaction (chatStore a) $ \db -> do
        Right user <- runExceptT $ getUser db 1
        [(receiptId, purchaseId)] <- DB.query_ db "SELECT badge_store_receipt_id, badge_purchase_id FROM badge_purchases"
        reholdStoreReceipt db user receiptId purchaseId SHKTierChange SPApple {jws = appleRenewalSubscriptionJWS} now `shouldReturn` True
      checks <- subscriptionChecks store
      alice ##> "/_app activate"
      alice <## "ok"
      heldStoreReceipts a `shouldEventuallyReturn` 0
      (alice </)
      subscriptionChecks store `shouldReturn` checks
      purchaseBadgeTypes a "badge_purchases" `shouldReturn` ["legend"]
      shownBadgeId a `shouldReturn` Just 1

-- | A monthly Apple subscription bought an hour into its first period and retired past its coverage, at its request
-- moment, on the service's answer that it ended: that moment.
appleSubscriptionRetired :: HasCallStack => BadgeServiceEnv -> TestCC -> IO UTCTime
appleSubscriptionRetired env@BadgeServiceEnv {bsClock, bsStore = store@FakeStore {appleSubscriptionJWS}} alice = do
  firstStart <- addUTCTime (-3600) <$> serviceNow env
  appleStoreReports store $ subscriptionCheck SSRenewing $ Just $ storeCharge "SUBSCR_BADGE_SUPPORTER_MONTH_01" firstStart 1
  alice ##> ("/_badge purchase 1 " <> paymentArg SPApple {jws = appleSubscriptionJWS})
  inAnyOrder alice [[openPurchase ""], ["1: supporter"]]
  waitWakeArmed (chatController alice)
  (requestAt, _) <- renewalMoments <$> ledgerRows (chatController alice) "badge_ledger"
  appleStoreReports store $ subscriptionCheck SSEnded Nothing
  setClockAt bsClock requestAt
  alice `send` "/_app activate"
  alice
    <### [ "/_app activate",
           "ok",
           StartsWith "badge alert: subscription_ended ",
           StartsWith "1: supporter",
           StartsWith "badge alert: subscription_ended "
         ]
  waitShownBadge (chatController alice) Nothing
  pure requestAt

appleStoreReports :: FakeStore -> SubscriptionCheck -> IO ()
appleStoreReports store = setSubscription store (SubscriptionApple appleSubscriptionOriginalId)

-- | Another transaction of appleSubscriptionOriginalId's subscription, bought at the time given for a month.
appleResubscription :: Text -> UTCTime -> Text
appleResubscription txId bought = appleTransactionDated txId bought (addMonths 1 bought)

appleTransactionDated :: Text -> UTCTime -> UTCTime -> Text
appleTransactionDated txId bought expires =
  subscriptionJWS appleSubscriptionOriginalId txId ["purchaseDate" J..= epochMs bought, "expiresDate" J..= epochMs expires]
  where
    epochMs t = round (utcTimeToPOSIXSeconds t * 1000) :: Int64

storeReceiptCreditErrors :: ChatController -> IO [Text]
storeReceiptCreditErrors ChatController {chatStore} =
  withTransaction chatStore $ \db -> map fromOnly <$> DB.query_ db "SELECT credit_error FROM badge_store_receipts WHERE credit_error IS NOT NULL"

testApplePurchaseChecksBounded :: HasCallStack => TestParams -> IO ()
testApplePurchaseChecksBounded ps =
  withBadgeServiceEnv ps $ \env@BadgeServiceEnv {bsClock, bsController = cc, bsStore = store@FakeStore {appleSubscriptionJWS, appleRenewalSubscriptionJWS, appleSecondSubscriptionJWS, appleThirdSubscriptionJWS}} -> do
    now <- serviceNow env
    let ended originalId = setSubscription store (SubscriptionApple originalId) $ subscriptionCheck SSEnded $ Just $ storeCharge "SUBSCR_BADGE_SUPPORTER_MONTH_01" (addUTCTime (-3600) now) 1
        present (purchaseKey, masterKey) jws = refusalOf <$> serviceCmd env purchaseKey (purchaseCmd masterKey SPApple {jws})
        heldOff (code, retryAfter) = code == BSEPaymentPending && maybe False (\t -> t > 3540 && t <= 3600) retryAfter
    ended appleSubscriptionOriginalId
    ended appleSecondSubscriptionOriginalId
    keys <- newPurchaseKeys
    otherKeys <- newPurchaseKeys
    checks <- subscriptionChecks store
    firstCheck <- testClockTime bsClock
    present keys appleSubscriptionJWS `shouldReturn` (BSEPaymentPending, Just 86400)
    subscriptionChecks store `shouldReturn` checks + 1
    present keys appleSubscriptionJWS >>= (`shouldSatisfy` heldOff)
    present otherKeys appleSubscriptionJWS >>= (`shouldSatisfy` heldOff)
    present keys appleRenewalSubscriptionJWS >>= (`shouldSatisfy` heldOff)
    subscriptionChecks store `shouldReturn` checks + 1
    present keys appleSecondSubscriptionJWS `shouldReturn` (BSEPaymentPending, Just 86400)
    subscriptionChecks store `shouldReturn` checks + 2
    -- an answer that would bring the client back sooner carries the hour its claim is held for
    present keys appleThirdSubscriptionJWS `shouldReturn` (BSEProviderUnavailable, Just 3600)
    subscriptionChecks store `shouldReturn` checks + 3
    setClockAt bsClock $ addUTCTime 3700 firstCheck
    present otherKeys appleSubscriptionJWS `shouldReturn` (BSEPaymentPending, Just 86400)
    subscriptionChecks store `shouldReturn` checks + 4
    nothingPurchased cc

testPlayPurchaseChecksBounded :: HasCallStack => TestParams -> IO ()
testPlayPurchaseChecksBounded ps =
  withBadgeServiceEnv ps $ \env@BadgeServiceEnv {bsClock, bsController = cc, bsStore = store} -> do
    let token n = "fake-play-token-bounded-" <> n <> ".AO-J1Om8l3biV8pPk4"
        reports t = setSubscription store (SubscriptionGoogle t)
        payment = googlePayment "subscr_badge_supporter_01"
        present (purchaseKey, masterKey) t = refusalOf <$> serviceCmd env purchaseKey (purchaseCmd masterKey $ payment t)
        heldOff (code, retryAfter) = code == BSEPaymentPending && maybe False (\s -> s > 3540 && s <= 3600) retryAfter
        waiting = subscriptionCheck SSBillingRetry Nothing
    now <- serviceNow env
    reports (token "1") waiting
    reports (token "2") waiting
    reports (token "3") waiting
    reports (token "4") $ subscriptionCheck SSRenewing $ Just $ storeCharge "subscr-badge-supporter-month-02" (addUTCTime 3600 now) 1
    keys <- newPurchaseKeys
    otherKeys <- newPurchaseKeys
    checks <- subscriptionChecks store
    firstCheck <- testClockTime bsClock
    present keys (token "1") `shouldReturn` (BSEPaymentPending, Just 86400)
    subscriptionChecks store `shouldReturn` checks + 1
    present keys (token "1") >>= (`shouldSatisfy` heldOff)
    present otherKeys (token "1") >>= (`shouldSatisfy` heldOff)
    subscriptionChecks store `shouldReturn` checks + 1
    present keys (token "2") `shouldReturn` (BSEPaymentPending, Just 86400)
    subscriptionChecks store `shouldReturn` checks + 2
    -- answers that would bring the client back sooner carry the hour the token is held for
    setGoogleDown store True
    present keys (token "3") `shouldReturn` (BSEProviderUnavailable, Just 3600)
    setGoogleDown store False
    present keys (token "4") `shouldReturn` (BSEPaymentPending, Just 3600)
    subscriptionChecks store `shouldReturn` checks + 4
    setClockAt bsClock $ addUTCTime 3700 firstCheck
    present otherKeys (token "1") `shouldReturn` (BSEPaymentPending, Just 86400)
    subscriptionChecks store `shouldReturn` checks + 5
    nothingPurchased cc
    -- a token credited to one key is held for another key too, and its own key is answered from the record
    creditedAt <- serviceNow env
    reports (token "5") $ subscriptionCheck SSRenewing $ Just $ storeCharge "subscr-badge-supporter-month-02" (addUTCTime (-3600) creditedAt) 1
    (creditKey, creditMasterKey) <- newPurchaseKeys
    credited <- serviceCmd env creditKey (purchaseCmd creditMasterKey $ payment $ token "5")
    credentialOf credited `shouldSatisfy` isJust
    subscriptionChecks store `shouldReturn` checks + 6
    present otherKeys (token "5") >>= (`shouldSatisfy` heldOff)
    credentialOf <$> serviceCmd env creditKey (purchaseCmd creditMasterKey $ payment $ token "5") `shouldReturn` credentialOf credited
    subscriptionChecks store `shouldReturn` checks + 6
    setClockAt bsClock $ addUTCTime 3700 creditedAt
    present otherKeys (token "5") `shouldReturn` (BSEReceiptUsed, Nothing)
    subscriptionChecks store `shouldReturn` checks + 7

testPurchaseChecksBounded :: HasCallStack => TestParams -> IO ()
testPurchaseChecksBounded _ = do
  PurchaseChecks {appleChecks = checks, googleChecks} <- newPurchaseChecks
  now <- getCurrentTime
  let waitAt t ref = atomically $ purchaseCheckWait checks t (purchaseClaim ref)
      purchaseClaim ref = ClaimKey {claimRef = ref, claimPurpose = ClaimPurchase}
      size = M.size <$> readTVarIO checks
  waitAt now "claim" `shouldReturn` Nothing
  waitAt (addUTCTime 600 now) "claim" `shouldReturn` Just 3000
  forM_ [2 .. maxPurchaseChecks] $ \i -> waitAt now (tshow i) `shouldReturn` Nothing
  size `shouldReturn` maxPurchaseChecks
  waitAt now "new" `shouldReturn` Nothing
  waitAt now "new" `shouldReturn` Nothing
  size `shouldReturn` maxPurchaseChecks
  atomically (purchaseCheckWait googleChecks now (purchaseClaim "play claim")) `shouldReturn` Nothing
  atomically (purchaseCheckWait googleChecks now (purchaseClaim "play claim")) `shouldReturn` Just 3600
  -- a restore is not held off by the purchase's own check of the same subscription
  atomically (purchaseCheckWait googleChecks now ClaimKey {claimRef = "play claim", claimPurpose = ClaimTransfer}) `shouldReturn` Nothing
  M.size <$> readTVarIO googleChecks `shouldReturn` 2
  waitAt (addUTCTime 3600 now) "new" `shouldReturn` Nothing
  size `shouldReturn` 1

testPlayReadsBounded :: HasCallStack => TestParams -> IO ()
testPlayReadsBounded _ = do
  PurchaseChecks {playReads = taken} <- newPurchaseChecks
  now <- getCurrentTime
  let readAt t = atomically $ playReadWait taken t
      nextMinute = addUTCTime 60 now
  forM_ [1 .. maxPlayPurchaseReads] $ \_ -> readAt now `shouldReturn` Nothing
  readAt now `shouldReturn` Just 60
  readAt (addUTCTime 30 now) `shouldReturn` Just 30
  readAt nextMinute `shouldReturn` Nothing
  forM_ [2 .. maxPlayPurchaseReads] $ \_ -> readAt nextMinute `shouldReturn` Nothing
  readAt nextMinute `shouldReturn` Just 60

testPlayPurchaseReadsLimited :: HasCallStack => TestParams -> IO ()
testPlayPurchaseReadsLimited ps =
  withBadgeServiceEnv ps $ \env@BadgeServiceEnv {bsClock, bsController = cc, bsPurchaseChecks = PurchaseChecks {playReads = taken}, bsStore = store@FakeStore {appleSubscriptionJWS}} -> do
    let token n = "fake-play-token-reads-" <> n <> ".AO-J1Om8l3biV8pPk4"
        play = googlePayment "subscr_badge_supporter_01" . token
        present (purchaseKey, masterKey) payment = refusalOf <$> serviceCmd env purchaseKey (purchaseCmd masterKey payment)
        heldOff (code, retryAfter) = code == BSEPaymentPending && maybe False (\s -> s > 3540 && s <= 3600) retryAfter
        readRefused (code, retryAfter) = code == BSERateLimited && maybe False (\s -> s >= 1 && s <= 60) retryAfter
        useUpReads = do
          wait_ <- testClockTime bsClock >>= atomically . playReadWait taken
          when (isNothing wait_) useUpReads
    now <- serviceNow env
    setSubscription store (SubscriptionGoogle $ token "A") $ subscriptionCheck SSBillingRetry Nothing
    setSubscription store (SubscriptionGoogle $ token "B") $ subscriptionCheck SSBillingRetry Nothing
    setSubscription store (SubscriptionApple appleSubscriptionOriginalId) $ subscriptionCheck SSEnded $ Just $ storeCharge "SUBSCR_BADGE_SUPPORTER_MONTH_01" (addUTCTime (-3600) now) 1
    keys <- newPurchaseKeys
    checks <- subscriptionChecks store
    present keys (play "A") `shouldReturn` (BSEPaymentPending, Just 86400)
    firstReadAfter <- testClockTime bsClock
    subscriptionChecks store `shouldReturn` checks + 1
    useUpReads
    present keys (play "A") >>= (`shouldSatisfy` heldOff)
    present keys (play "B") >>= (`shouldSatisfy` readRefused)
    subscriptionChecks store `shouldReturn` checks + 1
    present keys SPApple {jws = appleSubscriptionJWS} `shouldReturn` (BSEPaymentPending, Just 86400)
    subscriptionChecks store `shouldReturn` checks + 2
    setClockAt bsClock $ addUTCTime 61 firstReadAfter
    present keys (play "B") `shouldReturn` (BSEPaymentPending, Just 86400)
    subscriptionChecks store `shouldReturn` checks + 3
    nothingPurchased cc

-- | Apple's production host answers 404 for a Sandbox transaction, so the offline check refuses it first.
testAppleSandboxSubscriptionNotAsked :: HasCallStack => TestParams -> IO ()
testAppleSandboxSubscriptionNotAsked ps = do
  chain <- trustedChain
  withFakeAppStore ourBundleId $ \fake -> do
    Right check <- appleSubscriptionChecker (rootCert chain :| []) ourBundleId (fasApi fake)
    let checkApple ref _ = case ref of
          SubscriptionApple originalId -> check originalId
          SubscriptionGoogle _ -> pure $ Left SRNotConfigured
        verifier = noStoreVerifier {verifyApple = Just (verifyAppleTransaction (rootCert chain :| []) ourBundleId), checkSubscription = Just checkApple}
    withBadgeServiceVerifier ps (const verifier) $ \env@BadgeServiceEnv {bsController = cc} -> do
      jws <- signedBy chain $ subscriptionPayload [("environment", "Sandbox")] []
      (purchaseKey, masterKey) <- newPurchaseKeys
      refusalOf <$> serviceCmd env purchaseKey (purchaseCmd masterKey SPApple {jws}) `shouldReturn` (BSETestPurchase, Nothing)
      readTVarIO (fasTokens fake) `shouldReturn` []
      nothingPurchased cc

testAppleSubscriptionWithoutApiKey :: HasCallStack => TestParams -> IO ()
testAppleSubscriptionWithoutApiKey ps = do
  chain <- trustedChain
  createDirectoryIfMissing True "tests/tmp"
  withTempDirectory "tests/tmp" "apple-no-api-key" $ \d -> do
    B.writeFile (d </> "root.cer") (encodeSignedObject $ rootCert chain)
    writeFile (d </> "badge_service.ini") "[listener]\nstatic_dir = /srv/badges\n"
    Right cfg <- readServiceConfig (d </> "badge_service.ini")
    let apple = AppleStoreConfig {aBundleId = ourBundleId, aRootCertificates = (d </> "root.cer") :| [], aServerApi = Nothing}
    ServiceState {storeVerifier} <- newServiceState >>= configureStoreVerifier (Just cfg {appleStore = Just apple})
    withBadgeServiceVerifier ps (const storeVerifier) $ \env@BadgeServiceEnv {bsController = cc} -> do
      (purchaseKey, masterKey) <- newPurchaseKeys
      subscription <- signedBy chain $ subscriptionPayload [] []
      let presentSubscription = refusalOf <$> serviceCmd env purchaseKey (purchaseCmd masterKey SPApple {jws = subscription})
      presentSubscription `shouldReturn` (BSEProviderNotConfigured, Nothing)
      presentSubscription `shouldReturn` (BSEProviderNotConfigured, Nothing)
      nothingPurchased cc
      oneTime <- signedBy chain $ transactionPayload [] []
      badgeTypeOf <$> serviceCmd env purchaseKey (purchaseCmd masterKey SPApple {jws = oneTime}) `shouldReturn` Just BTSupporter

testPlaySubscriptionAcknowledgedBeforeCrediting :: HasCallStack => TestParams -> IO ()
testPlaySubscriptionAcknowledgedBeforeCrediting ps = do
  oneTimeAcknowledged <- newIORef []
  acknowledged <- newIORef []
  acknowledgement <- newIORef $ Left $ SRUnreachable "acknowledging: fake store is down"
  let acknowledging store =
        (fakeVerifier store)
          { acknowledgeGoogle = Just $ \_ token -> modifyIORef' oneTimeAcknowledged (token :) >> pure (Right ()),
            acknowledgeGoogleSubscription = Just $ \productId token -> modifyIORef' acknowledged ((productId, token) :) >> readIORef acknowledgement
          }
  withBadgeServiceVerifier ps acknowledging $ \env@BadgeServiceEnv {bsClock, bsController = cc, bsStore} -> do
    now <- serviceNow env
    (purchaseKey, masterKey) <- newPurchaseKeys
    let purchase = serviceCmd env purchaseKey $ purchaseCmd masterKey subscriptionPlay
        chargedFrom start = setSubscription bsStore (SubscriptionGoogle googleSubscriptionToken) $ subscriptionCheck SSRenewing $ Just $ storeCharge "subscr-badge-supporter-month-02" start 1
        -- the token is held an hour after each attempt
        nextHour = testClockTime bsClock >>= setClockAt bsClock . addUTCTime 3700
    -- only a charge about to be credited is acknowledged
    chargedFrom $ addUTCTime 3600 now
    fst . refusalOf <$> purchase `shouldReturn` BSEPaymentPending
    readIORef acknowledged `shouldReturn` []
    chargedFrom $ addUTCTime (-3600) now
    nextHour
    refusalOf <$> purchase >>= (`shouldSatisfy` \(code, retryAfter) -> code == BSEProviderUnavailable && isJust retryAfter)
    writeIORef acknowledgement $ Left $ SRVerifierFailed "acknowledging: the service account lacks the Play Console permission for this call"
    nextHour
    refusalOf <$> purchase `shouldReturn` (BSEInternal, Nothing)
    nothingPurchased cc
    rowCount cc "sx_badge_service_subscription_charges" `shouldReturn` 0
    writeIORef acknowledgement $ Right ()
    nextHour
    purchased <- purchase
    subscriptionOf purchased `shouldBe` Just SSRenewing
    replayed <- purchase
    credentialOf replayed `shouldBe` credentialOf purchased
    readIORef acknowledged `shouldReturn` replicate 3 ("subscr_badge_supporter_01", googleSubscriptionToken)
    readIORef oneTimeAcknowledged `shouldReturn` []

-- | The service as deployed with [google], against a Play that answers as its documentation says.
testPlaySubscriptionThroughPlay :: HasCallStack => TestParams -> IO ()
testPlaySubscriptionThroughPlay ps =
  withFakePlay $ \fake@FakePlay {fpConfig} -> do
    let ini = takeDirectory (gServiceAccountFile fpConfig) </> "badge_service.ini"
    writeFile ini "[listener]\nstatic_dir = /srv/badges\n"
    Right cfg <- readServiceConfig ini
    ServiceState {storeVerifier = withoutPlay} <- newServiceState >>= configureStoreVerifier (Just cfg)
    ServiceState {storeVerifier} <- newServiceState >>= configureStoreVerifier (Just cfg {playStore = Just fpConfig})
    withBadgeServiceVerifier ps (const storeVerifier) $ \env@BadgeServiceEnv {bsController = cc} -> do
      periodEnd <- addMonths 1 . addUTCTime (-3600) <$> serviceNow env
      answerPurchase fake googleSubscriptionToken $ PlayRecord $ subscriptionRecord "SUBSCRIPTION_STATE_ACTIVE" [] [lineItem [("expiryTime", J.toJSON periodEnd)] []]
      (purchaseKey, masterKey) <- newPurchaseKeys
      refusalOf <$> serviceCmd env {bsVerifier = withoutPlay} purchaseKey (purchaseCmd masterKey subscriptionPlay) `shouldReturn` (BSEProviderNotConfigured, Nothing)
      nothingPurchased cc
      readTVarIO (fpPurchasePaths fake) `shouldReturn` []
      purchased <- serviceCmd env purchaseKey $ purchaseCmd masterKey subscriptionPlay
      map entryTag (fst $ statementOf purchased) `shouldBe` ["charge", "badge"]
      -- Play reports no period start, so the month is granted the plan's length before the period ends
      map createdAtOf (take 1 $ fst $ statementOf purchased) `shouldBe` [addMonths (-1) periodEnd]
      badgeTypeOf purchased `shouldBe` Just BTSupporter
      subscriptionOf purchased `shouldBe` Just SSRenewing
      subscriptionPayments cc `shouldReturn` [("google", googlePurchaseRef googleSubscriptionToken, Just "renewing", Just googleSubscriptionToken)]
      readTVarIO (fpSubscriptionAcknowledged fake) `shouldReturn` [("subscr_badge_supporter_01", googleSubscriptionToken)]
      answerPurchase fake googleSupporterToken $ PlayRecord $ purchaseRecord 0 []
      (oneTimeKey, oneTimeMasterKey) <- newPurchaseKeys
      badgeTypeOf <$> serviceCmd env oneTimeKey (purchaseCmd oneTimeMasterKey supporterPlay) `shouldReturn` Just BTSupporter
      readTVarIO (fpAcknowledged fake) `shouldReturn` [googleSupporterToken]
      readTVarIO (fpSubscriptionAcknowledged fake) `shouldReturn` [("subscr_badge_supporter_01", googleSubscriptionToken)]

testMockSubscriptions :: HasCallStack => TestParams -> IO ()
testMockSubscriptions ps =
  withBadgeServiceVerifierIO ps (\clock _ -> newMockStoreVerifier (testClockTime clock)) $ \env@BadgeServiceEnv {bsClock, bsVerifier} -> do
    now <- serviceNow env
    let purchase payment = newPurchaseKeys >>= \(purchaseKey, masterKey) -> (purchaseKey,) <$> serviceCmd env purchaseKey (purchaseCmd masterKey payment)
        changes = map (\e -> let (c, m, _) = entryOf e in (c, m)) . fst . statementOf
        chargedAMinuteAgo r = case fst (statementOf r) of
          charge : _ -> abs (diffUTCTime (createdAtOf charge) (addUTCTime (-60) now)) < 2
          [] -> False
        appleJWS fields = unsignedJWS $ LB.toStrict $ J.encode $ J.object fields
        playSubscription = googlePayment "subscr_badge_supporter_01"
    -- Apple: the plan the product names
    (_, apple) <- purchase SPApple {jws = appleJWS ["transactionId" J..= ("2000000900000011" :: Text), "originalTransactionId" J..= ("2000000900000010" :: Text), "type" J..= ("Auto-Renewable Subscription" :: Text), "productId" J..= ("SUBSCR_BADGE_SUPPORTER_YEAR_01" :: Text)]}
    changes apple `shouldBe` [(12, 12), (-1, 11)]
    subscriptionOf apple `shouldBe` Just SSRenewing
    chargedAMinuteAgo apple `shouldBe` True
    -- Play: the base plan a test token names, else the product's monthly plan
    (_, testToken) <- purchase $ playSubscription "test.subscr-badge-supporter-year-01.3f2c9a1e-7d4b-4c55-9a0e-6b1f2d3c4e5f"
    changes testToken `shouldBe` [(12, 12), (-1, 11)]
    (monthlyKey, monthly) <- purchase $ playSubscription googleSubscriptionToken
    changes monthly `shouldBe` [(1, 1), (-1, 0)]
    subscriptionOf monthly `shouldBe` Just SSRenewing
    chargedAMinuteAgo monthly `shouldBe` True
    (forgottenKey, forgotten) <- purchase $ playSubscription "fake-play-token-forgotten.AO-J1Oq1n0dkW9pRl5"
    -- a renewal of one it credited is charged again; one a restarted mock never saw has ended
    setClockAt bsClock $ addUTCTime nominalDay $ addMonths 1 now
    renewed <- assertBalance env monthlyKey (last $ fst $ statementOf monthly)
    map entryTag (fst $ statementOf renewed) `shouldBe` ["charge", "badge"]
    subscriptionOf renewed `shouldBe` Just SSRenewing
    restarted <- newMockStoreVerifier (testClockTime bsClock)
    ended <- serviceCmd env {bsVerifier = restarted} forgottenKey BSCIssueBadge {balance = BadgeBalance {lastEntry = last $ fst $ statementOf forgotten}}
    subscriptionOf ended `shouldBe` Just SSEnded
    credentialOf ended `shouldBe` Nothing
    -- a first purchase naming no plan it knows is not a verdict
    checkStoreSubscription bsVerifier (SubscriptionApple "2000000900000020") (Just "BADGE_SUPPORTER_01") >>= (`shouldSatisfy` \case Left SRUnreachable {} -> True; _ -> False)
    -- a one-time purchase, as before
    (_, oneTime) <- purchase SPApple {jws = appleJWS ["transactionId" J..= ("2000000900000030" :: Text), "productId" J..= ("BADGE_SUPPORTER_01" :: Text)]}
    badgeTypeOf oneTime `shouldBe` Just BTSupporter

testStoreChecksDuringCoverage :: HasCallStack => TestParams -> IO ()
testStoreChecksDuringCoverage ps =
  withBadgeServiceVerifierIO ps (\clock _ -> checkingAtEveryRequest <$> newMockStoreVerifier (testClockTime clock)) $ \env@BadgeServiceEnv {bsClock, bsController = cc} -> do
    now <- serviceNow env
    (purchaseKey, masterKey) <- newPurchaseKeys
    purchased <- serviceCmd env purchaseKey $ purchaseCmd masterKey $ googlePayment "subscr_badge_supporter_01" googleSubscriptionToken
    let entries = fst . statementOf
        request r = assertBalance env purchaseKey (last $ entries r)
        stampedSince t previous = do
          [Just checkedAt] <- subscriptionCheckedAt cc
          checkedAt `shouldSatisfy` \c -> c >= t && c > previous
          pure checkedAt
        bankedAt = addUTCTime 120 now
        renewedAt = addUTCTime nominalDay $ addMonths 1 now
        endedAt = addUTCTime 60 renewedAt
    [Just purchaseStamp] <- subscriptionCheckedAt cc
    -- over a minute on, as the mock dates its charge a minute back and the client fails a row dated before the tip
    setClockAt bsClock bankedAt
    banked <- request purchased
    bankedStamp <- stampedSince bankedAt purchaseStamp
    map entryTag (entries banked) `shouldBe` ["charge"]
    -- a month granted on the running coverage: one left, paid through a month later
    entryOf (last $ entries banked) `shouldBe` (1, 1, nextDue (entries purchased))
    credentialOf banked `shouldBe` credentialOf purchased
    subscriptionOf banked `shouldBe` Just SSRenewing
    rowCount cc "sx_badge_service_subscription_charges" `shouldReturn` 2
    setClockAt bsClock renewedAt
    renewed <- request banked
    renewedStamp <- stampedSince renewedAt bankedStamp
    map entryTag (entries renewed) `shouldBe` ["charge", "badge"]
    -- the month banked earlier and the one this check banks, less the one issued
    (\(c, m, _) -> (c, m)) (entryOf $ last $ entries renewed) `shouldBe` (-1, 1)
    expiryOf renewed `shouldSatisfy` (> expiryOf purchased)
    -- a restarted mock has forgotten the plan, so the subscription has ended though coverage runs, and the banked month is refunded
    restarted <- checkingAtEveryRequest <$> newMockStoreVerifier (testClockTime bsClock)
    setClockAt bsClock endedAt
    ended <- serviceCmd env {bsVerifier = restarted} purchaseKey BSCIssueBadge {balance = BadgeBalance {lastEntry = last $ entries renewed}}
    void $ stampedSince endedAt renewedStamp
    (credentialOf ended, subscriptionOf ended) `shouldBe` (Nothing, Just SSEnded)
    map entryTag (entries ended) `shouldBe` ["refund"]
    (\(c, m, _) -> (c, m)) (entryOf $ last $ entries ended) `shouldBe` (-1, 0)
    -- with nothing left to refund, a later request that month writes no row
    setClockAt bsClock $ addUTCTime 60 endedAt
    again <- serviceCmd env {bsVerifier = restarted} purchaseKey BSCIssueBadge {balance = BadgeBalance {lastEntry = last $ entries ended}}
    (credentialOf again, subscriptionOf again) `shouldBe` (Nothing, Just SSEnded)
    map entryTag (entries again) `shouldBe` []
  where
    checkingAtEveryRequest v = v {storeChecks = ChecksAtEveryRequest}

testStoreChecksDuringCoverageGrantedOnce :: HasCallStack => TestParams -> IO ()
testStoreChecksDuringCoverageGrantedOnce ps =
  withBadgeServiceEnv ps $ \env@BadgeServiceEnv {bsController = cc, bsStore = store} -> do
    (purchaseKey, _, purchased) <- subscribedMonthly env
    ledger <- ledgerRows cc "sx_badge_service_badge_ledger"
    checks <- subscriptionChecks store
    answered <- serviceCmd env {bsVerifier = (fakeVerifier store) {storeChecks = ChecksAtEveryRequest}} purchaseKey BSCIssueBadge {balance = BadgeBalance {lastEntry = last purchased}}
    subscriptionChecks store `shouldReturn` checks + 1
    map entryTag (fst $ statementOf answered) `shouldBe` []
    ledgerRows cc "sx_badge_service_badge_ledger" `shouldReturn` ledger
    rowCount cc "sx_badge_service_subscription_charges" `shouldReturn` 1

testBankedMonthAnnounced :: HasCallStack => TestParams -> IO ()
testBankedMonthAnnounced ps =
  withRequestCountingService ps $ \env@BadgeServiceEnv {bsClock, bsClientCfg, bsStore = store} ->
    withNewTestChatCfg ps bsClientCfg {badgeDevRequestInterval = Just 600} "alice" aliceProfile $ \alice -> do
      (firstStart, rows) <- clientSubscribedMonthly env alice
      let t1 = devIntervalStart rows
          paidThrough = addMonths 2 firstStart
      storeReports store $ subscriptionCheck SSRenewing $ Just (storeCharge "subscr-badge-supporter-month-02" (addUTCTime (-60) t1) 1) {chargeRef = "GPA.banked"}
      setClockAt bsClock t1
      alice `send` "/_app activate"
      expectLinesWithin 30 alice ["/_app activate", "ok", "1: supporter, 1 months left, paid through " <> day paidThrough] ["subscribed "]
      drop 2 . rowChanges <$> ledgerRows (chatController alice) "badge_ledger" `shouldReturn` [(1, 1, dueAtOf rows, Just "charge")]
  where
    day = formatTime defaultTimeLocale "%Y-%m-%d"

testPurchaseBadge :: HasCallStack => TestParams -> IO ()
testPurchaseBadge ps =
  withBadgeServiceEnv ps $ \BadgeServiceEnv {bsClientCfg, bsController = cc} ->
    withNewTestChatCfg ps bsClientCfg "alice" aliceProfile $ \alice -> do
      invoiceId <- createInvoice alice 1
      alice ##> purchaseWithInvoice 1 invoiceId supporterPlay
      inAnyOrder alice [[invoicePurchase "" invoiceId], creditedLines "" invoiceId "1: supporter"]
      alice ##> "/create user alisa"
      showActiveUser alice "alisa"
      -- the app hands a purchase over until it is answered credited, and one already credited adds nothing
      alice ##> purchaseWithInvoice 2 invoiceId supporterPlay
      alice <## "[user: alice] badge already redeemed"
      storeReceiptRows (chatController alice) `shouldReturn` [(1, Just (T.pack invoiceId), True)]
      rowCount cc "sx_badge_service_badge_purchases" `shouldReturn` 1
      map (\(_, c, m, _, _, t) -> (c, m, t)) <$> ledgerRows (chatController alice) "badge_ledger" `shouldReturn` [(1, 1, Just "payment"), (-1, 0, Just "badge")]
      alice ##> "/p"
      showActiveUser alice "alisa"
      alice ##> "/user alice"
      showActiveUser alice "alice (Alice, * supporter)"

testPurchaseNamesNoTransaction :: HasCallStack => TestParams -> IO ()
testPurchaseNamesNoTransaction ps =
  withBadgeServiceEnv ps $ \BadgeServiceEnv {bsClientCfg, bsController = cc} ->
    withNewTestChatCfg ps bsClientCfg "alice" aliceProfile $ \alice -> do
      invoiceId <- createInvoice alice 1
      alice ##> purchaseWithInvoice 1 invoiceId SPApple {jws = "not.a-jws"}
      alice <## "cannot get badge: invalid store receipt"
      storeReceiptRows (chatController alice) `shouldReturn` [(1, Just (T.pack invoiceId), False)]
      nothingPurchased cc

testPurchaseStashReceiptUsed :: HasCallStack => TestParams -> IO ()
testPurchaseStashReceiptUsed ps =
  withBadgeServiceEnv ps $ \BadgeServiceEnv {bsClientCfg, bsController = cc} ->
    withNewTestChatCfg ps bsClientCfg "alice" aliceProfile $ \alice ->
      withNewTestChatCfg ps bsClientCfg "bob" bobProfile $ \bob -> do
        invoiceId <- createInvoice alice 1
        -- the store echoes the buyer's invoice on any device the purchase reaches
        let purchase = purchaseWithInvoice 1 invoiceId supporterPlay
        alice ##> purchase
        inAnyOrder alice [[invoicePurchase "" invoiceId], creditedLines "" invoiceId "1: supporter"]
        bob ##> purchase
        bob <### [StartsWith (invoicePurchase "" invoiceId), ConsoleString (refusedLine "" invoiceId "service_error final receipt_used")]
        bob ##> purchase
        bob <## "cannot get badge: badge service error: receipt_used"
        storeReceiptRows (chatController bob) `shouldReturn` [(1, Just (T.pack invoiceId), True)]
        heldStoreReceipts (chatController bob) `shouldReturn` 0
        rowCount (chatController alice) "badge_store_receipts" `shouldReturn` 1
        rowCount cc "sx_badge_service_badge_purchases" `shouldReturn` 1
        bob ##> "/p"
        showActiveUser bob "bob (Bob)"

testPurchaseUnsentPlayToken :: HasCallStack => TestParams -> IO ()
testPurchaseUnsentPlayToken ps =
  withBadgeServiceEnv ps $ \BadgeServiceEnv {bsClientCfg, bsController = cc} ->
    withNewTestChatCfg ps bsClientCfg "alice" aliceProfile $ \alice -> do
      alice ##> ("/_badge purchase 1 " <> paymentArg (googlePayment "badge_supporter_01" "token/other"))
      storePurchaseOpen alice ""
      heldStoreReceiptErrors (chatController alice) `shouldEventuallyReturn` ["service_error retry provider_unavailable"]
      heldStoreReceipts (chatController alice) `shouldReturn` 1
      nothingPurchased cc

testPurchaseStrandedUnderOtherProfile :: HasCallStack => TestParams -> IO ()
testPurchaseStrandedUnderOtherProfile ps = do
  broken <- newIORef True
  withBadgeServiceVerifier ps (brokenGoogle broken) $ \BadgeServiceEnv {bsClientCfg, bsController = cc} ->
    withNewTestChatCfg ps (fastRetryCfg bsClientCfg) "alice" aliceProfile $ \alice -> do
      invoiceId <- createInvoice alice 1
      let purchase userId = purchaseWithInvoice userId invoiceId supporterPlay
      alice ##> purchase 1
      alice <##. invoicePurchase "" invoiceId
      heldStoreReceiptErrors (chatController alice) `shouldEventuallyReturn` ["service_error final internal"]
      alice ##> "/create user alisa"
      showActiveUser alice "alisa"
      -- handed over again under whichever profile is active, the purchase stays with the keys alice holds
      alice ##> purchase 2
      alice <##. invoicePurchase "[user: alice] " invoiceId
      writeIORef broken False
      mapM_ (alice <##.) $ creditedLines "[user: alice] " invoiceId "1: supporter"
      (alice </)
      rowCount (chatController alice) "badge_store_receipts" `shouldReturn` 1
      rowCount cc "sx_badge_service_badge_purchases" `shouldReturn` 1
      alice ##> "/user alice"
      showActiveUser alice "alice (Alice, * supporter)"

testPurchaseDeliveredToHiddenProfile :: HasCallStack => TestParams -> IO ()
testPurchaseDeliveredToHiddenProfile ps = do
  broken <- newIORef True
  withBadgeServiceVerifier ps (brokenGoogle broken) $ \BadgeServiceEnv {bsClientCfg} ->
    withNewTestChatCfg ps (fastRetryCfg bsClientCfg) "alice" aliceProfile $ \alice -> do
      let purchase userId = "/_badge purchase " <> show (userId :: Int) <> " " <> paymentArg supporterPlay
      alice ##> purchase 1
      storePurchaseOpen alice ""
      heldStoreReceiptErrors (chatController alice) `shouldEventuallyReturn` ["service_error final internal"]
      alice ##> "/create user alisa"
      showActiveUser alice "alisa"
      alice ##> "/_hide user 1 \"password\""
      alice <## "user alice:"
      alice <## "messages are hidden (use /tail to view)"
      alice <## "profile is hidden"
      -- the answer and the credit are the hidden owner's, so the view prints none of them
      alice ##> purchase 2
      (alice </)
      writeIORef broken False
      heldStoreReceipts (chatController alice) `shouldEventuallyReturn` 0
      (alice </)
      alice ##> "/user alice password"
      showActiveUser alice "alice (Alice, * supporter)"

testInvoiceOtherProfile :: HasCallStack => TestParams -> IO ()
testInvoiceOtherProfile ps =
  withBadgeServiceEnv ps $ \BadgeServiceEnv {bsClientCfg, bsClock = clock} ->
    withNewTestChatCfg ps bsClientCfg "alice" aliceProfile $ \alice -> do
      invoiceId <- createInvoice alice 1
      getCurrentTime >>= setClockAt clock . addUTCTime (8 * nominalDay)
      alice ##> "/create user alisa"
      showActiveUser alice "alisa"
      alice ##> purchaseWithInvoice 2 invoiceId supporterPlay
      inAnyOrder alice [[invoicePurchase "[user: alice] " invoiceId], creditedLines "[user: alice] " invoiceId "1: supporter"]
      (alice </)
      storeReceiptRows (chatController alice) `shouldReturn` [(1, Just (T.pack invoiceId), True)]
      alice ##> "/user alice"
      showActiveUser alice "alice (Alice, * supporter)"

-- | The store lists an account's purchases on every device signed into it, so a listed one may be another device's.
testListedReceiptNotRecorded :: HasCallStack => TestParams -> IO ()
testListedReceiptNotRecorded ps =
  withBadgeServiceEnv ps $ \BadgeServiceEnv {bsClientCfg, bsController = cc} ->
    withNewTestChatCfg ps bsClientCfg "alice" aliceProfile $ \alice -> do
      let listed invoiceId_ payment = "/_badge purchase 1" <> maybe "" (" invoice=" <>) invoiceId_ <> " listed " <> paymentArg payment
      alice ##> listed (Just "0b6a5e6c-4f3e-4d51-9f55-3a0f7c2f9e11") supporterPlay
      alice <## "store purchase not recorded"
      storeReceiptRows (chatController alice) `shouldReturn` []
      nothingPurchased cc
      invoiceId <- createInvoice alice 1
      alice ##> listed (Just invoiceId) supporterPlay
      inAnyOrder alice [[invoicePurchase "" invoiceId], creditedLines "" invoiceId "1: supporter"]
      alice ##> listed Nothing supporterPlay
      alice <## "badge already redeemed"
      alice ##> listed Nothing subscriptionPlay
      alice <## "store purchase not recorded"
      storeReceiptRows (chatController alice) `shouldReturn` [(1, Just (T.pack invoiceId), True)]
      -- unmarked, as Restore sends it, the same transaction is recorded; held, as a badge is shown
      alice ##> ("/_badge purchase 1 " <> paymentArg subscriptionPlay)
      alice <##. "1: supporter"
      storePurchaseOpen alice ""
      storeReceiptRows (chatController alice) `shouldReturn` [(1, Just (T.pack invoiceId), True), (1, Nothing, True)]
      heldStoreReceipts (chatController alice) `shouldReturn` 1
      rowCount cc "sx_badge_service_payments" `shouldReturn` 1

-- | A badge redeemed over a held receipt would leave the receipt waiting behind it. With no subscription check
-- the service refuses a subscription for good, though not as a refused receipt, so the record stays held.
testRedeemRefusedWhileReceiptHeld :: HasCallStack => TestParams -> IO ()
testRedeemRefusedWhileReceiptHeld ps =
  withBadgeServiceVerifier ps (\store -> (fakeVerifier store) {checkSubscription = Nothing}) $ \BadgeServiceEnv {bsClientCfg, bsController = cc, bsStore = FakeStore {appleSubscriptionJWS}} ->
    withNewTestChatCfg ps bsClientCfg "alice" aliceProfile $ \alice -> do
      let refused cmd = do
            alice ##> cmd
            alice <## "cannot get badge: store purchase open"
          finalError = "service_error final provider_not_configured"
      alice ##> ("/_badge purchase 1 " <> paymentArg (googlePayment "badge_supporter_01" googlePendingToken))
      storePurchaseOpen alice ""
      heldStoreReceiptErrors (chatController alice) `shouldEventuallyReturn` ["service_error retry payment_pending"]
      code <- issueCode cc BTSupporter 1
      refused $ "/_redeem_badge_code 1 " <> codeArg code
      rowCount (chatController alice) "badge_code_redemptions" `shouldReturn` 0
      nothingPurchased cc
      refused "/_badge invoice 1"
      storeReceiptRows (chatController alice) `shouldReturn` [(1, Nothing, True)]
      -- a Play receipt that failed for good does not block
      alice ##> "/create user alisa"
      showActiveUser alice "alisa"
      alice ##> ("/_badge purchase 2 " <> paymentArg subscriptionPlay)
      storePurchaseOpen alice ""
      heldStoreReceiptErrors (chatController alice) `shouldEventuallyReturn` ["service_error retry payment_pending", finalError]
      alice ##> ("/_redeem_badge_code 2 " <> codeArg code)
      alice <## "badge redeemed"
      alice <## "supporter badge - active"
      alice <##. "expires "
      -- an Apple subscription that failed for good still blocks, as Apple keeps charging it
      alice ##> "/create user alina"
      showActiveUser alice "alina"
      alice ##> ("/_badge purchase 3 " <> paymentArg SPApple {jws = appleSubscriptionJWS})
      storePurchaseOpen alice ""
      heldStoreReceiptErrors (chatController alice) `shouldEventuallyReturn` ["service_error retry payment_pending", finalError, finalError]
      issueCode cc BTSupporter 1 >>= \code' -> refused ("/_redeem_badge_code 3 " <> codeArg code')
      refused "/_badge invoice 3"

-- | A first Apple purchase refunded while the service could not credit it: Apple charges it no more after its expiry.
testLapsedAppleChargeStopsBlocking :: HasCallStack => TestParams -> IO ()
testLapsedAppleChargeStopsBlocking ps =
  withBadgeServiceVerifier ps (\store -> (fakeVerifier store) {verifyApple = Nothing}) $ \env@BadgeServiceEnv {bsClock, bsClientCfg, bsController = cc} ->
    withNewTestChatCfg ps bsClientCfg "alice" aliceProfile $ \alice -> do
      let a = chatController alice
          openPurchases =
            sendChatCmdStr a "/_badge state 1" >>= \case
              Right (CRBadgeState _ _ open) -> pure $ length open
              _ -> error "expected the badge state"
      bought <- addUTCTime (-3600) <$> serviceNow env
      let expires = addMonths 1 bought
      alice ##> ("/_badge purchase 1 " <> paymentArg SPApple {jws = appleTransactionDated "2000000812345690" bought expires})
      storePurchaseOpen alice ""
      heldStoreReceiptErrors a `shouldEventuallyReturn` ["service_error final provider_not_configured"]
      setClockAt bsClock $ addUTCTime (nominalDay - 60) expires
      code <- issueCode cc BTSupporter 1
      forM_ ["/_redeem_badge_code 1 " <> codeArg code, "/_badge invoice 1"] $ \cmd -> do
        alice ##> cmd
        alice <## "cannot get badge: store purchase open"
      openPurchases `shouldReturn` 1
      setClockAt bsClock $ addUTCTime (nominalDay + 60) expires
      openPurchases `shouldReturn` 0
      heldStoreReceipts a `shouldReturn` 1
      void $ createInvoice alice 1
      alice ##> ("/_redeem_badge_code 1 " <> codeArg code)
      alice <## "badge redeemed"
      alice <## "supporter badge - active"
      alice <##. "expires "
      dropOutput alice

testInvoiceRefusedWhileFirstAttemptOpen :: HasCallStack => TestParams -> IO ()
testInvoiceRefusedWhileFirstAttemptOpen ps = do
  entered <- newEmptyMVar
  release <- newEmptyMVar
  hook <- newIORef $ putMVar entered () >> takeMVar release
  let holding store = (googleVerifierWithHook hook store) {verifyTimeout = 60000000}
  withBadgeServiceVerifier ps holding $ \BadgeServiceEnv {bsClientCfg} ->
    withNewTestChatCfg ps bsClientCfg "alice" aliceProfile $ \alice -> do
      alice ##> ("/_badge purchase 1 " <> paymentArg supporterPlay)
      storePurchaseOpen alice ""
      timeout 30000000 (takeMVar entered) `shouldReturn` Just ()
      heldStoreReceipts (chatController alice) `shouldReturn` 1
      heldStoreReceiptErrors (chatController alice) `shouldReturn` []
      alice ##> "/_badge invoice 1"
      alice <## "cannot get badge: store purchase open"
      putMVar release ()
      expectLinesWithin 60 alice ["1: supporter"] []

testInvoiceUnknown :: HasCallStack => TestParams -> IO ()
testInvoiceUnknown ps =
  withBadgeServiceEnv ps $ \BadgeServiceEnv {bsClientCfg} ->
    withNewTestChatCfg ps bsClientCfg "alice" aliceProfile $ \alice -> do
      alice ##> "/create user alisa"
      showActiveUser alice "alisa"
      -- a reinstall or a second device: the store echoes an invoice this database never created
      let unknown = "0b6a5e6c-4f3e-4d51-9f55-3a0f7c2f9e11"
      alice ##> purchaseWithInvoice 2 unknown supporterPlay
      inAnyOrder alice [[invoicePurchase "" unknown], creditedLines "" unknown "1: supporter"]
      storeReceiptRows (chatController alice) `shouldReturn` [(2, Just (T.pack unknown), True)]

testInvoiceHeldByAnotherRecord :: HasCallStack => TestParams -> IO ()
testInvoiceHeldByAnotherRecord ps =
  withBadgeServiceEnv ps $ \BadgeServiceEnv {bsClientCfg, bsController = cc} ->
    withNewTestChatCfg ps bsClientCfg "alice" aliceProfile $ \alice -> do
      invoiceId <- createInvoice alice 1
      alice ##> purchaseWithInvoice 1 invoiceId (googlePayment "badge_supporter_01" "not-a-purchase")
      alice <### [StartsWith (invoicePurchase "" invoiceId), ConsoleString (refusedLine "" invoiceId "service_error final receipt_invalid")]
      alice ##> purchaseWithInvoice 1 invoiceId supporterPlay
      inAnyOrder alice [[openPurchase ""], ["1: supporter"]]
      storeReceiptRows (chatController alice) `shouldReturn` [(1, Just (T.pack invoiceId), True), (1, Nothing, True)]
      rowCount cc "sx_badge_service_badge_purchases" `shouldReturn` 1

testInvoiceAgedOut :: HasCallStack => TestParams -> IO ()
testInvoiceAgedOut ps =
  withBadgeServiceEnv ps $ \BadgeServiceEnv {bsClientCfg, bsClock = clock} ->
    withNewTestChatCfg ps bsClientCfg "alice" aliceProfile $ \alice -> do
      unpaid <- createInvoice alice 1
      presented <- createInvoice alice 1
      refused <- createInvoice alice 1
      alice ##> purchaseWithInvoice 1 refused (googlePayment "badge_supporter_01" "not-a-purchase")
      alice
        <### [ ConsoleString ("store purchase open: invoice " <> unpaid),
               ConsoleString ("store purchase open: invoice " <> presented),
               StartsWith (invoicePurchase "" refused),
               ConsoleString (refusedLine "" refused "service_error final receipt_invalid")
             ]
      alice ##> purchaseWithInvoice 1 presented (googlePayment "badge_supporter_01" googlePendingToken)
      alice <## ("store purchase open: invoice " <> unpaid)
      alice <##. ("store purchase open: invoice " <> presented <> ", transaction ")
      heldStoreReceiptErrors (chatController alice) `shouldEventuallyReturn` ["service_error retry payment_pending"]
      -- no age hides a record: one with no receipt and one held stay listed, and only a settled one goes
      getCurrentTime >>= setClockAt clock . addUTCTime (8 * nominalDay)
      alice ##> "/_badge state 1"
      alice <## ("store purchase open: invoice " <> unpaid)
      alice <##. ("store purchase open: invoice " <> presented <> ", transaction ")
      (alice </)

testInvoiceRefusedBeforeCharge :: HasCallStack => TestParams -> IO ()
testInvoiceRefusedBeforeCharge ps = do
  withNewTestChatCfg ps testCfg {badgeServiceAddress = Nothing} "bob" bobProfile $ \bob -> do
    bob ##> "/_badge invoice 1"
    bob <## "cannot get badge: badge service not configured"
    storeReceiptRows (chatController bob) `shouldReturn` []
  withBadgeServiceEnv ps $ \BadgeServiceEnv {bsClientCfg, bsController = cc} ->
    withNewTestChatCfg ps bsClientCfg "alice" aliceProfile $ \alice -> do
      code <- issueCode cc BTSupporter 1
      redeemFirstBadge alice code
      alice ##> "/_badge invoice 1"
      alice <## "cannot get badge: badge already active"
      storeReceiptRows (chatController alice) `shouldReturn` []

testInvoiceWhileBadgeHeld :: HasCallStack => TestParams -> IO ()
testInvoiceWhileBadgeHeld ps =
  withBadgeServiceEnv ps $ \BadgeServiceEnv {bsClientCfg, bsController = cc} ->
    withNewTestChatCfg ps bsClientCfg "alice" aliceProfile $ \alice -> do
      invoiceId <- createInvoice alice 1
      code <- issueCode cc BTSupporter 1
      redeemFirstBadge alice code
      alice ##> purchaseWithInvoice 1 invoiceId supporterPlay
      alice <##. "1: supporter"
      alice <##. ("store purchase open: invoice " <> invoiceId <> ", transaction ")
      storeReceiptDue (chatController alice) 1 `shouldReturn` False
      storeReceiptRows (chatController alice) `shouldReturn` [(1, Just (T.pack invoiceId), True)]
      heldStoreReceipts (chatController alice) `shouldReturn` 1
      rowCount cc "sx_badge_service_payments" `shouldReturn` 0

testInvoiceStateOtherProfile :: HasCallStack => TestParams -> IO ()
testInvoiceStateOtherProfile ps =
  withBadgeServiceEnv ps $ \BadgeServiceEnv {bsClientCfg} ->
    withNewTestChatCfg ps bsClientCfg "alice" aliceProfile $ \alice -> do
      _ <- createInvoice alice 1
      alice ##> "/create user alisa"
      showActiveUser alice "alisa"
      alice ##> "/_hide user 1 \"password\""
      alice <## "user alice:"
      alice <## "messages are hidden (use /tail to view)"
      alice <## "profile is hidden"
      invoiceId <- createInvoice alice 2
      alice ##> "/_badge state 2"
      alice <## ("store purchase open: invoice " <> invoiceId)
      (alice </)

testStoreReceiptRetried :: HasCallStack => TestParams -> IO ()
testStoreReceiptRetried ps = do
  broken <- newIORef True
  withBadgeServiceVerifier ps (brokenGoogle broken) $ \BadgeServiceEnv {bsClientCfg, bsController = cc} ->
    withNewTestChatCfg ps (fastRetryCfg bsClientCfg) "alice" aliceProfile $ \alice -> do
      invoiceId <- createInvoice alice 1
      alice ##> purchaseWithInvoice 1 invoiceId supporterPlay
      alice <##. invoicePurchase "" invoiceId
      heldStoreReceiptErrors (chatController alice) `shouldEventuallyReturn` ["service_error final internal"]
      writeIORef broken False
      mapM_ (alice <##.) $ creditedLines "" invoiceId "1: supporter"
      rowCount cc "sx_badge_service_badge_purchases" `shouldReturn` 1

testStoreReceiptNotBlocked :: HasCallStack => TestParams -> IO ()
testStoreReceiptNotBlocked ps =
  withBadgeServiceEnv ps $ \BadgeServiceEnv {bsClientCfg, bsStore = store@FakeStore {appleSupporterJWS}} ->
    withNewTestChatCfg ps bsClientCfg "alice" aliceProfile $ \alice -> do
      setGoogleDown store True
      alice ##> ("/_badge purchase 1 " <> paymentArg supporterPlay)
      storePurchaseOpen alice ""
      heldStoreReceiptErrors (chatController alice) `shouldEventuallyReturn` ["service_error retry provider_unavailable"]
      alice ##> "/_badge invoice 1"
      alice <## "cannot get badge: store purchase open"
      -- handed over with no invoice, as Restore sends it, so no settlement is announced
      alice ##> ("/_badge purchase 1 " <> paymentArg SPApple {jws = appleSupporterJWS})
      inAnyOrder alice [[openPurchase "", openPurchase ""], ["1: supporter"]]
      heldStoreReceipts (chatController alice) `shouldReturn` 1

testStoreRefusalKept :: HasCallStack => TestParams -> IO ()
testStoreRefusalKept ps = do
  hook <- newIORef (pure ())
  verifications <- countGoogleVerifications hook
  withBadgeServiceVerifier ps (googleVerifierWithHook hook) $ \BadgeServiceEnv {bsClientCfg} ->
    withNewTestChatCfg ps bsClientCfg "alice" aliceProfile $ \alice -> do
      let refused = "/_badge purchase 1 " <> paymentArg (googlePayment "badge_supporter_01" "not-a-purchase")
      alice ##> refused
      storePurchaseOpen alice ""
      -- with no invoice id the app has no key to the store transaction, so the refusal is not announced
      heldStoreReceipts (chatController alice) `shouldEventuallyReturn` 0
      (alice </)
      alice ##> refused
      alice <## "cannot get badge: badge service error: receipt_invalid"
      alice ##> "/_badge state 1"
      (alice </)
      verifications `shouldReturn` 1
      storeReceiptRows (chatController alice) `shouldReturn` [(1, Nothing, True)]

testStoreTestPurchaseRefused :: HasCallStack => TestParams -> IO ()
testStoreTestPurchaseRefused ps =
  withBadgeServiceEnv ps $ \BadgeServiceEnv {bsClientCfg, bsStore = FakeStore {appleSandboxJWS}} ->
    withNewTestChatCfg ps bsClientCfg "alice" aliceProfile $ \alice -> do
      invoiceId <- createInvoice alice 1
      let purchase = purchaseWithInvoice 1 invoiceId SPApple {jws = appleSandboxJWS}
      alice ##> purchase
      alice <### [StartsWith (invoicePurchase "" invoiceId), ConsoleString (refusedLine "" invoiceId "service_error final test_purchase")]
      heldStoreReceipts (chatController alice) `shouldReturn` 0
      alice ##> purchase
      alice <## "cannot get badge: badge service error: test_purchase"

testStoreSettledOtherProfile :: HasCallStack => TestParams -> IO ()
testStoreSettledOtherProfile ps =
  withBadgeServiceEnv ps $ \BadgeServiceEnv {bsClientCfg} ->
    withNewTestChatCfg ps bsClientCfg "alice" aliceProfile $ \alice -> do
      shown <- createInvoice alice 1
      hidden <- createInvoice alice 1
      alice ##> "/create user alisa"
      showActiveUser alice "alisa"
      -- the invoice makes alice the owner, so the hand-over's answer and the settlement are hers
      alice ##> purchaseWithInvoice 2 shown (googlePayment "badge_supporter_01" "not-a-purchase")
      alice
        <### [ StartsWith ("[user: alice] store purchase open: invoice " <> shown <> ", transaction "),
               ConsoleString ("store purchase open: invoice " <> hidden),
               ConsoleString (refusedLine "[user: alice] " shown "service_error final receipt_invalid")
             ]
      alice ##> "/_hide user 1 \"password\""
      alice <## "user alice:"
      alice <## "messages are hidden (use /tail to view)"
      alice <## "profile is hidden"
      alice ##> purchaseWithInvoice 2 hidden (googlePayment "badge_supporter_01" "not-a-purchase-either")
      (alice </)
      heldStoreReceipts (chatController alice) `shouldEventuallyReturn` 0
      (alice </)
      storeReceiptRows (chatController alice) `shouldReturn` [(1, Just (T.pack shown), True), (1, Just (T.pack hidden), True)]

testStoreReceiptTwoAtOnce :: HasCallStack => TestParams -> IO ()
testStoreReceiptTwoAtOnce ps =
  withBadgeServiceEnv ps $ \BadgeServiceEnv {bsClientCfg, bsController = cc} ->
    withNewTestChatCfg ps bsClientCfg "alice" aliceProfile $ \alice -> do
      invoiceId <- createInvoice alice 1
      let handOver = void $ sendChatCmdStr (chatController alice) (purchaseWithInvoice 1 invoiceId supporterPlay)
      concurrentlyN_ [handOver, handOver]
      mapM_ (alice <##.) $ creditedLines "" invoiceId "1: supporter"
      (alice </)
      storeReceiptRows (chatController alice) `shouldReturn` [(1, Just (T.pack invoiceId), True)]
      rowCount cc "sx_badge_service_badge_purchases" `shouldReturn` 1

testStoreReceiptCleanup :: HasCallStack => TestParams -> IO ()
testStoreReceiptCleanup ps =
  withBadgeServiceEnv ps $ \BadgeServiceEnv {bsClientCfg, bsClock, bsStore = FakeStore {appleSupporterJWS}} -> do
    let cfg = bsClientCfg {initialCleanupManagerDelay = 0, cleanupManagerInterval = 1, cleanupManagerStepDelay = 0}
    withNewTestChatCfg ps cfg "alice" aliceProfile $ \alice -> do
      unpaid <- createInvoice alice 1
      alice ##> ("/_badge purchase 1 " <> paymentArg (googlePayment "badge_supporter_01" "not-a-purchase"))
      alice <## ("store purchase open: invoice " <> unpaid)
      storePurchaseOpen alice ""
      heldStoreReceipts (chatController alice) `shouldEventuallyReturn` 0
      alice ##> ("/_badge purchase 1 " <> paymentArg SPApple {jws = appleSupporterJWS})
      alice <### [ConsoleString ("store purchase open: invoice " <> unpaid), StartsWith (openPurchase ""), StartsWith "1: supporter"]
      alice ##> ("/_badge purchase 1 " <> paymentArg (googlePayment "badge_supporter_01" googleUnreachableToken))
      alice <##. "1: supporter"
      alice <## ("store purchase open: invoice " <> unpaid)
      storePurchaseOpen alice ""
      storeReceiptRows (chatController alice) `shouldReturn` [(1, Just (T.pack unpaid), False), (1, Nothing, True), (1, Nothing, True), (1, Nothing, True)]
      -- a month on, the record no receipt reached and the refusal go; the credited and the held one stay
      testClockTime bsClock >>= setClockAt bsClock . addUTCTime (31 * nominalDay)
      -- the move also ends the badge credited above; whether its worker wakes alone or on this signal, its lines are read here
      alice `send` "/_app activate"
      alice
        <### [ "/_app activate",
               "ok",
               StartsWith "badge alert: support_ended ",
               StartsWith "1: supporter",
               StartsWith "badge alert: support_ended "
             ]
      storeReceiptRows (chatController alice) `shouldEventuallyReturn` [(1, Nothing, True), (1, Nothing, True)]
      heldStoreReceipts (chatController alice) `shouldReturn` 1

testStoreReceiptAttemptThrows :: HasCallStack => TestParams -> IO ()
testStoreReceiptAttemptThrows ps = do
  broken <- newIORef True
  withBadgeServiceVerifier ps (brokenGoogle broken) $ \BadgeServiceEnv {bsClientCfg} ->
    withNewTestChatCfg ps (fastRetryCfg bsClientCfg) "alice" aliceProfile $ \alice -> do
      alice ##> ("/_badge purchase 1 " <> paymentArg supporterPlay)
      storePurchaseOpen alice ""
      heldStoreReceiptErrors (chatController alice) `shouldEventuallyReturn` ["service_error final internal"]
      -- a stored payment this version cannot read makes the attempt throw before anything is sent
      setStoreReceiptPayment (chatController alice) "not a payment"
      heldStoreReceiptErrors (chatController alice) `shouldEventuallyReturn` ["unexpected held store payment does not decode"]
      (alice </)
      heldStoreReceipts (chatController alice) `shouldReturn` 1

testStoreReceiptAfterRestart :: HasCallStack => TestParams -> IO ()
testStoreReceiptAfterRestart ps = do
  broken <- newIORef True
  withBadgeServiceVerifier ps (brokenGoogle broken) $ \BadgeServiceEnv {bsClientCfg} -> do
    let cfg = fastRetryCfg bsClientCfg
    invoiceId <- withNewTestChatCfg ps cfg "alice" aliceProfile $ \alice -> do
      invoiceId <- createInvoice alice 1
      alice ##> purchaseWithInvoice 1 invoiceId supporterPlay
      alice <##. invoicePurchase "" invoiceId
      heldStoreReceiptErrors (chatController alice) `shouldEventuallyReturn` ["service_error final internal"]
      pure invoiceId
    writeIORef broken False
    withTestChatCfg ps cfg "alice" $ \alice -> do
      expectLinesWithin 60 alice (creditedLines "" invoiceId "1: supporter") ["subscribed 1 connections on server localhost"]
      heldStoreReceipts (chatController alice) `shouldReturn` 0

testPlayHoldsNoOtherRequest :: HasCallStack => TestParams -> IO ()
testPlayHoldsNoOtherRequest ps = do
  entered <- newEmptyMVar
  release <- newEmptyMVar
  hook <- newIORef $ putMVar entered () >> takeMVar release
  -- past the 20s line wait, so a request queued behind the held call cannot be answered in time
  let holding store = (googleVerifierWithHook hook store) {verifyTimeout = 60000000}
  withBadgeServiceVerifier ps holding $ \BadgeServiceEnv {bsClientCfg, bsAddress, bsController = cc, bsStore = FakeStore {appleSupporterJWS}} ->
    withNewTestChatCfg ps bsClientCfg "alice" aliceProfile $ \alice ->
      withNewTestChatCfg ps bsClientCfg "bob" bobProfile $ \bob ->
        withNewTestChatCfg ps bsClientCfg "cath" cathProfile $ \cath -> do
          alice ##> ("/_badge purchase 1 " <> paymentArg supporterPlay)
          storePurchaseOpen alice ""
          timeout 30000000 (takeMVar entered) `shouldReturn` Just ()
          code <- issueCode cc BTSupporter 1
          bob ##> ("/_redeem_badge_code 1 " <> codeArg code)
          bob <## "badge redeemed"
          bob <## "supporter badge - active"
          bob <##. "expires "
          cath ##> ("/_badge purchase 1 " <> paymentArg SPApple {jws = appleSupporterJWS})
          cath <### [StartsWith (openPurchase ""), StartsWith "1: supporter"]
          g <- C.newRandom
          (upgradeKey, upgradePriv) <- atomically $ C.generateKeyPair g :: IO (C.KeyPair 'C.Ed25519)
          (fromPurchaseKey, fromPriv) <- atomically $ C.generateKeyPair g :: IO (C.KeyPair 'C.Ed25519)
          masterKey <- generateMasterKey g
          now <- getCurrentTime
          let upgrade = BadgeUpgrade {fromPurchaseKey, receipt = "receipt", receiptSignature = C.sign' fromPriv "receipt", balance = BadgeBalance {lastEntry = emptyEntry now BTSupporter}}
              upgradeReq = LB.unpack $ J.encode $ requestObject upgradeKey BSCPurchaseBadge {masterKey, payment = supporterPlay, upgrade = Just upgrade}
          bob ##> ("/_service_request 1 " <> bsAddress <> " sign_key=" <> B.unpack (strEncode $ C.StoredPrivateKey upgradePriv) <> " " <> upgradeReq)
          bob <## "service response: {\"code\":\"bad_request\",\"type\":\"error\"}"
          putMVar release ()
          expectLinesWithin 60 alice ["1: supporter"] []
          rowCount cc "sx_badge_service_badge_purchases" `shouldReturn` 3

testStoreChecksConfigured :: HasCallStack => TestParams -> IO ()
testStoreChecksConfigured _ = do
  createDirectoryIfMissing True "tests/tmp"
  withTempDirectory "tests/tmp" "store-checks" $ \d -> do
    writeFile (d </> "badge_service.ini") "[listener]\nstatic_dir = /srv/badges\n"
    Right cfg <- readServiceConfig (d </> "badge_service.ini")
    let checksFor c = (\ServiceState {storeVerifier} -> storeChecks storeVerifier) <$> (newServiceState >>= configureStoreVerifier c)
        creditsFor c = (\ServiceState {storeVerifier} -> creditTestPurchases storeVerifier) <$> (newServiceState >>= configureStoreVerifier c)
        dev accept during credit = Just cfg {devAcceptUnverifiedStoreReceipts = accept, devCheckStoreDuringCoverage = during, devCreditTestPurchases = credit}
    checksFor Nothing `shouldReturn` ChecksAfterCoverage
    checksFor (dev False False False) `shouldReturn` ChecksAfterCoverage
    checksFor (dev True False False) `shouldReturn` ChecksAfterCoverage
    -- refused by the parser, and still not armed if set by hand
    checksFor (dev False True False) `shouldReturn` ChecksAfterCoverage
    checksFor (dev True True False) `shouldReturn` ChecksAtEveryRequest
    checksFor (dev False False True) `shouldReturn` ChecksAfterCoverage
    checksFor (dev False True True) `shouldReturn` ChecksAtEveryRequest
    creditsFor Nothing `shouldReturn` False
    creditsFor (dev False False False) `shouldReturn` False
    creditsFor (dev False False True) `shouldReturn` True
    -- the mock never reports a test purchase
    creditsFor (dev True False True) `shouldReturn` False

testStoreRouting :: HasCallStack => TestParams -> IO ()
testStoreRouting ps =
  withBadgeServiceEnv ps $ \env@BadgeServiceEnv {bsClock, bsController = cc, bsStore = store@FakeStore {appleSubscriptionJWS, appleLegendSubscriptionJWS, appleSupporterJWS}} -> do
    let routedAs signer key cmd = requestLane ChecksAfterCoverage cc (Just signer) (requestObject key cmd)
        routed key = routedAs key key
    (purchaseKey, masterKey) <- newPurchaseKeys
    let purchase payment = routed purchaseKey $ purchaseCmd masterKey payment
    purchase supporterPlay `shouldReturn` PurchaseLane
    purchase subscriptionPlay `shouldReturn` PurchaseLane
    purchase SPApple {jws = appleSubscriptionJWS} `shouldReturn` PurchaseLane
    purchase SPApple {jws = appleSupporterJWS} `shouldReturn` MainLane
    routed purchaseKey (transferCmd masterKey subscriptionPlay) `shouldReturn` PurchaseLane
    routed purchaseKey (transferCmd masterKey SPApple {jws = appleSubscriptionJWS}) `shouldReturn` PurchaseLane
    routed purchaseKey (transferCmd masterKey SPApple {jws = appleSupporterJWS}) `shouldReturn` MainLane
    g <- C.newRandom
    (fromPurchaseKey, fromPriv) <- atomically $ C.generateKeyPair g :: IO (C.KeyPair 'C.Ed25519)
    now <- getCurrentTime
    let upgrade = BadgeUpgrade {fromPurchaseKey, receipt = "receipt", receiptSignature = C.sign' fromPriv "receipt", balance = BadgeBalance {lastEntry = emptyEntry now BTSupporter}}
    routed purchaseKey BSCPurchaseBadge {masterKey, payment = supporterPlay, upgrade = Just upgrade} `shouldReturn` MainLane
    code <- issueCode cc BTSupporter 1
    codeKeys@(codeKey, codeMasterKey) <- newPurchaseKeys
    routed codeKey BSCRedeemBadgeCode {masterKey = codeMasterKey, code = badgeCodeText code} `shouldReturn` MainLane
    redeemed <- redeemWithKeys env codeKeys code
    routed codeKey BSCIssueBadge {balance = BadgeBalance {lastEntry = last $ fst $ statementOf redeemed}} `shouldReturn` MainLane
    (subscriberKey, firstStart, purchased) <- subscribedMonthly env
    let issue = BSCIssueBadge {balance = BadgeBalance {lastEntry = last purchased}}
        coverageEnd = addMonths 1 firstStart
        routedAt t = setClockAt bsClock t >> routed subscriberKey issue
        answeredWithoutStore = do
          checks <- subscriptionChecks store
          _ <- assertBalance env subscriberKey (last purchased)
          subscriptionChecks store `shouldReturn` checks
    routedAt (addUTCTime (-90) coverageEnd) `shouldReturn` MainLane
    routedAt (addUTCTime (-30) coverageEnd) `shouldReturn` RenewalLane
    answeredWithoutStore
    storeReports store $ subscriptionCheck SSBillingRetry Nothing
    routedAt (addUTCTime 60 coverageEnd) `shouldReturn` RenewalLane
    checks <- subscriptionChecks store
    _ <- assertBalance env subscriberKey (last purchased)
    subscriptionChecks store `shouldReturn` checks + 1
    [Just lastCheck] <- subscriptionCheckedAt cc
    -- within the hour a due month still queues on the renewal lane, where it is answered from the record
    routedAt (addUTCTime 3530 lastCheck) `shouldReturn` RenewalLane
    answeredWithoutStore
    routedAt (addUTCTime 3700 lastCheck) `shouldReturn` RenewalLane
    routedAs purchaseKey subscriberKey issue `shouldReturn` MainLane
    let upgradeCmd = BSCUpgradeBadgeSubscription {payment = SPApple {jws = appleSubscriptionJWS}, balance = BadgeBalance {lastEntry = last purchased}}
    routed subscriberKey upgradeCmd `shouldReturn` MainLane
    routedAs purchaseKey subscriberKey upgradeCmd `shouldReturn` MainLane
    routed codeKey upgradeCmd `shouldReturn` MainLane
    (unknownKey, _) <- newPurchaseKeys
    routed unknownKey issue `shouldReturn` MainLane
    routed unknownKey upgradeCmd `shouldReturn` MainLane
    -- an App Store subscription's upgrade may ask the store once the hour since its last check has passed
    let boughtAt = addUTCTime 7200 lastCheck
    setClockAt bsClock boughtAt
    appleStoreReports store $ subscriptionCheck SSRenewing $ Just $ storeCharge "SUBSCR_BADGE_SUPPORTER_MONTH_01" (addUTCTime (-3600) boughtAt) 1
    (appleKey, appleMasterKey) <- newPurchaseKeys
    bought <- serviceCmd env appleKey $ purchaseCmd appleMasterKey SPApple {jws = appleSubscriptionJWS}
    let appleUpgrade = upgradeCmdWith appleLegendSubscriptionJWS $ last $ fst $ statementOf bought
        upgradeRoutedAt t = setClockAt bsClock t >> routed appleKey appleUpgrade
    upgradeRoutedAt (addUTCTime 3530 boughtAt) `shouldReturn` MainLane
    upgradeRoutedAt (addUTCTime 3550 boughtAt) `shouldReturn` RenewalLane
    upgradeRoutedAt (addUTCTime 3700 boughtAt) `shouldReturn` RenewalLane
    routedAs purchaseKey appleKey appleUpgrade `shouldReturn` MainLane

testRenewalHoldsNoOtherRequest :: HasCallStack => TestParams -> IO ()
testRenewalHoldsNoOtherRequest ps = do
  entered <- newEmptyMVar
  release <- newEmptyMVar
  -- the purchase asks the store too, so the hold is armed once it is credited
  hook <- newIORef (pure ())
  let holding store = (subscriptionCheckWithHook hook store) {verifyTimeout = 60000000}
  withBadgeServiceVerifier ps holding $ \env@BadgeServiceEnv {bsClientCfg, bsClock, bsController = cc, bsStore = store@FakeStore {appleSupporterJWS}} -> do
    -- her request outlives the hold, so her pass does not give up and send it again
    let patientCfg = bsClientCfg {agentConfig = (agentConfig bsClientCfg) {serviceRequestTimeout = 120}}
    withNewTestChatCfg ps patientCfg "alice" aliceProfile $ \alice ->
      withNewTestChatCfg ps bsClientCfg "bob" bobProfile $ \bob ->
        withNewTestChatCfg ps bsClientCfg "cath" cathProfile $ \cath -> do
          (firstStart, rows) <- clientSubscribedMonthly env alice
          let (requestAt, _) = renewalMoments rows
              renewalStart = addMonths 1 firstStart
          writeIORef hook $ putMVar entered () >> takeMVar release
          storeReports store $ subscriptionCheck SSRenewing $ Just $ monthlyCharge "subscr-badge-supporter-month-02" firstStart 1
          setClockAt bsClock requestAt
          alice ##> "/_app activate"
          alice <## "ok"
          timeout 30000000 (takeMVar entered) `shouldReturn` Just ()
          code <- issueCode cc BTSupporter 1
          bob ##> ("/_redeem_badge_code 1 " <> codeArg code)
          bob <## "badge redeemed"
          bob <## "supporter badge - active"
          bob <##. "expires "
          cath ##> ("/_badge purchase 1 " <> paymentArg SPApple {jws = appleSupporterJWS})
          cath <### [StartsWith (openPurchase ""), StartsWith "1: supporter"]
          withNewTestChatCfg ps bsClientCfg "dan" danProfile $ \dan -> do
            dan ##> ("/_badge purchase 1 " <> paymentArg supporterPlay)
            dan <### [StartsWith (openPurchase ""), StartsWith "1: supporter"]
          putMVar release ()
          alice <##. "1: supporter"
          renewed <- waitLedgerRows (chatController alice) 4
          drop 2 (rowChanges renewed) `shouldBe` [(1, 1, renewalStart, Just "charge"), (-1, 0, addMonths 1 renewalStart, Just "badge")]

testPurchaseHoldsNoRenewal :: HasCallStack => TestParams -> IO ()
testPurchaseHoldsNoRenewal ps = do
  entered <- newEmptyMVar
  release <- newEmptyMVar
  hook <- newIORef $ putMVar entered () >> takeMVar release
  let holding store = (googleVerifierWithHook hook store) {verifyTimeout = 60000000}
  withBadgeServiceVerifier ps holding $ \env@BadgeServiceEnv {bsClientCfg, bsClock, bsStore = store} ->
    withNewTestChatCfg ps bsClientCfg "alice" aliceProfile $ \alice ->
      withNewTestChatCfg ps bsClientCfg "bob" bobProfile $ \bob -> do
        (firstStart, rows) <- clientSubscribedMonthly env alice
        let (requestAt, _) = renewalMoments rows
            renewalStart = addMonths 1 firstStart
        bob ##> ("/_badge purchase 1 " <> paymentArg supporterPlay)
        storePurchaseOpen bob ""
        timeout 30000000 (takeMVar entered) `shouldReturn` Just ()
        storeReports store $ subscriptionCheck SSRenewing $ Just $ monthlyCharge "subscr-badge-supporter-month-02" firstStart 1
        setClockAt bsClock requestAt
        alice ##> "/_app activate"
        alice <## "ok"
        alice <##. "1: supporter"
        renewed <- waitLedgerRows (chatController alice) 4
        drop 2 (rowChanges renewed) `shouldBe` [(1, 1, renewalStart, Just "charge"), (-1, 0, addMonths 1 renewalStart, Just "badge")]
        putMVar release ()
        expectLinesWithin 60 bob ["1: supporter"] []

testAppleSubscriptionHoldsNoOtherRequest :: HasCallStack => TestParams -> IO ()
testAppleSubscriptionHoldsNoOtherRequest ps = do
  entered <- newEmptyMVar
  release <- newEmptyMVar
  hook <- newIORef $ putMVar entered () >> takeMVar release
  let holding store = (subscriptionCheckWithHook hook store) {verifyTimeout = 60000000}
  withBadgeServiceVerifier ps holding $ \env@BadgeServiceEnv {bsClientCfg, bsController = cc, bsStore = store@FakeStore {appleSubscriptionJWS}} ->
    withNewTestChatCfg ps bsClientCfg "dan" danProfile $ \dan ->
      withNewTestChatCfg ps bsClientCfg "bob" bobProfile $ \bob -> do
        now <- serviceNow env
        setSubscription store (SubscriptionApple appleSubscriptionOriginalId) $ subscriptionCheck SSRenewing $ Just $ storeCharge "SUBSCR_BADGE_SUPPORTER_MONTH_01" (addUTCTime (-3600) now) 1
        dan ##> ("/_badge purchase 1 " <> paymentArg SPApple {jws = appleSubscriptionJWS})
        storePurchaseOpen dan ""
        timeout 30000000 (takeMVar entered) `shouldReturn` Just ()
        code <- issueCode cc BTSupporter 1
        bob ##> ("/_redeem_badge_code 1 " <> codeArg code)
        bob <## "badge redeemed"
        bob <## "supporter badge - active"
        bob <##. "expires "
        putMVar release ()
        expectLinesWithin 60 dan ["1: supporter"] []
        rowCount cc "sx_badge_service_badge_purchases" `shouldReturn` 2

createInvoice :: HasCallStack => TestCC -> Int -> IO String
createInvoice cc userId = do
  cc ##> ("/_badge invoice " <> show userId)
  line <- getTermLine cc
  maybe (error $ "expected an invoice, got " <> line) pure $ stripPrefix "badge invoice: " line

purchaseWithInvoice :: Int -> String -> ServicePayment -> String
purchaseWithInvoice userId invoiceId payment = "/_badge purchase " <> show userId <> " invoice=" <> invoiceId <> " " <> paymentArg payment

-- | Each store purchase record's profile, invoice id, and whether a receipt reached it.
storeReceiptRows :: ChatController -> IO [(Int64, Maybe Text, Bool)]
storeReceiptRows ChatController {chatStore} =
  withTransaction chatStore $ \db ->
    DB.query_ db $
      "SELECT user_id, invoice_id, transaction_ref IS NOT NULL "
        <> "FROM badge_store_receipts ORDER BY badge_store_receipt_id"

storeReceiptDue :: ChatController -> Int64 -> IO Bool
storeReceiptDue ChatController {chatStore} userId =
  withTransaction chatStore $ \db ->
    getCurrentTime >>= getNextHeldStoreReceipt db userId >>= either (error . show) (pure . isJust)

heldStoreReceipts :: ChatController -> IO Int
heldStoreReceipts ChatController {chatStore} =
  withTransaction chatStore $ \db -> do
    [Only n] <- DB.query_ db "SELECT COUNT(*) FROM badge_store_receipts WHERE payment IS NOT NULL"
    pure n

heldStoreReceiptErrors :: ChatController -> IO [Text]
heldStoreReceiptErrors ChatController {chatStore} =
  withTransaction chatStore $ \db ->
    map fromOnly <$> DB.query_ db "SELECT credit_error FROM badge_store_receipts WHERE payment IS NOT NULL AND credit_error IS NOT NULL"

setStoreReceiptPayment :: ChatController -> Text -> IO ()
setStoreReceiptPayment ChatController {chatStore} payment =
  withTransaction chatStore $ \db -> DB.execute db "UPDATE badge_store_receipts SET payment = ?" (Only payment)

fastRetryCfg :: ChatConfig -> ChatConfig
fastRetryCfg cfg = cfg {badgeRetryInterval = RetryInterval {initialInterval = 100000, increaseAfter = 0, maxInterval = 500000}}

-- | The fake store's Google verdicts, failing as a verifier bug while broken is set: internal, with no retryAfter.
brokenGoogle :: IORef Bool -> FakeStore -> StoreVerifier
brokenGoogle broken FakeStore {fakeVerifier = v@StoreVerifier {verifyGoogle}} =
  v {verifyGoogle = unlessBroken <$> verifyGoogle}
  where
    unlessBroken verify productId token =
      readIORef broken >>= \case
        True -> pure $ Left $ SRVerifierFailed "fake verifier bug"
        False -> verify productId token

-- | Each block's lines in order, the blocks in any order: the worker is signalled before the command that
-- signalled it has answered, so its events can print first. Every line is matched as a prefix.
inAnyOrder :: HasCallStack => TestCC -> [[String]] -> IO ()
inAnyOrder _ [] = pure ()
inAnyOrder cc blocks = do
  line <- getTermLine cc
  case break (startsWith line) blocks of
    (earlier, (_ : rest) : others) -> do
      mapM_ (cc <##.) rest
      inAnyOrder cc (earlier <> others)
    _ -> error $ "expected a line starting one of " <> show blocks <> ", got " <> show line
  where
    startsWith l = \case
      start : _ -> start `isPrefixOf` l
      [] -> False

-- | The required lines under one budget in any order, and the tolerated ones only if they come at
-- all: the agent announces UP for a subscription that was still pending in the session answering it
-- (simplexmq processSubOk), so "subscribed N connections" may never be printed, and when it is it
-- races a settlement that arrived in the same batch. getTermLine' would bound each line at 20s.
expectLinesWithin :: HasCallStack => Int -> TestCC -> [String] -> [String] -> IO ()
expectLinesWithin seconds cc@TestCC {termQ, printOutput} required tolerated = go (seconds * 10) required
  where
    go n outstanding =
      100000 `timeout` atomically (readTQueue termQ) >>= \case
        Just line -> do
          when printOutput $ userName cc >>= \name -> putStrLn $ name <> ": " <> line
          case break (`isPrefixOf` line) outstanding of
            (earlier, _ : others) -> go n (earlier <> others)
            _
              | any (`isPrefixOf` line) tolerated -> go n outstanding
              | otherwise -> error $ "unexpected " <> show line <> ", awaiting " <> show outstanding
        Nothing
          | null outstanding -> pure ()
          | n > 0 -> go (n - 1) outstanding
          | otherwise -> error $ "no line starting one of " <> show outstanding <> " in " <> show seconds <> " seconds"

-- | Counts the Google verifications the service makes, re-arming the one-shot hook each time.
countGoogleVerifications :: IORef (IO ()) -> IO (IO Int)
countGoogleVerifications hook = do
  n <- newIORef 0
  let count = modifyIORef' n (+ 1) >> writeIORef hook count
  writeIORef hook count
  pure $ readIORef n

storePurchaseOpen :: HasCallStack => TestCC -> String -> IO ()
storePurchaseOpen cc userPrefix = cc <##. openPurchase userPrefix

openPurchase :: String -> String
openPurchase userPrefix = userPrefix <> "store purchase open: invoice none, transaction "

invoicePurchase :: String -> String -> String
invoicePurchase userPrefix invoiceId = userPrefix <> "store purchase open: invoice " <> invoiceId <> ", transaction "

-- | The worker's credit as the owner's terminal shows it: the badge, then the settlement.
creditedLines :: String -> String -> String -> [String]
creditedLines userPrefix invoiceId badge = [userPrefix <> badge, userPrefix <> "store purchase credited: invoice " <> invoiceId]

refusedLine :: String -> String -> String -> String
refusedLine userPrefix invoiceId failure = userPrefix <> "store purchase refused: invoice " <> invoiceId <> ", " <> failure

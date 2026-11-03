{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module BadgeService.Service
  ( ServiceState (..),
    newServiceState,
    configureStoreVerifier,
    welcomeGetOpts,
    checkIssuerKey,
    badgeService,
    badgeServiceCLI,
    badgeServiceResponse,
    RequestLane (..),
    requestLane,
    PurchaseChecks (..),
    ClaimChecks,
    ClaimKey (..),
    ClaimPurpose (..),
    newPurchaseChecks,
    purchaseCheckWait,
    maxPurchaseChecks,
    PlayReads,
    playReadWait,
    maxPlayPurchaseReads,
    badgeErrorRetryAfter,
    grantableCharge,
    NotGranted (..),
    shownServiceRequest,
    survive,
    IssueCodeOpts (..),
    issueBadgeCode,
  )
where

import BadgeService.Catalog (StoreProduct (..), defaultCatalog, storeProduct)
import BadgeService.Codes (issueFailedText, issueOneCode, singleUse)
import BadgeService.Config (AppleStoreConfig (..), BadgeIssuerKey (..), PlayStoreConfig (..), ServiceConfig (..), readServiceConfig)
import BadgeService.Group (GroupEvent, ensureManagedGroup, groupEvent, hasTracker, refreshTracker, revokeWithTracker, runGroupLane)
import BadgeService.Group.Command (badgeTypeP, codeP, maxMonths, textTokenP)
import BadgeService.Log (logError, logInfo, logWarn)
import BadgeService.Options
import BadgeService.Poller (newPollerEnv, newReadHints, runPoller)
import BadgeService.Providers.BTCPay (btcpayProvider)
import BadgeService.Providers.Stripe (stripeProvider)
import BadgeService.Store
import BadgeService.Store.Invoices (seedCatalog, truncateToSecond)
import BadgeService.Store.Migrate (runBadgeServiceMigrations)
import BadgeService.StoreReceipts
import BadgeService.StoreReceipts.Apple (appleSubscriptionChecker, readAppleRoot, verifyAppleTransaction)
import BadgeService.StoreReceipts.Google (PlayVerifier (..), playStoreVerifier)
import BadgeService.StoreReceipts.Mock (newMockStoreVerifier)
import BadgeService.Waiters (Waiters, newWaiters)
import BadgeService.Web.Server (Bucket, exportWebapp, newWebEnv, runWebListener, takeFrom)
import Control.Applicative (optional)
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as J
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Attoparsec.ByteString.Char8 as A
import Data.ByteString.Char8 (ByteString)
import Data.Either (fromRight)
import Data.Functor (($>), (<&>))
import Data.Int (Int64)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, isJust, isNothing, maybeToList)
import qualified Data.Text as T
import Data.Time.Clock (NominalDiffTime, UTCTime, addUTCTime, diffUTCTime, getCurrentTime)
import Data.Word (Word32)
import Simplex.Chat.Badges
import Simplex.Chat.Badges.Code
import Simplex.Chat.Badges.Ledger
import Simplex.Chat.Badges.Service
import Simplex.Chat.Badges.StorePlan (storePlan)
import Simplex.Chat.Badges.Types (BadgeCodePaymentStatus (..))
import Simplex.Chat.Bot (initializeBotAddress')
import Simplex.Chat.Bot.Store (withDB, withDB')
import Simplex.Chat.Controller
import Simplex.Chat.Core (sendChatCmd, simplexChatCore)
import Simplex.Chat.Options (printDbOpts)
import Simplex.Chat.PaymentService (AppleTransactionFacts (..), ServicePayment (..), appleTransactionFacts)
import Simplex.Chat.PaymentService.Types (PaymentProvider (..), StoreTransactionRef (..))
import Simplex.Chat.Terminal (terminalChatConfig)
import Simplex.Chat.Terminal.Main (simplexChatCLI')
import Simplex.Chat.Types (AgentInvId (..), User (..))
import Simplex.Messaging.Agent.Store.Common (DBStore)
import qualified Simplex.Messaging.Agent.Store.DB as DB
import qualified Simplex.Messaging.Crypto as C
import Simplex.Messaging.Crypto.BBS (bbsPublicKey)
import Simplex.Messaging.Encoding.String (strEncode)
import Simplex.Messaging.Util (catchOwn', raceAny_, safeDecodeUtf8, tshow)
import Simplex.Messaging.Version (isCompatible)
import System.Directory (getAppUserDataDirectory)
import System.Exit (exitFailure)

data ServiceState = ServiceState
  { serviceCC :: TMVar ChatController,
    serviceRequestQ :: TQueue (User, AgentInvId, Maybe C.PublicKeyEd25519, J.Object),
    groupEventQ :: TQueue GroupEvent,
    storeVerifier :: StoreVerifier
  }

-- | With no store verifier, which badge_service.ini configures, every store receipt not already credited
-- is answered provider_not_configured.
newServiceState :: IO ServiceState
newServiceState = do
  serviceCC <- newEmptyTMVarIO
  serviceRequestQ <- newTQueueIO
  groupEventQ <- newTQueueIO
  pure ServiceState {serviceCC, serviceRequestQ, groupEventQ, storeVerifier = noStoreVerifier}

welcomeGetOpts :: IO BadgeServiceOpts
welcomeGetOpts = do
  appDir <- getAppUserDataDirectory "simplex"
  opts@BadgeServiceOpts {coreOptions, testing, serviceName} <- getBadgeServiceOpts appDir "simplex_badge_service"
  unless testing $ do
    putStrLn $ "SimpleX Badge Service v" ++ versionNumber
    printDbOpts coreOptions
    putStrLn $ "Service name: " ++ T.unpack serviceName
  pure opts

checkIssuerKey :: BadgeServiceOpts -> Maybe ServiceConfig -> ChatConfig -> IO (Either String BadgeIssuerKey)
checkIssuerKey BadgeServiceOpts {issuerKey} serviceCfg cfg = case issuerKey of
  Left e -> pure (Left e)
  Right (Just k) -> checkOne cfg k
  Right Nothing -> case serviceCfg >>= issuer of
    Nothing -> pure $ Left "an issuer key is required - pass --issuer-key-idx and --issuer-secret, or add an [issuer] section to badge_service.ini (see `simplex-chat badge keygen`)"
    Just k -> checkOne cfg k

checkOne :: ChatConfig -> BadgeIssuerKey -> IO (Either String BadgeIssuerKey)
checkOne ChatConfig {badgePublicKeys} k@BadgeIssuerKey {keyIdx, secretKey} =
  bbsPublicKey secretKey >>= \case
    Left e -> pure $ Left $ "issuer secret at index " <> show keyIdx <> " is not a valid key: " <> e
    Right pk -> pure $ case M.lookup keyIdx badgePublicKeys of
      Just pk' | pk' == pk -> Right k
      Just _ -> Left $ "issuer secret does not match the configured key at index " <> show keyIdx <> ", its public key is " <> T.unpack (safeDecodeUtf8 $ strEncode pk)
      Nothing -> Left $ "no configured badge key at index " <> show keyIdx <> ", clients could not verify what this service signs"

requireIssuerKey :: BadgeServiceOpts -> Maybe ServiceConfig -> ChatConfig -> IO BadgeIssuerKey
requireIssuerKey opts serviceCfg cfg =
  checkIssuerKey opts serviceCfg cfg >>= either (\e -> putStrLn ("Error: " <> e) >> exitFailure) (\k@BadgeIssuerKey {keyIdx} -> logInfo ("signing badges with issuer key " <> tshow keyIdx) $> k)

readConfigOrExit :: FilePath -> IO ServiceConfig
readConfigOrExit path =
  readServiceConfig path >>= \case
    Left e -> putStrLn (path <> ": " <> e) >> exitFailure
    Right sc -> pure sc

-- | Only one of the mock and the real verifiers applies: the config refuses
-- accept_unverified_store_receipts beside [apple] or [google]. The other two dev settings,
-- credit_test_purchases and check_store_during_coverage, are meant to sit beside a real verifier.
configureStoreVerifier :: Maybe ServiceConfig -> ServiceState -> IO ServiceState
configureStoreVerifier serviceCfg env@ServiceState {serviceCC, storeVerifier = v} = case serviceCfg of
  Just ServiceConfig {devAcceptUnverifiedStoreReceipts = True, devCheckStoreDuringCoverage} -> do
    logWarn "[dev] accept_unverified_store_receipts is on: any store receipt is accepted without verification"
    -- the badge clock is read at each check, as the controller that holds it starts after this
    mock <- newMockStoreVerifier $ atomically (readTMVar serviceCC) >>= badgeNow
    pure env {storeVerifier = mock {storeChecks = if devCheckStoreDuringCoverage then ChecksAtEveryRequest else ChecksAfterCoverage}}
  Just ServiceConfig {appleStore, playStore, devCheckStoreDuringCoverage, devCreditTestPurchases} -> do
    when devCreditTestPurchases $ logWarn "[dev] credit_test_purchases is on: a purchase the store calls a test is credited as a real one"
    apple <- forM appleStore $ \AppleStoreConfig {aBundleId, aRootCertificates, aServerApi} -> do
      roots <- mapM (orExit . readAppleRoot) aRootCertificates
      appleSubscriptions <- forM aServerApi $ orExit . appleSubscriptionChecker roots aBundleId
      pure (verifyAppleTransaction roots aBundleId, appleSubscriptions)
    google <- forM playStore $ orExit . playStoreVerifier
    forM_ appleStore $ \AppleStoreConfig {aBundleId, aServerApi} ->
      logInfo $ "verifying App Store receipts for " <> aBundleId <> maybe ", but not its subscriptions: [apple] sets no API key" (const "") aServerApi
    forM_ playStore $ \PlayStoreConfig {gPackageName} -> logInfo $ "verifying Play purchases for " <> gPackageName
    pure
      env
        { storeVerifier =
            v
              { verifyApple = maybe (verifyApple v) (Just . fst) apple,
                verifyGoogle = maybe (verifyGoogle v) (Just . verifyPlayPurchase) google,
                acknowledgeGoogle = maybe (acknowledgeGoogle v) (Just . acknowledgePlayPurchase) google,
                acknowledgeGoogleSubscription = maybe (acknowledgeGoogleSubscription v) (Just . acknowledgePlaySubscription) google,
                checkSubscription = case (snd =<< apple, checkPlaySubscription <$> google) of
                  (Nothing, Nothing) -> checkSubscription v
                  (checkApple_, checkGoogle_) -> Just $ \ref _ -> case ref of
                    SubscriptionApple originalId -> maybe notConfigured ($ originalId) checkApple_
                    SubscriptionGoogle token -> maybe notConfigured ($ token) checkGoogle_,
                storeChecks = if devCheckStoreDuringCoverage && devCreditTestPurchases then ChecksAtEveryRequest else ChecksAfterCoverage,
                creditTestPurchases = devCreditTestPurchases
              }
        }
  Nothing -> pure env
  where
    orExit = (>>= either (\e -> putStrLn ("Error: " <> e) >> exitFailure) pure)
    notConfigured = pure $ Left SRNotConfigured

badgeService :: BadgeServiceOpts -> ChatConfig -> ServiceState -> IO ()
badgeService opts@BadgeServiceOpts {serviceConfigFile} cfg env = do
  serviceCfg <- traverse readConfigOrExit serviceConfigFile
  key <- requireIssuerKey opts serviceCfg cfg
  waiters <- newWaiters
  requestEnv <- configureStoreVerifier serviceCfg env
  let groupCfg = serviceCfg >>= \ServiceConfig {group} -> group
      trackerQ_ = groupEventQ env <$ groupCfg
      readEvents cc =
        forever $
          atomically (readTBQueue $ outputQ cc) >>= \case
            (_, Right (CEvtServiceRequest u reqId sigKey reqData)) ->
              atomically $ writeTQueue (serviceRequestQ env) (u, reqId, sigKey, reqData)
            (_, Right ev)
              | isJust groupCfg -> forM_ (groupEvent ev) $ atomically . writeTQueue (groupEventQ env)
            _ -> pure ()
      startLanes cc = do
        lanes <- maybe (pure []) (serviceLanes waiters cc) serviceCfg
        -- The group is resolved before the other lanes start, so a failed lookup exits at once rather than waiting for them to end.
        groupLane_ <- forM groupCfg $ \gc -> runGroupLane cc (groupEventQ env) <$> ensureManagedGroup cc gc
        raceAny_ $ processQueuedRequests key trackerQ_ requestEnv : maybeToList groupLane_ <> lanes
      chatHooks =
        defaultChatHooks
          { preStartHook = Just $ badgePreStartHook opts,
            postStartHook = Just $ badgePostStartHook opts env,
            preCmdHook = Just badgeCmdHook
          }
  -- The reader runs from the start and must not block, since outputQ carries every chat event and a full queue stalls the core.
  simplexChatCore cfg {chatHooks} (mkChatOpts opts) $ \_ cc -> raceAny_ [readEvents cc, startLanes cc]
  where
    serviceLanes :: Waiters -> ChatController -> ServiceConfig -> IO [IO ()]
    serviceLanes ws ChatController {chatStore} sc = do
      -- Seed before the listener accepts anything, since every checkout is priced from these.
      seedServiceCatalog chatStore
      btc <- maybe (pure []) (fmap (: []) . btcpayProvider) (btcpay sc)
      str <- maybe (pure []) (fmap (: []) . stripeProvider) (stripe sc)
      let providers = btc <> str
      hints <- newReadHints
      webEnv <- newWebEnv chatStore sc ws hints providers
      exportWebapp (listener sc) (stripe sc)
      pollerEnv <- newPollerEnv chatStore ws hints providers (poll sc)
      pure [runWebListener webEnv, runPoller pollerEnv]

seedServiceCatalog :: DBStore -> IO ()
seedServiceCatalog st = do
  now <- truncateToSecond <$> getCurrentTime
  let (prices, offers) = defaultCatalog now
  (seededPrices, seededOffers) <- seedCatalog st prices offers
  logInfo $
    "badge catalog: " <> tshow (length prices) <> " prices and " <> tshow (length offers) <> " offers compiled in, "
      <> tshow seededPrices <> " prices and " <> tshow seededOffers <> " offers inserted"

badgeServiceCLI :: BadgeServiceOpts -> IO ()
badgeServiceCLI opts@BadgeServiceOpts {serviceConfigFile} = do
  serviceCfg <- traverse readConfigOrExit serviceConfigFile
  key <- requireIssuerKey opts serviceCfg terminalChatConfig
  env <- newServiceState >>= configureStoreVerifier serviceCfg
  let eventHook _cc = \case
        Right (CEvtServiceRequest u reqId sigKey reqData) -> do
          atomically $ writeTQueue (serviceRequestQ env) (u, reqId, sigKey, reqData)
          pure $ Right $ CEvtServiceRequest u reqId sigKey (shownServiceRequest reqData)
        ev -> pure ev
      chatHooks =
        defaultChatHooks
          { preStartHook = Just $ badgePreStartHook opts,
            postStartHook = Just $ badgePostStartHook opts env,
            preCmdHook = Just badgeCmdHook,
            eventHook = Just eventHook
          }
  raceAny_
    [ simplexChatCLI' terminalChatConfig {chatHooks} (mkChatOpts opts) Nothing,
      processQueuedRequests key Nothing env
    ]

-- | The terminal prints each request, and requests carry codes and store receipts, which are bearer secrets.
shownServiceRequest :: J.Object -> J.Object
shownServiceRequest reqData = case KM.lookup "request" reqData of
  Just (J.Object cmd) | Just t <- KM.lookup "type" cmd -> KM.singleton "request" $ J.object ["type" J..= t]
  _ -> KM.empty

badgeCmdHook :: ChatController -> ChatCommand -> IO (Either (Either ChatError ChatResponse) ChatCommand)
badgeCmdHook cc = \case
  CustomChatCommand cmd -> Left <$> runBadgeCmd cc cmd
  cmd -> pure $ Right cmd

runBadgeCmd :: ChatController -> ByteString -> IO (Either ChatError ChatResponse)
runBadgeCmd cc cmd
  | Right issueOpts <- A.parseOnly issueCmdP cmd =
      issueBadgeCode cc issueOpts >>= \case
        Right code -> pure $ Right CRCustomChatResponse {user_ = Nothing, response = "Code: " <> formatBadgeCode code}
        Left _ -> pure $ chatCmdError (T.unpack issueFailedText)
  | Right code <- A.parseOnly revokeCmdP cmd =
      revokeWithTracker cc code <&> \case
        Right response -> Right CRCustomChatResponse {user_ = Nothing, response}
        Left e -> chatCmdError (T.unpack e)
  | otherwise = pure $ chatCmdError $ "Usage: //issue supporter|legend|investor [months 1-" <> show maxMonths <> "] [paid|unpaid|free], or //revoke <code>"

revokeCmdP :: A.Parser BadgeCode
revokeCmdP = "revoke " *> codeP <* (A.skipSpace *> A.endOfInput)

issueCmdP :: A.Parser IssueCodeOpts
issueCmdP =
  "issue " *> do
    badgeType <- badgeTypeP
    months_ <- optional (A.space *> (A.decimal :: A.Parser Integer))
    -- Kept outside optional, which would otherwise backtrack past a bad count.
    months <- maybe (pure 1) checkMonths months_
    paymentStatus <- fromMaybe CPSFree <$> optional (A.space *> textTokenP)
    A.skipSpace
    A.endOfInput
    pure IssueCodeOpts {badgeType, months, paymentStatus}
  where
    -- Integer, because attoparsec's decimal wraps silently at Int, so the guard would check a truncated count.
    checkMonths n
      | n >= 1 && n <= fromIntegral maxMonths = pure (fromInteger n)
      | otherwise = fail $ "months must be between 1 and " <> show maxMonths

data IssueCodeOpts = IssueCodeOpts
  { badgeType :: BadgeType,
    months :: Int,
    paymentStatus :: BadgeCodePaymentStatus
  }

issueBadgeCode :: ChatController -> IssueCodeOpts -> IO (Either String BadgeCode)
issueBadgeCode cc IssueCodeOpts {badgeType, months, paymentStatus} =
  fmap fst <$> issueOneCode cc badgeType months paymentStatus singleUse

processQueuedRequests :: BadgeIssuerKey -> Maybe (TQueue GroupEvent) -> ServiceState -> IO ()
processQueuedRequests key trackerQ_ env = do
  cc <- atomically $ readTMVar $ serviceCC env
  -- a store call can take verifyTimeout, and a Play hand-over two, so a request that may make one waits on a lane of its own, and renewal checks on one no purchase can fill
  purchaseQ <- newTQueueIO
  renewalQ <- newTQueueIO
  checks <- newPurchaseChecks
  let answer (u, reqId, sigKey, reqData) = handleServiceRequest key (storeVerifier env) checks cc trackerQ_ u reqId sigKey reqData
      route req@(_, _, sigKey, reqData) =
        requestLane (storeChecks $ storeVerifier env) cc sigKey reqData >>= \case
          MainLane -> answer req
          PurchaseLane -> atomically $ writeTQueue purchaseQ req
          RenewalLane -> atomically $ writeTQueue renewalQ req
      lane q act = forever $ do
        req@(_, reqId, _, _) <- atomically $ readTQueue q
        survive ("request " <> safeDecodeUtf8 (strEncode reqId)) $ act req
  raceAny_ [lane (serviceRequestQ env) route, lane purchaseQ answer, lane renewalQ answer]

data RequestLane = MainLane | PurchaseLane | RenewalLane
  deriving (Eq, Show)

-- | Store calls wait off the main lane, so appends to one ledger come from more than one lane: each takes its
-- purchase's row lock before re-reading the tip, so the lanes may run concurrently at any --pool-size.
requestLane :: StoreChecks -> ChatController -> Maybe C.PublicKeyEd25519 -> J.Object -> IO RequestLane
requestLane checks cc sigKey reqData = case J.fromJSON (J.Object reqData) of
  J.Success BadgeServiceRequest {request = BSCPurchaseBadge {payment = SPGoogle {}, upgrade = Nothing}} -> pure PurchaseLane
  J.Success BadgeServiceRequest {request = BSCPurchaseBadge {payment = SPApple {jws}, upgrade = Nothing}}
    | maybe False autoRenewable (appleTransactionFacts jws) -> pure PurchaseLane
  J.Success BadgeServiceRequest {request = BSCTransferBadge {payment = SPApple {jws}}}
    | maybe False autoRenewable (appleTransactionFacts jws) -> pure PurchaseLane
  J.Success BadgeServiceRequest {request = BSCTransferBadge {payment = SPGoogle {}}} -> pure PurchaseLane
  J.Success BadgeServiceRequest {purchaseKey = Just k, request = BSCIssueBadge {}}
    | sigKey == Just k -> do
        now <- badgeNow cc
        withDB' "requestLane" cc (\db -> getIssuePurchase db k) <&> \case
          -- a due month goes to the serial renewal lane even within the hour, so it queues behind a check still in flight
          Right (Just (_, Just tip, Just _))
            | checks == ChecksAtEveryRequest || balanceStartTs tip <= addUTCTime routeLookahead now -> RenewalLane
          Right _ -> MainLane
          -- a read that failed may hide a due check, and the main lane is what must not wait on a store
          Left _ -> RenewalLane
  J.Success BadgeServiceRequest {purchaseKey = Just k, request = BSCUpgradeBadgeSubscription {}}
    | sigKey == Just k -> do
        now <- badgeNow cc
        withDB' "requestLane" cc (\db -> getIssuePurchase db k) <&> \case
          Right (Just (_, Just _, Just subscription@PurchaseSubscription {subscriptionRef = SubscriptionApple _}))
            | isNothing $ checkWait (addUTCTime routeLookahead now) subscription -> RenewalLane
          Right _ -> MainLane
          Left _ -> RenewalLane
  _ -> pure MainLane

-- | The router judges a minute ahead, so a handler reading the clock moments later finds due only what the router routed off the main lane.
routeLookahead :: NominalDiffTime
routeLookahead = 60

-- | Cancellation is rethrown, since that is how the race stops this thread. The
-- exception itself is not logged: it can quote the request, which carries codes and store receipts.
survive :: T.Text -> IO () -> IO ()
survive what action = action `catchOwn'` \_ -> logError $ "badge service: " <> what <> " failed; the next one will be handled"

badgePreStartHook :: BadgeServiceOpts -> ChatController -> IO ()
badgePreStartHook opts ChatController {config, chatStore} =
  runBadgeServiceMigrations opts config chatStore

badgePostStartHook :: BadgeServiceOpts -> ServiceState -> ChatController -> IO ()
badgePostStartHook BadgeServiceOpts {noAddress, testing} env cc = do
  -- Core starts this False and gates service request delivery on it, so the hook must set it.
  atomically $ writeTVar (processServiceRequests cc) True
  readTVarIO (currentUser cc) >>= \case
    Nothing -> putStrLn "No current user" >> exitFailure
    Just _ -> do
      -- The address carries service RPC only, so contact requests are never auto-accepted.
      unless noAddress $ initializeBotAddress' (not testing) (Just True) False cc
      void $ atomically $ tryPutTMVar (serviceCC env) cc

handleServiceRequest :: BadgeIssuerKey -> StoreVerifier -> PurchaseChecks -> ChatController -> Maybe (TQueue GroupEvent) -> User -> AgentInvId -> Maybe C.PublicKeyEd25519 -> J.Object -> IO ()
handleServiceRequest key verifier checks cc trackerQ_ User {userId} reqId sigKey reqData = do
  let reqIdT = safeDecodeUtf8 (strEncode reqId)
  logInfo $ "badge service request " <> reqIdT
  resp <-
    badgeServiceResponse key verifier checks cc trackerQ_ sigKey reqData `catchOwn'` \_ ->
      logError ("badge service request " <> reqIdT <> " failed") $> errorResponse BSEInternal
  logInfo $ "badge answer: " <> responseName resp
  sendChatCmd cc (APISendServiceResponse userId reqId (responseObject resp)) >>= \case
    Right _ -> pure ()
    Left e -> logError $ "badge service response failed for " <> reqIdT <> ": " <> tshow e

responseObject :: BadgeServiceResponse -> J.Object
responseObject r = case J.toJSON r of
  J.Object o -> o
  _ -> KM.fromList [("type", J.String "error"), ("code", J.toJSON BSEInternal)]

errorResponse :: BadgeServiceErrorCode -> BadgeServiceResponse
errorResponse code = BSPError {code, message = Nothing, retryAfter = badgeErrorRetryAfter code}

badgeErrorRetryAfter :: BadgeServiceErrorCode -> Maybe Word32
badgeErrorRetryAfter = \case
  BSEPaymentPending -> Just 300
  BSEProviderUnavailable -> Just 300
  BSERateLimited -> Just 60
  _ -> Nothing


-- | The agent verified the signature, so sigKey is a key the sender holds; a differing purchaseKey would let a client claim a purchase it cannot sign for.
badgeServiceResponse :: BadgeIssuerKey -> StoreVerifier -> PurchaseChecks -> ChatController -> Maybe (TQueue GroupEvent) -> Maybe C.PublicKeyEd25519 -> J.Object -> IO BadgeServiceResponse
badgeServiceResponse key verifier checks cc trackerQ_ sigKey reqData = case J.fromJSON (J.Object reqData) of
  J.Error _ -> pure $ errorResponse BSEBadRequest
  J.Success BadgeServiceRequest {version, purchaseKey, request}
    | not (version `isCompatible` supportedBadgeServiceVRange) -> pure $ errorResponse BSEUnsupportedVersion
    | purchaseKey /= sigKey -> pure $ errorResponse BSEBadRequest
    | otherwise -> logInfo ("badge request: " <> commandName request) >> case request of
        BSCRedeemBadgeCode {masterKey, code} -> case purchaseKey of
          Just k -> redeemCode key cc trackerQ_ k masterKey code
          Nothing -> pure $ errorResponse BSEBadRequest
        BSCPurchaseBadge {masterKey, payment, upgrade}
          | Just receipt_ <- toStoreReceipt verifier payment -> case (purchaseKey, upgrade) of
              (Just k, Nothing) -> either storeRefusalResponse (purchaseWithReceipt key cc verifier checks RUPurchase k masterKey) receipt_
              -- store upgrades are not built, and ignoring one would credit its months at the discounted price
              (Just _, Just _) -> pure $ errorResponse BSEBadRequest
              (Nothing, _) -> pure $ errorResponse BSEBadRequest
        BSCIssueBadge {balance} -> case purchaseKey of
          Just k -> issueBadgeCmd key cc verifier k Nothing balance
          Nothing -> pure $ errorResponse BSEBadRequest
        -- only an App Store subscription changes tier on the same transaction; Play gives a change a new token
        BSCUpgradeBadgeSubscription {payment = payment@SPApple {}, balance}
          | Just k <- purchaseKey,
            Just receipt_ <- toStoreReceipt verifier payment ->
              either storeRefusalResponse (upgradeSubscriptionCmd key cc verifier k balance) receipt_
        BSCUpgradeBadgeSubscription {} -> pure $ errorResponse BSEBadRequest
        BSCTransferBadge {masterKey, payment}
          | Just k <- purchaseKey,
            Just receipt_ <- toStoreReceipt verifier payment ->
              either storeRefusalResponse (purchaseWithReceipt key cc verifier checks (RUTransfer $ presentedTransaction payment) k masterKey) receipt_
        BSCTransferBadge {} -> pure $ errorResponse BSEBadRequest
        -- Every command but redeemBadgeCode and purchaseBadge from a store needs a key the service already knows; those two create the purchase, so their key is unknown on a first purchase.
        _ -> case purchaseKey of
          Nothing -> pure $ errorResponse BSEUnsupportedVersion
          Just k ->
            withDB' "purchaseKeyExists" cc (`purchaseKeyExists` k) >>= \case
              Right True -> pure $ errorResponse BSEUnsupportedVersion
              Right False -> pure $ errorResponse BSEUnknownPurchaseKey
              Left _ -> pure $ errorResponse BSEInternal

badgeNow :: ChatController -> IO UTCTime
badgeNow ChatController {config = ChatConfig {badgeCurrentTime}} = badgeCurrentTime

randomId :: ChatController -> IO T.Text
randomId cc = safeDecodeUtf8 . strEncode <$> atomically (C.randomBytes 16 (random cc))

-- | Neither the badge type nor the expiry comes from the caller; both derive from the entry.
credentialForEntry :: BadgeIssuerKey -> BadgeMasterKey -> StatementEntry -> IO (Either String (StatementEntry, BadgeCredential))
credentialForEntry BadgeIssuerKey {keyIdx, secretKey} masterKey e@StatementEntry {balanceStartTs = periodEnd, balanceBadgeType} = do
  let badgeInfo = BadgeInfo {badgeType = balanceBadgeType, badgeExpiry = endOfMondayAfter periodEnd, badgeExtra = ""}
  fmap (e,) <$> issueBadge keyIdx secretKey (VerifiedBadgeRequest BadgeRequest {masterKey, badgeInfo})

issuanceAfter :: StatementEntry -> (StatementEntry, BadgeCredential) -> (StatementEntry, StatementEntry, BadgeCredential)
issuanceAfter previous (issued, credential) = (previous, issued, credential)

credentialResponse :: Maybe BadgeCredential -> Maybe T.Text -> Maybe SubscriptionState -> [StatementEntry] -> BadgeServiceResponse
credentialResponse credential previousEntryId subscription entries =
  BSPBadgeCredential {credential, receipt = Nothing, statement = BadgeStatement {entries, previousEntryId}, subscription}

-- | Nothing is written until the credential is signed, so a signing failure leaves the code unspent.
redeemCode :: BadgeIssuerKey -> ChatController -> Maybe (TQueue GroupEvent) -> C.PublicKeyEd25519 -> BadgeMasterKey -> T.Text -> IO BadgeServiceResponse
redeemCode key cc trackerQ_ purchaseKey masterKey codeText = case parseBadgeCode codeText of
  Nothing -> pure $ errorResponse BSECodeInvalid
  Just code -> do
    now <- badgeNow cc
    withDB "getBadgeCode" cc (readCode now code) >>= \case
      Left _ -> pure $ errorResponse BSEInternal
      Right (Left resp) -> pure resp
      Right (Right IssuedCode {badgeCodeId, badgeType, months, redeemLimit}) ->
        signFirstMonth key cc masterKey badgeType months SCCode now >>= \case
          Left resp -> pure resp
          Right firstMonth -> do
            -- If the code was revoked or used up while signing, the claim fails. Read the code again to tell the client why.
            r <- withDB "writeCodeRedemption" cc $ \db ->
              liftIO (createCodePurchase db NewCodePurchase {badgeCodeId, purchaseKey, masterKey, badgeType} now) >>= \case
                Nothing ->
                  readCode now code db >>= \case
                    Left resp -> pure (resp, False)
                    Right _ -> logError "badge service: redeeming a code failed, but the code has uses left and is not revoked" $> (errorResponse BSEInternal, False)
                Just purchaseId -> liftIO $ (,True) <$> firstMonthResponse db purchaseId Nothing Nothing firstMonth
            let (resp, claimed) = fromRight (errorResponse BSEInternal, False) r
            when (claimed && hasTracker redeemLimit) $ refreshTracker cc trackerQ_ badgeCodeId code
            pure resp
  where
    readCode now code db = liftIO $
      getBadgeCode db (badgeCodeHash code) >>= \case
        Nothing -> pure $ Left $ errorResponse BSECodeInvalid
        Just c@IssuedCode {badgeCodeId, revokedAt, paymentStatus, expiresAt, redeemLimit, redeemCount} ->
          getCodePurchaseForKey db badgeCodeId purchaseKey >>= \case
            -- A key that already redeemed gets its credential back without a use, even if the code has since
            -- expired or been revoked: a client whose reply was lost retries, and would otherwise lose the badge.
            KeyRedeemed KeyPurchase {badgePurchaseId, credential} ->
              maybe (Left $ errorResponse BSEInternal) (Left . credentialResponse (Just credential) Nothing Nothing)
                <$> getLedgerEntries db badgePurchaseId 0
            -- code_used would make the client drop its keys, so the holder could never get the badge back.
            KeyRedeemedUnreadable ->
              logError "badge service: a redeemed code's credential is missing or unreadable" $> Left (errorResponse BSEInternal)
            KeyUnredeemed
              -- Revoked is checked first, so it answers as if the code never existed.
              | Just _ <- revokedAt -> pure $ Left $ errorResponse BSECodeInvalid
              -- Redeeming an unpaid code would issue a free badge, so unpaid is refused.
              | CPSUnpaid <- paymentStatus -> pure $ Left $ errorResponse BSEPaymentPending
              | redeemCount >= redeemLimit -> pure $ Left $ errorResponse BSECodeUsed
              | maybe False (now >=) expiresAt -> pure $ Left $ errorResponse BSECodeExpired
              | otherwise -> pure $ Right c

-- | What a store receipt is presented for. A transfer carries the Apple transaction the device presented,
-- as only the subscription's latest one moves it.
data ReceiptUse = RUPurchase | RUTransfer (Maybe T.Text)

presentedTransaction :: ServicePayment -> Maybe T.Text
presentedTransaction = \case
  SPApple {jws} -> (\AppleTransactionFacts {transactionId} -> transactionId) <$> appleTransactionFacts jws
  _ -> Nothing

-- | Every refusal is answered before anything is written, so it leaves the receipt unclaimed; and
-- nothing is written until the credential is signed.
purchaseWithReceipt :: BadgeIssuerKey -> ChatController -> StoreVerifier -> PurchaseChecks -> ReceiptUse -> C.PublicKeyEd25519 -> BadgeMasterKey -> StoreReceipt -> IO BadgeServiceResponse
purchaseWithReceipt key cc verifier checks use purchaseKey masterKey StoreReceipt {txRef = StoreTransactionRef {provider, transactionRef = providerRef}, receiptProductId, subscriptionRef, verifyReceipt, acknowledgeReceipt, acknowledgeSubscription} =
  withDB' "getStorePayment" cc (\db -> (,) <$> getStorePaymentCredit db provider providerRef <*> keyHoldsPurchase db) >>= \case
    Left _ -> pure $ errorResponse BSEInternal
    Right (credit, keyHeld)
      -- only the key it credited can ask, and it is told only what it was given, so the store is not asked
      | creditedBy credit -> answerCredit credit
      -- a payment moves only to a key with no purchase, so one transferOut alone names the purchase holding it
      | keyHeld -> pure $ errorResponse BSEBadRequest
      -- a Play subscription is verified by asking about the subscription itself
      | Just StoreSubscription <- storeProduct provider =<< receiptProductId -> purchaseSubscription receiptProductId credit
      | otherwise ->
          verifyReceipt >>= \case
            Left refusal -> storeRefusalResponse refusal
            -- the claim was read from the evidence before it was verified, so it must name the transaction the store vouched for
            Right VerifiedStoreTransaction {transactionRef}
              | transactionRef /= providerRef -> storeRefusalResponse $ SRVerifierFailed "verified a transaction other than the one claimed"
            Right VerifiedStoreTransaction {quantity}
              | quantity /= 1 -> storeRefusalResponse $ SRVerifierFailed $ "verified a quantity of " <> tshow quantity
            Right VerifiedStoreTransaction {testPurchase = True}
              | not (creditTestPurchases verifier) -> storeRefusalResponse SRTestPurchase
            -- another key's credit is told only once the store vouched for the receipt, or it would reveal which transactions were credited
            Right tx@VerifiedStoreTransaction {productId} -> case credit of
              Uncredited -> newPurchase tx
              _
                | RUTransfer _ <- use,
                  Just StoreSubscription <- storeProduct provider productId ->
                    purchaseSubscription (Just productId) credit
              _ -> answerCredit credit
  where
    keyHoldsPurchase db = case use of
      RUPurchase -> pure False
      RUTransfer _ -> purchaseKeyExists db purchaseKey
    creditedBy = \case
      Credited CreditedPurchase {purchaseKey = k} -> k == purchaseKey
      _ -> False
    answerCredit credit =
      withDB' "answerStoreCredit" cc (\db -> creditedResponse db purchaseKey credit) <&> \case
        Right (Left resp) -> resp
        _ -> errorResponse BSEInternal
    -- the product is read only for a receipt not yet credited, so retiring it leaves its replays answered
    newPurchase tx@VerifiedStoreTransaction {productId, paid, testPurchase} = case storeProduct provider productId of
      Nothing -> pure $ errorResponse BSEProductUnavailable
      -- an Apple subscription, whose JWS was verified offline
      Just StoreSubscription -> purchaseSubscription (Just productId) Uncredited
      Just StoreOneTime {badgeType, months} -> do
        when testPurchase $ logWarn "[dev] crediting a store test purchase as paid: credit_test_purchases is on"
        acknowledged (($ tx) <$> acknowledgeReceipt) $ do
          now <- badgeNow cc
          signFirstMonth key cc masterKey badgeType months (SCPayment Nothing) now >>= \case
            Left resp -> pure resp
            Right firstMonth ->
              claimPurchase now (\paymentId -> NewStorePurchase {paymentId, provider, providerRef, paid, purchaseKey, masterKey, badgeType, subscriptionState = Nothing, providerToken = Nothing}) $
                \db purchaseId paymentId -> firstMonthResponse db purchaseId (Just paymentId) Nothing firstMonth
    -- another key's credit is told only once the store vouched for the subscription, as for a one-time purchase;
    -- the store is asked at most once an interval per subscription and per purpose, whoever presents it
    purchaseSubscription product_ credit = do
      now <- badgeNow cc
      atomically (subscriptionAsk now) >>= \case
        ClaimHeld remaining -> pure BSPError {code = BSEPaymentPending, message = Nothing, retryAfter = Just (ceiling remaining)}
        NoPlayRead wait -> do
          logWarn "Play subscription read refused: the purchase path's reads for this minute are used"
          pure BSPError {code = BSERateLimited, message = Nothing, retryAfter = Just (ceiling wait)}
        AskStore ->
          checkStoreSubscription verifier subscriptionRef product_ >>= \case
            -- no store was asked, so the claim is not held off
            Left SRNotConfigured -> atomically (modifyTVar' claimChecks $ M.delete claimKey) >> storeRefusalResponse SRNotConfigured
            Left refusal -> atLeastCheckInterval <$> storeRefusalResponse refusal
            Right checked -> atLeastCheckInterval <$> case (credit, use) of
              (Uncredited, _) -> newSubscription checked
              (Credited from, RUTransfer presented) -> moveSubscription presented from checked
              _ -> answerCredit credit
    subscriptionAsk now =
      purchaseCheckWait claimChecks now claimKey >>= \case
        Just remaining -> pure $ ClaimHeld remaining
        Nothing ->
          maybe (pure Nothing) (`playReadWait` now) playReads_ >>= \case
            Nothing -> pure AskStore
            -- a claim refused a read is not held, as Play was not asked
            Just wait -> modifyTVar' claimChecks (M.delete claimKey) $> NoPlayRead wait
    claimKey = ClaimKey {claimRef = providerRef, claimPurpose}
    claimPurpose = case use of
      RUPurchase -> ClaimPurchase
      RUTransfer _ -> ClaimTransfer
    (claimChecks, playReads_) = case subscriptionRef of
      SubscriptionApple _ -> (appleChecks checks, Nothing)
      SubscriptionGoogle _ -> (googleChecks checks, Just $ playReads checks)
    newSubscription checked@SubscriptionCheck {subscriptionState} = do
      now <- badgeNow cc
      case grantableCharge provider now checked >>= fundsMonthNow purchaseKey now Nothing of
        Left notGranted -> firstChargeNotGranted provider notGranted
        Right (charge@StoreCharge {periodStart, chargePaid = paid}, (badgeType, months)) ->
          acknowledged acknowledgeSubscription $ do
            chargeId <- randomId cc
            entryIds <- (,,,) <$> randomId cc <*> randomId cc <*> randomId cc <*> randomId cc
            let seed = emptyEntry periodStart badgeType
            signMonthAfter key cc masterKey now seed (chargeEntries periodStart now badgeType months purchaseKey chargeId entryIds seed) >>= \case
              Left resp -> pure resp
              Right firstMonth ->
                claimPurchase now (\paymentId -> NewStorePurchase {paymentId, provider, providerRef, paid, purchaseKey, masterKey, badgeType, subscriptionState = Just subscriptionState, providerToken}) $
                  \db purchaseId paymentId -> do
                    insertSubscriptionCharge db paymentId chargeId charge now
                    firstMonthResponse db purchaseId Nothing (Just subscriptionState) firstMonth
    moveSubscription presented CreditedPurchase {badgePurchaseId = fromPurchaseId} checked@SubscriptionCheck {subscriptionState, latestCharge}
      -- an older transaction stops moving the subscription once the store charges again, so a leaked receipt lapses
      | SubscriptionApple _ <- subscriptionRef,
        presented /= ((\StoreCharge {chargeRef} -> chargeRef) <$> latestCharge) =
          pure $ errorResponse BSEPaymentPending
      | otherwise = do
          now <- badgeNow cc
          withDB' "getMovedPayment" cc (\db -> getPurchaseSubscription db fromPurchaseId >>= mapM (movedPayment db)) >>= \case
            Right (Just (paymentId, movedFromExpiry_, Just fromTip))
              -- A store receipt proves control of the store account but is no one-time secret, so this is the only bound on moves:
              -- waiting out the credential of the key the payment last moved from keeps each paid month with at most two keys.
              | maybe False (now <) movedFromExpiry_ -> pure $ errorResponse BSETransferLimited
              | otherwise -> case grantableCharge provider now checked >>= fundsMonthNow purchaseKey now Nothing of
                  Left notGranted -> firstChargeNotGranted provider notGranted
                  Right grant -> movePayment now paymentId fromTip grant
            _ -> pure $ errorResponse BSEInternal
      where
        movedPayment db PurchaseSubscription {paymentId} = (paymentId,,) <$> getMovedFromExpiry db paymentId <*> getLedgerTip db fromPurchaseId
        movePayment now paymentId fromTip (charge@StoreCharge {chargeRef, periodStart}, (badgeType, months)) =
          withDB' "getSubscriptionCharge" cc (\db -> getSubscriptionCharge db paymentId chargeRef) >>= \case
            Left _ -> pure $ errorResponse BSEInternal
            Right recorded -> do
              chargeId <- maybe (randomId cc) (pure . fst) recorded
              entryIds <- (,,,) <$> randomId cc <*> randomId cc <*> randomId cc <*> randomId cc
              (lapseId, transferId) <- (,) <$> randomId cc <*> randomId cc
              let seed = emptyEntry periodStart badgeType
                  lapsed = lapseEntry now lapseId fromTip
                  fromRows = maybeToList lapsed <> [debitEntry now transferId (SDTransferOut purchaseKey) (fromMaybe fromTip lapsed)]
              signMonthAfter key cc masterKey now seed (chargeEntries periodStart now badgeType months purchaseKey chargeId entryIds seed) >>= \case
                Left resp -> pure resp
                Right firstMonth -> do
                  r <- withDB "transferStorePurchase" cc $ \db -> liftIO $
                    rebindStorePayment db PaymentRebind {paymentId, fromPurchaseId, fromTipId = entryId fromTip, fromRows, purchaseKey, masterKey, badgeType} now >>= \case
                      Nothing -> pure Nothing
                      Just purchaseId -> do
                        insertSubscriptionCharge db paymentId chargeId charge now
                        setSubscriptionState db paymentId subscriptionState now
                        Just <$> firstMonthResponse db purchaseId Nothing (Just subscriptionState) firstMonth
                  case r of
                    Right (Just resp) -> logInfo "store subscription moved to another key" $> resp
                    Right Nothing -> logError "badge service: a subscription moved, or its ledger changed, while its transfer was signed" $> errorResponse BSEInternal
                    Left _ -> pure $ errorResponse BSEInternal
    providerToken = case subscriptionRef of
      SubscriptionGoogle token -> Just token
      SubscriptionApple _ -> Nothing
    acknowledged acknowledge_ crediting =
      fromMaybe (pure $ Right ()) acknowledge_ >>= \case
        Left refusal -> storeRefusalResponse refusal
        Right () -> crediting
    claimPurchase now purchaseFor writeCredit = do
      paymentId <- randomId cc
      r <- withDB "writeStorePurchase" cc $ \db ->
        liftIO (createStorePurchase db (purchaseFor paymentId) now) >>= \case
          -- Credited to another key, or to this one by a request that ran alongside it, while signing.
          Nothing ->
            liftIO (getStorePaymentCredit db provider providerRef >>= creditedResponse db purchaseKey) >>= \case
              Left resp -> pure resp
              Right () -> logError "badge service: a store payment exists but funds no purchase" $> errorResponse BSEInternal
          Just purchaseId -> liftIO $ writeCredit db purchaseId paymentId
      pure $ fromRight (errorResponse BSEInternal) r

-- | Why a store charge grants nothing.
data NotGranted
  = NGEnded
  | NGNoCharge
  | NGMalformedPeriod
  | NGRevoked
  | NGOffer
  | NGUnknownPlan
  | NGNotStarted
  | NGPeriodOver
  deriving (Show)

-- | The grant rule of a first purchase and a renewal alike, but for whether a ledger row already references
-- the charge, which is the caller's to check. It grants the tier of the plan the store charged, which may not be the ledger's.
grantableCharge :: PaymentProvider -> UTCTime -> SubscriptionCheck -> Either NotGranted (StoreCharge, (BadgeType, Int))
grantableCharge provider now SubscriptionCheck {subscriptionState, latestCharge}
  | subscriptionState == SSEnded = Left NGEnded
  | otherwise = case latestCharge of
      Nothing -> Left NGNoCharge
      -- the plan first: Play derives a period from the plan's length, so a plan it does not know has none
      Just charge@StoreCharge {planId, periodStart, periodEnd, revoked, offer} -> case storePlan provider planId of
        Nothing -> Left NGUnknownPlan
        Just plan
          | periodEnd <= periodStart -> Left NGMalformedPeriod
          | revoked -> Left NGRevoked
          | offer -> Left NGOffer
          | now < periodStart -> Left NGNotStarted
          | now >= periodEnd -> Left NGPeriodOver
          | otherwise -> Right (charge, plan)

-- | Play dates a period from its end less the plan's months, clipped at month ends, so the months the ledger grants
-- from that start can end before Play's period does: a charge still in its period may then fund no month now.
fundsMonthNow :: C.PublicKeyEd25519 -> UTCTime -> Maybe StatementEntry -> (StoreCharge, (BadgeType, Int)) -> Either NotGranted (StoreCharge, (BadgeType, Int))
fundsMonthNow purchaseKey now tip_ grant@(StoreCharge {periodStart}, (badgeType, months))
  | isJust $ issueEntry now "" $ last $ tip : chargeEntries periodStart now badgeType months purchaseKey "" ("", "", "", "") tip = Right grant
  | otherwise = Left NGPeriodOver
  where
    tip = fromMaybe (emptyEntry periodStart badgeType) tip_

-- | The store ended the subscription, or revoked the latest charge it reports, one this payment was granted: a refund or a reversal.
coverageEnded :: DB.Connection -> T.Text -> SubscriptionCheck -> IO Bool
coverageEnded db paymentId SubscriptionCheck {subscriptionState, latestCharge}
  | subscriptionState == SSEnded = pure True
  | Just StoreCharge {chargeRef, revoked = True} <- latestCharge = maybe False snd <$> getSubscriptionCharge db paymentId chargeRef
  | otherwise = pure False

-- | No payment row exists yet to stamp, so a first purchase's checks are bounded by the purchase checks.
firstChargeNotGranted :: PaymentProvider -> NotGranted -> IO BadgeServiceResponse
firstChargeNotGranted provider = \case
  NGMalformedPeriod -> storeRefusalResponse $ SRVerifierFailed "a charge whose period does not end after its start"
  -- Play issues a new token when an ended subscription is bought again, so this one can never be credited
  NGEnded | PPGoogle <- provider -> storeRefusalResponse $ SRInvalid "the subscription ended"
  NGNotStarted -> storeRefusalResponse SRPending
  -- a later charge, or an Apple resubscription under the same original id, may yet be credited from this receipt
  reason -> logInfo ("store subscription not credited yet: " <> tshow reason) $> BSPError {code = BSEPaymentPending, message = Nothing, retryAfter = Just 86400}

-- | What each request was answered, to pair with its command in a log. No payload: a credential,
-- an invoice and a statement are the client's, and an error's message can quote what it refused.
responseName :: BadgeServiceResponse -> T.Text
responseName = \case
  BSPBadgeCatalog {} -> "catalog"
  BSPBadgeInvoice {} -> "invoice"
  BSPBadgeCredential {credential = Just _, subscription} ->
    "badge issued" <> maybe "" (\s -> ", subscription " <> tshow s) subscription
  BSPBadgeCredential {credential = Nothing} -> "no month to issue"
  BSPError {code} -> "error " <> tshow code

-- | The command each request ran, so a log can be followed without the request ids. No payload:
-- a payment holds a store receipt, and an upgrade a receipt code.
commandName :: BadgeServiceCommand -> T.Text
commandName = \case
  BSCGetBadgeCatalog -> "getBadgeCatalog"
  BSCGetBadgeInvoice {} -> "getBadgeInvoice"
  BSCRedeemBadgeCode {} -> "redeemBadgeCode"
  BSCPurchaseBadge {payment} -> "purchaseBadge " <> paymentName payment
  BSCUpgradeBadgeSubscription {payment} -> "upgradeBadgeSubscription " <> paymentName payment
  BSCTransferBadge {payment} -> "transferBadge " <> paymentName payment
  BSCIssueBadge {} -> "issueBadge"
  BSCPauseBadge -> "pauseBadge"
  where
    paymentName = \case
      SPApple {} -> "(App Store)"
      SPGoogle {} -> "(Play)"
      SPInvoice {} -> "(invoice)"
      SPReceipt {} -> "(receipt)"

-- | A forged receipt and a refunded one are both receipt_invalid, so the client learns nothing a
-- forger could iterate on. A test purchase is named, since the device that made it already knows.
storeRefusalResponse :: StoreRefusal -> IO BadgeServiceResponse
storeRefusalResponse = \case
  -- warn, not info: every one of these ends a purchase for good, and one of them (a store that no longer
  -- knows a subscription it once sold) can do that to a purchase the user really paid for
  SRInvalid reason -> logWarn ("store receipt refused for good: " <> reason) $> errorResponse BSEReceiptInvalid
  SRTestPurchase -> logInfo "store receipt refused: test purchase" $> errorResponse BSETestPurchase
  SRPending -> pure $ errorResponse BSEPaymentPending
  SRUnreachable reason -> logWarn ("store unreachable: " <> reason) $> errorResponse BSEProviderUnavailable
  SRVerifierFailed reason -> logError ("store receipt not verified: " <> reason) $> errorResponse BSEInternal
  SRNotConfigured -> logWarn "store receipt refused: no verifier for this store is configured" $> errorResponse BSEProviderNotConfigured

creditedResponse :: DB.Connection -> C.PublicKeyEd25519 -> FundingCredit -> IO (Either BadgeServiceResponse ())
creditedResponse db purchaseKey = \case
  Uncredited -> pure $ Right ()
  CreditedUnreadable -> pure $ Left $ errorResponse BSEInternal
  Credited CreditedPurchase {purchaseKey = k, badgePurchaseId, credential, subscription}
    | k /= purchaseKey -> pure $ Left $ errorResponse BSEReceiptUsed
    | otherwise ->
        maybe (Left $ errorResponse BSEInternal) (Left . credentialResponse (Just credential) Nothing subscription)
          <$> getLedgerEntries db badgePurchaseId 0

signFirstMonth :: BadgeIssuerKey -> ChatController -> BadgeMasterKey -> BadgeType -> Int -> StatementCreditType -> UTCTime -> IO (Either BadgeServiceResponse ([StatementEntry], (StatementEntry, StatementEntry, BadgeCredential)))
signFirstMonth key cc masterKey badgeType months credit now = do
  grantUuid <- randomId cc
  -- TODO [badges] a top-up grants onto an existing ledger, and must lapse before it or the
  -- months it adds are counted from a start already in the past
  let seed = emptyEntry now badgeType
  signMonthAfter key cc masterKey now seed [grantEntry now grantUuid badgeType months credit seed]

-- | The rows that fund a month, after the entry they follow, with that month issued at now and signed.
signMonthAfter :: BadgeIssuerKey -> ChatController -> BadgeMasterKey -> UTCTime -> StatementEntry -> [StatementEntry] -> IO (Either BadgeServiceResponse ([StatementEntry], (StatementEntry, StatementEntry, BadgeCredential)))
signMonthAfter key cc masterKey now previous rows = do
  issueUuid <- randomId cc
  let funded = last (previous : rows)
  case issueEntry now issueUuid funded of
    Nothing -> logError "badge service: the rows written fund no month to issue" $> Left (errorResponse BSEInternal)
    Just issued ->
      credentialForEntry key masterKey issued >>= \case
        Left e -> logError ("badge service signing failed: " <> T.pack e) $> Left (errorResponse BSEInternal)
        Right signed -> pure $ Right (rows, issuanceAfter funded signed)

firstMonthResponse :: DB.Connection -> Int64 -> Maybe T.Text -> Maybe SubscriptionState -> ([StatementEntry], (StatementEntry, StatementEntry, BadgeCredential)) -> IO BadgeServiceResponse
firstMonthResponse db purchaseId creditPaymentId_ subscription (rows, issuance@(_, _, credential)) = do
  appendLedgerPlan db purchaseId creditPaymentId_ rows $ Just issuance
  entries_ <- getLedgerEntries db purchaseId 0
  pure $ maybe (errorResponse BSEInternal) (credentialResponse (Just credential) Nothing subscription) entries_

-- | The subscription a device's transaction names, its product and that product's tier: what an upgrade waits for, never what it grants.
data SubscriptionUpgrade = SubscriptionUpgrade
  { upgradeRef :: T.Text,
    upgradeProductId :: T.Text,
    upgradeTier :: BadgeType
  }

-- | Refused before the store is asked: a transaction that does not verify, or that is of no subscription product.
upgradeSubscriptionCmd :: BadgeIssuerKey -> ChatController -> StoreVerifier -> C.PublicKeyEd25519 -> BadgeBalance -> StoreReceipt -> IO BadgeServiceResponse
upgradeSubscriptionCmd key cc verifier purchaseKey balance StoreReceipt {txRef = StoreTransactionRef {provider, transactionRef = claimedRef}, verifyReceipt} =
  verifyReceipt >>= \case
    Left refusal -> storeRefusalResponse refusal
    Right VerifiedStoreTransaction {transactionRef}
      | transactionRef /= claimedRef -> storeRefusalResponse $ SRVerifierFailed "verified a transaction other than the one claimed"
    Right VerifiedStoreTransaction {productId} -> case storePlan provider productId of
      Nothing -> pure $ errorResponse BSEBadRequest
      Just (tier, _) -> issueBadgeCmd key cc verifier purchaseKey (Just SubscriptionUpgrade {upgradeRef = claimedRef, upgradeProductId = productId, upgradeTier = tier}) balance

-- | The purchase is reached through the verified signer key and no other way.
issueBadgeCmd :: BadgeIssuerKey -> ChatController -> StoreVerifier -> C.PublicKeyEd25519 -> Maybe SubscriptionUpgrade -> BadgeBalance -> IO BadgeServiceResponse
issueBadgeCmd key cc verifier purchaseKey upgrade_ BadgeBalance {lastEntry} = do
  now <- badgeNow cc
  purchase_ <- withDB' "getBadgePurchase" cc (\db -> getIssuePurchase db purchaseKey)
  case purchase_ of
    Left _ -> pure $ errorResponse BSEInternal
    Right Nothing -> pure $ errorResponse BSEUnknownPurchaseKey
    Right (Just (purchase, Just tip, Just subscription))
      | Just upgrade <- upgrade_ -> upgradeSubscription upgrade purchase tip subscription now
      -- once a subscription's paid coverage has ended, only a new store charge funds another month
      | paidThrough tip <= now -> renewSubscription Nothing purchase tip subscription now
      | ChecksAtEveryRequest <- storeChecks verifier -> checkStoreDuringCoverage purchase tip subscription now
      | balanceStartTs tip <= now -> checkStoreBeforeMonth purchase tip subscription now
    Right (Just _) | isJust upgrade_ -> pure $ errorResponse BSEBadRequest
    Right (Just (ServicePurchase {badgePurchaseId}, Just StatementEntry {entryType = SEDebit SDTransferOut {}}, Nothing)) ->
      answered "answerMovedOut" $ \db -> withoutCredential <$> issueResponse db badgePurchaseId now (Just SSEnded)
    Right (Just (purchase, tip, subscription_)) -> issueDue purchase tip ((\PurchaseSubscription {storedState} -> storedState) <$> subscription_) now
  where
    issueDue ServicePurchase {badgePurchaseId, masterKey, badgeType} tip state_ now = do
      (lapseUuid, issueUuid) <- (,) <$> randomId cc <*> randomId cc
      let tipEntry = fromMaybe (emptyEntry now badgeType) tip
          lapsed = lapseEntry now lapseUuid tipEntry
          current = fromMaybe tipEntry lapsed
      case issueEntry now issueUuid current of
        Nothing -> writeIssued badgePurchaseId tip (maybeToList lapsed) now Nothing state_
        Just e ->
          credentialForEntry key masterKey e >>= \case
            Left err -> logError ("badge service signing failed: " <> T.pack err) $> errorResponse BSEInternal
            Right signed ->
              writeIssued badgePurchaseId tip (maybeToList lapsed) now (Just $ issuanceAfter current signed) state_
    -- Write only if the tip has not moved, since another request may have advanced it.
    writeIssued purchaseId tip rows t issuance_ state_ = do
      r <- withDB "issueBadge" cc $ \db -> liftIO $ do
        tip' <- lockedLedgerTip db purchaseId t
        when (fmap entryId tip' == fmap entryId tip) $ appendLedgerPlan db purchaseId Nothing rows issuance_
        issueResponse db purchaseId t state_
      pure $ fromRight (errorResponse BSEInternal) r
    -- Answered pending until the tier the device saw is granted, so the old tier's credential is never taken for the new one.
    upgradeSubscription upgrade@SubscriptionUpgrade {upgradeRef, upgradeTier} purchase@ServicePurchase {badgePurchaseId} tip@StatementEntry {balanceBadgeType} subscription@PurchaseSubscription {subscriptionRef, storedState} now
      | not ownSubscription = pure $ errorResponse BSEBadRequest
      | balanceBadgeType == upgradeTier = answered "answerUpgraded" $ \db -> issueResponse db badgePurchaseId now (Just storedState)
      | Just wait <- checkWait now subscription =
          pure BSPError {code = BSEPaymentPending, message = Nothing, retryAfter = Just $ ceiling wait}
      | otherwise = renewSubscription (Just upgrade) purchase tip subscription now
      where
        ownSubscription = case subscriptionRef of
          SubscriptionApple ref -> ref == upgradeRef
          SubscriptionGoogle _ -> False
    -- Nothing but the store's answer is written until a charge is granted: a lapse at now would be dated
    -- after a grant a later check may make at an earlier period start, and the client would read it as a pass that issued.
    renewSubscription upgrading_ ServicePurchase {badgePurchaseId, masterKey} tip@StatementEntry {balanceBadgeType} subscription@PurchaseSubscription {paymentId, provider, subscriptionRef, storedState} now
      | isNothing upgrading_ && not (subscriptionCheckDue (storeChecks verifier) now tip subscription) = answered "answerStoredSubscription" $ \db -> issueResponse db badgePurchaseId now (Just storedState)
      | otherwise =
          withDB' "stampSubscriptionCheck" cc (\db -> stampSubscriptionCheck db paymentId now) >>= \case
            Left _ -> pure $ errorResponse BSEInternal
            Right () ->
              -- the stores ignore the raise's product; the development mock charges its plan from now on
              checkStoreSubscription verifier subscriptionRef (upgradeProductId <$> upgrading_) >>= \case
                Left refusal -> atLeastCheckInterval <$> storeRefusalResponse refusal
                Right checked@SubscriptionCheck {subscriptionState, latestCharge} -> case grantableCharge provider now checked >>= fundsMonthNow purchaseKey now (Just tip) of
                  Left NGMalformedPeriod -> storeRefusalResponse $ SRVerifierFailed "a charge whose period does not end after its start"
                  Left reason -> do
                    forM_ latestCharge $ \_ -> logInfo $ "store subscription charge not granted: " <> tshow reason
                    recordCheck latestCharge subscriptionState
                  Right (charge@StoreCharge {chargeRef, periodStart}, (badgeType, months)) ->
                    withDB' "getSubscriptionCharge" cc (\db -> getSubscriptionCharge db paymentId chargeRef) >>= \case
                      Left _ -> pure $ errorResponse BSEInternal
                      -- the store still reports the charge the ledger last granted
                      Right (Just (_, True)) -> recordCheck (Just charge) subscriptionState
                      Right recorded -> do
                        chargeId <- maybe (randomId cc) (pure . fst) recorded
                        entryIds <- (,,,) <$> randomId cc <*> randomId cc <*> randomId cc <*> randomId cc
                        signMonthAfter key cc masterKey now tip (chargeEntries periodStart now badgeType months purchaseKey chargeId entryIds tip) >>= \case
                          Left resp -> pure resp
                          Right (rows, issuance) -> do
                            r <- withDB "renewSubscription" cc $ \db -> liftIO $ do
                              tip' <- lockedLedgerTip db badgePurchaseId now
                              if fmap entryId tip' /= Just (entryId tip)
                                then pure Nothing
                                else do
                                  insertSubscriptionCharge db paymentId chargeId charge now
                                  appendLedgerPlan db badgePurchaseId Nothing rows $ Just issuance
                                  setPurchaseBadgeType db badgePurchaseId badgeType now
                                  setSubscriptionState db paymentId subscriptionState now
                                  Just <$> checkedResponse db badgeType subscriptionState
                            case r of
                              Right (Just resp) -> logBanked months $> resp
                              Right Nothing -> logError "badge service: a subscription's ledger moved while its renewal was signed" $> errorResponse BSEInternal
                              Left _ -> pure $ errorResponse BSEInternal
      where
        -- the store's answer is kept, and a charge it reports is recorded once, granted or not
        recordCheck charge_ state = do
          chargeId <- randomId cc
          answered "recordSubscriptionCheck" $ \db -> do
            forM_ charge_ $ \charge -> insertSubscriptionCharge db paymentId chargeId charge now
            setSubscriptionState db paymentId state now
            checkedResponse db balanceBadgeType state
        checkedResponse db ledgerTier state = case upgradeTier <$> upgrading_ of
          Just tier | tier /= ledgerTier -> pure $ atLeastCheckInterval $ errorResponse BSEPaymentPending
          _ -> issueResponse db badgePurchaseId now (Just state)
    -- A charge a dev deployment's store reports while coverage runs is banked with no issuance, and its month issued once coverage reaches it.
    checkStoreDuringCoverage purchase@ServicePurchase {badgePurchaseId} tip PurchaseSubscription {paymentId, provider, subscriptionRef} now =
      withDB' "stampSubscriptionCheck" cc (\db -> stampSubscriptionCheck db paymentId now) >>= \case
        Left _ -> pure $ errorResponse BSEInternal
        Right () ->
          checkStoreSubscription verifier subscriptionRef Nothing >>= \case
            Left refusal -> atLeastCheckInterval <$> storeRefusalResponse refusal
            Right checked@SubscriptionCheck {subscriptionState, latestCharge} -> do
              newChargeId <- randomId cc
              entryIds <- (,,,) <$> randomId cc <*> randomId cc <*> randomId cc <*> randomId cc
              r <- withDB' "checkStoreDuringCoverage" cc $ \db -> do
                tip' <- lockedLedgerTip db badgePurchaseId now
                ended <- coverageEnded db paymentId checked
                banked <-
                  if not ended && fmap entryId tip' == Just (entryId tip)
                    then do
                      bankedMonths <- case grantableCharge provider now checked of
                        Right (charge@StoreCharge {chargeRef, periodStart}, (badgeType, months)) ->
                          getSubscriptionCharge db paymentId chargeRef >>= \case
                            Just (_, True) -> pure Nothing
                            recorded -> do
                              let chargeId = maybe newChargeId fst recorded
                              insertSubscriptionCharge db paymentId chargeId charge now
                              appendLedgerPlan db badgePurchaseId Nothing (chargeEntries periodStart now badgeType months purchaseKey chargeId entryIds tip) Nothing
                              setPurchaseBadgeType db badgePurchaseId badgeType now
                              pure $ Just months
                        Left _ -> Nothing <$ forM_ latestCharge (\charge -> insertSubscriptionCharge db paymentId newChargeId charge now)
                      setSubscriptionState db paymentId subscriptionState now
                      pure bankedMonths
                    else pure Nothing
                (ended, banked,) <$> getLedgerTip db badgePurchaseId
              case r of
                Left _ -> pure $ errorResponse BSEInternal
                Right (True, _, _) -> refundRemaining purchase tip paymentId now
                Right (False, banked, tip_) -> do
                  forM_ banked logBanked
                  issueDue purchase tip_ (Just subscriptionState) now
    -- While coverage runs the store is asked before each new month, so a refund ends the months not yet issued; a store
    -- that fails or does not answer ends nothing, and the month is issued as before.
    checkStoreBeforeMonth purchase tip subscription@PurchaseSubscription {paymentId, subscriptionRef, storedState} now
      | held, storedState == SSEnded = refundRemaining purchase tip paymentId now
      | held = issueDue purchase (Just tip) (Just storedState) now
      | otherwise =
          withDB' "stampSubscriptionCheck" cc (\db -> stampSubscriptionCheck db paymentId now) >>= \case
            Left _ -> pure $ errorResponse BSEInternal
            Right () ->
              checkStoreSubscription verifier subscriptionRef Nothing >>= \case
                Left refusal -> storeRefusalResponse refusal >> issueDue purchase (Just tip) (Just storedState) now
                Right checked@SubscriptionCheck {subscriptionState, latestCharge} -> do
                  chargeId <- randomId cc
                  r <- withDB' "recordSubscriptionCheck" cc $ \db -> do
                    ended <- coverageEnded db paymentId checked
                    -- a new charge is only recorded: it is granted once coverage ends, as a renewal
                    unless ended $ forM_ latestCharge $ \charge -> insertSubscriptionCharge db paymentId chargeId charge now
                    setSubscriptionState db paymentId (if ended then SSEnded else subscriptionState) now
                    pure ended
                  case r of
                    Left _ -> pure $ errorResponse BSEInternal
                    Right True -> refundRemaining purchase tip paymentId now
                    Right False -> issueDue purchase (Just tip) (Just subscriptionState) now
      where
        held = isJust $ checkWait now subscription
    refundRemaining ServicePurchase {badgePurchaseId} tip paymentId now = do
      (lapseId, refundId) <- (,) <$> randomId cc <*> randomId cc
      let lapsed = lapseEntry now lapseId tip
          refund@StatementEntry {changeMonths = refunded} = debitEntry now refundId SDRefund (fromMaybe tip lapsed)
      r <- withDB' "refundSubscription" cc $ \db -> do
        tip' <- lockedLedgerTip db badgePurchaseId now
        if fmap entryId tip' /= Just (entryId tip)
          then pure Nothing
          else do
            -- with no months left there is nothing to refund, as when an earlier request already refunded them
            unless (refunded == 0) $ appendLedgerPlan db badgePurchaseId Nothing (maybeToList lapsed <> [refund]) Nothing
            setSubscriptionState db paymentId SSEnded now
            Just . withoutCredential <$> issueResponse db badgePurchaseId now (Just SSEnded)
      case r of
        Right (Just resp) -> resp <$ unless (refunded == 0) (logInfo $ "store subscription ended during coverage: " <> tshow (negate refunded) <> " months refunded")
        Right Nothing -> logError "badge service: a subscription's ledger moved while its refund was written" $> errorResponse BSEInternal
        Left _ -> pure $ errorResponse BSEInternal
    answered what action = fromRight (errorResponse BSEInternal) <$> withDB' what cc action
    withoutCredential = \case
      BSPBadgeCredential {receipt, statement, subscription} -> BSPBadgeCredential {credential = Nothing, receipt, statement, subscription}
      resp -> resp
    logBanked months = logInfo $ "store subscription charge banked: " <> tshow months <> " months"
    -- Only the asserted entry's identity is read, never the months it claims.
    issueResponse db purchaseId t subscription = do
      let StatementEntry {entryId = assertedUuid} = lastEntry
      assertedId <- getLedgerEntryId db purchaseId assertedUuid
      -- TODO [badges] when the assertion does not resolve, heal the ledger and restate it as a
      -- single opening credit, rather than resending the whole history
      entries_ <- getLedgerEntries db purchaseId (fromMaybe 0 assertedId)
      credential_ <- getCurrentIssuance db purchaseId t
      pure $ maybe (errorResponse BSEInternal) (credentialResponse credential_ (assertedUuid <$ assertedId) subscription) entries_

getIssuePurchase :: DB.Connection -> C.PublicKeyEd25519 -> IO (Maybe (ServicePurchase, Maybe StatementEntry, Maybe PurchaseSubscription))
getIssuePurchase db purchaseKey = do
  p_ <- getPurchaseByKey db purchaseKey
  forM p_ $ \p@ServicePurchase {badgePurchaseId} -> (p,,) <$> getLedgerTip db badgePurchaseId <*> getPurchaseSubscription db badgePurchaseId

-- | Due with each month, within coverage too, so a refund ends the months not yet issued; at most once an interval.
subscriptionCheckDue :: StoreChecks -> UTCTime -> StatementEntry -> PurchaseSubscription -> Bool
subscriptionCheckDue ChecksAtEveryRequest _ _ _ = True
subscriptionCheckDue ChecksAfterCoverage now tip subscription =
  balanceStartTs tip <= now && isNothing (checkWait now subscription)

-- | What remains of the interval since the subscription last asked its store, if anything.
checkWait :: UTCTime -> PurchaseSubscription -> Maybe NominalDiffTime
checkWait now PurchaseSubscription {checkedAt} =
  mfilter (> 0) $ (\t -> subscriptionCheckInterval - diffUTCTime now t) <$> checkedAt

-- | Each subscription asks its store at most this often, failed attempts included, so a client
-- repeating its requests cannot spend the store quota every subscriber shares.
subscriptionCheckInterval :: NominalDiffTime
subscriptionCheckInterval = 3600

-- | The next check waits out the interval whatever the client is told, so its retry waits as long.
atLeastCheckInterval :: BadgeServiceResponse -> BadgeServiceResponse
atLeastCheckInterval = \case
  BSPError {code, message, retryAfter = Just t} -> BSPError {code, message, retryAfter = Just $ max t $ round subscriptionCheckInterval}
  resp -> resp

-- | The subscription claims the purchase path asked a store about, one map per store: the App Store's by original
-- transaction id, Play's by the token's hash, so Play tokens, which cost nothing to make up, cannot fill the App Store's;
-- and the minute's allowance of Play reads.
data PurchaseChecks = PurchaseChecks {appleChecks :: ClaimChecks, googleChecks :: ClaimChecks, playReads :: PlayReads}

type ClaimChecks = TVar (M.Map ClaimKey UTCTime)

-- | A claim is held per subscription and per what its receipt was presented for, so a restore on another
-- device is not held off by the purchase the buyer made minutes earlier, and each still asks at most once an interval.
data ClaimKey = ClaimKey {claimRef :: T.Text, claimPurpose :: ClaimPurpose}
  deriving (Eq, Ord)

data ClaimPurpose = ClaimPurchase | ClaimTransfer
  deriving (Eq, Ord)

-- | The Play subscription checks the purchase path made in the current minute, counted as the web listener counts requests.
type PlayReads = TVar (Maybe Bucket)

newPurchaseChecks :: IO PurchaseChecks
newPurchaseChecks = PurchaseChecks <$> newTVarIO M.empty <*> newTVarIO M.empty <*> newTVarIO Nothing

-- | How long a claim waits before its store is asked again; Nothing records a check at now. A claim that finds
-- the table full of checks inside the interval is asked without being recorded, so a full table refuses no one.
purchaseCheckWait :: ClaimChecks -> UTCTime -> ClaimKey -> STM (Maybe NominalDiffTime)
purchaseCheckWait checks now key = stateTVar checks $ \m ->
  let live = M.filter (\t -> diffUTCTime now t < subscriptionCheckInterval) m
   in case M.lookup key live of
        Just t -> (Just $ subscriptionCheckInterval - diffUTCTime now t, live)
        Nothing
          | M.size live >= maxPurchaseChecks -> (Nothing, live)
          | otherwise -> (Nothing, M.insert key now live)

-- | Per store, far above an hour of genuine purchases: every App Store key is one Apple signed for this app,
-- while Play keys cost nothing to make up, so a full Play map only stops recording Play claims.
maxPurchaseChecks :: Int
maxPurchaseChecks = 10000

data SubscriptionAsk = AskStore | ClaimHeld NominalDiffTime | NoPlayRead NominalDiffTime

-- | How long until the purchase path may read Play again; Nothing takes one of this minute's reads.
playReadWait :: PlayReads -> UTCTime -> STM (Maybe NominalDiffTime)
playReadWait taken now = stateTVar taken $ \b_ -> either (\wait -> (Just wait, b_)) (\b -> (Nothing, Just b)) (takeFrom maxPlayPurchaseReads now b_)

-- | A tenth of Play's default Subscriptions quota of 3,000 reads a minute, which renewal checks share: any token in the
-- accepted grammar costs a read, so this caps what made-up tokens can spend; far above a minute of genuine purchases.
maxPlayPurchaseReads :: Int
maxPlayPurchaseReads = 300

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module StoreService.Service
  ( ServiceState (..),
    newServiceState,
    configureStoreVerifier,
    welcomeGetOpts,
    trustDevKeys,
    checkDevChain,
    requireDevKeys,
    checkIssuerKey,
    storeService,
    storeServiceCLI,
    storeServiceResponse,
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

import StoreService.Catalog (StoreProduct (..), defaultCatalog, storeProduct)
import StoreService.Codes (issueFailedText, issueOneCode, singleUse)
import StoreService.Config (AppleStoreConfig (..), BadgeIssuerKey (..), ChainConfig (..), NamesConfig (..), PlayStoreConfig (..), ServiceConfig (..), localChainId, minNameYears, readServiceConfig)
import StoreService.Group (GroupEvent, ensureManagedGroup, fmtTime, groupEvent, hasTracker, refreshTracker, revokeWithTracker, runGroupLane)
import StoreService.Group.Command (badgeTypeP, codeP, maxMonths, maxNameYears, nameCodeP, textTokenP)
import StoreService.Names.Balances (RefundRecord (..), RefundRefusal (..), recordRefund)
import StoreService.Names.Chain (newChainClient, nodeChainId)
import StoreService.Names.Codes (issueNameCode, revokeNameCode)
import StoreService.Names.Env (NamesEnv (..), newNamesEnv)
import StoreService.Names.Orders (ordersEnv)
import StoreService.Names.Phases (RegistrationId (..))
import StoreService.Names.Records (runRecordEdits)
import StoreService.Names.Sender (runSender)
import StoreService.Names.Store (Balance (..), BalanceHold (..), BalanceLink (..), LateCredit (..), RegistrationPayment (..), balanceByReference, balanceChain, methodText)
import StoreService.Names.Turns (Turns, newTurns)
import StoreService.Names.Worker (newWorker, runCodeSweep, runWorkers)
import StoreService.Options
import StoreService.Poller (expiryGrace, newPollerEnv, newReadHints, runPoller)
import StoreService.Providers (Provider)
import StoreService.Providers.BTCPay (btcpayProvider)
import StoreService.Providers.Stripe (stripeProvider)
import StoreService.Store
import StoreService.Store.Invoices (seedCatalog, truncateToSecond)
import StoreService.Store.Migrate (runStoreServiceMigrations)
import StoreService.StoreReceipts
import StoreService.StoreReceipts.Apple (appleSubscriptionChecker, readAppleRoot, verifyAppleTransaction)
import StoreService.StoreReceipts.Google (PlayVerifier (..), playStoreVerifier)
import StoreService.StoreReceipts.Mock (newMockStoreVerifier)
import StoreService.Waiters (Waiters, newWaiters)
import StoreService.Web.Http (Bucket, takeFrom)
import StoreService.Web.Server (checkSiteDirs, exportWebapp, newWebEnv, runWebListeners)
import Control.Applicative (optional)
import Control.Concurrent.STM
import StoreService.Log (logError, logInfo, logWarn)
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as J
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Attoparsec.ByteString.Char8 as A
import Data.Bifunctor (first)
import Data.ByteString.Char8 (ByteString)
import Data.Char (isSpace)
import Data.Either (fromRight, isRight)
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
import Simplex.Chat.StoreService
import Simplex.Chat.Badges.StorePlan (storePlan)
import Simplex.Chat.Badges.Types (BadgeCodePaymentStatus (..))
import Simplex.Chat.Bot (initializeBotAddress')
import Simplex.Chat.Bot.Store (withDB, withDB')
import Simplex.Chat.Controller
import Simplex.Chat.Core (sendChatCmd, simplexChatCore)
import Simplex.Chat.Options (printDbOpts)
import Simplex.Chat.PaymentService (AppleTransactionFacts (..), ServicePayment (..), appleTransactionFacts)
import Simplex.Chat.PaymentService.Types (InvoiceId (..), PaymentProvider (..), StoreTransactionRef (..))
import Simplex.Chat.Terminal (terminalChatConfig)
import Simplex.Chat.Terminal.Main (simplexChatCLI')
import Simplex.Chat.Types (AgentInvId (..), User (..))
import Simplex.Messaging.Agent.Store.Common (DBStore)
import qualified Simplex.Messaging.Agent.Store.DB as DB
import qualified Simplex.Messaging.Crypto as C
import Simplex.Messaging.Crypto.BBS (bbsPublicKey)
import Simplex.Messaging.Encoding.String (strEncode)
import Simplex.Messaging.Util (catchOwn, catchOwn', raceAny_, safeDecodeUtf8, tshow)
import Simplex.Messaging.Version (isCompatible)
import System.Directory (getAppUserDataDirectory)
import System.Exit (exitFailure)

data ServiceState = ServiceState
  { serviceCC :: TMVar ChatController,
    serviceRequestQ :: TQueue (User, AgentInvId, Maybe C.PublicKeyEd25519, J.Object),
    groupEventQ :: TQueue GroupEvent,
    storeVerifier :: StoreVerifier
  }

-- | With no store verifier, which store_service.ini configures, every store receipt not already credited
-- is answered provider_not_configured.
newServiceState :: IO ServiceState
newServiceState = do
  serviceCC <- newEmptyTMVarIO
  serviceRequestQ <- newTQueueIO
  groupEventQ <- newTQueueIO
  pure ServiceState {serviceCC, serviceRequestQ, groupEventQ, storeVerifier = noStoreVerifier}

welcomeGetOpts :: IO StoreServiceOpts
welcomeGetOpts = do
  appDir <- getAppUserDataDirectory "simplex"
  opts@StoreServiceOpts {coreOptions, testing, serviceName} <- getStoreServiceOpts appDir "simplex_badge_service"
  unless testing $ do
    putStrLn $ "SimpleX Store Service v" ++ versionNumber
    printDbOpts coreOptions
    putStrLn $ "Service name: " ++ T.unpack serviceName
  pure opts

-- | The ini's dev_keys, trusted beside the clients' keys; the config accepts them only on the local chain.
trustDevKeys :: Maybe ServiceConfig -> ChatConfig -> IO (Either String ChatConfig)
trustDevKeys serviceCfg cfg@ChatConfig {badgePublicKeys} = do
  r <- fmap trusted . sequence <$> mapM publicKey devKeys
  when (isRight r && not (null devKeys)) $
    logWarn $ "trusting issuer dev_keys at indexes " <> T.intercalate ", " (map (\BadgeIssuerKey {keyIdx} -> tshow keyIdx) devKeys) <> "; no client trusts them, never set them in production"
  pure r
  where
    devKeys = maybe [] devIssuerKeys serviceCfg
    trusted ks = cfg {badgePublicKeys = M.union badgePublicKeys (M.fromList ks)}
    publicKey BadgeIssuerKey {keyIdx, secretKey}
      | M.member keyIdx badgePublicKeys = pure $ Left $ "issuer.dev_keys index " <> show keyIdx <> " is the index of a key clients trust"
      | otherwise = fmap (keyIdx,) . first (("issuer.dev_keys secret at index " <> show keyIdx <> " is not a valid key: ") <>) <$> bbsPublicKey secretKey

-- | With dev keys, the node itself must be the local chain, not only the ini's chain_id.
checkDevChain :: Maybe ServiceConfig -> IO (Either String ())
checkDevChain = \case
  Just ServiceConfig {devIssuerKeys = _ : _, names = Just NamesConfig {nChain}} -> do
    chain <- C.newRandom >>= (`newChainClient` nChain)
    nodeChainId chain <&> \case
      Right chainId
        | chainId == localChainId -> Right ()
        | otherwise -> Left $ refused <> "is " <> show chainId <> ", not " <> show localChainId
      Left _ -> Left $ refused <> "could not be read"
  _ -> pure $ Right ()
  where
    refused = "issuer.dev_keys is refused: the node's chain id "

checkIssuerKey :: StoreServiceOpts -> Maybe ServiceConfig -> ChatConfig -> IO (Either String BadgeIssuerKey)
checkIssuerKey StoreServiceOpts {issuerKey} serviceCfg cfg = case issuerKey of
  Left e -> pure (Left e)
  Right (Just k) -> checkOne cfg k
  Right Nothing -> case serviceCfg >>= issuer of
    Nothing -> pure $ Left "an issuer key is required - pass --issuer-key-idx and --issuer-secret, or add an [issuer] section to store_service.ini (see `simplex-chat badge keygen`)"
    Just k -> checkOne cfg k

checkOne :: ChatConfig -> BadgeIssuerKey -> IO (Either String BadgeIssuerKey)
checkOne ChatConfig {badgePublicKeys} k@BadgeIssuerKey {keyIdx, secretKey} =
  bbsPublicKey secretKey >>= \case
    Left e -> pure $ Left $ "issuer secret at index " <> show keyIdx <> " is not a valid key: " <> e
    Right pk -> pure $ case M.lookup keyIdx badgePublicKeys of
      Just pk' | pk' == pk -> Right k
      Just _ -> Left $ "issuer secret does not match the configured key at index " <> show keyIdx <> ", its public key is " <> T.unpack (safeDecodeUtf8 $ strEncode pk)
      Nothing -> Left $ "no configured badge key at index " <> show keyIdx <> ", clients could not verify what this service signs"

requireIssuerKey :: StoreServiceOpts -> Maybe ServiceConfig -> ChatConfig -> IO BadgeIssuerKey
requireIssuerKey opts serviceCfg cfg =
  checkIssuerKey opts serviceCfg cfg >>= orExit >>= \k@BadgeIssuerKey {keyIdx} -> logInfo ("signing badges with issuer key " <> tshow keyIdx) $> k

requireDevKeys :: Maybe ServiceConfig -> ChatConfig -> IO ChatConfig
requireDevKeys serviceCfg cfg = do
  checkDevChain serviceCfg >>= orExit
  trustDevKeys serviceCfg cfg >>= orExit

orExit :: Either String a -> IO a
orExit = either (\e -> putStrLn ("Error: " <> e) >> exitFailure) pure

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
      roots <- mapM ((>>= orExit) . readAppleRoot) aRootCertificates
      appleSubscriptions <- forM aServerApi $ (>>= orExit) . appleSubscriptionChecker roots aBundleId
      pure (verifyAppleTransaction roots aBundleId, appleSubscriptions)
    google <- forM playStore $ (>>= orExit) . playStoreVerifier
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
    notConfigured = pure $ Left SRNotConfigured

storeService :: StoreServiceOpts -> ChatConfig -> ServiceState -> IO ()
storeService opts@StoreServiceOpts {serviceConfigFile} baseCfg env = do
  serviceCfg <- traverse readConfigOrExit serviceConfigFile
  forM_ serviceCfg $ \sc -> checkSiteDirs (listener sc) >>= orExit
  cfg <- requireDevKeys serviceCfg baseCfg
  key <- requireIssuerKey opts serviceCfg cfg
  waiters <- newWaiters
  -- The hooks' //revoke takes a prepaid code's turn, so the turns exist before the chat starts.
  turns <- newTurns
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
        lanes <- maybe (pure []) (serviceLanes waiters turns cc) serviceCfg
        -- The group is resolved before the other lanes start, so a failed lookup exits at once rather than waiting for them to end.
        groupLane_ <- forM groupCfg $ \gc -> runGroupLane cc (groupEventQ env) <$> ensureManagedGroup cc gc
        raceAny_ $ processQueuedRequests key trackerQ_ requestEnv : maybeToList groupLane_ <> lanes
      chatHooks =
        defaultChatHooks
          { preStartHook = Just $ badgePreStartHook opts,
            postStartHook = Just $ badgePostStartHook opts env,
            preCmdHook = Just $ badgeCmdHook turns
          }
  -- The reader runs from the start and must not block, since outputQ carries every chat event and a full queue stalls the core.
  simplexChatCore cfg {chatHooks} (mkChatOpts opts) $ \_ cc -> raceAny_ [readEvents cc, startLanes cc]
  where
    serviceLanes :: Waiters -> Turns -> ChatController -> ServiceConfig -> IO [IO ()]
    serviceLanes ws turns cc@ChatController {chatStore} sc = do
      -- Seed before the listener accepts anything, since every checkout is priced from these.
      seedServiceCatalog chatStore
      btc <- maybe (pure []) (fmap (: []) . btcpayProvider) (btcpay sc)
      str <- maybe (pure []) (fmap (: []) . stripeProvider) (stripe sc)
      let providers = btc <> str
      hints <- newReadHints
      namesEnv <- forM (names sc) (startNames cc turns ws providers)
      webEnv <- newWebEnv chatStore sc ws hints providers namesEnv
      exportWebapp sc
      pollerEnv <- newPollerEnv chatStore ws (nePhases <$> namesEnv) hints providers (poll sc)
      nameLanes <- maybe (pure []) namesLanes namesEnv
      pure $ [runWebListeners webEnv, runPoller pollerEnv] <> nameLanes

startNames :: ChatController -> Turns -> Waiters -> [Provider] -> NamesConfig -> IO NamesEnv
startNames cc@ChatController {chatStore} turns waiters providers nc@NamesConfig {nTLD, nCodesOnly, nChain = ChainConfig {cChainId}} =
  readTVarIO (currentUser cc) >>= \case
    Nothing -> putStrLn "No current user" >> exitFailure
    Just User {userId} -> do
      logInfo $ "names: on, TLD " <> safeDecodeUtf8 (strEncode nTLD) <> ", chain " <> tshow cChainId <> codesOnlyNote
      newNamesEnv cc userId nc chatStore turns waiters providers
  where
    codesOnlyNote = if nCodesOnly then ", codes only: registration needs a prepaid code" else ""

-- | The workers resume every registration in their phases, and the code sweep runs at the start and then daily.
namesLanes :: NamesEnv -> IO [IO ()]
namesLanes ne@NamesEnv {neChain, neSender, neRecords} = do
  worker <- newWorker (ordersEnv ne) neChain neSender
  pure [runSender neSender, runRecordEdits neRecords, runWorkers worker, runCodeSweep worker]

seedServiceCatalog :: DBStore -> IO ()
seedServiceCatalog st = do
  now <- truncateToSecond <$> getCurrentTime
  let (prices, offers) = defaultCatalog now
  (seededPrices, seededOffers) <- seedCatalog st prices offers
  logInfo $
    "badge catalog: " <> tshow (length prices) <> " prices and " <> tshow (length offers) <> " offers compiled in, "
      <> tshow seededPrices <> " prices and " <> tshow seededOffers <> " offers inserted"

storeServiceCLI :: StoreServiceOpts -> IO ()
storeServiceCLI opts@StoreServiceOpts {serviceConfigFile} = do
  serviceCfg <- traverse readConfigOrExit serviceConfigFile
  cfg <- requireDevKeys serviceCfg terminalChatConfig
  key <- requireIssuerKey opts serviceCfg cfg
  env <- newServiceState >>= configureStoreVerifier serviceCfg
  -- This mode runs no names lanes, so only its own //revoke takes these turns.
  turns <- newTurns
  let eventHook _cc = \case
        Right (CEvtServiceRequest u reqId sigKey reqData) -> do
          atomically $ writeTQueue (serviceRequestQ env) (u, reqId, sigKey, reqData)
          pure $ Right $ CEvtServiceRequest u reqId sigKey (shownServiceRequest reqData)
        ev -> pure ev
      chatHooks =
        defaultChatHooks
          { preStartHook = Just $ badgePreStartHook opts,
            postStartHook = Just $ badgePostStartHook opts env,
            preCmdHook = Just $ badgeCmdHook turns,
            eventHook = Just eventHook
          }
  raceAny_
    [ simplexChatCLI' cfg {chatHooks} (mkChatOpts opts) Nothing,
      processQueuedRequests key Nothing env
    ]

-- | The terminal prints each request, and requests carry codes and store receipts, which are bearer secrets.
shownServiceRequest :: J.Object -> J.Object
shownServiceRequest reqData = case KM.lookup "request" reqData of
  Just (J.Object cmd) | Just t <- KM.lookup "type" cmd -> KM.singleton "request" $ J.object ["type" J..= t]
  _ -> KM.empty

badgeCmdHook :: Turns -> ChatController -> ChatCommand -> IO (Either (Either ChatError ChatResponse) ChatCommand)
badgeCmdHook turns cc = \case
  CustomChatCommand cmd -> Left <$> runBadgeCmd turns cc cmd
  cmd -> pure $ Right cmd

runBadgeCmd :: Turns -> ChatController -> ByteString -> IO (Either ChatError ChatResponse)
runBadgeCmd turns cc cmd
  | Right issueOpts <- A.parseOnly issueCmdP cmd =
      issuedReply formatBadgeCode <$> issueBadgeCode cc issueOpts
  | Right (len, years, paymentStatus) <- A.parseOnly issueNameCmdP cmd =
      issuedReply (formatNameCode . NCPrepaid) <$> issueNameCode cc len years paymentStatus
  | Right code <- A.parseOnly revokeCmdP cmd = textReply <$> revokeWithTracker cc code
  | Right code <- A.parseOnly revokeNameCmdP cmd = textReply <$> revokeNameCode turns cc code
  | Right (ref, cents_) <- A.parseOnly refundCmdP cmd = textReply <$> maybe (showRefund cc ref) (refundBalanceCmd turns cc ref) cents_
  | otherwise =
      pure $ chatCmdError $
        "Usage: //issue supporter|legend|investor [months 1-" <> show maxMonths <> "] [paid|unpaid|free], //issue name <length "
          <> show minNameLength <> "-" <> show maxNameLength <> "> [years " <> show minNameYears <> "-" <> show maxNameYears
          <> "] [paid|unpaid|free], //revoke <code>, or //refund <reference> [<cents>]"
  where
    issuedReply :: (c -> T.Text) -> Either String c -> Either ChatError ChatResponse
    issuedReply format = \case
      Right code -> Right CRCustomChatResponse {user_ = Nothing, response = "Code: " <> format code}
      Left _ -> chatCmdError (T.unpack issueFailedText)
    textReply = \case
      Right response -> Right CRCustomChatResponse {user_ = Nothing, response}
      Left e -> chatCmdError (T.unpack e)

revokeCmdP :: A.Parser BadgeCode
revokeCmdP = "revoke " *> codeP <* (A.skipSpace *> A.endOfInput)

revokeNameCmdP :: A.Parser NamePrepaidCode
revokeNameCmdP = "revoke " *> prepaidCodeP <* (A.skipSpace *> A.endOfInput)
  where
    prepaidCodeP =
      parseNameCode . safeDecodeUtf8 <$> A.takeWhile1 (not . isSpace) >>= \case
        Just (NCPrepaid code) -> pure code
        _ -> fail "not a prepaid name code"

-- | The reference, and the cents of the balance to refund; without them the balance is shown.
refundCmdP :: A.Parser (T.Text, Maybe Int)
refundCmdP = do
  ref <- "refund " *> (safeDecodeUtf8 <$> A.takeWhile1 (not . isSpace))
  cents_ <- optional (A.space *> (A.decimal :: A.Parser Integer))
  A.skipSpace
  A.endOfInput
  (ref,) <$> mapM checkCents cents_
  where
    -- Integer, as in issueCmdP, so a figure past Int is refused rather than wrapped.
    checkCents n
      | n >= 1 && n <= toInteger (maxBound :: Int) = pure (fromInteger n)
      | otherwise = fail "cents out of range"

-- | Left is a refusal or a failure; both sides are the text to show the operator.
showRefund :: ChatController -> T.Text -> IO (Either T.Text T.Text)
showRefund cc ref = do
  now <- getCurrentTime
  withDB' "showRefund" cc (\db -> balanceByReference db ref now >>= mapM (withChain db)) <&> \case
    Right Nothing -> Left $ "No order has the reference " <> ref <> "."
    Right (Just (RegistrationId rid, Nothing)) -> Left $ "Order " <> rid <> " has no balance."
    Right (Just (RegistrationId rid, Just (balance@Balance {blAmount, blCredited, blRefunded}, chain))) ->
      Right $
        "Order " <> rid <> ": balance " <> dollars blAmount <> " (credited " <> dollars blCredited <> ", refunded " <> dollars blRefunded
          <> "). Paid by: " <> chainText chain <> ". " <> nextStep balance
    Left _ -> Left "The balance could not be read."
  where
    withChain db (rid, balance_) = (rid,) <$> mapM (\balance -> (balance,) <$> balanceChain db rid) balance_
    nextStep Balance {blAmount, blHold} = case blHold of
      Just hold -> "It cannot be refunded now: " <> heldText hold <> "."
      Nothing
        | blAmount > 0 -> "To record its refund: //refund " <> ref <> " " <> tshow blAmount
        | otherwise -> "Nothing to refund."

-- | The refund is recorded before support pays it, so the balance cannot be spent meanwhile.
refundBalanceCmd :: Turns -> ChatController -> T.Text -> Int -> IO (Either T.Text T.Text)
refundBalanceCmd turns ChatController {chatStore} ref cents = do
  now <- getCurrentTime
  ((Right <$> recordRefund turns chatStore ref cents now) `catchOwn` (pure . Left)) >>= \case
    Right (Right RefundRecord {rdRegistration = RegistrationId rid, rdCents, rdChain}) -> do
      -- The registration id, not the reference, since an invoice id is kept out of the logs.
      logInfo $ "names: recorded a refund of " <> tshow rdCents <> " cents for order " <> rid
      pure $ Right $ "Recorded a refund of " <> dollars rdCents <> " for order " <> rid <> ". Refund it now, by: " <> chainText rdChain <> "."
    Right (Left refusal) -> pure $ Left $ "Nothing recorded: " <> refusalText refusal <> "."
    Left e -> Left "The refund could not be recorded." <$ logError ("names: recording a refund failed: " <> tshow e)
  where
    refusalText = \case
      RefundNoOrder -> "no order has the reference " <> ref
      RefundNoBalance -> "the order of " <> ref <> " has no balance"
      RefundAmount amount -> "the balance is " <> dollars amount <> " now, not " <> dollars cents
      RefundHeld hold -> heldText hold

heldText :: BalanceHold -> T.Text
heldText BalanceHold {bhHolder = RegistrationId holder, bhUntil, bhOverdue, bhFunded}
  | bhOverdue = heldBy <> "its invoice expired at " <> fmtTime bhUntil <> "; start the service so the poller closes that order, " <> tshow graceMinutes <> " minutes after the expiry at the earliest, then stop it and run //refund again"
  | bhFunded = heldBy <> "whose invoice holds money; it holds until that payment settles or the invoice closes; run the command again then"
  | otherwise = heldBy <> "its invoice open until " <> fmtTime bhUntil <> "; run the command again then"
  where
    heldBy = "held by order " <> holder <> ", "
    graceMinutes = round (expiryGrace / minute) :: Int
    minute = 60 :: NominalDiffTime

-- | From the balance's own order back to the first; a part paid from a balance names the next order, whose balance paid it.
chainText :: [BalanceLink] -> T.Text
chainText chain = T.intercalate "; " $ zipWith linkText chain (map (Just . lkRegistration) (drop 1 chain) <> [Nothing])
  where
    linkText BalanceLink {lkRegistration = RegistrationId rid, lkPayment, lkPaid, lkFromBalance, lkLateCredits} source_ =
      "order " <> rid <> ": " <> case paid lkPayment lkPaid lkLateCredits <> fromBalance lkFromBalance source_ of
        [] -> "no payment recorded"
        parts -> T.intercalate ", " parts
    paid payment cents lateCredits = case payment of
      RPInvoice method (InvoiceId invId) ->
        let by = " by " <> methodText method <> " on invoice " <> invId
            late = sum $ map lcCents lateCredits
            -- an order its invoice paid is credited only the extra of an overpayment
            kind = if cents > 0 then " overpaid" else " late"
         in [dollars cents <> by | cents > 0] <> [dollars late <> kind <> by | late > 0]
      RPCode _ -> ["a prepaid code"]
      RPBalance -> []
    fromBalance cents = \case
      Just (RegistrationId source) | cents > 0 -> [dollars cents <> " from the balance of order " <> source]
      _ -> []

dollars :: Int -> T.Text
dollars cents = "$" <> tshow whole <> "." <> T.justifyRight 2 '0' (tshow part)
  where
    (whole, part) = cents `quotRem` centsPerDollar
    centsPerDollar = 100

issueNameCmdP :: A.Parser (Int, Int, BadgeCodePaymentStatus)
issueNameCmdP = do
  (len, years) <- "issue " *> nameCodeP
  paymentStatus <- fromMaybe CPSFree <$> optional (A.space *> textTokenP)
  A.skipSpace
  A.endOfInput
  pure (len, years, paymentStatus)

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
  J.Success StoreServiceRequest {request = STCPurchaseBadge {payment = SPGoogle {}, upgrade = Nothing}} -> pure PurchaseLane
  J.Success StoreServiceRequest {request = STCPurchaseBadge {payment = SPApple {jws}, upgrade = Nothing}}
    | maybe False autoRenewable (appleTransactionFacts jws) -> pure PurchaseLane
  J.Success StoreServiceRequest {request = STCTransferBadge {payment = SPApple {jws}}}
    | maybe False autoRenewable (appleTransactionFacts jws) -> pure PurchaseLane
  J.Success StoreServiceRequest {request = STCTransferBadge {payment = SPGoogle {}}} -> pure PurchaseLane
  J.Success StoreServiceRequest {purchaseKey = Just k, request = STCIssueBadge {}}
    | sigKey == Just k -> do
        now <- badgeNow cc
        withDB' "requestLane" cc (\db -> getIssuePurchase db k) <&> \case
          -- a due month goes to the serial renewal lane even within the hour, so it queues behind a check still in flight
          Right (Just (_, Just tip, Just _))
            | checks == ChecksAtEveryRequest || balanceStartTs tip <= addUTCTime routeLookahead now -> RenewalLane
          Right _ -> MainLane
          -- a read that failed may hide a due check, and the main lane is what must not wait on a store
          Left _ -> RenewalLane
  J.Success StoreServiceRequest {purchaseKey = Just k, request = STCUpgradeBadgeSubscription {}}
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
survive what action = action `catchOwn'` \_ -> logError $ "store service: " <> what <> " failed; the next one will be handled"

badgePreStartHook :: StoreServiceOpts -> ChatController -> IO ()
badgePreStartHook opts ChatController {config, chatStore} =
  runStoreServiceMigrations opts config chatStore

badgePostStartHook :: StoreServiceOpts -> ServiceState -> ChatController -> IO ()
badgePostStartHook StoreServiceOpts {noAddress, testing} env cc = do
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
  logInfo $ "store service request " <> reqIdT
  resp <-
    storeServiceResponse key verifier checks cc trackerQ_ sigKey reqData `catchOwn'` \_ ->
      logError ("store service request " <> reqIdT <> " failed") $> errorResponse STEInternal
  logInfo $ "badge answer: " <> responseName resp
  sendChatCmd cc (APISendServiceResponse userId reqId (responseObject resp)) >>= \case
    Right _ -> pure ()
    Left e -> logError $ "store service response failed for " <> reqIdT <> ": " <> tshow e

responseObject :: StoreServiceResponse -> J.Object
responseObject r = case J.toJSON r of
  J.Object o -> o
  _ -> KM.fromList [("type", J.String "error"), ("code", J.toJSON STEInternal)]

errorResponse :: StoreServiceErrorCode -> StoreServiceResponse
errorResponse code = STPError {code, message = Nothing, retryAfter = badgeErrorRetryAfter code}

badgeErrorRetryAfter :: StoreServiceErrorCode -> Maybe Word32
badgeErrorRetryAfter = \case
  STEPaymentPending -> Just 300
  STEProviderUnavailable -> Just 300
  STERateLimited -> Just 60
  _ -> Nothing


-- | The agent verified the signature, so sigKey is a key the sender holds; a differing purchaseKey would let a client claim a purchase it cannot sign for.
storeServiceResponse :: BadgeIssuerKey -> StoreVerifier -> PurchaseChecks -> ChatController -> Maybe (TQueue GroupEvent) -> Maybe C.PublicKeyEd25519 -> J.Object -> IO StoreServiceResponse
storeServiceResponse key verifier checks cc trackerQ_ sigKey reqData = case J.fromJSON (J.Object reqData) of
  J.Error _ -> pure $ errorResponse STEBadRequest
  J.Success StoreServiceRequest {version, purchaseKey, request}
    | not (version `isCompatible` supportedStoreServiceVRange) -> pure $ errorResponse STEUnsupportedVersion
    | purchaseKey /= sigKey -> pure $ errorResponse STEBadRequest
    | otherwise -> logInfo ("badge request: " <> commandName request) >> case request of
        STCRedeemBadgeCode {masterKey, code} -> case purchaseKey of
          Just k -> redeemCode key cc trackerQ_ k masterKey code
          Nothing -> pure $ errorResponse STEBadRequest
        STCPurchaseBadge {masterKey, payment, upgrade}
          | Just receipt_ <- toStoreReceipt verifier payment -> case (purchaseKey, upgrade) of
              (Just k, Nothing) -> either storeRefusalResponse (purchaseWithReceipt key cc verifier checks RUPurchase k masterKey) receipt_
              -- store upgrades are not built, and ignoring one would credit its months at the discounted price
              (Just _, Just _) -> pure $ errorResponse STEBadRequest
              (Nothing, _) -> pure $ errorResponse STEBadRequest
        STCIssueBadge {balance} -> case purchaseKey of
          Just k -> issueBadgeCmd key cc verifier k Nothing balance
          Nothing -> pure $ errorResponse STEBadRequest
        -- only an App Store subscription changes tier on the same transaction; Play gives a change a new token
        STCUpgradeBadgeSubscription {payment = payment@SPApple {}, balance}
          | Just k <- purchaseKey,
            Just receipt_ <- toStoreReceipt verifier payment ->
              either storeRefusalResponse (upgradeSubscriptionCmd key cc verifier k balance) receipt_
        STCUpgradeBadgeSubscription {} -> pure $ errorResponse STEBadRequest
        STCTransferBadge {masterKey, payment}
          | Just k <- purchaseKey,
            Just receipt_ <- toStoreReceipt verifier payment ->
              either storeRefusalResponse (purchaseWithReceipt key cc verifier checks (RUTransfer $ presentedTransaction payment) k masterKey) receipt_
        STCTransferBadge {} -> pure $ errorResponse STEBadRequest
        -- Every command but redeemBadgeCode and purchaseBadge from a store needs a key the service already knows; those two create the purchase, so their key is unknown on a first purchase.
        _ -> case purchaseKey of
          Nothing -> pure $ errorResponse STEUnsupportedVersion
          Just k ->
            withDB' "purchaseKeyExists" cc (`purchaseKeyExists` k) >>= \case
              Right True -> pure $ errorResponse STEUnsupportedVersion
              Right False -> pure $ errorResponse STEUnknownPurchaseKey
              Left _ -> pure $ errorResponse STEInternal

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

credentialResponse :: Maybe BadgeCredential -> Maybe T.Text -> Maybe SubscriptionState -> [StatementEntry] -> StoreServiceResponse
credentialResponse credential previousEntryId subscription entries =
  STPBadgeCredential {credential, receipt = Nothing, statement = BadgeStatement {entries, previousEntryId}, subscription}

-- | Nothing is written until the credential is signed, so a signing failure leaves the code unspent.
redeemCode :: BadgeIssuerKey -> ChatController -> Maybe (TQueue GroupEvent) -> C.PublicKeyEd25519 -> BadgeMasterKey -> T.Text -> IO StoreServiceResponse
redeemCode key cc trackerQ_ purchaseKey masterKey codeText = case parseBadgeCode codeText of
  Nothing -> pure $ errorResponse STECodeInvalid
  Just code -> do
    now <- badgeNow cc
    withDB "getBadgeCode" cc (readCode now code) >>= \case
      Left _ -> pure $ errorResponse STEInternal
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
                    Right _ -> logError "store service: redeeming a code failed, but the code has uses left and is not revoked" $> (errorResponse STEInternal, False)
                Just purchaseId -> liftIO $ (,True) <$> firstMonthResponse db purchaseId Nothing Nothing firstMonth
            let (resp, claimed) = fromRight (errorResponse STEInternal, False) r
            when (claimed && hasTracker redeemLimit) $ refreshTracker cc trackerQ_ badgeCodeId code
            pure resp
  where
    readCode now code db = liftIO $
      getBadgeCode db (badgeCodeHash code) >>= \case
        Nothing -> pure $ Left $ errorResponse STECodeInvalid
        Just c@IssuedCode {badgeCodeId, revokedAt, paymentStatus, expiresAt, redeemLimit, redeemCount} ->
          getCodePurchaseForKey db badgeCodeId purchaseKey >>= \case
            -- A key that already redeemed gets its credential back without a use, even if the code has since
            -- expired or been revoked: a client whose reply was lost retries, and would otherwise lose the badge.
            KeyRedeemed KeyPurchase {badgePurchaseId, credential} ->
              maybe (Left $ errorResponse STEInternal) (Left . credentialResponse (Just credential) Nothing Nothing)
                <$> getLedgerEntries db badgePurchaseId 0
            -- code_used would make the client drop its keys, so the holder could never get the badge back.
            KeyRedeemedUnreadable ->
              logError "store service: a redeemed code's credential is missing or unreadable" $> Left (errorResponse STEInternal)
            KeyUnredeemed
              -- Revoked is checked first, so it answers as if the code never existed.
              | Just _ <- revokedAt -> pure $ Left $ errorResponse STECodeInvalid
              -- Redeeming an unpaid code would issue a free badge, so unpaid is refused.
              | CPSUnpaid <- paymentStatus -> pure $ Left $ errorResponse STEPaymentPending
              | redeemCount >= redeemLimit -> pure $ Left $ errorResponse STECodeUsed
              | maybe False (now >=) expiresAt -> pure $ Left $ errorResponse STECodeExpired
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
purchaseWithReceipt :: BadgeIssuerKey -> ChatController -> StoreVerifier -> PurchaseChecks -> ReceiptUse -> C.PublicKeyEd25519 -> BadgeMasterKey -> StoreReceipt -> IO StoreServiceResponse
purchaseWithReceipt key cc verifier checks use purchaseKey masterKey StoreReceipt {txRef = StoreTransactionRef {provider, transactionRef = providerRef}, receiptProductId, subscriptionRef, verifyReceipt, acknowledgeReceipt, acknowledgeSubscription} =
  withDB' "getStorePayment" cc (\db -> (,) <$> getStorePaymentCredit db provider providerRef <*> keyHoldsPurchase db) >>= \case
    Left _ -> pure $ errorResponse STEInternal
    Right (credit, keyHeld)
      -- only the key it credited can ask, and it is told only what it was given, so the store is not asked
      | creditedBy credit -> answerCredit credit
      -- a payment moves only to a key with no purchase, so one transferOut alone names the purchase holding it
      | keyHeld -> pure $ errorResponse STEBadRequest
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
        _ -> errorResponse STEInternal
    -- the product is read only for a receipt not yet credited, so retiring it leaves its replays answered
    newPurchase tx@VerifiedStoreTransaction {productId, paid, testPurchase} = case storeProduct provider productId of
      Nothing -> pure $ errorResponse STEProductUnavailable
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
        ClaimHeld remaining -> pure STPError {code = STEPaymentPending, message = Nothing, retryAfter = Just (ceiling remaining)}
        NoPlayRead wait -> do
          logWarn "Play subscription read refused: the purchase path's reads for this minute are used"
          pure STPError {code = STERateLimited, message = Nothing, retryAfter = Just (ceiling wait)}
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
          pure $ errorResponse STEPaymentPending
      | otherwise = do
          now <- badgeNow cc
          withDB' "getMovedPayment" cc (\db -> getPurchaseSubscription db fromPurchaseId >>= mapM (movedPayment db)) >>= \case
            Right (Just (paymentId, movedFromExpiry_, Just fromTip))
              -- A store receipt proves control of the store account but is no one-time secret, so this is the only bound on moves:
              -- waiting out the credential of the key the payment last moved from keeps each paid month with at most two keys.
              | maybe False (now <) movedFromExpiry_ -> pure $ errorResponse STETransferLimited
              | otherwise -> case grantableCharge provider now checked >>= fundsMonthNow purchaseKey now Nothing of
                  Left notGranted -> firstChargeNotGranted provider notGranted
                  Right grant -> movePayment now paymentId fromTip grant
            _ -> pure $ errorResponse STEInternal
      where
        movedPayment db PurchaseSubscription {paymentId} = (paymentId,,) <$> getMovedFromExpiry db paymentId <*> getLedgerTip db fromPurchaseId
        movePayment now paymentId fromTip (charge@StoreCharge {chargeRef, periodStart}, (badgeType, months)) =
          withDB' "getSubscriptionCharge" cc (\db -> getSubscriptionCharge db paymentId chargeRef) >>= \case
            Left _ -> pure $ errorResponse STEInternal
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
                    Right Nothing -> logError "store service: a subscription moved, or its ledger changed, while its transfer was signed" $> errorResponse STEInternal
                    Left _ -> pure $ errorResponse STEInternal
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
              Right () -> logError "store service: a store payment exists but funds no purchase" $> errorResponse STEInternal
          Just purchaseId -> liftIO $ writeCredit db purchaseId paymentId
      pure $ fromRight (errorResponse STEInternal) r

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
firstChargeNotGranted :: PaymentProvider -> NotGranted -> IO StoreServiceResponse
firstChargeNotGranted provider = \case
  NGMalformedPeriod -> storeRefusalResponse $ SRVerifierFailed "a charge whose period does not end after its start"
  -- Play issues a new token when an ended subscription is bought again, so this one can never be credited
  NGEnded | PPGoogle <- provider -> storeRefusalResponse $ SRInvalid "the subscription ended"
  NGNotStarted -> storeRefusalResponse SRPending
  -- a later charge, or an Apple resubscription under the same original id, may yet be credited from this receipt
  reason -> logInfo ("store subscription not credited yet: " <> tshow reason) $> STPError {code = STEPaymentPending, message = Nothing, retryAfter = Just 86400}

-- | What each request was answered, to pair with its command in a log. No payload: a credential,
-- an invoice and a statement are the client's, and an error's message can quote what it refused.
responseName :: StoreServiceResponse -> T.Text
responseName = \case
  STPBadgeCatalog {} -> "catalog"
  STPBadgeInvoice {} -> "invoice"
  STPBadgeCredential {credential = Just _, subscription} ->
    "badge issued" <> maybe "" (\s -> ", subscription " <> tshow s) subscription
  STPBadgeCredential {credential = Nothing} -> "no month to issue"
  STPError {code} -> "error " <> tshow code

-- | The command each request ran, so a log can be followed without the request ids. No payload:
-- a payment holds a store receipt, and an upgrade a receipt code.
commandName :: StoreServiceCommand -> T.Text
commandName = \case
  STCGetBadgeCatalog -> "getBadgeCatalog"
  STCGetBadgeInvoice {} -> "getBadgeInvoice"
  STCRedeemBadgeCode {} -> "redeemBadgeCode"
  STCPurchaseBadge {payment} -> "purchaseBadge " <> paymentName payment
  STCUpgradeBadgeSubscription {payment} -> "upgradeBadgeSubscription " <> paymentName payment
  STCTransferBadge {payment} -> "transferBadge " <> paymentName payment
  STCIssueBadge {} -> "issueBadge"
  STCPauseBadge -> "pauseBadge"
  where
    paymentName = \case
      SPApple {} -> "(App Store)"
      SPGoogle {} -> "(Play)"
      SPInvoice {} -> "(invoice)"
      SPReceipt {} -> "(receipt)"

-- | A forged receipt and a refunded one are both receipt_invalid, so the client learns nothing a
-- forger could iterate on. A test purchase is named, since the device that made it already knows.
storeRefusalResponse :: StoreRefusal -> IO StoreServiceResponse
storeRefusalResponse = \case
  -- warn, not info: every one of these ends a purchase for good, and one of them (a store that no longer
  -- knows a subscription it once sold) can do that to a purchase the user really paid for
  SRInvalid reason -> logWarn ("store receipt refused for good: " <> reason) $> errorResponse STEReceiptInvalid
  SRTestPurchase -> logInfo "store receipt refused: test purchase" $> errorResponse STETestPurchase
  SRPending -> pure $ errorResponse STEPaymentPending
  SRUnreachable reason -> logWarn ("store unreachable: " <> reason) $> errorResponse STEProviderUnavailable
  SRVerifierFailed reason -> logError ("store receipt not verified: " <> reason) $> errorResponse STEInternal
  SRNotConfigured -> logWarn "store receipt refused: no verifier for this store is configured" $> errorResponse STEProviderNotConfigured

creditedResponse :: DB.Connection -> C.PublicKeyEd25519 -> FundingCredit -> IO (Either StoreServiceResponse ())
creditedResponse db purchaseKey = \case
  Uncredited -> pure $ Right ()
  CreditedUnreadable -> pure $ Left $ errorResponse STEInternal
  Credited CreditedPurchase {purchaseKey = k, badgePurchaseId, credential, subscription}
    | k /= purchaseKey -> pure $ Left $ errorResponse STEReceiptUsed
    | otherwise ->
        maybe (Left $ errorResponse STEInternal) (Left . credentialResponse (Just credential) Nothing subscription)
          <$> getLedgerEntries db badgePurchaseId 0

signFirstMonth :: BadgeIssuerKey -> ChatController -> BadgeMasterKey -> BadgeType -> Int -> StatementCreditType -> UTCTime -> IO (Either StoreServiceResponse ([StatementEntry], (StatementEntry, StatementEntry, BadgeCredential)))
signFirstMonth key cc masterKey badgeType months credit now = do
  grantUuid <- randomId cc
  -- TODO [badges] a top-up grants onto an existing ledger, and must lapse before it or the
  -- months it adds are counted from a start already in the past
  let seed = emptyEntry now badgeType
  signMonthAfter key cc masterKey now seed [grantEntry now grantUuid badgeType months credit seed]

-- | The rows that fund a month, after the entry they follow, with that month issued at now and signed.
signMonthAfter :: BadgeIssuerKey -> ChatController -> BadgeMasterKey -> UTCTime -> StatementEntry -> [StatementEntry] -> IO (Either StoreServiceResponse ([StatementEntry], (StatementEntry, StatementEntry, BadgeCredential)))
signMonthAfter key cc masterKey now previous rows = do
  issueUuid <- randomId cc
  let funded = last (previous : rows)
  case issueEntry now issueUuid funded of
    Nothing -> logError "store service: the rows written fund no month to issue" $> Left (errorResponse STEInternal)
    Just issued ->
      credentialForEntry key masterKey issued >>= \case
        Left e -> logError ("store service signing failed: " <> T.pack e) $> Left (errorResponse STEInternal)
        Right signed -> pure $ Right (rows, issuanceAfter funded signed)

firstMonthResponse :: DB.Connection -> Int64 -> Maybe T.Text -> Maybe SubscriptionState -> ([StatementEntry], (StatementEntry, StatementEntry, BadgeCredential)) -> IO StoreServiceResponse
firstMonthResponse db purchaseId creditPaymentId_ subscription (rows, issuance@(_, _, credential)) = do
  appendLedgerPlan db purchaseId creditPaymentId_ rows $ Just issuance
  entries_ <- getLedgerEntries db purchaseId 0
  pure $ maybe (errorResponse STEInternal) (credentialResponse (Just credential) Nothing subscription) entries_

-- | The subscription a device's transaction names, its product and that product's tier: what an upgrade waits for, never what it grants.
data SubscriptionUpgrade = SubscriptionUpgrade
  { upgradeRef :: T.Text,
    upgradeProductId :: T.Text,
    upgradeTier :: BadgeType
  }

-- | Refused before the store is asked: a transaction that does not verify, or that is of no subscription product.
upgradeSubscriptionCmd :: BadgeIssuerKey -> ChatController -> StoreVerifier -> C.PublicKeyEd25519 -> BadgeBalance -> StoreReceipt -> IO StoreServiceResponse
upgradeSubscriptionCmd key cc verifier purchaseKey balance StoreReceipt {txRef = StoreTransactionRef {provider, transactionRef = claimedRef}, verifyReceipt} =
  verifyReceipt >>= \case
    Left refusal -> storeRefusalResponse refusal
    Right VerifiedStoreTransaction {transactionRef}
      | transactionRef /= claimedRef -> storeRefusalResponse $ SRVerifierFailed "verified a transaction other than the one claimed"
    Right VerifiedStoreTransaction {productId} -> case storePlan provider productId of
      Nothing -> pure $ errorResponse STEBadRequest
      Just (tier, _) -> issueBadgeCmd key cc verifier purchaseKey (Just SubscriptionUpgrade {upgradeRef = claimedRef, upgradeProductId = productId, upgradeTier = tier}) balance

-- | The purchase is reached through the verified signer key and no other way.
issueBadgeCmd :: BadgeIssuerKey -> ChatController -> StoreVerifier -> C.PublicKeyEd25519 -> Maybe SubscriptionUpgrade -> BadgeBalance -> IO StoreServiceResponse
issueBadgeCmd key cc verifier purchaseKey upgrade_ BadgeBalance {lastEntry} = do
  now <- badgeNow cc
  purchase_ <- withDB' "getBadgePurchase" cc (\db -> getIssuePurchase db purchaseKey)
  case purchase_ of
    Left _ -> pure $ errorResponse STEInternal
    Right Nothing -> pure $ errorResponse STEUnknownPurchaseKey
    Right (Just (purchase, Just tip, Just subscription))
      | Just upgrade <- upgrade_ -> upgradeSubscription upgrade purchase tip subscription now
      -- once a subscription's paid coverage has ended, only a new store charge funds another month
      | paidThrough tip <= now -> renewSubscription Nothing purchase tip subscription now
      | ChecksAtEveryRequest <- storeChecks verifier -> checkStoreDuringCoverage purchase tip subscription now
      | balanceStartTs tip <= now -> checkStoreBeforeMonth purchase tip subscription now
    Right (Just _) | isJust upgrade_ -> pure $ errorResponse STEBadRequest
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
            Left err -> logError ("store service signing failed: " <> T.pack err) $> errorResponse STEInternal
            Right signed ->
              writeIssued badgePurchaseId tip (maybeToList lapsed) now (Just $ issuanceAfter current signed) state_
    -- Write only if the tip has not moved, since another request may have advanced it.
    writeIssued purchaseId tip rows t issuance_ state_ = do
      r <- withDB "issueBadge" cc $ \db -> liftIO $ do
        tip' <- lockedLedgerTip db purchaseId t
        when (fmap entryId tip' == fmap entryId tip) $ appendLedgerPlan db purchaseId Nothing rows issuance_
        issueResponse db purchaseId t state_
      pure $ fromRight (errorResponse STEInternal) r
    -- Answered pending until the tier the device saw is granted, so the old tier's credential is never taken for the new one.
    upgradeSubscription upgrade@SubscriptionUpgrade {upgradeRef, upgradeTier} purchase@ServicePurchase {badgePurchaseId} tip@StatementEntry {balanceBadgeType} subscription@PurchaseSubscription {subscriptionRef, storedState} now
      | not ownSubscription = pure $ errorResponse STEBadRequest
      | balanceBadgeType == upgradeTier = answered "answerUpgraded" $ \db -> issueResponse db badgePurchaseId now (Just storedState)
      | Just wait <- checkWait now subscription =
          pure STPError {code = STEPaymentPending, message = Nothing, retryAfter = Just $ ceiling wait}
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
            Left _ -> pure $ errorResponse STEInternal
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
                      Left _ -> pure $ errorResponse STEInternal
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
                              Right Nothing -> logError "store service: a subscription's ledger moved while its renewal was signed" $> errorResponse STEInternal
                              Left _ -> pure $ errorResponse STEInternal
      where
        -- the store's answer is kept, and a charge it reports is recorded once, granted or not
        recordCheck charge_ state = do
          chargeId <- randomId cc
          answered "recordSubscriptionCheck" $ \db -> do
            forM_ charge_ $ \charge -> insertSubscriptionCharge db paymentId chargeId charge now
            setSubscriptionState db paymentId state now
            checkedResponse db balanceBadgeType state
        checkedResponse db ledgerTier state = case upgradeTier <$> upgrading_ of
          Just tier | tier /= ledgerTier -> pure $ atLeastCheckInterval $ errorResponse STEPaymentPending
          _ -> issueResponse db badgePurchaseId now (Just state)
    -- A charge a dev deployment's store reports while coverage runs is banked with no issuance, and its month issued once coverage reaches it.
    checkStoreDuringCoverage purchase@ServicePurchase {badgePurchaseId} tip PurchaseSubscription {paymentId, provider, subscriptionRef} now =
      withDB' "stampSubscriptionCheck" cc (\db -> stampSubscriptionCheck db paymentId now) >>= \case
        Left _ -> pure $ errorResponse STEInternal
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
                Left _ -> pure $ errorResponse STEInternal
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
            Left _ -> pure $ errorResponse STEInternal
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
                    Left _ -> pure $ errorResponse STEInternal
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
        Right Nothing -> logError "store service: a subscription's ledger moved while its refund was written" $> errorResponse STEInternal
        Left _ -> pure $ errorResponse STEInternal
    answered what action = fromRight (errorResponse STEInternal) <$> withDB' what cc action
    withoutCredential = \case
      STPBadgeCredential {receipt, statement, subscription} -> STPBadgeCredential {credential = Nothing, receipt, statement, subscription}
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
      pure $ maybe (errorResponse STEInternal) (credentialResponse credential_ (assertedUuid <$ assertedId) subscription) entries_

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
atLeastCheckInterval :: StoreServiceResponse -> StoreServiceResponse
atLeastCheckInterval = \case
  STPError {code, message, retryAfter = Just t} -> STPError {code, message, retryAfter = Just $ max t $ round subscriptionCheckInterval}
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

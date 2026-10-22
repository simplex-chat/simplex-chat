{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module StoreService.Service
  ( ServiceState (..),
    newServiceState,
    welcomeGetOpts,
    trustDevKeys,
    checkIssuerKey,
    storeService,
    storeServiceCLI,
    storeServiceResponse,
    badgeErrorRetryAfter,
    IssueCodeOpts (..),
    issueBadgeCode,
  )
where

import StoreService.Catalog (defaultCatalog)
import StoreService.Codes (issueFailedText, issueOneCode, singleUse)
import StoreService.Config (BadgeIssuerKey (..), ChainConfig (..), NamesConfig (..), ServiceConfig (..), minNameYears, readServiceConfig)
import StoreService.Group (GroupEvent, ensureManagedGroup, fmtTime, groupEvent, hasTracker, refreshTracker, revokeWithTracker, runGroupLane)
import StoreService.Group.Command (badgeTypeP, codeP, maxMonths, maxNameYears, nameCodeP, textTokenP)
import StoreService.Names.Balances (RefundRecord (..), RefundRefusal (..), recordRefund)
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
import StoreService.Waiters (Waiters, newWaiters)
import StoreService.Web.Server (exportWebapp, newWebEnv, runWebListener)
import Control.Applicative (optional)
import Control.Concurrent.STM
import StoreService.Log (logError, logInfo)
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as J
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Attoparsec.ByteString.Char8 as A
import Data.Bifunctor (first)
import Data.ByteString.Char8 (ByteString)
import Data.Char (isSpace)
import Data.Either (fromRight)
import Data.Functor (($>), (<&>))
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, isJust, maybeToList)
import qualified Data.Text as T
import Data.Time.Clock (NominalDiffTime, UTCTime, getCurrentTime)
import Data.Word (Word32)
import Simplex.Chat.Badges
import Simplex.Chat.Badges.Code
import Simplex.Chat.Badges.Ledger
import Simplex.Chat.StoreService
import Simplex.Chat.Badges.Types (BadgeCodePaymentStatus (..))
import Simplex.Chat.Bot (initializeBotAddress')
import Simplex.Chat.Bot.Store (withDB, withDB')
import Simplex.Chat.Controller
import Simplex.Chat.Core (sendChatCmd, simplexChatCore)
import Simplex.Chat.Options (printDbOpts)
import Simplex.Chat.PaymentService.Types (InvoiceId (..))
import Simplex.Chat.Terminal (terminalChatConfig)
import Simplex.Chat.Terminal.Main (simplexChatCLI')
import Simplex.Chat.Types (AgentInvId (..), User (..))
import Simplex.Messaging.Agent.Store.Common (DBStore)
import qualified Simplex.Messaging.Crypto as C
import Simplex.Messaging.Crypto.BBS (bbsPublicKey)
import Simplex.Messaging.Encoding.String (strEncode)
import Simplex.Messaging.Util (catchOwn, raceAny_, safeDecodeUtf8, tshow)
import Simplex.Messaging.Version (isCompatible)
import System.Directory (getAppUserDataDirectory)
import System.Exit (exitFailure)

data ServiceState = ServiceState
  { serviceCC :: TMVar ChatController,
    serviceRequestQ :: TQueue (User, AgentInvId, Maybe C.PublicKeyEd25519, J.Object),
    groupEventQ :: TQueue GroupEvent
  }

newServiceState :: IO ServiceState
newServiceState = do
  serviceCC <- newEmptyTMVarIO
  serviceRequestQ <- newTQueueIO
  groupEventQ <- newTQueueIO
  pure ServiceState {serviceCC, serviceRequestQ, groupEventQ}

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
trustDevKeys serviceCfg cfg@ChatConfig {badgePublicKeys} =
  fmap trusted . sequence <$> mapM publicKey (maybe [] devIssuerKeys serviceCfg)
  where
    trusted ks = cfg {badgePublicKeys = M.union badgePublicKeys (M.fromList ks)}
    publicKey BadgeIssuerKey {keyIdx, secretKey}
      | M.member keyIdx badgePublicKeys = pure $ Left $ "issuer.dev_keys index " <> show keyIdx <> " is the index of a key clients trust"
      | otherwise = fmap (keyIdx,) . first (("issuer.dev_keys secret at index " <> show keyIdx <> " is not a valid key: ") <>) <$> bbsPublicKey secretKey

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
  checkIssuerKey opts serviceCfg cfg >>= orExit

requireDevKeys :: Maybe ServiceConfig -> ChatConfig -> IO ChatConfig
requireDevKeys serviceCfg cfg = trustDevKeys serviceCfg cfg >>= orExit

orExit :: Either String a -> IO a
orExit = either (\e -> putStrLn ("Error: " <> e) >> exitFailure) pure

readConfigOrExit :: FilePath -> IO ServiceConfig
readConfigOrExit path =
  readServiceConfig path >>= \case
    Left e -> putStrLn (path <> ": " <> e) >> exitFailure
    Right sc -> pure sc

storeService :: StoreServiceOpts -> ChatConfig -> ServiceState -> IO ()
storeService opts@StoreServiceOpts {serviceConfigFile} baseCfg env = do
  serviceCfg <- traverse readConfigOrExit serviceConfigFile
  cfg <- requireDevKeys serviceCfg baseCfg
  key <- requireIssuerKey opts serviceCfg cfg
  waiters <- newWaiters
  -- The hooks' //revoke takes a prepaid code's turn, so the turns exist before the chat starts.
  turns <- newTurns
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
        raceAny_ $ processQueuedRequests key trackerQ_ env : maybeToList groupLane_ <> lanes
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
      exportWebapp (listener sc) (stripe sc)
      pollerEnv <- newPollerEnv chatStore ws (nePhases <$> namesEnv) hints providers (poll sc)
      nameLanes <- maybe (pure []) namesLanes namesEnv
      pure $ [runWebListener webEnv, runPoller pollerEnv] <> nameLanes

startNames :: ChatController -> Turns -> Waiters -> [Provider] -> NamesConfig -> IO NamesEnv
startNames cc@ChatController {chatStore} turns waiters providers nc@NamesConfig {nTLD, nChain = ChainConfig {cChainId}} =
  readTVarIO (currentUser cc) >>= \case
    Nothing -> putStrLn "No current user" >> exitFailure
    Just User {userId} -> do
      logInfo $ "names: on, TLD " <> safeDecodeUtf8 (strEncode nTLD) <> ", chain " <> tshow cChainId
      newNamesEnv cc userId nc chatStore turns waiters providers

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
  env <- newServiceState
  -- This mode runs no names lanes, so only its own //revoke takes these turns.
  turns <- newTurns
  let eventHook _cc ev = do
        case ev of
          Right (CEvtServiceRequest u reqId sigKey reqData) ->
            atomically $ writeTQueue (serviceRequestQ env) (u, reqId, sigKey, reqData)
          _ -> pure ()
        pure ev
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
         in [dollars cents <> by | cents > 0] <> [dollars late <> " late" <> by | late > 0]
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
  forever $ do
    (u, reqId, sigKey, reqData) <- atomically $ readTQueue $ serviceRequestQ env
    handleServiceRequest key cc trackerQ_ u reqId sigKey reqData

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

handleServiceRequest :: BadgeIssuerKey -> ChatController -> Maybe (TQueue GroupEvent) -> User -> AgentInvId -> Maybe C.PublicKeyEd25519 -> J.Object -> IO ()
handleServiceRequest key cc trackerQ_ User {userId} reqId sigKey reqData = do
  let reqIdT = safeDecodeUtf8 (strEncode reqId)
  logInfo $ "store service request " <> reqIdT
  resp <- storeServiceResponse key cc trackerQ_ sigKey reqData
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
storeServiceResponse :: BadgeIssuerKey -> ChatController -> Maybe (TQueue GroupEvent) -> Maybe C.PublicKeyEd25519 -> J.Object -> IO StoreServiceResponse
storeServiceResponse key cc trackerQ_ sigKey reqData = case J.fromJSON (J.Object reqData) of
  J.Error _ -> pure $ errorResponse STEBadRequest
  J.Success StoreServiceRequest {version, purchaseKey, request}
    | not (version `isCompatible` supportedStoreServiceVRange) -> pure $ errorResponse STEUnsupportedVersion
    | purchaseKey /= sigKey -> pure $ errorResponse STEBadRequest
    | otherwise -> case request of
        STCRedeemBadgeCode {masterKey, code} -> case purchaseKey of
          Just k -> redeemCode key cc trackerQ_ k masterKey code
          Nothing -> pure $ errorResponse STEBadRequest
        STCIssueBadge {balance} -> case purchaseKey of
          Just k -> issueBadgeCmd key cc k balance
          Nothing -> pure $ errorResponse STEBadRequest
        -- Every command but redeemBadgeCode needs a key the service already knows; that one creates the purchase, so its key is unknown on a first redemption.
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

credentialResponse :: Maybe BadgeCredential -> Maybe T.Text -> [StatementEntry] -> StoreServiceResponse
credentialResponse credential previousEntryId entries =
  STPBadgeCredential {credential, receipt = Nothing, statement = BadgeStatement {entries, previousEntryId}}

-- | Nothing is written until the credential is signed, so a signing failure leaves the code unspent.
redeemCode :: BadgeIssuerKey -> ChatController -> Maybe (TQueue GroupEvent) -> C.PublicKeyEd25519 -> BadgeMasterKey -> T.Text -> IO StoreServiceResponse
redeemCode key cc trackerQ_ purchaseKey masterKey codeText = case parseBadgeCode codeText of
  Nothing -> pure $ errorResponse STECodeInvalid
  Just code -> do
    now <- badgeNow cc
    withDB "getBadgeCode" cc (readCode now code) >>= \case
      Left _ -> pure $ errorResponse STEInternal
      Right (Left resp) -> pure resp
      Right (Right IssuedCode {badgeCodeId, badgeType, months, redeemLimit}) -> do
        (grantUuid, issueUuid) <- (,) <$> randomId cc <*> randomId cc
        -- TODO [badges] a top-up grants onto an existing ledger, and must lapse before it or the
        -- months it adds are counted from a start already in the past
        let granted = grantEntry now grantUuid months SCCode $ emptyEntry now badgeType
        -- A grant of at least one month starting now always has a month to issue.
        case issueEntry now issueUuid granted of
          Nothing -> pure $ errorResponse STEInternal
          Just issued -> credentialForEntry key masterKey issued >>= \case
            Left e -> logError ("store service signing failed: " <> T.pack e) $> errorResponse STEInternal
            Right signed -> do
              -- If the code was revoked or used up while signing, the claim fails. Read the code again to tell the client why.
              r <- withDB "writeCodeRedemption" cc $ \db ->
                liftIO (createCodePurchase db NewCodePurchase {badgeCodeId, purchaseKey, masterKey, badgeType} now) >>= \case
                  Nothing ->
                    readCode now code db >>= \case
                      Left resp -> pure (resp, False)
                      Right _ -> logError "store service: redeeming a code failed, but the code has uses left and is not revoked" $> (errorResponse STEInternal, False)
                  Just purchaseId -> liftIO $ do
                    appendLedgerPlan db purchaseId [granted] $ Just $ issuanceAfter granted signed
                    entries_ <- getLedgerEntries db purchaseId 0
                    pure (maybe (errorResponse STEInternal) (credentialResponse (Just $ snd signed) Nothing) entries_, True)
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
              maybe (Left $ errorResponse STEInternal) (Left . credentialResponse (Just credential) Nothing)
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

-- | The purchase is reached through the verified signer key and no other way.
issueBadgeCmd :: BadgeIssuerKey -> ChatController -> C.PublicKeyEd25519 -> BadgeBalance -> IO StoreServiceResponse
issueBadgeCmd key cc purchaseKey BadgeBalance {lastEntry} = do
  now <- badgeNow cc
  purchase_ <- withDB' "getBadgePurchase" cc $ \db -> do
    p_ <- getPurchaseByKey db purchaseKey
    forM p_ $ \p@ServicePurchase {badgePurchaseId} -> (p,) <$> getLedgerTip db badgePurchaseId
  case purchase_ of
    Left _ -> pure $ errorResponse STEInternal
    Right Nothing -> pure $ errorResponse STEUnknownPurchaseKey
    Right (Just (ServicePurchase {badgePurchaseId, masterKey, badgeType}, tip)) -> do
      (lapseUuid, issueUuid) <- (,) <$> randomId cc <*> randomId cc
      let tipEntry = fromMaybe (emptyEntry now badgeType) tip
          lapsed = lapseEntry now lapseUuid tipEntry
          current = fromMaybe tipEntry lapsed
      case issueEntry now issueUuid current of
        Nothing -> writeIssued badgePurchaseId tip (maybeToList lapsed) now Nothing
        Just e ->
          credentialForEntry key masterKey e >>= \case
            Left err -> logError ("store service signing failed: " <> T.pack err) $> errorResponse STEInternal
            Right signed ->
              writeIssued badgePurchaseId tip (maybeToList lapsed) now $ Just $ issuanceAfter current signed
  where
    -- Write only if the tip has not moved, since another request may have advanced it.
    writeIssued purchaseId tip rows t issuance_ = do
      r <- withDB "issueBadge" cc $ \db -> liftIO $ do
        tip' <- getLedgerTip db purchaseId
        when (fmap entryId tip' == fmap entryId tip) $ appendLedgerPlan db purchaseId rows issuance_
        issueResponse db purchaseId t
      pure $ fromRight (errorResponse STEInternal) r
    -- Only the asserted entry's identity is read, never the months it claims.
    issueResponse db purchaseId t = do
      let StatementEntry {entryId = assertedUuid} = lastEntry
      assertedId <- getLedgerEntryId db purchaseId assertedUuid
      -- TODO [badges] when the assertion does not resolve, heal the ledger and restate it as a
      -- single opening credit, rather than resending the whole history
      entries_ <- getLedgerEntries db purchaseId (fromMaybe 0 assertedId)
      credential_ <- getCurrentIssuance db purchaseId t
      pure $ maybe (errorResponse STEInternal) (credentialResponse credential_ (assertedUuid <$ assertedId)) entries_

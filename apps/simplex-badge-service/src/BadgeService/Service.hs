{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module BadgeService.Service
  ( ServiceState (..),
    newServiceState,
    welcomeGetOpts,
    checkIssuerKey,
    badgeService,
    badgeServiceCLI,
    badgeServiceResponse,
    badgeErrorRetryAfter,
    IssueCodeOpts (..),
    issueBadgeCode,
  )
where

import BadgeService.Catalog (defaultCatalog)
import BadgeService.Codes (issueFailedText, issueOneCode, singleUse)
import BadgeService.Config (BadgeIssuerKey (..), ServiceConfig (..), readServiceConfig)
import BadgeService.Group (GroupEvent, ensureManagedGroup, groupEvent, hasTracker, refreshTracker, revokeWithTracker, runGroupLane)
import BadgeService.Group.Command (badgeTypeP, codeP, maxMonths, textTokenP)
import BadgeService.Options
import BadgeService.Poller (newPollerEnv, newReadHints, runPoller)
import BadgeService.Providers.BTCPay (btcpayProvider)
import BadgeService.Providers.Stripe (stripeProvider)
import BadgeService.Store
import BadgeService.Store.Invoices (seedCatalog, truncateToSecond)
import BadgeService.Store.Migrate (runBadgeServiceMigrations)
import BadgeService.Waiters (Waiters, newWaiters)
import BadgeService.Web.Server (exportWebapp, newWebEnv, runWebListener)
import Control.Applicative (optional)
import Control.Concurrent.STM
import BadgeService.Log (logError, logInfo)
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as J
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Attoparsec.ByteString.Char8 as A
import Data.ByteString.Char8 (ByteString)
import Data.Either (fromRight)
import Data.Functor (($>), (<&>))
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, isJust, maybeToList)
import qualified Data.Text as T
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.Word (Word32)
import Simplex.Chat.Badges
import Simplex.Chat.Badges.Code
import Simplex.Chat.Badges.Ledger
import Simplex.Chat.Badges.Service
import Simplex.Chat.Badges.Types (BadgeCodePaymentStatus (..))
import Simplex.Chat.Bot (initializeBotAddress')
import Simplex.Chat.Bot.Store (withDB, withDB')
import Simplex.Chat.Controller
import Simplex.Chat.Core (sendChatCmd, simplexChatCore)
import Simplex.Chat.Options (printDbOpts)
import Simplex.Chat.Terminal (terminalChatConfig)
import Simplex.Chat.Terminal.Main (simplexChatCLI')
import Simplex.Chat.Types (AgentInvId (..), User (..))
import Simplex.Messaging.Agent.Store.Common (DBStore)
import qualified Simplex.Messaging.Crypto as C
import Simplex.Messaging.Crypto.BBS (bbsPublicKey)
import Simplex.Messaging.Encoding.String (strEncode)
import Simplex.Messaging.Util (raceAny_, safeDecodeUtf8, tshow)
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
  checkIssuerKey opts serviceCfg cfg >>= either (\e -> putStrLn ("Error: " <> e) >> exitFailure) pure

readConfigOrExit :: FilePath -> IO ServiceConfig
readConfigOrExit path =
  readServiceConfig path >>= \case
    Left e -> putStrLn (path <> ": " <> e) >> exitFailure
    Right sc -> pure sc

badgeService :: BadgeServiceOpts -> ChatConfig -> ServiceState -> IO ()
badgeService opts@BadgeServiceOpts {serviceConfigFile} cfg env = do
  serviceCfg <- traverse readConfigOrExit serviceConfigFile
  key <- requireIssuerKey opts serviceCfg cfg
  waiters <- newWaiters
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
        raceAny_ $ processQueuedRequests key trackerQ_ env : maybeToList groupLane_ <> lanes
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
  env <- newServiceState
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
            preCmdHook = Just badgeCmdHook,
            eventHook = Just eventHook
          }
  raceAny_
    [ simplexChatCLI' terminalChatConfig {chatHooks} (mkChatOpts opts) Nothing,
      processQueuedRequests key Nothing env
    ]

badgeCmdHook :: ChatController -> ChatCommand -> IO (Either (Either ChatError ChatResponse) ChatCommand)
badgeCmdHook cc = \case
  CustomChatCommand cmd -> Left <$> runBadgeCmd cc cmd
  cmd -> pure $ Right cmd

runBadgeCmd :: ChatController -> ByteString -> IO (Either ChatError ChatResponse)
runBadgeCmd cc cmd
  | Right issueOpts <- A.parseOnly issueCmdP cmd =
      issueBadgeCode cc issueOpts >>= \case
        Right code -> pure $ Right CRCustomChatResponse {user_ = Nothing, response = "code " <> formatBadgeCode code}
        Left _ -> pure $ chatCmdError (T.unpack issueFailedText)
  | Right code <- A.parseOnly revokeCmdP cmd =
      revokeWithTracker cc code <&> \case
        Right response -> Right CRCustomChatResponse {user_ = Nothing, response}
        Left e -> chatCmdError (T.unpack e)
  | otherwise = pure $ chatCmdError $ "use: //issue supporter|legend|investor [months 1-" <> show maxMonths <> "] [paid|unpaid|free], or //revoke <code>"

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
  forever $ do
    (u, reqId, sigKey, reqData) <- atomically $ readTQueue $ serviceRequestQ env
    handleServiceRequest key cc trackerQ_ u reqId sigKey reqData

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

handleServiceRequest :: BadgeIssuerKey -> ChatController -> Maybe (TQueue GroupEvent) -> User -> AgentInvId -> Maybe C.PublicKeyEd25519 -> J.Object -> IO ()
handleServiceRequest key cc trackerQ_ User {userId} reqId sigKey reqData = do
  let reqIdT = safeDecodeUtf8 (strEncode reqId)
  logInfo $ "badge service request " <> reqIdT
  resp <- badgeServiceResponse key cc trackerQ_ sigKey reqData
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
badgeServiceResponse :: BadgeIssuerKey -> ChatController -> Maybe (TQueue GroupEvent) -> Maybe C.PublicKeyEd25519 -> J.Object -> IO BadgeServiceResponse
badgeServiceResponse key cc trackerQ_ sigKey reqData = case J.fromJSON (J.Object reqData) of
  J.Error _ -> pure $ errorResponse BSEBadRequest
  J.Success BadgeServiceRequest {version, purchaseKey, request}
    | not (version `isCompatible` supportedBadgeServiceVRange) -> pure $ errorResponse BSEUnsupportedVersion
    | purchaseKey /= sigKey -> pure $ errorResponse BSEBadRequest
    | otherwise -> case request of
        BSCRedeemBadgeCode {masterKey, code} -> case purchaseKey of
          Just k -> redeemCode key cc trackerQ_ k masterKey code
          Nothing -> pure $ errorResponse BSEBadRequest
        BSCIssueBadge {balance} -> case purchaseKey of
          Just k -> issueBadgeCmd key cc k balance
          Nothing -> pure $ errorResponse BSEBadRequest
        -- Every command but redeemBadgeCode needs a key the service already knows; that one creates the purchase, so its key is unknown on a first redemption.
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

credentialResponse :: Maybe BadgeCredential -> Maybe T.Text -> [StatementEntry] -> BadgeServiceResponse
credentialResponse credential previousEntryId entries =
  BSPBadgeCredential {credential, receipt = Nothing, statement = BadgeStatement {entries, previousEntryId}}

-- | Nothing is written until the credential is signed, so a signing failure leaves the code unspent.
redeemCode :: BadgeIssuerKey -> ChatController -> Maybe (TQueue GroupEvent) -> C.PublicKeyEd25519 -> BadgeMasterKey -> T.Text -> IO BadgeServiceResponse
redeemCode key cc trackerQ_ purchaseKey masterKey codeText = case parseBadgeCode codeText of
  Nothing -> pure $ errorResponse BSECodeInvalid
  Just code -> do
    now <- badgeNow cc
    withDB "getBadgeCode" cc (readCode now code) >>= \case
      Left _ -> pure $ errorResponse BSEInternal
      Right (Left resp) -> pure resp
      Right (Right IssuedCode {badgeCodeId, badgeType, months, redeemLimit}) -> do
        (grantUuid, issueUuid) <- (,) <$> randomId cc <*> randomId cc
        -- TODO [badges] a top-up grants onto an existing ledger, and must lapse before it or the
        -- months it adds are counted from a start already in the past
        let granted = grantEntry now grantUuid months SCCode $ emptyEntry now badgeType
        -- A grant of at least one month starting now always has a month to issue.
        case issueEntry now issueUuid granted of
          Nothing -> pure $ errorResponse BSEInternal
          Just issued -> credentialForEntry key masterKey issued >>= \case
            Left e -> logError ("badge service signing failed: " <> T.pack e) $> errorResponse BSEInternal
            Right signed -> do
              -- If the code was revoked or used up while signing, the claim fails. Read the code again to tell the client why.
              r <- withDB "writeCodeRedemption" cc $ \db ->
                liftIO (createCodePurchase db NewCodePurchase {badgeCodeId, purchaseKey, masterKey, badgeType} now) >>= \case
                  Nothing ->
                    readCode now code db >>= \case
                      Left resp -> pure (resp, Nothing)
                      Right _ -> logError "badge service: redeeming a code failed, but the code has uses left and is not revoked" $> (errorResponse BSEInternal, Nothing)
                  Just (purchaseId, claimedCount) -> liftIO $ do
                    appendLedgerPlan db purchaseId [granted] $ Just $ issuanceAfter granted signed
                    entries_ <- getLedgerEntries db purchaseId 0
                    pure (maybe (errorResponse BSEInternal) (credentialResponse (Just $ snd signed) Nothing) entries_, Just claimedCount)
              let (resp, claimedCount_) = fromRight (errorResponse BSEInternal, Nothing) r
              when (hasTracker redeemLimit) $ forM_ claimedCount_ $ refreshTracker cc trackerQ_ badgeCodeId code
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
              maybe (Left $ errorResponse BSEInternal) (Left . credentialResponse (Just credential) Nothing)
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

-- | The purchase is reached through the verified signer key and no other way.
issueBadgeCmd :: BadgeIssuerKey -> ChatController -> C.PublicKeyEd25519 -> BadgeBalance -> IO BadgeServiceResponse
issueBadgeCmd key cc purchaseKey BadgeBalance {lastEntry} = do
  now <- badgeNow cc
  purchase_ <- withDB' "getBadgePurchase" cc $ \db -> do
    p_ <- getPurchaseByKey db purchaseKey
    forM p_ $ \p@ServicePurchase {badgePurchaseId} -> (p,) <$> getLedgerTip db badgePurchaseId
  case purchase_ of
    Left _ -> pure $ errorResponse BSEInternal
    Right Nothing -> pure $ errorResponse BSEUnknownPurchaseKey
    Right (Just (ServicePurchase {badgePurchaseId, masterKey, badgeType}, tip)) -> do
      (lapseUuid, issueUuid) <- (,) <$> randomId cc <*> randomId cc
      let tipEntry = fromMaybe (emptyEntry now badgeType) tip
          lapsed = lapseEntry now lapseUuid tipEntry
          current = fromMaybe tipEntry lapsed
      case issueEntry now issueUuid current of
        Nothing -> writeIssued badgePurchaseId tip (maybeToList lapsed) now Nothing
        Just e ->
          credentialForEntry key masterKey e >>= \case
            Left err -> logError ("badge service signing failed: " <> T.pack err) $> errorResponse BSEInternal
            Right signed ->
              writeIssued badgePurchaseId tip (maybeToList lapsed) now $ Just $ issuanceAfter current signed
  where
    -- Write only if the tip has not moved, since another request may have advanced it.
    writeIssued purchaseId tip rows t issuance_ = do
      r <- withDB "issueBadge" cc $ \db -> liftIO $ do
        tip' <- getLedgerTip db purchaseId
        when (fmap entryId tip' == fmap entryId tip) $ appendLedgerPlan db purchaseId rows issuance_
        issueResponse db purchaseId t
      pure $ fromRight (errorResponse BSEInternal) r
    -- Only the asserted entry's identity is read, never the months it claims.
    issueResponse db purchaseId t = do
      let StatementEntry {entryId = assertedUuid} = lastEntry
      assertedId <- getLedgerEntryId db purchaseId assertedUuid
      -- TODO [badges] when the assertion does not resolve, heal the ledger and restate it as a
      -- single opening credit, rather than resending the whole history
      entries_ <- getLedgerEntries db purchaseId (fromMaybe 0 assertedId)
      credential_ <- getCurrentIssuance db purchaseId t
      pure $ maybe (errorResponse BSEInternal) (credentialResponse credential_ (assertedUuid <$ assertedId)) entries_

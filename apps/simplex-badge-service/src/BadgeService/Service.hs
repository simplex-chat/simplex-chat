{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
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
import BadgeService.Config (IssuerConfig (..), ServiceConfig (..), readServiceConfig)
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
import Control.Logger.Simple
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as J
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Attoparsec.ByteString.Char8 as A
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as LB
import Data.Char (isSpace)
import Data.Either (fromRight)
import Data.Functor (($>))
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, maybeToList)
import qualified Data.Text as T
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.Word (Word32)
import Simplex.Chat.Badges
import Simplex.Chat.Badges.Code
import Simplex.Chat.Badges.Ledger
import Simplex.Chat.Badges.Service
import Simplex.Chat.Badges.Types (BadgeCodePaymentStatus (..))
import Simplex.Chat.Bot (initializeBotAddress', sendMessage)
import Simplex.Chat.Bot.Store (withDB, withDB')
import Simplex.Chat.Controller
import Simplex.Chat.Core (sendChatCmd, simplexChatCore)
import Simplex.Chat.Messages
import Simplex.Chat.Messages.CIContent (CIContent (..), SMsgDirection (..), ciContentToText)
import Simplex.Chat.Options (printDbOpts)
import Simplex.Chat.Terminal (terminalChatConfig)
import Simplex.Chat.Terminal.Main (simplexChatCLI')
import Simplex.Chat.Types (AgentInvId (..), Contact, User (..))
import Simplex.Messaging.Agent.Store.Common (DBStore)
import qualified Simplex.Messaging.Crypto as C
import Simplex.Messaging.Crypto.BBS (bbsPublicKey)
import Simplex.Messaging.Encoding.String (TextEncoding, strEncode, textDecode, textEncode)
import Simplex.Messaging.Util (raceAny_, safeDecodeUtf8, tshow)
import Simplex.Messaging.Version (isCompatible)
import System.Directory (getAppUserDataDirectory)
import System.Exit (exitFailure)

data ServiceState = ServiceState
  { serviceCC :: TMVar ChatController,
    serviceRequestQ :: TQueue (User, AgentInvId, Maybe C.PublicKeyEd25519, J.Object),
    chatRedeemQ :: TQueue (Contact, T.Text)
  }

newServiceState :: IO ServiceState
newServiceState = do
  serviceCC <- newEmptyTMVarIO
  serviceRequestQ <- newTQueueIO
  chatRedeemQ <- newTQueueIO
  pure ServiceState {serviceCC, serviceRequestQ, chatRedeemQ}

welcomeGetOpts :: IO BadgeServiceOpts
welcomeGetOpts = do
  appDir <- getAppUserDataDirectory "simplex"
  opts@BadgeServiceOpts {coreOptions, testing, serviceName} <- getBadgeServiceOpts appDir "simplex_badge_service"
  unless testing $ do
    putStrLn $ "SimpleX Badge Service v" ++ versionNumber
    printDbOpts coreOptions
    putStrLn $ "Service name: " ++ T.unpack serviceName
  pure opts

-- | The key this service signs with: the command line wins over [issuer].
-- Every key in [issuer] is checked, not only the one that signs, so a key clients could not
-- verify fails before anyone rotates onto it.
checkIssuerKey :: BadgeServiceOpts -> Maybe ServiceConfig -> ChatConfig -> IO (Either String BadgeIssuerKey)
checkIssuerKey BadgeServiceOpts {issuerKey} serviceCfg cfg = case issuerKey of
  Left e -> pure (Left e)
  Right (Just k) -> checkOne cfg k
  Right Nothing -> case serviceCfg >>= issuer of
    Nothing -> pure $ Left "an issuer key is required - pass --issuer-key-idx and --issuer-secret, or add an [issuer] section to badge_service.ini (see `simplex-chat badge keygen`)"
    Just IssuerConfig {iKeys, iDefaultIdx} -> do
      checked <- mapM (checkOne cfg . uncurry BadgeIssuerKey) (M.toList iKeys)
      pure $ case [e | Left e <- checked] of
        e : _ -> Left e
        [] -> maybe (Left $ "no issuer key at index " <> show iDefaultIdx) Right $
          BadgeIssuerKey iDefaultIdx <$> M.lookup iDefaultIdx iKeys

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
  let devRedeem = maybe False devChatRedeem serviceCfg
      chatHooks =
        defaultChatHooks
          { preStartHook = Just $ badgePreStartHook opts,
            postStartHook = Just $ badgePostStartHook opts devRedeem env,
            preCmdHook = Just badgeCmdHook
          }
  when devRedeem $ logWarn "[dev] chat_redeem is on: /redeem over chat hands out credentials this service can link"
  -- the reader must not block: outputQ carries every chat event
  simplexChatCore cfg {chatHooks} (mkChatOpts opts) $ \_ cc -> do
    lanes <- maybe (pure []) (serviceLanes waiters cc) serviceCfg
    raceAny_ $
      [ forever $
          atomically (readTBQueue $ outputQ cc) >>= \case
            (_, Right (CEvtServiceRequest u reqId sigKey reqData)) ->
              atomically $ writeTQueue (serviceRequestQ env) (u, reqId, sigKey, reqData)
            (_, Right CEvtNewChatItems {chatItems = AChatItem _ SMDRcv (DirectChat ct) ChatItem {content = mc@CIRcvMsgContent {}} : _})
              | devRedeem -> atomically $ writeTQueue (chatRedeemQ env) (ct, ciContentToText mc)
            _ -> pure (),
        processQueuedRequests key env,
        processChatRedeems key env
      ]
        <> lanes
  where
    serviceLanes :: Waiters -> ChatController -> ServiceConfig -> IO [IO ()]
    serviceLanes ws ChatController {chatStore} sc = do
      -- before the listener accepts anything, since every checkout is priced from these
      seedServiceCatalog chatStore
      btc <- maybe (pure []) (fmap (: []) . btcpayProvider) (btcpay sc)
      str <- maybe (pure []) (fmap (: []) . stripeProvider) (stripe sc)
      let providers = btc <> str
      hints <- newReadHints
      webEnv <- newWebEnv chatStore sc ws hints providers
      exportWebapp (listener sc) (stripe sc)
      pollerEnv <- newPollerEnv chatStore ws hints providers (poll sc)
      pure [runWebListener webEnv, runPoller pollerEnv]

-- | Insert-only, so it is safe on every start. Nothing else writes these tables, and
-- until they are written the service can sell nothing.
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
  -- the same file the listener reads: issuing a code is a CLI command, and the key it signs
  -- with is as likely to be in [issuer] as on the command line
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
            postStartHook = Just $ badgePostStartHook opts False env,
            preCmdHook = Just badgeCmdHook,
            eventHook = Just eventHook
          }
  raceAny_
    [ simplexChatCLI' terminalChatConfig {chatHooks} (mkChatOpts opts) Nothing,
      processQueuedRequests key env
    ]

-- | issuing codes lives here rather than in core: every user's app would otherwise ship it
badgeCmdHook :: ChatController -> ChatCommand -> IO (Either (Either ChatError ChatResponse) ChatCommand)
badgeCmdHook cc = \case
  CustomChatCommand cmd -> Left <$> runBadgeCmd cc cmd
  cmd -> pure $ Right cmd

runBadgeCmd :: ChatController -> ByteString -> IO (Either ChatError ChatResponse)
runBadgeCmd cc cmd
  | Right issueOpts <- A.parseOnly issueCmdP cmd =
      issueBadgeCode cc issueOpts >>= \case
        Right code -> pure $ Right CRCustomChatResponse {user_ = Nothing, response = "code " <> formatBadgeCode code}
        Left e -> pure $ chatCmdError $ "issuing code: " <> e
  | Right code <- A.parseOnly revokeCmdP cmd =
      revokeBadgeCode cc code >>= \case
        Right True -> pure $ Right CRCustomChatResponse {user_ = Nothing, response = "revoked"}
        Right False -> pure $ chatCmdError "no such code, or it was revoked already"
        Left e -> pure $ chatCmdError $ "revoking code: " <> e
  | otherwise = pure $ chatCmdError "use: //issue supporter|legend|investor [months 1-255] [paid|unpaid|free], or //revoke <code>"

revokeCmdP :: A.Parser BadgeCode
revokeCmdP =
  "revoke " *> (A.takeWhile1 (not . isSpace) >>= maybe (fail "not a badge code") pure . parseBadgeCode . safeDecodeUtf8)
    <* (A.skipSpace *> A.endOfInput)

-- | A refunded or leaked code. Redemption refuses a revoked code as if it had never existed,
-- so the holder learns nothing from trying.
revokeBadgeCode :: ChatController -> BadgeCode -> IO (Either String Bool)
revokeBadgeCode cc code = do
  now <- truncateToSecond <$> getCurrentTime
  withDB' "revokeBadgeCode" cc $ \db -> revokeCode db (badgeCodeHash code) now

issueCmdP :: A.Parser IssueCodeOpts
issueCmdP =
  "issue " *> do
    badgeType <- badgeTypeP
    months_ <- optional (A.space *> (A.decimal :: A.Parser Integer))
    -- outside `optional`, which would otherwise backtrack past a bad count
    months <- maybe (pure 1) checkMonths months_
    paymentStatus <- fromMaybe CPSFree <$> optional (A.space *> textTokenP)
    A.skipSpace
    A.endOfInput
    pure IssueCodeOpts {badgeType, months, paymentStatus}
  where
    -- Integer, because attoparsec's decimal wraps silently at Int and the guard would then be
    -- checking a truncated count
    checkMonths n
      | n >= 1 && n <= 255 = pure (fromInteger n)
      | otherwise = fail "months must be between 1 and 255"
    -- BadgeType decodes anything to BTUnknown, so a typo would issue an unusable code
    badgeTypeP =
      textTokenP >>= \case
        BTUnknown t -> fail $ "unknown badge type " <> T.unpack t
        bt -> pure bt
    textTokenP :: TextEncoding a => A.Parser a
    textTokenP = do
      t <- A.takeWhile1 (not . isSpace)
      maybe (fail "invalid value") pure $ textDecode $ safeDecodeUtf8 t

data IssueCodeOpts = IssueCodeOpts
  { badgeType :: BadgeType,
    months :: Int,
    paymentStatus :: BadgeCodePaymentStatus
  }

-- | The caller sees the code once; only its hash is stored, so a lost code cannot be recovered.
issueBadgeCode :: ChatController -> IssueCodeOpts -> IO (Either String BadgeCode)
issueBadgeCode cc IssueCodeOpts {badgeType, months, paymentStatus} = do
  code <- randomBadgeCode $ random cc
  now <- getCurrentTime
  r <- withDB' "issueBadgeCode" cc $ \db -> insertBadgeCode db (badgeCodeHash code) badgeType months paymentStatus now
  pure $ code <$ r

processQueuedRequests :: BadgeIssuerKey -> ServiceState -> IO ()
processQueuedRequests key env = do
  cc <- atomically $ readTMVar $ serviceCC env
  forever $ do
    (u, reqId, sigKey, reqData) <- atomically $ readTQueue $ serviceRequestQ env
    handleServiceRequest key cc u reqId sigKey reqData

processChatRedeems :: BadgeIssuerKey -> ServiceState -> IO ()
processChatRedeems key env = do
  cc <- atomically $ readTMVar $ serviceCC env
  forever $ do
    (ct, msg) <- atomically $ readTQueue $ chatRedeemQ env
    chatRedeem key cc ct msg

-- | The RPC path signs over a master key only the client holds. Here the service generates it,
-- so it can link the badge it hands back - which is why [dev] chat_redeem gates this.
chatRedeem :: BadgeIssuerKey -> ChatController -> Contact -> T.Text -> IO ()
chatRedeem key cc ct msg = case T.stripPrefix "/redeem" (T.strip msg) of
  Just rest | not (T.null (T.strip rest)) -> do
    masterKey <- generateMasterKey (random cc)
    (purchaseKey, _) <- atomically $ C.generateKeyPair (random cc) :: IO (C.KeyPair 'C.Ed25519)
    resp <- redeemCode key cc purchaseKey masterKey (T.strip rest)
    sendMessage cc ct $ case resp of
      BSPBadgeCredential {credential = Just cred} -> safeDecodeUtf8 $ LB.toStrict $ J.encode cred
      BSPError {code} -> "error: " <> textEncode code
      _ -> "unexpected response"
  _ -> sendMessage cc ct "send: /redeem <code>"

badgePreStartHook :: BadgeServiceOpts -> ChatController -> IO ()
badgePreStartHook opts ChatController {config, chatStore} =
  runBadgeServiceMigrations opts config chatStore

-- | Contact requests are accepted only for [dev] chat_redeem: the address is otherwise for
-- service RPC, which needs no contact.
badgePostStartHook :: BadgeServiceOpts -> Bool -> ServiceState -> ChatController -> IO ()
badgePostStartHook BadgeServiceOpts {noAddress, testing} devRedeem env cc = do
  -- core starts this False and gates service request delivery on it, so the hook has to set it
  atomically $ writeTVar (processServiceRequests cc) True
  readTVarIO (currentUser cc) >>= \case
    Nothing -> putStrLn "No current user" >> exitFailure
    Just _ -> do
      unless noAddress $ initializeBotAddress' (not testing) (Just True) devRedeem cc
      void $ atomically $ tryPutTMVar (serviceCC env) cc

handleServiceRequest :: BadgeIssuerKey -> ChatController -> User -> AgentInvId -> Maybe C.PublicKeyEd25519 -> J.Object -> IO ()
handleServiceRequest key cc User {userId} reqId sigKey reqData = do
  let reqIdT = safeDecodeUtf8 (strEncode reqId)
  logInfo $ "badge service request " <> reqIdT
  resp <- badgeServiceResponse key cc sigKey reqData
  sendChatCmd cc (APISendServiceResponse userId reqId (responseObject resp)) >>= \case
    Right _ -> pure ()
    Left e -> logError $ "badge service response failed for " <> reqIdT <> ": " <> tshow e

responseObject :: BadgeServiceResponse -> J.Object
responseObject r = case J.toJSON r of
  J.Object o -> o
  _ -> KM.fromList [("type", J.String "error"), ("code", J.toJSON BSEInternal)]

errorResponse :: BadgeServiceErrorCode -> BadgeServiceResponse
errorResponse code = BSPError {code, message = Nothing, retryAfter = badgeErrorRetryAfter code}

-- | Seconds, for the three codes badges-rpc.md marks transient. Every other code is terminal for
-- the command attempted - internal included, which would otherwise press a failing service.
badgeErrorRetryAfter :: BadgeServiceErrorCode -> Maybe Word32
badgeErrorRetryAfter = \case
  BSEPaymentPending -> Just 300
  BSEProviderUnavailable -> Just 300
  BSERateLimited -> Just 60
  _ -> Nothing


-- | The agent verified the signature, so sigKey is a key the sender holds - a purchaseKey that
-- differs would let a client claim a purchase it cannot sign for.
badgeServiceResponse :: BadgeIssuerKey -> ChatController -> Maybe C.PublicKeyEd25519 -> J.Object -> IO BadgeServiceResponse
badgeServiceResponse key cc sigKey reqData = case J.fromJSON (J.Object reqData) of
  J.Error _ -> pure $ errorResponse BSEBadRequest
  J.Success BadgeServiceRequest {version, purchaseKey, request}
    | not (version `isCompatible` supportedBadgeServiceVRange) -> pure $ errorResponse BSEUnsupportedVersion
    | purchaseKey /= sigKey -> pure $ errorResponse BSEBadRequest
    | otherwise -> case request of
        BSCRedeemBadgeCode {masterKey, code} -> case purchaseKey of
          Just k -> redeemCode key cc k masterKey code
          Nothing -> pure $ errorResponse BSEBadRequest
        BSCIssueBadge {balance} -> case purchaseKey of
          Just k -> issueBadgeCmd key cc k balance
          Nothing -> pure $ errorResponse BSEBadRequest
        -- every command but redeemBadgeCode needs a key the service already knows: that one
        -- creates the purchase, so its key is unknown on a first redemption
        _ -> case purchaseKey of
          Nothing -> pure $ errorResponse BSEUnsupportedVersion
          Just k ->
            withDB' "purchaseKeyExists" cc (`purchaseKeyExists` k) >>= \case
              Right True -> pure $ errorResponse BSEUnsupportedVersion
              Right False -> pure $ errorResponse BSEUnknownPurchaseKey
              Left _ -> pure $ errorResponse BSEInternal

-- | The only clock the service reads, so a test can move both sides of a request together.
badgeNow :: ChatController -> IO UTCTime
badgeNow ChatController {config = ChatConfig {badgeCurrentTime}} = badgeCurrentTime

randomId :: ChatController -> IO T.Text
randomId cc = safeDecodeUtf8 . strEncode <$> atomically (C.randomBytes 16 (random cc))

-- | Neither value comes from the caller: the badge type is the entry's, the expiry is derived
-- from the period end it carries.
credentialForEntry :: BadgeIssuerKey -> BadgeMasterKey -> StatementEntry -> IO (Either String (StatementEntry, BadgeCredential))
credentialForEntry BadgeIssuerKey {keyIdx, secretKey} masterKey e@StatementEntry {balanceStartTs = periodEnd, balanceBadgeType} = do
  let badgeInfo = BadgeInfo {badgeType = balanceBadgeType, badgeExpiry = endOfMondayAfter periodEnd, badgeExtra = ""}
  fmap (e,) <$> issueBadge keyIdx secretKey (VerifiedBadgeRequest BadgeRequest {masterKey, badgeInfo})

-- | Pairs the issued entry with the one before it, which the writer needs for the period start.
issuanceAfter :: StatementEntry -> (StatementEntry, BadgeCredential) -> (StatementEntry, StatementEntry, BadgeCredential)
issuanceAfter previous (issued, credential) = (previous, issued, credential)

credentialResponse :: Maybe BadgeCredential -> Maybe T.Text -> [StatementEntry] -> BadgeServiceResponse
credentialResponse credential previousEntryId entries =
  BSPBadgeCredential {credential, receipt = Nothing, statement = BadgeStatement {entries, previousEntryId}}

-- | Nothing is written until the credential is signed, so a signing failure leaves the code
-- unspent rather than spent with nothing behind it.
redeemCode :: BadgeIssuerKey -> ChatController -> C.PublicKeyEd25519 -> BadgeMasterKey -> T.Text -> IO BadgeServiceResponse
redeemCode key cc purchaseKey masterKey codeText = case parseBadgeCode codeText of
  Nothing -> pure $ errorResponse BSECodeInvalid
  Just code -> do
    now <- badgeNow cc
    withDB "getBadgeCode" cc (readCode now code) >>= \case
      Left _ -> pure $ errorResponse BSEInternal
      Right (Left resp) -> pure resp
      Right (Right IssuedCode {badgeCodeId, badgeType, months}) -> do
        (grantUuid, issueUuid) <- (,) <$> randomId cc <*> randomId cc
        -- the purchase is created here, so there is no ledger to lapse
        -- TODO [badges] a top-up grants onto an existing ledger, and must lapse before it or the
        -- months it adds are counted from a start already in the past
        let granted = grantEntry now grantUuid months SCCode $ emptyEntry now badgeType
        -- a grant of at least one month starting now always has a month to issue
        case issueEntry now issueUuid granted of
          Nothing -> pure $ errorResponse BSEInternal
          Just issued -> credentialForEntry key masterKey issued >>= \case
            Left e -> logError ("badge service signing failed: " <> T.pack e) $> errorResponse BSEInternal
            Right signed -> do
              -- re-read: a redemption or a revoke may have landed while this one was signing
              r <- withDB "writeCodeRedemption" cc $ \db ->
                readCode now code db >>= \case
                  Left resp -> pure resp
                  Right _ -> liftIO $ do
                    purchaseId <- createCodePurchase db NewCodePurchase {badgeCodeId, purchaseKey, masterKey, badgeType} now
                    appendLedgerPlan db purchaseId [granted] $ Just $ issuanceAfter granted signed
                    entries_ <- getLedgerEntries db purchaseId 0
                    pure $ maybe (errorResponse BSEInternal) (credentialResponse (Just $ snd signed) Nothing) entries_
              pure $ fromRight (errorResponse BSEInternal) r
  where
    -- used before signing and again inside the write transaction; every Left is a finished
    -- response - an unknown, revoked, unpaid, spent or expired code included. The guards the
    -- ledger read cannot see (revoke, unpaid, expiry) are re-run each time, since a revoke may
    -- land between the two reads.
    readCode now code db = liftIO $
      getBadgeCode db (badgeCodeHash code) >>= \case
        Nothing -> pure $ Left $ errorResponse BSECodeInvalid
        Just c@IssuedCode {revokedAt, paymentStatus, expiresAt, redemption}
          -- revoked first, so it answers as if it had never existed whatever else is true of it
          | Just _ <- revokedAt -> pure $ Left $ errorResponse BSECodeInvalid
          -- a code the web checkout wrote exists from the moment the invoice is created, and
          -- settlement is what marks it paid: redeeming before that would issue a free badge
          | CPSUnpaid <- paymentStatus -> pure $ Left $ errorResponse BSEPaymentPending
          | otherwise ->
              checkUnspent db redemption >>= \case
                -- already spent keeps answering with the credential it was redeemed for
                Left resp -> pure $ Left resp
                -- unspent: the deadline is on redeeming, not on holding
                Right ()
                  | maybe False (now >=) expiresAt -> pure $ Left $ errorResponse BSECodeExpired
                  | otherwise -> pure $ Right c
    checkUnspent db = \case
      CodeUnredeemed -> pure $ Right ()
      CodeRedeemedUnreadable -> pure $ Left $ errorResponse BSEInternal
      CodeRedeemed RedeemedCode {purchaseKey = k, badgePurchaseId, credential}
        | k /= purchaseKey -> pure $ Left $ errorResponse BSECodeUsed
        -- the whole ledger, so a client that lost the first response still ends holding it
        | otherwise ->
            maybe (Left $ errorResponse BSEInternal) (Left . credentialResponse (Just credential) Nothing)
              <$> getLedgerEntries db badgePurchaseId 0

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
    -- the rows were computed from a tip that another request may have moved, and an issuance was
    -- signed against it - so write only if it is still the tip
    writeIssued purchaseId tip rows t issuance_ = do
      r <- withDB "issueBadge" cc $ \db -> liftIO $ do
        tip' <- getLedgerTip db purchaseId
        when (fmap entryId tip' == fmap entryId tip) $ appendLedgerPlan db purchaseId rows issuance_
        issueResponse db purchaseId t
      pure $ fromRight (errorResponse BSEInternal) r
    -- entries after the one asserted, or the whole ledger when this purchase does not hold it.
    -- Only the asserted entry's identity is read, never the months it claims.
    issueResponse db purchaseId t = do
      let StatementEntry {entryId = assertedUuid} = lastEntry
      assertedId <- getLedgerEntryId db purchaseId assertedUuid
      -- TODO [badges] when the assertion does not resolve, heal the ledger and restate it as a
      -- single opening credit (badges-rpc.md), rather than resending the whole history
      entries_ <- getLedgerEntries db purchaseId (fromMaybe 0 assertedId)
      credential_ <- getCurrentIssuance db purchaseId t
      pure $ maybe (errorResponse BSEInternal) (credentialResponse credential_ (assertedUuid <$ assertedId)) entries_

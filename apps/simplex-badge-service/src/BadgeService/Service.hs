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
    shownServiceRequest,
    IssueCodeOpts (..),
    issueBadgeCode,
  )
where

import BadgeService.Catalog (StoreProduct (..), defaultCatalog, storeProduct)
import BadgeService.Config (BadgeIssuerKey (..), ServiceConfig (..), readServiceConfig)
import BadgeService.Options
import BadgeService.Poller (newPollerEnv, newReadHints, runPoller)
import BadgeService.Providers.BTCPay (btcpayProvider)
import BadgeService.Providers.Stripe (stripeProvider)
import BadgeService.Store
import BadgeService.Store.Invoices (seedCatalog, truncateToSecond)
import BadgeService.Store.Migrate (runBadgeServiceMigrations)
import BadgeService.StoreReceipts
import BadgeService.Waiters (Waiters, newWaiters)
import BadgeService.Web.Server (exportWebapp, newWebEnv, runWebListener)
import Control.Applicative (optional)
import Control.Concurrent.STM
import BadgeService.Log (logError, logInfo, logWarn)
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as J
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Attoparsec.ByteString.Char8 as A
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as LB
import Data.Char (isSpace)
import Data.Either (fromRight)
import Data.Functor (($>), (<&>))
import Data.Int (Int64)
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
import qualified Simplex.Messaging.Agent.Store.DB as DB
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
    chatRedeemQ :: TQueue (Contact, T.Text),
    storeVerifier :: StoreVerifier
  }

-- | No store verifier exists yet, so every store receipt is answered provider_unavailable: the
-- purchase may be real, and the app keeps presenting it until one is deployed.
newServiceState :: IO ServiceState
newServiceState = do
  serviceCC <- newEmptyTMVarIO
  serviceRequestQ <- newTQueueIO
  chatRedeemQ <- newTQueueIO
  pure ServiceState {serviceCC, serviceRequestQ, chatRedeemQ, storeVerifier = noStoreVerifier}

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
  let devRedeem = maybe False devChatRedeem serviceCfg
      chatHooks =
        defaultChatHooks
          { preStartHook = Just $ badgePreStartHook opts,
            postStartHook = Just $ badgePostStartHook opts devRedeem env,
            preCmdHook = Just badgeCmdHook
          }
  when devRedeem $ logWarn "[dev] chat_redeem is on: /redeem over chat hands out credentials this service can link"
  -- The reader must not block, since outputQ carries every chat event.
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
        processQueuedRequests key env
      ]
        <> [processChatRedeems key env | devRedeem]
        <> lanes
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
  let eventHook _cc = \case
        Right (CEvtServiceRequest u reqId sigKey reqData) -> do
          atomically $ writeTQueue (serviceRequestQ env) (u, reqId, sigKey, reqData)
          pure $ Right $ CEvtServiceRequest u reqId sigKey (shownServiceRequest reqData)
        ev -> pure ev
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
        Right code -> pure $ Right CRCustomChatResponse {user_ = Nothing, response = "code " <> formatBadgeCode code}
        Left e -> pure $ chatCmdError $ "issuing code: " <> e
  | Right code <- A.parseOnly revokeCmdP cmd =
      revokeBadgeCode cc code >>= \case
        Right Revoked -> pure $ Right CRCustomChatResponse {user_ = Nothing, response = "revoked"}
        Right AlreadyRevoked -> pure $ chatCmdError "code was revoked already"
        Right AlreadyRedeemed -> pure $ chatCmdError "code was redeemed already, so it cannot be revoked"
        Right NoSuchCode -> pure $ chatCmdError "no such code"
        Left e -> pure $ chatCmdError $ "revoking code: " <> e
  | otherwise = pure $ chatCmdError "use: //issue supporter|legend|investor [months 1-255] [paid|unpaid|free], or //revoke <code>"

revokeCmdP :: A.Parser BadgeCode
revokeCmdP =
  "revoke " *> (A.takeWhile1 (not . isSpace) >>= maybe (fail "not a badge code") pure . parseBadgeCode . safeDecodeUtf8)
    <* (A.skipSpace *> A.endOfInput)

revokeBadgeCode :: ChatController -> BadgeCode -> IO (Either String RevokeResult)
revokeBadgeCode cc code = do
  now <- truncateToSecond <$> getCurrentTime
  withDB' "revokeBadgeCode" cc $ \db -> revokeCode db (badgeCodeHash code) now

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
    handleServiceRequest key (storeVerifier env) cc u reqId sigKey reqData

processChatRedeems :: BadgeIssuerKey -> ServiceState -> IO ()
processChatRedeems key env = do
  cc <- atomically $ readTMVar $ serviceCC env
  forever $ do
    (ct, msg) <- atomically $ readTQueue $ chatRedeemQ env
    chatRedeem key cc ct msg

-- | Here the service generates the master key and can link the badge, so [dev] chat_redeem gates this.
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

badgePostStartHook :: BadgeServiceOpts -> Bool -> ServiceState -> ChatController -> IO ()
badgePostStartHook BadgeServiceOpts {noAddress, testing} devRedeem env cc = do
  -- Core starts this False and gates service request delivery on it, so the hook must set it.
  atomically $ writeTVar (processServiceRequests cc) True
  readTVarIO (currentUser cc) >>= \case
    Nothing -> putStrLn "No current user" >> exitFailure
    Just _ -> do
      unless noAddress $ initializeBotAddress' (not testing) (Just True) devRedeem cc
      void $ atomically $ tryPutTMVar (serviceCC env) cc

handleServiceRequest :: BadgeIssuerKey -> StoreVerifier -> ChatController -> User -> AgentInvId -> Maybe C.PublicKeyEd25519 -> J.Object -> IO ()
handleServiceRequest key verifier cc User {userId} reqId sigKey reqData = do
  let reqIdT = safeDecodeUtf8 (strEncode reqId)
  logInfo $ "badge service request " <> reqIdT
  resp <- badgeServiceResponse key verifier cc sigKey reqData
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
badgeServiceResponse :: BadgeIssuerKey -> StoreVerifier -> ChatController -> Maybe C.PublicKeyEd25519 -> J.Object -> IO BadgeServiceResponse
badgeServiceResponse key verifier cc sigKey reqData = case J.fromJSON (J.Object reqData) of
  J.Error _ -> pure $ errorResponse BSEBadRequest
  J.Success BadgeServiceRequest {version, purchaseKey, request}
    | not (version `isCompatible` supportedBadgeServiceVRange) -> pure $ errorResponse BSEUnsupportedVersion
    | purchaseKey /= sigKey -> pure $ errorResponse BSEBadRequest
    | otherwise -> case request of
        BSCRedeemBadgeCode {masterKey, code} -> case purchaseKey of
          Just k -> redeemCode key cc k masterKey code
          Nothing -> pure $ errorResponse BSEBadRequest
        BSCPurchaseBadge {masterKey, payment, upgrade}
          | Just receipt_ <- storeReceipt verifier payment -> case (purchaseKey, upgrade) of
              (Just k, Nothing) -> either storeRefusalResponse (purchaseWithReceipt key cc k masterKey) receipt_
              -- store upgrades are not built, and ignoring one would credit its months at the discounted price
              (Just _, Just _) -> pure $ errorResponse BSEBadRequest
              (Nothing, _) -> pure $ errorResponse BSEBadRequest
        BSCIssueBadge {balance} -> case purchaseKey of
          Just k -> issueBadgeCmd key cc k balance
          Nothing -> pure $ errorResponse BSEBadRequest
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

credentialResponse :: Maybe BadgeCredential -> Maybe T.Text -> [StatementEntry] -> BadgeServiceResponse
credentialResponse credential previousEntryId entries =
  BSPBadgeCredential {credential, receipt = Nothing, statement = BadgeStatement {entries, previousEntryId}}

-- | Nothing is written until the credential is signed, so a signing failure leaves the code unspent.
redeemCode :: BadgeIssuerKey -> ChatController -> C.PublicKeyEd25519 -> BadgeMasterKey -> T.Text -> IO BadgeServiceResponse
redeemCode key cc purchaseKey masterKey codeText = case parseBadgeCode codeText of
  Nothing -> pure $ errorResponse BSECodeInvalid
  Just code -> do
    now <- badgeNow cc
    withDB "getBadgeCode" cc (readCode now code) >>= \case
      Left _ -> pure $ errorResponse BSEInternal
      Right (Left resp) -> pure resp
      Right (Right IssuedCode {badgeCodeId, badgeType, months}) ->
        signFirstMonth key cc masterKey badgeType months SCCode now >>= \case
          Left resp -> pure resp
          Right firstMonth -> do
            -- If the code was revoked or redeemed while signing, the claim fails. Read the code again to tell the client why.
            r <- withDB "writeCodeRedemption" cc $ \db ->
              liftIO (createCodePurchase db NewCodePurchase {badgeCodeId, purchaseKey, masterKey, badgeType} now) >>= \case
                Nothing ->
                  readCode now code db >>= \case
                    Left resp -> pure resp
                    Right _ -> logError "badge service: redeeming a code failed, but the code is neither redeemed nor revoked" $> errorResponse BSEInternal
                Just purchaseId -> liftIO $ firstMonthResponse db purchaseId Nothing firstMonth
            pure $ fromRight (errorResponse BSEInternal) r
  where
    readCode now code db = liftIO $
      getBadgeCode db (badgeCodeHash code) >>= \case
        Nothing -> pure $ Left $ errorResponse BSECodeInvalid
        Just c@IssuedCode {revokedAt, paymentStatus, expiresAt, redemption}
          -- Revoked is checked first, so it answers as if the code never existed.
          | Just _ <- revokedAt -> pure $ Left $ errorResponse BSECodeInvalid
          -- Redeeming an unpaid code would issue a free badge, so unpaid is refused.
          | CPSUnpaid <- paymentStatus -> pure $ Left $ errorResponse BSEPaymentPending
          | otherwise ->
              claimedResponse db BSECodeUsed purchaseKey redemption >>= \case
                Left resp -> pure $ Left resp
                Right ()
                  | maybe False (now >=) expiresAt -> pure $ Left $ errorResponse BSECodeExpired
                  | otherwise -> pure $ Right c

-- | Every refusal is answered before anything is written, so it leaves the receipt unclaimed; and
-- nothing is written until the credential is signed.
purchaseWithReceipt :: BadgeIssuerKey -> ChatController -> C.PublicKeyEd25519 -> BadgeMasterKey -> StoreReceipt -> IO BadgeServiceResponse
purchaseWithReceipt key cc purchaseKey masterKey StoreReceipt {provider, providerRef, verifyReceipt} =
  withDB' "getStorePayment" cc (\db -> getStorePaymentClaim db provider providerRef) >>= \case
    Left _ -> pure $ errorResponse BSEInternal
    Right claim
      -- only the key it credited can ask, and it is told only what it was given, so the store is not asked
      | claimedBy claim -> answerClaim claim
      | otherwise ->
          verifyReceipt >>= \case
            Left refusal -> storeRefusalResponse refusal
            Right StoreTransaction {environment = SETest} -> storeRefusalResponse $ SRInvalid "test purchase"
            -- another key's claim is told only once the store vouched for the receipt, or it would reveal which transactions were credited
            Right tx -> case claim of
              Unclaimed -> newPurchase tx
              _ -> answerClaim claim
  where
    claimedBy = \case
      Claimed ClaimedPurchase {purchaseKey = k} -> k == purchaseKey
      _ -> False
    answerClaim claim =
      withDB' "answerStoreClaim" cc (\db -> claimedResponse db BSEReceiptUsed purchaseKey claim) <&> \case
        Right (Left resp) -> resp
        _ -> errorResponse BSEInternal
    -- the product is read only for a receipt not yet credited, so retiring it leaves its replays answered
    newPurchase StoreTransaction {productId, quantity, paid} = case storeProduct provider productId of
      Nothing -> pure $ errorResponse BSEProductUnavailable
      Just StoreProduct {badgeType, months} -> do
        now <- badgeNow cc
        signFirstMonth key cc masterKey badgeType (months * quantity) (SCPayment Nothing) now >>= \case
          Left resp -> pure resp
          Right firstMonth -> do
            paymentId <- randomId cc
            r <- withDB "writeStorePurchase" cc $ \db ->
              liftIO (createStorePurchase db NewStorePurchase {paymentId, provider, providerRef, paid, purchaseKey, masterKey, badgeType} now) >>= \case
                -- Credited to another key, or to this one by a request that ran alongside it, while signing.
                Nothing ->
                  liftIO (getStorePaymentClaim db provider providerRef >>= claimedResponse db BSEReceiptUsed purchaseKey) >>= \case
                    Left resp -> pure resp
                    Right () -> logError "badge service: claiming a store payment failed, but it funds no purchase" $> errorResponse BSEInternal
                Just purchaseId -> liftIO $ firstMonthResponse db purchaseId (Just paymentId) firstMonth
            pure $ fromRight (errorResponse BSEInternal) r

-- | The client learns only the code: a forged receipt, a refunded purchase and a test one are all receipt_invalid.
storeRefusalResponse :: StoreRefusal -> IO BadgeServiceResponse
storeRefusalResponse = \case
  SRInvalid reason -> logInfo ("store receipt refused: " <> reason) $> errorResponse BSEReceiptInvalid
  SRPending -> pure $ errorResponse BSEPaymentPending
  SRUnreachable reason -> logWarn ("store unreachable: " <> reason) $> errorResponse BSEProviderUnavailable
  SRVerifierFailed reason -> logError ("store receipt not verified: " <> reason) $> errorResponse BSEInternal

claimedResponse :: DB.Connection -> BadgeServiceErrorCode -> C.PublicKeyEd25519 -> FundingClaim -> IO (Either BadgeServiceResponse ())
claimedResponse db usedCode purchaseKey = \case
  Unclaimed -> pure $ Right ()
  ClaimedUnreadable -> pure $ Left $ errorResponse BSEInternal
  Claimed ClaimedPurchase {purchaseKey = k, badgePurchaseId, credential}
    | k /= purchaseKey -> pure $ Left $ errorResponse usedCode
    | otherwise ->
        maybe (Left $ errorResponse BSEInternal) (Left . credentialResponse (Just credential) Nothing)
          <$> getLedgerEntries db badgePurchaseId 0

signFirstMonth :: BadgeIssuerKey -> ChatController -> BadgeMasterKey -> BadgeType -> Int -> StatementCreditType -> UTCTime -> IO (Either BadgeServiceResponse (StatementEntry, (StatementEntry, BadgeCredential)))
signFirstMonth key cc masterKey badgeType months credit now = do
  (grantUuid, issueUuid) <- (,) <$> randomId cc <*> randomId cc
  -- TODO [badges] a top-up grants onto an existing ledger, and must lapse before it or the
  -- months it adds are counted from a start already in the past
  let granted = grantEntry now grantUuid months credit $ emptyEntry now badgeType
  -- A grant of at least one month starting now always has a month to issue.
  case issueEntry now issueUuid granted of
    Nothing -> pure $ Left $ errorResponse BSEInternal
    Just issued ->
      credentialForEntry key masterKey issued >>= \case
        Left e -> logError ("badge service signing failed: " <> T.pack e) $> Left (errorResponse BSEInternal)
        Right signed -> pure $ Right (granted, signed)

firstMonthResponse :: DB.Connection -> Int64 -> Maybe T.Text -> (StatementEntry, (StatementEntry, BadgeCredential)) -> IO BadgeServiceResponse
firstMonthResponse db purchaseId creditPaymentId_ (granted, signed) = do
  appendLedgerPlan db purchaseId creditPaymentId_ [granted] $ Just $ issuanceAfter granted signed
  entries_ <- getLedgerEntries db purchaseId 0
  pure $ maybe (errorResponse BSEInternal) (credentialResponse (Just $ snd signed) Nothing) entries_

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
        when (fmap entryId tip' == fmap entryId tip) $ appendLedgerPlan db purchaseId Nothing rows issuance_
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

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module BadgeService.Service
  ( ServiceState (..),
    newServiceState,
    welcomeGetOpts,
    checkIssuerKey,
    badgeService,
    badgeServiceCLI,
    IssueCodeOpts (..),
    issueBadgeCode,
  )
where

import BadgeService.Catalog (defaultCatalog)
import BadgeService.Config (IssuerConfig (..), ServiceConfig (..), readServiceConfig)
import BadgeService.Options
import BadgeService.Poller (newPollerEnv, newReadHints, runPoller)
import BadgeService.Providers.BTCPay (btcpayProvider)
import BadgeService.Store
import BadgeService.Store.Invoices (seedCatalog, truncateToSecond)
import BadgeService.Store.Migrate (runBadgeServiceMigrations)
import BadgeService.Waiters (Waiters, newWaiters)
import BadgeService.Web.Server (newWebEnv, runWebListener)
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
import Data.Functor (($>))
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Data.Time.Calendar (addDays, addGregorianMonthsClip)
import Data.Time.Calendar.WeekDate (toWeekDate)
import Data.Time.Clock (UTCTime (..), getCurrentTime)
import Simplex.Chat.Badges
import Simplex.Chat.Badges.Code
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
      providers <- maybe (pure []) (fmap (: []) . btcpayProvider) (btcpay sc)
      hints <- newReadHints
      webEnv <- newWebEnv chatStore sc ws hints providers
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
errorResponse code = BSPError {code, message = Nothing, retryAfter = Nothing}


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
        -- every command but redeemBadgeCode needs a key the service already knows: that one
        -- creates the purchase, so its key is unknown on a first redemption
        _ -> case purchaseKey of
          Nothing -> pure $ errorResponse BSEUnsupportedVersion
          Just k ->
            withDB' "purchaseKeyExists" cc (`purchaseKeyExists` k) >>= \case
              Right True -> pure $ errorResponse BSEUnsupportedVersion
              Right False -> pure $ errorResponse BSEUnknownPurchaseKey
              Left _ -> pure $ errorResponse BSEInternal

-- | Nothing is written until the credential is signed, so a signing failure leaves the code
-- unspent rather than spent with nothing behind it.
redeemCode :: BadgeIssuerKey -> ChatController -> C.PublicKeyEd25519 -> BadgeMasterKey -> T.Text -> IO BadgeServiceResponse
redeemCode BadgeIssuerKey {keyIdx, secretKey} cc purchaseKey masterKey codeText = case parseBadgeCode codeText of
  Nothing -> pure $ errorResponse BSECodeInvalid
  Just code ->
    withDB' "getBadgeCode" cc (`getBadgeCode` badgeCodeHash code) >>= \case
      Left _ -> pure $ errorResponse BSEInternal
      Right Nothing -> pure $ errorResponse BSECodeInvalid
      Right (Just issued@IssuedCode {badgeCodeId, badgeType, months}) -> do
        now <- getCurrentTime
        case refusal issued now of
          Just resp -> pure resp
          Nothing -> do
            let periodEnd = addMonths (toInteger months) now
                badgeInfo = BadgeInfo {badgeType, badgeExpiry = endOfSundayAfter periodEnd, badgeExtra = ""}
            issueBadge keyIdx secretKey (VerifiedBadgeRequest BadgeRequest {masterKey, badgeInfo}) >>= \case
              Left e -> logError ("badge service signing failed: " <> T.pack e) $> errorResponse BSEInternal
              Right credential -> do
                issuanceId <- safeDecodeUtf8 . strEncode <$> atomically (C.randomBytes 16 $ random cc)
                let newRedemption =
                      NewBadgeCodeRedemption
                        { badgeCodeId,
                          issuanceId,
                          purchaseKey,
                          masterKey,
                          badgeType,
                          credential,
                          periodStart = now,
                          periodEnd,
                          expiry = endOfSundayAfter periodEnd
                        }
                -- re-read: a redemption or a revoke may have landed while this one was signing
                r <- withDB "writeCodeRedemption" cc $ \db ->
                  liftIO (getBadgeCode db $ badgeCodeHash code) >>= \case
                    Just current | Just resp <- refusal current now -> pure resp
                    _ -> liftIO $ credentialResponse credential <$ writeCodeRedemption db newRedemption now
                pure $ either (const $ errorResponse BSEInternal) id r
  where
    -- Why this code may not be redeemed now, if it may not. Read before signing and again
    -- inside the write, where a revoke or another redemption may have landed in between: two
    -- spellings of the same set would drift, and the second one is the one holding the money.
    refusal :: IssuedCode -> UTCTime -> Maybe BadgeServiceResponse
    refusal IssuedCode {revokedAt, paymentStatus, expiresAt, redemption} now
      -- first, so a revoked code answers as if it had never existed whatever else is true of it
      | Just _ <- revokedAt = Just $ errorResponse BSECodeInvalid
      -- a code the web checkout wrote exists from the moment the invoice is created, and
      -- settlement is what marks it paid: redeeming before that would issue a free badge
      | CPSUnpaid <- paymentStatus = Just $ errorResponse BSEPaymentPending
      -- a code already redeemed keeps answering with the credential it was redeemed for
      | Just resp <- redeemedResponse redemption = Just resp
      -- the deadline is on redeeming, not on holding
      | maybe False (now >=) expiresAt = Just $ errorResponse BSECodeExpired
      | otherwise = Nothing
    -- one definition, used before signing and again inside the write transaction
    redeemedResponse = \case
      CodeUnredeemed -> Nothing
      CodeRedeemedUnreadable -> Just $ errorResponse BSEInternal
      CodeRedeemed RedeemedCode {purchaseKey = k, credential}
        | k == purchaseKey -> Just $ credentialResponse credential
        | otherwise -> Just $ errorResponse BSECodeUsed

-- TODO [badges] the statement is empty until the ledger is written
credentialResponse :: BadgeCredential -> BadgeServiceResponse
credentialResponse credential =
  BSPBadgeCredential {credential = Just credential, receipt = Nothing, statement = BadgeStatement {entries = [], previousEntryId = Nothing}}

addMonths :: Integer -> UTCTime -> UTCTime
addMonths n (UTCTime d t) = UTCTime (addGregorianMonthsClip n d) t

-- Every badge in a week expires together, revealing nothing about when it was bought.
-- The end of a Sunday is the next Monday at 00:00, so this returns a Monday and 8 is right.
endOfSundayAfter :: UTCTime -> UTCTime
endOfSundayAfter (UTCTime d _) =
  let (_, _, dayOfWeek) = toWeekDate d -- 1 Monday .. 7 Sunday
   in UTCTime (addDays (toInteger (8 - dayOfWeek)) d) 0

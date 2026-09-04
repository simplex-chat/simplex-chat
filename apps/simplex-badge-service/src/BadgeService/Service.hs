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
    IssueCodeOpts (..),
    issueBadgeCode,
  )
where

import BadgeService.Options
import BadgeService.Store
import BadgeService.Store.Migrate (runBadgeServiceMigrations)
import Control.Applicative (optional)
import Control.Concurrent.STM
import Control.Logger.Simple
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as J
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Attoparsec.ByteString.Char8 as A
import Data.ByteString.Char8 (ByteString)
import Data.Char (isSpace)
import Data.Functor (($>))
import Data.Maybe (fromMaybe)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Data.Time.Clock (UTCTime, getCurrentTime)
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
import qualified Simplex.Messaging.Crypto as C
import Simplex.Messaging.Crypto.BBS (bbsPublicKey)
import Simplex.Messaging.Encoding.String (TextEncoding, strEncode, textDecode)
import Simplex.Messaging.Version (isCompatible)
import Simplex.Messaging.Util (raceAny_, safeDecodeUtf8, tshow)
import System.Directory (getAppUserDataDirectory)
import System.Exit (exitFailure)

data ServiceState = ServiceState
  { serviceCC :: TMVar ChatController,
    serviceRequestQ :: TQueue (User, AgentInvId, Maybe C.PublicKeyEd25519, J.Object)
  }

newServiceState :: IO ServiceState
newServiceState = do
  serviceCC <- newEmptyTMVarIO
  serviceRequestQ <- newTQueueIO
  pure ServiceState {serviceCC, serviceRequestQ}

welcomeGetOpts :: IO BadgeServiceOpts
welcomeGetOpts = do
  appDir <- getAppUserDataDirectory "simplex"
  opts@BadgeServiceOpts {coreOptions, testing, serviceName} <- getBadgeServiceOpts appDir "simplex_badge_service"
  unless testing $ do
    putStrLn $ "SimpleX Badge Service v" ++ versionNumber
    printDbOpts coreOptions
    putStrLn $ "Service name: " ++ T.unpack serviceName
  pure opts

-- | Check the secret is the key trusted at its index: otherwise every code redeemed is burned.
checkIssuerKey :: BadgeServiceOpts -> ChatConfig -> IO (Either String BadgeIssuerKey)
checkIssuerKey BadgeServiceOpts {issuerKey} ChatConfig {badgePublicKeys} = case issuerKey of
  Nothing -> pure $ Left "an issuer key is required - pass both --issuer-key-idx and --issuer-secret (see `simplex-chat badge keygen`)"
  Just k@BadgeIssuerKey {keyIdx, secretKey} ->
    bbsPublicKey secretKey >>= \case
      Left e -> pure $ Left $ "issuer secret is not a valid key: " <> e
      Right pk -> pure $ case M.lookup keyIdx badgePublicKeys of
        Just pk' | pk' == pk -> Right k
        Just _ -> Left $ "issuer secret does not match the configured key at index " <> show keyIdx <> ", its public key is " <> T.unpack (safeDecodeUtf8 $ strEncode pk)
        Nothing -> Left $ "no configured badge key at index " <> show keyIdx <> ", clients could not verify what this service signs"

requireIssuerKey :: BadgeServiceOpts -> ChatConfig -> IO BadgeIssuerKey
requireIssuerKey opts cfg =
  checkIssuerKey opts cfg >>= either (\e -> putStrLn ("Error: " <> e) >> exitFailure) pure

badgeService :: BadgeServiceOpts -> ChatConfig -> ServiceState -> IO ()
badgeService opts cfg env = do
  key <- requireIssuerKey opts cfg
  let chatHooks =
        defaultChatHooks
          { preStartHook = Just $ badgePreStartHook opts,
            postStartHook = Just $ badgePostStartHook opts env,
            preCmdHook = Just badgeCmdHook
          }
  -- the reader must not block: outputQ carries every chat event
  simplexChatCore cfg {chatHooks} (mkChatOpts opts) $ \_ cc ->
    raceAny_
      [ forever $
          atomically (readTBQueue $ outputQ cc) >>= \case
            (_, Right (CEvtServiceRequest u reqId sigKey reqData)) ->
              atomically $ writeTQueue (serviceRequestQ env) (u, reqId, sigKey, reqData)
            _ -> pure (),
        processQueuedRequests key env
      ]

badgeServiceCLI :: BadgeServiceOpts -> IO ()
badgeServiceCLI opts = do
  key <- requireIssuerKey opts terminalChatConfig
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
      processQueuedRequests key env
    ]

-- | issuing codes lives here rather than in core: every user's app would otherwise ship it
badgeCmdHook :: ChatController -> ChatCommand -> IO (Either (Either ChatError ChatResponse) ChatCommand)
badgeCmdHook cc = \case
  CustomChatCommand cmd -> Left <$> runBadgeCmd cc cmd
  cmd -> pure $ Right cmd

runBadgeCmd :: ChatController -> ByteString -> IO (Either ChatError ChatResponse)
runBadgeCmd cc cmd = case A.parseOnly issueCmdP cmd of
  Left _ -> pure $ chatCmdError "use: //issue supporter|legend|investor [months 1-255] [paid|unpaid|free]"
  Right issueOpts ->
    issueBadgeCode cc issueOpts >>= \case
      Right code -> pure $ Right CRCustomChatResponse {user_ = Nothing, response = "code " <> formatBadgeCode code}
      Left e -> pure $ chatCmdError $ "issuing code: " <> e

issueCmdP :: A.Parser IssueCodeOpts
issueCmdP =
  "issue " *> do
    badgeType <- badgeTypeP
    months_ <- optional (A.space *> A.decimal)
    -- outside `optional`, which would otherwise backtrack past a bad count
    months <- maybe (pure 1) checkMonths months_
    paymentStatus <- fromMaybe CPSFree <$> optional (A.space *> textTokenP)
    A.skipSpace
    A.endOfInput
    pure IssueCodeOpts {badgeType, months, paymentStatus}
  where
    checkMonths n
      | n >= 1 && n <= (255 :: Int) = pure n
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

badgePreStartHook :: BadgeServiceOpts -> ChatController -> IO ()
badgePreStartHook opts ChatController {config, chatStore} =
  runBadgeServiceMigrations opts config chatStore

badgePostStartHook :: BadgeServiceOpts -> ServiceState -> ChatController -> IO ()
badgePostStartHook BadgeServiceOpts {noAddress, testing} env cc = do
  -- SREQ delivery gates on this flag; Core starts serviceRequests=False, so the hook must set it.
  atomically $ writeTVar (processServiceRequests cc) True
  readTVarIO (currentUser cc) >>= \case
    Nothing -> putStrLn "No current user" >> exitFailure
    -- DR required for service RPC; autoAccept off because badge service ignores contact events.
    Just _ -> do
      unless noAddress $ initializeBotAddress' (not testing) (Just True) False cc
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
        BSCIssueBadge {badgeRequest, balance} -> case purchaseKey of
          Just k -> issueBadgeCmd key cc k badgeRequest balance
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

-- | Signs before anything is written, so a signing failure leaves the month still due.
-- The expiry cap is a client's proposal, which can only shorten what the balance funds.
signLedgerPlan :: BadgeIssuerKey -> BadgeMasterKey -> Maybe UTCTime -> UTCTime -> Maybe (Int, StatementCreditType) -> LedgerBalance -> IO (Either String SignedPlan)
signLedgerPlan BadgeIssuerKey {keyIdx, secretKey} masterKey expiryCap now grant_ b0 =
  case planIssuance plan of
    Nothing -> pure $ Right $ SignedPlan {spRows = planRows plan, spIssuance = Nothing}
    Just (row@LedgerRow {rowBalance = LedgerBalance {balanceBadgeType}}, period@BadgePeriod {badgeExpiry}) -> do
      -- the type is the one the balance funds, never one the caller proposed
      let badgeInfo = BadgeInfo {badgeType = balanceBadgeType, badgeExpiry = maybe badgeExpiry (min badgeExpiry) expiryCap, badgeExtra = ""}
      fmap (\credential -> SignedPlan {spRows = planRows plan, spIssuance = Just (row, period, credential)})
        <$> issueBadge keyIdx secretKey (VerifiedBadgeRequest BadgeRequest {masterKey, badgeInfo})
  where
    plan = ledgerPlan now grant_ b0

signedCredential :: SignedPlan -> Maybe BadgeCredential
signedCredential SignedPlan {spIssuance} = (\(_, _, c) -> c) <$> spIssuance

statementEntry :: ServiceLedgerEntry -> StatementEntry
statementEntry ServiceLedgerEntry {entryUuid, changeMonths, balance = LedgerBalance {balanceMonths, balanceStartTs, balanceAnchorTs, balanceBadgeType}, entryType, createdAt} =
  StatementEntry {entryId = entryUuid, changeMonths, balanceMonths, balanceStartTs, balanceAnchorTs, balanceBadgeType, wasPausedSince = Nothing, createdAt, entryType}

credentialResponse :: Maybe BadgeCredential -> Maybe T.Text -> [ServiceLedgerEntry] -> BadgeServiceResponse
credentialResponse credential previousEntryId entries =
  BSPBadgeCredential {credential, receipt = Nothing, statement = BadgeStatement {entries = map statementEntry entries, previousEntryId}}

-- | Nothing is written until the credential is signed, so a signing failure leaves the code
-- unspent rather than spent with nothing behind it.
redeemCode :: BadgeIssuerKey -> ChatController -> C.PublicKeyEd25519 -> BadgeMasterKey -> T.Text -> IO BadgeServiceResponse
redeemCode key cc purchaseKey masterKey codeText = case parseBadgeCode codeText of
  Nothing -> pure $ errorResponse BSECodeInvalid
  Just code ->
    withDB "getBadgeCode" cc (readCode code) >>= \case
      Left _ -> pure $ errorResponse BSEInternal
      Right Nothing -> pure $ errorResponse BSECodeInvalid
      Right (Just (Left resp)) -> pure resp
      Right (Just (Right IssuedCode {badgeCodeId, badgeType, months})) -> do
        now <- badgeNow cc
        -- a fresh purchase starts from an exhausted balance dated now, which the grant then credits
        let b0 = LedgerBalance {balanceMonths = 0, balanceStartTs = now, balanceAnchorTs = now, balanceBadgeType = badgeType}
        signLedgerPlan key masterKey Nothing now (Just (months, SCCode)) b0 >>= \case
          Left e -> logError ("badge service signing failed: " <> T.pack e) $> errorResponse BSEInternal
          Right signed -> do
            -- re-read: a concurrent redemption may have landed while this one was signing
            r <- withDB "writeCodeRedemption" cc $ \db ->
              readCode code db >>= \case
                Just (Left resp) -> pure resp
                Just (Right _) -> liftIO $ do
                  purchaseId <- createCodePurchase db NewCodePurchase {badgeCodeId, purchaseKey, masterKey, badgeType} now
                  appendLedgerPlan db (random cc) purchaseId signed now
                  entries_ <- getLedgerEntries db purchaseId 0
                  pure $ maybe (errorResponse BSEInternal) (credentialResponse (signedCredential signed) Nothing) entries_
                Nothing -> pure $ errorResponse BSECodeInvalid
            pure $ either (const $ errorResponse BSEInternal) id r
  where
    -- used before signing and again inside the write transaction: Left answers a spent code
    readCode code db = liftIO $ do
      c_ <- getBadgeCode db (badgeCodeHash code)
      forM c_ $ \c@IssuedCode {redemption} -> fmap (const c) <$> spentResponse db redemption
    spentResponse db = \case
      CodeUnredeemed -> pure $ Right ()
      CodeRedeemedUnreadable -> pure $ Left $ errorResponse BSEInternal
      CodeRedeemed RedeemedCode {purchaseKey = k, badgePurchaseId, credential}
        | k /= purchaseKey -> pure $ Left $ errorResponse BSECodeUsed
        -- the whole ledger, so a client that lost the first response still ends holding it
        | otherwise ->
            maybe (Left $ errorResponse BSEInternal) (Left . credentialResponse (Just credential) Nothing)
              <$> getLedgerEntries db badgePurchaseId 0

-- | The purchase is reached through the verified signer key and no other way.
issueBadgeCmd :: BadgeIssuerKey -> ChatController -> C.PublicKeyEd25519 -> BadgeRequest -> BadgeBalance -> IO BadgeServiceResponse
issueBadgeCmd key cc purchaseKey BadgeRequest {masterKey, badgeInfo = BadgeInfo {badgeType = askedType, badgeExpiry = askedExpiry}} BadgeBalance {lastEntry} = do
  now <- badgeNow cc
  purchase_ <- withDB' "getBadgePurchase" cc $ \db -> do
    p_ <- getPurchaseByKey db purchaseKey
    forM p_ $ \p@ServicePurchase {badgePurchaseId} -> (p,) <$> getLedgerTip db badgePurchaseId
  case purchase_ of
    Left _ -> pure $ errorResponse BSEInternal
    Right Nothing -> pure $ errorResponse BSEUnknownPurchaseKey
    Right (Just (ServicePurchase {badgePurchaseId, badgeType}, tip))
      -- a supporter balance must not sign a legend credential
      | askedType /= badgeType -> pure $ errorResponse BSEBadRequest
      | otherwise -> do
          let emptyBalance = LedgerBalance {balanceMonths = 0, balanceStartTs = now, balanceAnchorTs = now, balanceBadgeType = badgeType}
              b0 = maybe emptyBalance tipBalance tip
          signLedgerPlan key masterKey (Just askedExpiry) now Nothing b0 >>= \case
            Left e -> logError ("badge service signing failed: " <> T.pack e) $> errorResponse BSEInternal
            Right signed -> do
              r <- withDB "issueBadge" cc $ \db -> liftIO $ do
                -- a row appended since the read makes the signed period stale
                tip' <- getLedgerTip db badgePurchaseId
                when (fmap tipEntryId tip' == fmap tipEntryId tip) $
                  appendLedgerPlan db (random cc) badgePurchaseId signed now
                issueResponse db badgePurchaseId now
              pure $ either (const $ errorResponse BSEInternal) id r
  where
    -- entries after the one asserted, or the whole ledger when this purchase does not hold it.
    -- Only the asserted entry's identity is read, never the months it claims.
    issueResponse db purchaseId t = do
      let StatementEntry {entryId = assertedUuid} = lastEntry
      assertedId <- getLedgerEntryId db purchaseId assertedUuid
      entries_ <- getLedgerEntries db purchaseId (fromMaybe 0 assertedId)
      credential_ <- getCurrentIssuance db purchaseId t
      pure $ maybe (errorResponse BSEInternal) (credentialResponse credential_ (assertedUuid <$ assertedId)) entries_

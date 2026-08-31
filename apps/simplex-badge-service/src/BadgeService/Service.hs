{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module BadgeService.Service
  ( ServiceState (..),
    newServiceState,
    welcomeGetOpts,
    badgeService,
    badgeServiceCLI,
    MintCodeOpts (..),
    mintBadgeCode,
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
import qualified Data.Text as T
import Data.Time.Calendar (addDays, addGregorianMonthsClip)
import Data.Time.Calendar.WeekDate (toWeekDate)
import Data.Time.Clock (UTCTime (..), getCurrentTime)
import Simplex.Chat.Badges
import Simplex.Chat.Badges.Code
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

requireIssuerKey :: BadgeServiceOpts -> IO BadgeIssuerKey
requireIssuerKey BadgeServiceOpts {issuerKey} = case issuerKey of
  Just k -> pure k
  Nothing -> do
    putStrLn "Error: an issuer key is required - pass both --issuer-key-idx and --issuer-secret (see `simplex-chat badge keygen`)"
    exitFailure

badgeService :: BadgeServiceOpts -> ChatConfig -> ServiceState -> IO ()
badgeService opts cfg env = do
  key <- requireIssuerKey opts
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
  key <- requireIssuerKey opts
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

-- | minting lives here rather than in core: every user's app would otherwise ship it
badgeCmdHook :: ChatController -> ChatCommand -> IO (Either (Either ChatError ChatResponse) ChatCommand)
badgeCmdHook cc = \case
  CustomChatCommand cmd -> Left <$> runBadgeCmd cc cmd
  cmd -> pure $ Right cmd

runBadgeCmd :: ChatController -> ByteString -> IO (Either ChatError ChatResponse)
runBadgeCmd cc cmd = case A.parseOnly mintCmdP cmd of
  Left _ -> pure $ chatCmdError "use: //mint supporter|legend|investor [months 1-255] [paid|unpaid|free]"
  Right mintOpts ->
    mintBadgeCode cc mintOpts >>= \case
      Right code -> pure $ Right CRCustomChatResponse {user_ = Nothing, response = "code " <> formatBadgeCode code}
      Left e -> pure $ chatCmdError $ "minting code: " <> e

mintCmdP :: A.Parser MintCodeOpts
mintCmdP =
  "mint " *> do
    badgeType <- badgeTypeP
    months_ <- optional (A.space *> A.decimal)
    -- outside `optional`, which would otherwise backtrack past a bad count
    months <- maybe (pure 1) checkMonths months_
    paymentStatus <- fromMaybe CPSFree <$> optional (A.space *> textTokenP)
    A.skipSpace
    A.endOfInput
    pure MintCodeOpts {badgeType, months, paymentStatus}
  where
    checkMonths n
      | n >= 1 && n <= (255 :: Int) = pure n
      | otherwise = fail "months must be between 1 and 255"
    -- BadgeType decodes anything to BTUnknown, so a typo would mint an unusable code
    badgeTypeP =
      textTokenP >>= \case
        BTUnknown t -> fail $ "unknown badge type " <> T.unpack t
        bt -> pure bt
    textTokenP :: TextEncoding a => A.Parser a
    textTokenP = do
      t <- A.takeWhile1 (not . isSpace)
      maybe (fail "invalid value") pure $ textDecode $ safeDecodeUtf8 t

data MintCodeOpts = MintCodeOpts
  { badgeType :: BadgeType,
    months :: Int,
    paymentStatus :: BadgeCodePaymentStatus
  }

-- | The caller sees the code once; only its hash is stored, so a lost code cannot be recovered.
mintBadgeCode :: ChatController -> MintCodeOpts -> IO (Either String BadgeCode)
mintBadgeCode cc MintCodeOpts {badgeType, months, paymentStatus} = do
  code <- randomBadgeCode $ random cc
  now <- getCurrentTime
  r <- withDB' "mintBadgeCode" cc $ \db -> insertBadgeCode db (badgeCodeHash code) badgeType months paymentStatus now
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
      Right (Just MintedCode {badgeCodeId, badgeType, redemption}) -> case redeemedResponse redemption of
        Just resp -> pure resp
        Nothing -> do
          now <- getCurrentTime
          let periodEnd = addMonths 1 now
              badgeInfo = BadgeInfo {badgeType, badgeExpiry = Just (endOfSundayAfter periodEnd), badgeExtra = ""}
          issueBadge keyIdx secretKey (VerifiedBadgeRequest BadgeRequest {masterKey, badgeInfo}) >>= \case
            Left e -> logError ("badge service signing failed: " <> T.pack e) $> errorResponse BSEInternal
            Right credential -> do
              issuanceId <- safeDecodeUtf8 . strEncode <$> atomically (C.randomBytes 16 $ random cc)
              let issuance =
                    CodeIssuance
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
              -- re-read: a concurrent redemption may have landed while this one was signing
              r <- withDB "writeCodeRedemption" cc $ \db ->
                liftIO (getBadgeCode db $ badgeCodeHash code) >>= \case
                  Just MintedCode {redemption = current} | Just resp <- redeemedResponse current -> pure resp
                  _ -> liftIO $ credentialResponse credential <$ writeCodeRedemption db issuance now
              pure $ either (const $ errorResponse BSEInternal) id r
  where
    -- one definition, used before signing and again inside the write transaction
    redeemedResponse = \case
      CodeUnredeemed -> Nothing
      CodeRedeemedUnreadable -> Just $ errorResponse BSEInternal
      CodeRedeemed RedeemedCode {purchaseKey = k, credential}
        | k == purchaseKey -> Just $ credentialResponse credential
        | otherwise -> Just $ errorResponse BSECodeUsed

-- the ledger is not written yet, so the statement is empty
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

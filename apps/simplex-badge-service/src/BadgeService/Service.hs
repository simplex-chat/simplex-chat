{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module BadgeService.Service
  ( welcomeGetOpts,
    badgeService,
    badgeServiceCLI,
    -- * Exposed for BadgeServiceTests: the catch-all's forcing behaviour is proved directly
    -- against a lazily-thunked pure exception, not just observed indirectly through the RPC
    -- round trip (which cannot yet construct one -- see B5's report).
    runHandler,
  )
where

import BadgeService.Catalog (seedCatalog)
import BadgeService.Config (BadgeServiceEnv (..), checkFailureBuckets, newBadgeServiceEnv, readBadgeServiceConfig)
import BadgeService.Options
import BadgeService.Store (getPurchaseByKey, withServiceTransaction)
import BadgeService.Store.Migrate (runBadgeServiceMigrations)
import Control.Concurrent.STM
import Control.Exception (SomeException, catch, evaluate)
import Control.Logger.Simple
import Control.Monad
import qualified Data.Aeson as J
import qualified Data.Aeson.Types as JT
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word32)
import Simplex.Chat.Badges.Service
  ( BadgeServiceCommand (..),
    BadgeServiceErrorCode (..),
    BadgeServiceRequest (..),
    BadgeServiceResponse (..),
    minSupportedBadgeVersion,
  )
import Simplex.Chat.Bot (initializeBotAddress')
import Simplex.Chat.Controller
import Simplex.Chat.Core (sendChatCmd, simplexChatCore)
import Simplex.Chat.Options (printDbOpts)
import Simplex.Chat.PaymentService (ServicePayment (..))
import Simplex.Chat.Terminal (terminalChatConfig)
import Simplex.Chat.Terminal.Main (simplexChatCLI')
import Simplex.Chat.Types (AgentInvId (..), User (..))
import qualified Simplex.Messaging.Crypto as C
import Simplex.Messaging.Encoding.String (strEncode)
import Simplex.Messaging.Util (raceAny_, safeDecodeUtf8, tshow)
import System.Directory (getAppUserDataDirectory)
import System.Exit (exitFailure)

data ServiceState = ServiceState
  { serviceCC :: TMVar ChatController,
    serviceEnv :: TMVar BadgeServiceEnv,
    serviceRequestQ :: TQueue (User, AgentInvId, Maybe C.PublicKeyEd25519, J.Object)
  }

newServiceState :: IO ServiceState
newServiceState = do
  serviceCC <- newEmptyTMVarIO
  serviceEnv <- newEmptyTMVarIO
  serviceRequestQ <- newTQueueIO
  pure ServiceState {serviceCC, serviceEnv, serviceRequestQ}

welcomeGetOpts :: IO BadgeServiceOpts
welcomeGetOpts = do
  appDir <- getAppUserDataDirectory "simplex"
  opts@BadgeServiceOpts {coreOptions, testing, serviceName} <- getBadgeServiceOpts appDir "simplex_badge_service"
  unless testing $ do
    putStrLn $ "SimpleX Badge Service v" ++ versionNumber
    printDbOpts coreOptions
    putStrLn $ "Service name: " ++ T.unpack serviceName
  pure opts

badgeService :: BadgeServiceOpts -> ChatConfig -> IO ()
badgeService opts cfg = do
  env <- newServiceState
  let chatHooks =
        defaultChatHooks
          { preStartHook = Just $ badgePreStartHook opts env,
            postStartHook = Just $ badgePostStartHook opts env
          }
  simplexChatCore cfg {chatHooks} (mkChatOpts opts) $ \_ cc -> do
    -- preStartHook (badgePreStartHook) already ran and populated serviceEnv by the time this
    -- callback starts (Core.hs runs it before postStartHook, which runs before this), so a
    -- single read here is safe -- the value never changes again for the life of the process.
    bsEnv <- atomically $ readTMVar $ serviceEnv env
    forever $ do
      (_, event) <- atomically . readTBQueue $ outputQ cc
      case event of
        Right (CEvtServiceRequest u reqId sigKey_ reqData) -> handleServiceRequest bsEnv cc u reqId sigKey_ reqData
        _ -> pure ()

badgeServiceCLI :: BadgeServiceOpts -> IO ()
badgeServiceCLI opts = do
  env <- newServiceState
  let eventHook _cc ev = do
        case ev of
          Right (CEvtServiceRequest u reqId sigKey_ reqData) ->
            atomically $ writeTQueue (serviceRequestQ env) (u, reqId, sigKey_, reqData)
          _ -> pure ()
        pure ev
      chatHooks =
        defaultChatHooks
          { preStartHook = Just $ badgePreStartHook opts env,
            postStartHook = Just $ badgePostStartHook opts env,
            eventHook = Just eventHook
          }
  raceAny_
    [ simplexChatCLI' terminalChatConfig {chatHooks} (mkChatOpts opts) Nothing,
      processQueuedRequests env
    ]

processQueuedRequests :: ServiceState -> IO ()
processQueuedRequests env = do
  cc <- atomically $ readTMVar $ serviceCC env
  bsEnv <- atomically $ readTMVar $ serviceEnv env
  forever $ do
    (u, reqId, sigKey_, reqData) <- atomically $ readTQueue $ serviceRequestQ env
    handleServiceRequest bsEnv cc u reqId sigKey_ reqData

-- Seeded here, after migrations and before badgePostStartHook starts the bot: every start
-- of the service must see the catalog before it can serve a request. B8's operator
-- subcommand (not yet implemented) will need to call seedCatalog the same way, so operator
-- tooling sees the same catalog.
--
-- The badge service env is built here too, once, after migrations and seedCatalog: it reads
-- and validates badge_service.ini, exits on a bad config (naming the file and the offending
-- key), and stores the built env for badgePostStartHook and the request handlers to reach.
badgePreStartHook :: BadgeServiceOpts -> ServiceState -> ChatController -> IO ()
badgePreStartHook opts@BadgeServiceOpts {configFile} ServiceState {serviceEnv} ChatController {config, chatStore} = do
  runBadgeServiceMigrations opts config chatStore
  seedCatalog chatStore
  readBadgeServiceConfig configFile >>= \case
    Left e -> putStrLn e >> exitFailure
    Right bsConfig -> do
      bsEnv <- newBadgeServiceEnv bsConfig chatStore
      atomically $ putTMVar serviceEnv bsEnv

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

handleServiceRequest :: BadgeServiceEnv -> ChatController -> User -> AgentInvId -> Maybe C.PublicKeyEd25519 -> J.Object -> IO ()
handleServiceRequest bsEnv cc User {userId} reqId signerKey reqData = do
  let reqIdT = safeDecodeUtf8 (strEncode reqId)
  logInfo $ "badge service request " <> reqIdT
  respObj <- runHandler reqIdT (dispatchRequest bsEnv signerKey reqData)
  sendChatCmd cc (APISendServiceResponse userId reqId respObj) >>= \case
    Right _ -> pure ()
    Left e -> logError $ "badge service response failed for " <> reqIdT <> ": " <> tshow e

-- | Runs 'action' and converts its result to a wire object, forcing that object fully
-- (encoding it and demanding the whole encoded length) before returning, all still inside the
-- 'catch' below. Laziness would otherwise let an exception escape uncaught: 'action' finishing
-- and returning a lazily-built 'BadgeServiceResponse' does not itself throw, even if a field
-- deep inside is an unevaluated 'error' thunk (as 'BadgeService.Catalog.chargeableMonths'
-- produces for a malformed offer, once B6\/B7 wire catalog totals into request handling) --
-- that thunk would only be forced later, by 'sendChatCmd''s own JSON encoding, OUTSIDE this
-- function, where nothing would catch it. Forcing the encoding here, inside the 'catch',
-- is what makes this a genuine catch-all rather than one that only covers IO exceptions.
--
-- 'processQueuedRequests' is a single-threaded 'forever' loop: an exception that got out of
-- here would kill the service for every user, not just fail the one request that caused it.
-- Every exception, IO or pure, is logged with the request id and turned into 'internal',
-- never repeating its message back to the client.
runHandler :: Text -> IO BadgeServiceResponse -> IO J.Object
runHandler reqIdT action =
  buildResponse `catch` \(e :: SomeException) -> do
    logError $ "badge service internal error for " <> reqIdT <> ": " <> tshow e
    pure $ responseObject BSPError {code = BSEInternal, message = Nothing, retryAfter = Nothing}
  where
    buildResponse = do
      resp <- action
      let obj = responseObject resp
      _ <- evaluate (LBS.length (J.encode (J.Object obj)))
      pure obj

-- | 'BadgeServiceResponse's four constructors are all records, so 'taggedObjectJSON' always
-- encodes them as a JSON object (never a bare string) -- the wildcard case cannot be hit by
-- any value of this type; it exists only so this function is total.
responseObject :: BadgeServiceResponse -> J.Object
responseObject resp = case J.toJSON resp of
  J.Object o -> o
  _ -> error "BadgeServiceResponse always encodes to a JSON object"

errorResponse :: BadgeServiceErrorCode -> Maybe Text -> Maybe Word32 -> BadgeServiceResponse
errorResponse code message retryAfter = BSPError {code, message, retryAfter}

notImplemented :: BadgeServiceResponse
notImplemented = errorResponse BSEInternal (Just "not implemented") Nothing

-- | Decode, version gate, and the signer\/record precondition (RPC doc "Identity"), then
-- dispatch. Order matches badges-rpc.md and the B5 brief: a decode failure is 'bad_request'
-- before anything else is even looked at; then the version gate; then the signer check
-- (badges-rpc.md: "the service rejects a purchaseKey that differs from [the verified signer]
-- with bad_request"); then the per-command signer\/record rule; only then dispatch.
dispatchRequest :: BadgeServiceEnv -> Maybe C.PublicKeyEd25519 -> J.Object -> IO BadgeServiceResponse
dispatchRequest bsEnv signerKey reqData =
  case decodeRequest reqData of
    Left _ -> pure $ errorResponse BSEBadRequest Nothing Nothing
    Right BadgeServiceRequest {version, purchaseKey, request}
      | version < minSupportedBadgeVersion -> pure $ errorResponse BSEUnsupportedVersion Nothing Nothing
      | signerKey /= purchaseKey -> pure $ errorResponse BSEBadRequest Nothing Nothing
      | otherwise ->
          checkSignerRecord bsEnv request purchaseKey >>= \case
            Left err -> pure $ errorResponse err Nothing Nothing
            Right () -> dispatchCommand bsEnv purchaseKey request

decodeRequest :: J.Object -> Either String BadgeServiceRequest
decodeRequest = JT.parseEither J.parseJSON . J.Object

-- | The signer\/record precondition, applied to every command before dispatch:
--   * 'getBadgeCatalog' may be unsigned (no key at all); nothing further is required of it.
--   * 'purchaseBadge' requires a signature but NOT a pre-existing record -- an unknown key is
--     the normal first-purchase case, because B7 is what creates the purchase row. Getting
--     this inverted would make first purchases impossible.
--   * every other command, including a *signed* 'getBadgeCatalog', requires both a signature
--     and an existing purchase row: no key at all is 'bad_request' (nothing was signed), an
--     unknown key is 'unknown_purchase_key'.
checkSignerRecord :: BadgeServiceEnv -> BadgeServiceCommand -> Maybe C.PublicKeyEd25519 -> IO (Either BadgeServiceErrorCode ())
checkSignerRecord _ BSCGetBadgeCatalog Nothing = pure $ Right ()
checkSignerRecord bsEnv BSCGetBadgeCatalog (Just key) = requirePurchaseRecord bsEnv key
checkSignerRecord _ (BSCPurchaseBadge {}) Nothing = pure $ Left BSEBadRequest
checkSignerRecord _ (BSCPurchaseBadge {}) (Just _) = pure $ Right ()
checkSignerRecord _ _ Nothing = pure $ Left BSEBadRequest
checkSignerRecord bsEnv _ (Just key) = requirePurchaseRecord bsEnv key

requirePurchaseRecord :: BadgeServiceEnv -> C.PublicKeyEd25519 -> IO (Either BadgeServiceErrorCode ())
requirePurchaseRecord BadgeServiceEnv {store} key =
  withServiceTransaction store (\db -> getPurchaseByKey db key) >>= \case
    Right (Just _) -> pure $ Right ()
    Right Nothing -> pure $ Left BSEUnknownPurchaseKey
    Left _ -> pure $ Left BSEInternal

-- | Dispatch on the command, once the signer\/record precondition already passed.
-- 'getBadgeInvoice', 'upgradeBadgeSubscription' and 'pauseBadge' are out of scope (decision 5
-- \/ §6) and always 'bad_request'; 'getBadgeCatalog' and 'issueBadge' are B6\/B7's commands and
-- answer 'internal' \"not implemented\" until those steps land.
dispatchCommand :: BadgeServiceEnv -> Maybe C.PublicKeyEd25519 -> BadgeServiceCommand -> IO BadgeServiceResponse
dispatchCommand _ _ (BSCGetBadgeInvoice {}) = pure $ errorResponse BSEBadRequest Nothing Nothing
dispatchCommand _ _ (BSCUpgradeBadgeSubscription {}) = pure $ errorResponse BSEBadRequest Nothing Nothing
dispatchCommand _ _ BSCPauseBadge = pure $ errorResponse BSEBadRequest Nothing Nothing
dispatchCommand _ _ BSCGetBadgeCatalog = pure notImplemented
dispatchCommand _ _ (BSCIssueBadge {}) = pure notImplemented
dispatchCommand bsEnv purchaseKey (BSCPurchaseBadge {payment}) = dispatchPurchase bsEnv purchaseKey payment

-- | 'checkSignerRecord' already requires a signature for every 'purchaseBadge', so
-- 'purchaseKey' is 'Just' here in every reachable case; the 'Nothing' clause only keeps this
-- function total.
--
-- Only 'SPCode' is implemented (B7); the others verify store evidence or transfer a receipt,
-- both out of scope (§6), so they are 'bad_request' permanently, not \"not implemented\".
--
-- The throttle (B5 decision 5) runs before 'SPCode' is processed: an empty per-signer or
-- global-failure bucket rejects the request with 'rate_limited' before it would otherwise
-- reach B7's (not yet implemented) code classifier. Neither bucket is debited here --
-- 'checkFailureBuckets' only peeks; only a classified failure debits, which is B7's job.
dispatchPurchase :: BadgeServiceEnv -> Maybe C.PublicKeyEd25519 -> ServicePayment -> IO BadgeServiceResponse
dispatchPurchase bsEnv (Just signerKey) (SPCode _code) =
  checkFailureBuckets bsEnv signerKey >>= \case
    Left retryAfter -> pure $ errorResponse BSERateLimited Nothing (Just retryAfter)
    Right () -> pure notImplemented
dispatchPurchase _ Nothing (SPCode _) = pure $ errorResponse BSEBadRequest Nothing Nothing
dispatchPurchase _ _ (SPApple {}) = pure $ errorResponse BSEBadRequest Nothing Nothing
dispatchPurchase _ _ (SPGoogle {}) = pure $ errorResponse BSEBadRequest Nothing Nothing
dispatchPurchase _ _ (SPInvoice {}) = pure $ errorResponse BSEBadRequest Nothing Nothing
dispatchPurchase _ _ (SPReceipt {}) = pure $ errorResponse BSEBadRequest Nothing Nothing

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module BadgeService.Service
  ( welcomeGetOpts,
    badgeService,
    badgeServiceCLI,
  )
where

import BadgeService.Options
import BadgeService.Store.Migrate (runBadgeServiceMigrations)
import Control.Concurrent.STM
import Control.Monad
import qualified Data.Aeson as J
import qualified Data.Aeson.KeyMap as KM
import Data.Text (Text)
import qualified Data.Text as T
import Simplex.Chat.Badges.Service (BadgeServiceErrorCode (..))
import Simplex.Chat.Bot (initializeBotAddress')
import Simplex.Chat.Controller
import Simplex.Chat.Core (sendChatCmd, simplexChatCore)
import Simplex.Chat.Options (printDbOpts)
import Simplex.Chat.Terminal (terminalChatConfig)
import Simplex.Chat.Terminal.Main (simplexChatCLI')
import Simplex.Chat.Types (User (..))
import Simplex.Messaging.Agent.Protocol (AgentInvId)
import Simplex.Messaging.Util (raceAny_)
import System.Directory (getAppUserDataDirectory)
import System.Exit (exitFailure)

data ServiceState = ServiceState
  { serviceCC :: TMVar ChatController,
    serviceRequestQ :: TQueue (User, AgentInvId, J.Object)
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

badgeService :: BadgeServiceOpts -> ChatConfig -> IO ()
badgeService opts cfg = do
  let chatHooks =
        defaultChatHooks
          { preStartHook = Just $ badgePreStartHook opts,
            postStartHook = Just $ badgePostStartHook opts
          }
  simplexChatCore cfg {chatHooks} (mkChatOpts opts) $ \_ cc ->
    forever $ do
      (_, event) <- atomically . readTBQueue $ outputQ cc
      case event of
        Right (CEvtServiceRequest u reqId _sigKey reqData) ->
          handleServiceRequest cc u reqId reqData
        _ -> pure ()

badgeServiceCLI :: BadgeServiceOpts -> IO ()
badgeServiceCLI opts = do
  env <- newServiceState
  let eventHook _cc ev = do
        case ev of
          Right (CEvtServiceRequest u reqId _sigKey reqData) ->
            atomically $ writeTQueue (serviceRequestQ env) (u, reqId, reqData)
          _ -> pure ()
        pure ev
      chatHooks =
        defaultChatHooks
          { preStartHook = Just $ badgePreStartHook opts,
            postStartHook = Just $ badgePostStartHookCLI opts env,
            eventHook = Just eventHook
          }
  raceAny_
    [ simplexChatCLI' terminalChatConfig {chatHooks} (mkChatOpts opts) Nothing,
      processQueuedRequests env
    ]

processQueuedRequests :: ServiceState -> IO ()
processQueuedRequests env = do
  cc <- atomically $ readTMVar $ serviceCC env
  forever $ do
    (u, reqId, reqData) <- atomically $ readTQueue $ serviceRequestQ env
    handleServiceRequest cc u reqId reqData

badgePreStartHook :: BadgeServiceOpts -> ChatController -> IO ()
badgePreStartHook opts ChatController {config, chatStore} =
  runBadgeServiceMigrations opts config chatStore

badgePostStartHook :: BadgeServiceOpts -> ChatController -> IO ()
badgePostStartHook BadgeServiceOpts {noAddress, testing} cc = do
  atomically $ writeTVar (processServiceRequests cc) True
  readTVarIO (currentUser cc) >>= \case
    Nothing -> putStrLn "No current user" >> exitFailure
    Just _ -> unless noAddress $ initializeBotAddress' (not testing) cc

badgePostStartHookCLI :: BadgeServiceOpts -> ServiceState -> ChatController -> IO ()
badgePostStartHookCLI opts env cc = do
  badgePostStartHook opts cc
  void $ atomically $ tryPutTMVar (serviceCC env) cc

handleServiceRequest :: ChatController -> User -> AgentInvId -> J.Object -> IO ()
handleServiceRequest cc User {userId} reqId _reqData =
  void $ sendChatCmd cc (APISendServiceResponse userId reqId $ errorResponse BSEUnsupportedVersion)

errorResponse :: BadgeServiceErrorCode -> J.Object
errorResponse errCode =
  KM.fromList
    [ ("type", J.String "error"),
      ("code", J.String $ errorCodeText errCode)
    ]

errorCodeText :: BadgeServiceErrorCode -> Text
errorCodeText = \case
  BSEBadRequest -> "bad_request"
  BSEUnsupportedVersion -> "unsupported_version"
  BSEUnknownPurchaseKey -> "unknown_purchase_key"
  BSEUnknownOfferId -> "unknown_offer_id"
  BSEOfferDisabled -> "offer_disabled"
  BSEOfferMismatch -> "offer_mismatch"
  BSEProductUnavailable -> "product_unavailable"
  BSEPaymentNotEntitled -> "payment_not_entitled"
  BSEPaymentPending -> "payment_pending"
  BSEProviderUnavailable -> "provider_unavailable"
  BSERateLimited -> "rate_limited"
  BSECodeInvalid -> "code_invalid"
  BSECodeUsed -> "code_used"
  BSECodeExpired -> "code_expired"
  BSEReceiptInvalid -> "receipt_invalid"
  BSEReceiptUsed -> "receipt_used"
  BSEInternal -> "internal"

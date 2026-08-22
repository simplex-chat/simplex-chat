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

import BadgeService.Catalog (seedCatalog)
import BadgeService.Config (BadgeServiceEnv, newBadgeServiceEnv, readBadgeServiceConfig)
import BadgeService.Options
import BadgeService.Store.Migrate (runBadgeServiceMigrations)
import Control.Concurrent.STM
import Control.Logger.Simple
import Control.Monad
import qualified Data.Aeson as J
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Text as T
import Simplex.Chat.Badges.Service (BadgeServiceErrorCode (..))
import Simplex.Chat.Bot (initializeBotAddress')
import Simplex.Chat.Controller
import Simplex.Chat.Core (sendChatCmd, simplexChatCore)
import Simplex.Chat.Options (printDbOpts)
import Simplex.Chat.Terminal (terminalChatConfig)
import Simplex.Chat.Terminal.Main (simplexChatCLI')
import Simplex.Chat.Types (AgentInvId (..), User (..))
import Simplex.Messaging.Encoding.String (strEncode)
import Simplex.Messaging.Util (raceAny_, safeDecodeUtf8, tshow)
import System.Directory (getAppUserDataDirectory)
import System.Exit (exitFailure)

data ServiceState = ServiceState
  { serviceCC :: TMVar ChatController,
    serviceEnv :: TMVar BadgeServiceEnv,
    serviceRequestQ :: TQueue (User, AgentInvId, J.Object)
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
  simplexChatCore cfg {chatHooks} (mkChatOpts opts) $ \_ cc ->
    forever $ do
      (_, event) <- atomically . readTBQueue $ outputQ cc
      case event of
        -- TODO enforce _sigKey == BadgeServiceRequest.purchaseKey (docs/protocol/badges-rpc.md).
        Right (CEvtServiceRequest u reqId _sigKey reqData) -> handleServiceRequest cc u reqId reqData
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
  forever $ do
    (u, reqId, reqData) <- atomically $ readTQueue $ serviceRequestQ env
    handleServiceRequest cc u reqId reqData

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

handleServiceRequest :: ChatController -> User -> AgentInvId -> J.Object -> IO ()
handleServiceRequest cc User {userId} reqId _reqData = do
  let reqIdT = safeDecodeUtf8 (strEncode reqId)
      respObj = KM.fromList [("type", J.String "error"), ("code", J.toJSON BSEUnsupportedVersion)]
  logInfo $ "badge service request " <> reqIdT
  sendChatCmd cc (APISendServiceResponse userId reqId respObj) >>= \case
    Right _ -> pure ()
    Left e -> logError $ "badge service response failed for " <> reqIdT <> ": " <> tshow e

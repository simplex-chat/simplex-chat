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
import Simplex.Chat.Store.Profiles (AddressSettings (..))
import Simplex.Chat.Terminal (terminalChatConfig)
import Simplex.Chat.Terminal.Main (simplexChatCLI')
import Simplex.Chat.Types (User (..))
import Simplex.Messaging.Agent.Protocol (AgentInvId)
import Simplex.Messaging.Encoding.String (strEncode)
import Simplex.Messaging.Util (raceAny_, safeDecodeUtf8, tshow)
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
          -- TODO [badge service] the handler must enforce `_sigKey == BadgeServiceRequest.purchaseKey`
          -- (docs/protocol/badges-rpc.md: "rejects a purchaseKey that differs from it with bad_request,
          -- and a key it holds no record of with unknown_purchase_key"). This is the identity guarantee
          -- of the whole protocol - do not drop this binding when the real handler lands.
          handleServiceRequest cc u reqId reqData
        _ -> pure ()

badgeServiceCLI :: BadgeServiceOpts -> IO ()
badgeServiceCLI opts = do
  env <- newServiceState
  let eventHook _cc ev = do
        case ev of
          Right (CEvtServiceRequest u reqId _sigKey reqData) ->
            -- Same _sigKey obligation as the non-CLI branch above.
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
  -- SREQ delivery depends on this flag being True (src/Simplex/Chat/Library/Subscriber.hs:1366-1372).
  -- Core hardcodes serviceRequests=False when starting the chat (src/Simplex/Chat/Core.hs:93);
  -- flipping the TVar here in postStartHook is the current mechanism to enable service requests
  -- for a service bot. Any SREQ arriving between agentSubscriber starting and this write is
  -- dropped via dropSReq. This race window is currently accepted.
  atomically $ writeTVar (processServiceRequests cc) True
  readTVarIO (currentUser cc) >>= \case
    Nothing -> putStrLn "No current user" >> exitFailure
    Just _ -> unless noAddress $ do
      -- Service RPC requires a double-ratchet address (simplexmq Agent.hs:1738 rejects non-DR
      -- with ASENotDRAddress), so pass `Just True` to CreateMyAddress when creating.
      initializeBotAddress' (not testing) (Just True) cc
      -- The badge service handles service requests only; it does not reply to contact requests
      -- (see loop above, which matches only CEvtServiceRequest). Disable autoAccept explicitly
      -- so a stray contact request is not silently accepted and then left in limbo forever.
      let noContactSettings =
            AddressSettings {businessAddress = False, autoAccept = Nothing, autoReply = Nothing}
      sendChatCmd cc (SetAddressSettings Nothing noContactSettings) >>= \case
        Right _ -> pure ()
        Left e -> logError $ "badge service: failed to disable autoAccept: " <> tshow e

badgePostStartHookCLI :: BadgeServiceOpts -> ServiceState -> ChatController -> IO ()
badgePostStartHookCLI opts env cc = do
  badgePostStartHook opts cc
  void $ atomically $ tryPutTMVar (serviceCC env) cc

handleServiceRequest :: ChatController -> User -> AgentInvId -> J.Object -> IO ()
handleServiceRequest cc User {userId} reqId _reqData = do
  let reqIdT = safeDecodeUtf8 (strEncode reqId)
  logInfo $ "badge service request " <> reqIdT
  sendChatCmd cc (APISendServiceResponse userId reqId $ errorResponse BSEUnsupportedVersion) >>= \case
    Right _ -> pure ()
    Left e -> logError $ "badge service response failed for " <> reqIdT <> ": " <> tshow e

errorResponse :: BadgeServiceErrorCode -> J.Object
errorResponse errCode =
  KM.fromList
    [ ("type", J.String "error"),
      ("code", J.toJSON errCode)
    ]

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
import Data.ByteString (ByteString)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (UTCTime, addUTCTime, getCurrentTime)
import Simplex.Chat.Badges.Service (BadgeServiceErrorCode (..))
import Simplex.Chat.Bot (initializeBotAddress')
import Simplex.Chat.Controller
import Simplex.Chat.Core (sendChatCmd, simplexChatCore)
import Simplex.Chat.Names.Protocol
import Simplex.Chat.Options (printDbOpts)
import Simplex.Chat.Terminal (terminalChatConfig)
import Simplex.Chat.Terminal.Main (simplexChatCLI')
import Simplex.Chat.Types (AgentInvId (..), User (..))
import Simplex.Messaging.Encoding.String (strEncode)
import Simplex.Messaging.Eth.Address (Address)
import Simplex.Messaging.Eth.Keccak (keccak256)
import Simplex.Messaging.Util (raceAny_, safeDecodeUtf8, tshow)
import System.Directory (getAppUserDataDirectory)
import System.Exit (exitFailure)

data ServiceState = ServiceState
  { serviceCC :: TMVar ChatController,
    serviceRequestQ :: TQueue (User, AgentInvId, J.Object),
    serviceNamesChain :: TVar NamesChain
  }

newServiceState :: IO ServiceState
newServiceState = do
  serviceCC <- newEmptyTMVarIO
  serviceRequestQ <- newTQueueIO
  serviceNamesChain <- newTVarIO emptyNamesChain
  pure ServiceState {serviceCC, serviceRequestQ, serviceNamesChain}

-- | In-memory mock of the name registry chain: committed hashes and registered
-- names. Stands in for a deployed SNRC; swap for a relayer to make it real.
data NamesChain = NamesChain
  { chainCommitments :: Set ByteString,
    chainNames :: Map Text NameEntry
  }

data NameEntry = NameEntry {neOwner :: Address, neSimplexLink :: Text, neExpiry :: UTCTime}

emptyNamesChain :: NamesChain
emptyNamesChain = NamesChain S.empty M.empty

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
          { preStartHook = Just $ badgePreStartHook opts,
            postStartHook = Just $ badgePostStartHook opts env
          }
  simplexChatCore cfg {chatHooks} (mkChatOpts opts) $ \_ cc ->
    forever $ do
      (_, event) <- atomically . readTBQueue $ outputQ cc
      case event of
        -- TODO enforce _sigKey == BadgeServiceRequest.purchaseKey (docs/protocol/badges-rpc.md).
        Right (CEvtServiceRequest u reqId _sigKey reqData) -> handleServiceRequest cc (serviceNamesChain env) u reqId reqData
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
    handleServiceRequest cc (serviceNamesChain env) u reqId reqData

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

handleServiceRequest :: ChatController -> TVar NamesChain -> User -> AgentInvId -> J.Object -> IO ()
handleServiceRequest cc chain User {userId} reqId reqData = do
  let reqIdT = safeDecodeUtf8 (strEncode reqId)
  logInfo $ "service request " <> reqIdT
  respObj <- case J.fromJSON (J.Object reqData) of
    J.Success req -> handleNamesRequest chain req
    -- Non-names request: badge dispatch is still a stub.
    J.Error _ -> pure $ KM.fromList [("type", J.String "error"), ("code", J.toJSON BSEUnsupportedVersion)]
  sendChatCmd cc (APISendServiceResponse userId reqId respObj) >>= \case
    Right _ -> pure ()
    Left e -> logError $ "service response failed for " <> reqIdT <> ": " <> tshow e

-- | Commit\/reveal against the chain mock. Commit stores the commitment and is
-- idempotent; reveal registers the name only if it was committed and is not
-- already registered — a second reveal of a live name fails with @name_taken@.
handleNamesRequest :: TVar NamesChain -> NamesRequest -> IO J.Object
handleNamesRequest chain NamesRequest {nrVersion, nrRequest}
  | nrVersion /= currentNamesVersion = pure $ respObj $ NRPError NECUnsupportedVersion Nothing Nothing
  | otherwise = case nrRequest of
      NRCommit {nrCommitment} -> do
        atomically $ modifyTVar' chain $ \c ->
          c {chainCommitments = S.insert (unCommitment nrCommitment) (chainCommitments c)}
        pure $ respObj $ NRPCommitted (mockTxHash "commit" $ unCommitment nrCommitment)
      NRReveal {nrName, nrOwner, nrSecret, nrTtl, nrLink} -> do
        now <- getCurrentTime
        let commitment = unCommitment (mkCommitment nrName nrOwner nrSecret nrTtl)
            expiry = addUTCTime (fromIntegral nrTtl) now
        atomically $ do
          c <- readTVar chain
          if not (S.member commitment (chainCommitments c))
            then pure $ respObj $ NRPError NECBadRequest (Just "no matching commitment") Nothing
            else case M.lookup nrName (chainNames c) of
              -- A registered name is taken, including by you: re-registering is not
              -- an update, and edits need signing (out of scope). Retry-idempotency
              -- cannot be inferred from matching fields — it needs a request key,
              -- which arrives with retries themselves.
              Just _ -> pure $ respObj $ NRPError NECNameTaken Nothing Nothing
              Nothing -> do
                writeTVar chain c {chainNames = M.insert nrName (NameEntry nrOwner nrLink expiry) (chainNames c)}
                pure $ respObj $ NRPRegistered nrName expiry (mockTxHash "reveal" commitment)
  where
    -- A NamesResponse always encodes to a JSON object.
    respObj r = case J.toJSON r of J.Object o -> o; _ -> KM.empty
    -- commit and reveal are distinct chain writes, so their hashes differ
    mockTxHash tag = TxHash . keccak256 . (tag <>)

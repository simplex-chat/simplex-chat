{-# LANGUAGE CPP #-}
{-# LANGUAGE MultiWayIf #-}
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
import qualified Data.ByteString as B
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (NominalDiffTime, UTCTime, addUTCTime, diffUTCTime, getCurrentTime)
import Data.Text.Encoding (encodeUtf8)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.Word (Word32, Word8)
import Simplex.Chat.Badges.Service (BadgeServiceErrorCode (..))
import Simplex.Chat.Bot (initializeBotAddress')
import Simplex.Chat.Controller
import Simplex.Chat.Core (sendChatCmd, simplexChatCore)
import qualified Simplex.Chat.Names.Codes as Codes
import Simplex.Chat.Names.Protocol
import Simplex.Chat.Names.Snrc (Intent (..), RecordKey (..), SnrcDeployment (..), intentDigest, parseRecordKey)
import Simplex.Chat.Wallet (parseEthSignature, recoverSigner)
import Simplex.Chat.Options (printDbOpts)
import Simplex.Chat.Terminal (terminalChatConfig)
import Simplex.Chat.Terminal.Main (simplexChatCLI')
import Simplex.Chat.Types (AgentInvId (..), User (..))
import Simplex.Messaging.Encoding.String (strEncode)
import Simplex.Messaging.Eth.Address (Address, mkAddress)
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
  { -- | commitment -> when it was published, so minimum commitment age is real
    chainCommitments :: Map ByteString UTCTime,
    chainNames :: Map Text NameEntry,
    -- | Spent redemption codes. A blind-signed code is its own nullifier: the
    -- issuer never saw it, so recording it stops reuse and reveals nothing.
    chainSpentCodes :: Set ByteString,
    -- | Answered requests, so a resent request is not executed twice.
    chainRequests :: Map ByteString NamesResponse,
    -- | Per-signer nonce, mirroring SimplexResolver: one counter per address,
    -- shared across every name it owns and consumed strictly in order.
    chainNonces :: Map Address Integer
  }

data NameEntry = NameEntry
  { neOwner :: Address,
    neContact :: [Text],
    neChannel :: [Text],
    neExpiry :: UTCTime,
    -- | Relayed edits left. Metering is off chain: the contracts count nothing,
    -- the relayer bounds what it is willing to pay for.
    neEditsLeft :: Word32
  }

editsPerName :: Word32
editsPerName = 10

-- | Production is 60s; the mock keeps it short enough to test but non-zero,
-- because a zero minimum is what makes reveal front-runnable.
minCommitmentAge :: NominalDiffTime
minCommitmentAge = 1

-- | Names shorter than this are refused, matching the controller's
-- @minCharLength@. A code may require more; the stricter of the two wins.
minNameLength :: Int
minNameLength = 6

reservedLabels :: Set Text
reservedLabels = S.fromList ["simplex", "support", "admin", "acme"]

emptyNamesChain :: NamesChain
emptyNamesChain = NamesChain M.empty M.empty S.empty M.empty M.empty

welcomeGetOpts :: IO BadgeServiceOpts
welcomeGetOpts = do
  appDir <- getAppUserDataDirectory "simplex"
  opts@BadgeServiceOpts {coreOptions, testing, serviceName} <- getBadgeServiceOpts appDir "simplex_badge_service"
  unless testing $ do
    putStrLn $ "SimpleX Badge Service v" ++ versionNumber
    printDbOpts coreOptions
    putStrLn $ "Service name: " ++ T.unpack serviceName
  pure opts

-- | Mint a handful of development codes and print them, so the whole purchase
-- flow is runnable locally without an issuer.
--
-- The key is fixed, so these are byte-identical on every run and tests can
-- hardcode them. They span the refusal paths rather than repeating one valid
-- code: ten valid codes test one case ten times.
printDevCodes :: IO ()
printDevCodes = do
#if defined(dev_codes)
  putStrLn ""
  putStrLn "  !! DEVELOPMENT redemption codes - this build trusts a published key !!"
  putStrLn ""
  forM_ [1 :: Int .. 4] $ \i -> do
    r <- Codes.signDevCode ("dev-" <> encodeUtf8 (tshow i))
    case r of
      Left e -> putStrLn $ "  code " <> show i <> ": FAILED " <> show e
      Right c -> putStrLn $ "  " <> T.unpack c
  putStrLn ""
  where
    _ = ()
#else
  pure ()
#endif

badgeService :: BadgeServiceOpts -> ChatConfig -> IO ()
badgeService opts cfg = do
  printDevCodes
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

-- | The deployment the mock stands in for. The client signs against the same
-- values, so the digests must match exactly.
mockDeployment :: SnrcDeployment
mockDeployment =
  SnrcDeployment
    { sdTld = "simplex",
      sdChainId = 1,
      sdRegistrar = mockAddr 1,
      sdResolver = mockAddr 2
    }

mockAddr :: Word8 -> Address
mockAddr n = either error id $ mkAddress (B.replicate 19 0 <> B.singleton n)

-- | Commit\/reveal against the chain mock. Commit stores the commitment and is
-- idempotent; reveal registers the name only if it was committed and is not
-- already registered — a second reveal of a live name fails with @name_taken@.
handleNamesRequest :: TVar NamesChain -> NamesRequest -> IO J.Object
handleNamesRequest chain NamesRequest {nrVersion, nrRequest}
  | nrVersion /= currentNamesVersion = pure $ respObj $ NRPError NECUnsupportedVersion Nothing Nothing
  | otherwise = do
      now <- getCurrentTime
      respObj <$> case nrRequest of
        NRCommit {nrCommitment} -> do
          atomically $ modifyTVar' chain $ \c ->
            c {chainCommitments = M.insertWith (\_ old' -> old') (unCommitment nrCommitment) now (chainCommitments c)}
          pure $ NRPCommitted (mockTxHash "commit" $ unCommitment nrCommitment)
        NRReveal {nrName, nrOwner, nrSecret, nrTtl, nrLink} ->
          atomically $ do
            let commitment = unCommitment (mkCommitment nrName nrOwner nrSecret nrTtl)
            c <- readTVar chain
            -- the commitment is what reveal is for, so it is checked first: a
            -- reveal with no aged commitment is the front-running case, and
            -- saying "too short" instead would hide it
            case M.lookup commitment (chainCommitments c) of
              Nothing -> pure $ NRPError NECBadRequest (Just "no matching commitment") Nothing
              Just at
                | diffUTCTime now at < minCommitmentAge ->
                    pure $ NRPError NECBadRequest (Just "commitment is too new") Nothing
                | Just e <- checkGates nrName -> pure e
                | otherwise -> register c now nrName nrOwner nrLink (addUTCTime (fromIntegral nrTtl) now) commitment
        NRQuote {nrLabel, nrYears} -> atomically $ do
          c <- readTVar chain
          let full = nrLabel <> ".simplex"
              live = M.lookup full (chainNames c)
          pure
            NRPQuote
              { nrLabel,
                nrAvailable = maybe (not (S.member nrLabel reservedLabels) && T.length nrLabel >= minNameLength) (const False) live,
                nrTakenUntil = neExpiry <$> live,
                nrReserved = S.member nrLabel reservedLabels,
                -- $10/yr for 6+ characters, the only rung reachable while the
                -- minimum length is 6
                nrPriceUsdCents = 1000 * nrYears,
                nrYears
              }
        NRBuy {nrRequestId, nrName, nrOwner, nrCode, nrLink} ->
          idempotent nrRequestId $ atomically $ do
            c <- readTVar chain
            let code = unRedemptionCode nrCode
                label = T.takeWhile (/= '.') nrName
            case Codes.verifyCode code of
              Left e -> pure $ NRPError NECPaymentRejected (Just (Codes.codeErrorText e)) Nothing
              Right vc
                -- keyed on the decoded nonce, never the code string: base64's
                -- last character is malleable, so the same code can be written
                -- more than one way
                | S.member (Codes.vcNonce vc) (chainSpentCodes c) -> pure $ NRPError NECCodeSpent Nothing Nothing
                | Codes.vcExpires vc < now -> pure $ NRPError NECCodeExpired Nothing Nothing
                | T.length label < Codes.vcMinLength vc ->
                    pure $ NRPError NECNameTooShort (Just $ "this code covers names of " <> tshow (Codes.vcMinLength vc) <> " letters or more") Nothing
                | otherwise -> case checkGates nrName of
                    Just e -> pure e
                    Nothing -> do
                      let expiry = addUTCTime (fromIntegral (Codes.vcYears vc) * 31536000) now
                      r <- register c now nrName nrOwner nrLink expiry (encodeUtf8 code)
                      case r of
                        NRPRegistered {} -> do
                          modifyTVar' chain $ \c' -> c' {chainSpentCodes = S.insert (Codes.vcNonce vc) (chainSpentCodes c')}
                          pure r
                        _ -> pure r
        NRResolve {nrName} -> atomically $ do
          c <- readTVar chain
          pure $ case M.lookup nrName (chainNames c) of
            Nothing -> NRPError NECNotFound Nothing Nothing
            Just NameEntry {neOwner, neContact, neChannel, neExpiry, neEditsLeft}
              | neExpiry < now -> NRPError NECNotFound (Just "registration expired") Nothing
              | otherwise -> NRPRecord nrName neOwner neContact neChannel neExpiry neEditsLeft
        NROwnedBy {nrAddress} -> atomically $ do
          c <- readTVar chain
          pure $ NRPNames [n | (n, e) <- M.toList (chainNames c), neOwner e == nrAddress, neExpiry e >= now]
        NRNonce {nrAddress} -> atomically $ do
          c <- readTVar chain
          pure $ NRPNonce (M.findWithDefault 0 nrAddress (chainNonces c))
        NRRelayIntent {nrRequestId, nrName, nrRecordKey, nrValue, nrNonce, nrDeadline, nrSig} ->
          idempotent nrRequestId $ atomically $ do
            c <- readTVar chain
            case (parseRecordKey nrRecordKey, M.lookup nrName (chainNames c)) of
              (Left e, _) -> pure $ NRPError NECBadRequest (Just (T.pack e)) Nothing
              (_, Nothing) -> pure $ NRPError NECNotFound Nothing Nothing
              (Right rk, Just entry)
                | neExpiry entry < now -> pure $ NRPError NECNotFound (Just "registration expired") Nothing
                | neEditsLeft entry == 0 -> pure $ NRPError NECNoEditCredits Nothing Nothing
                | nrDeadline < floor (utcTimeToPOSIXSeconds now) -> pure $ NRPError NECExpiredIntent Nothing Nothing
                | nrNonce /= M.findWithDefault 0 (neOwner entry) (chainNonces c) -> pure $ NRPError NECBadNonce Nothing Nothing
                | otherwise -> case recovered rk of
                    Left _ -> pure $ NRPError NECBadSignature Nothing Nothing
                    Right signer
                      | signer /= neOwner entry -> pure $ NRPError NECNotOwner Nothing Nothing
                      | otherwise -> do
                          let entry' = case rk of
                                RKContact -> entry {neContact = [nrValue], neEditsLeft = neEditsLeft entry - 1}
                                RKChannel -> entry {neChannel = [nrValue], neEditsLeft = neEditsLeft entry - 1}
                          modifyTVar' chain $ \c' ->
                            c'
                              { chainNames = M.insert nrName entry' (chainNames c'),
                                chainNonces = M.insert (neOwner entry) (nrNonce + 1) (chainNonces c')
                              }
                          pure $ NRPRelayed (mockTxHash "setText" (unIntentSig nrSig))
          where
            recovered rk = do
              digest <- intentDigest mockDeployment (SetTextRecord nrName rk nrValue nrNonce nrDeadline)
              sig <- parseEthSignature (unIntentSig nrSig)
              recoverSigner sig digest
  where
    -- A resent request must not execute twice: matching fields cannot tell a
    -- retry from a user doing the same thing again, which is why every mutating
    -- call carries an id.
    idempotent rid act = do
      prior <- atomically $ M.lookup (unRequestId rid) . chainRequests <$> readTVar chain
      case prior of
        Just r -> pure r
        Nothing -> do
          r <- act
          atomically $ modifyTVar' chain $ \c -> c {chainRequests = M.insert (unRequestId rid) r (chainRequests c)}
          pure r
    checkGates nm =
      let label = T.takeWhile (/= '.') nm
       in if
            | T.length label < minNameLength -> Just $ NRPError NECNameTooShort Nothing Nothing
            | S.member label reservedLabels -> Just $ NRPError NECNameReserved Nothing Nothing
            | otherwise -> Nothing
    -- A live registration is taken, including by its own owner: re-registering
    -- is not an edit. An expired one is available again.
    register c now' nm owner link expiry tag = case M.lookup nm (chainNames c) of
      Just e | neExpiry e >= now' -> pure $ NRPError NECNameTaken Nothing Nothing
      _ -> do
        modifyTVar' chain $ \c' ->
          c' {chainNames = M.insert nm (NameEntry owner [link] [] expiry editsPerName) (chainNames c')}
        pure $ NRPRegistered nm expiry (mockTxHash "register" tag)
    -- A NamesResponse always encodes to a JSON object.
    respObj r = case J.toJSON r of J.Object o -> o; _ -> KM.empty
    -- commit and reveal are distinct chain writes, so their hashes differ
    mockTxHash tag = TxHash . keccak256 . (tag <>)

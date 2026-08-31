{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module BadgeService.Service
  ( welcomeGetOpts,
    badgeService,
    badgeServiceCLI,
    -- Exposed so the names request handler can be tested as a function, with
    -- no connection, no forked service and no waiting on terminal output.
    handleNamesRequest,
    NamesChain (..),
    emptyNamesChain,
    devCodeTable,
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
import Data.Maybe (listToMaybe)
import Simplex.Chat.Names.Protocol
import Simplex.Chat.Names.Snrc (Intent (..), RecordKey (..), SnrcDeployment (..), devChainId, intentDigest, parseRecordKey)
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
  now <- getCurrentTime
  serviceNamesChain <- newTVarIO emptyNamesChain {chainCodes = devCodeTable now}
  pure ServiceState {serviceCC, serviceRequestQ, serviceNamesChain}

-- | In-memory mock of the name registry chain: committed hashes and registered
-- names. Stands in for a deployed SNRC; swap for a relayer to make it real.
data NamesChain = NamesChain
  { -- | commitment -> when it was published, so minimum commitment age is real
    chainCommitments :: Map ByteString UTCTime,
    chainNames :: Map Text NameEntry,
    -- | Redemption codes issued ahead of time, by code. Unguessable random
    -- values, so holding one is the entitlement — there is nothing to verify,
    -- only to look up.
    chainCodes :: Map Text CodeEntry,
    -- | Answered requests, so a resent request is not executed twice. Kept with
    -- the time they were answered: a request id is chosen by the caller, so a
    -- map that only grows is an unauthenticated party deciding how much memory
    -- this service allocates.
    chainRequests :: Map ByteString (UTCTime, NamesResponse),
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

-- | How long an answered request id is replayed, and how long a commitment
-- stays revealable. Both were unbounded; R14 states a retention for the
-- spent-code set and nothing stated one for these.
requestRetention, commitmentRetention :: NominalDiffTime
requestRetention = 3600
commitmentRetention = 86400

-- | The longest term the code-less reveal path will honour. Without it a raw
-- request can ask for a Word32 of seconds - about 136 years - for nothing, on a
-- path whose only constraint was a sentence in the RFC saying a real registrar
-- would not expose it.
maxNameTtl :: NameTtl
maxNameTtl = 10 * 31536000

-- | Production is 60s; the mock keeps it short enough to test but non-zero,
-- because a zero minimum is what makes reveal front-runnable.
minCommitmentAge :: NominalDiffTime
minCommitmentAge = 1

-- | Names shorter than this are refused, matching the controller's
-- @minCharLength@. A code may require more; the stricter of the two wins.
minNameLength :: Int
minNameLength = 6

-- | The other end, which was missing: a gate that carefully refuses five
-- characters accepted any number of them. 63 is the DNS label limit and what a
-- registry contract would most likely settle on.
maxNameLength :: Int
maxNameLength = 63

-- | What the contract accepts in a label. Mirrored here because the mock is
-- what every test and every local run registers against: a mock that is more
-- permissive than the chain makes the gates look enforced when they are not.
-- | The TLD this registry serves, as Text: 'sdTld' is the on-chain byte form.
mockTld :: Text
mockTld = safeDecodeUtf8 (sdTld mockDeployment)

reservedLabels :: Set Text
reservedLabels = S.fromList ["simplex", "support", "admin", "acme"]

data CodeEntry = CodeEntry
  { ceMinLength :: Int,
    ceYears :: Word32,
    ceExpires :: UTCTime,
    ceSpent :: Bool
  }

emptyNamesChain :: NamesChain
emptyNamesChain = NamesChain M.empty M.empty M.empty M.empty M.empty

welcomeGetOpts :: IO BadgeServiceOpts
welcomeGetOpts = do
  appDir <- getAppUserDataDirectory "simplex"
  opts@BadgeServiceOpts {coreOptions, testing, serviceName} <- getBadgeServiceOpts appDir "simplex_badge_service"
  unless testing $ do
    putStrLn $ "SimpleX Badge Service v" ++ versionNumber
    printDbOpts coreOptions
    putStrLn $ "Service name: " ++ T.unpack serviceName
  pure opts

-- | The pre-issued code table.
--
-- A code is simply an unguessable random value: holding one /is/ the
-- entitlement, so there is nothing to verify, only to look up. What a code is
-- worth — minimum name length, term, expiry — is a property of its row, not of
-- the code itself, so tiers and expiry dates can be changed by reissuing the
-- table rather than by shipping anything to clients.
--
-- Codes are fixed here so a local run always prints the same ones and tests can
-- hardcode them. A real deployment loads a table it issued out of band.
--
-- This is deliberately the simple scheme. It links a code to whoever it was
-- issued to, because the issuer holds the table — see the blind-signature work
-- for the unlinkable version.
devCodeTable :: UTCTime -> Map Text CodeEntry
devCodeTable now =
  M.fromList
    [ (c, CodeEntry {ceMinLength = 6, ceYears = 2, ceExpires = addUTCTime (365 * 86400) now, ceSpent = False})
      | c <- devCodes
    ]

devCodes :: [Text]
devCodes =
  [ "SMPX-4K2P-7TQW-9XRM",
    "SMPX-8H3N-2VBD-6JYK",
    "SMPX-5L9C-4WFT-1ZQA",
    "SMPX-7R6M-8PGX-3NHV"
  ]

printCodes :: IO ()
printCodes = do
  putStrLn ""
  putStrLn "  Pre-issued redemption codes (development table):"
  mapM_ (\c -> putStrLn $ "    " <> T.unpack c) devCodes
  putStrLn ""

badgeService :: BadgeServiceOpts -> ChatConfig -> IO ()
badgeService opts cfg = do
  printCodes
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
      -- Not 1: these are placeholder contracts, and a domain separator that
      -- says "mainnet" is one that a real deployment could be made to accept.
      sdChainId = devChainId,
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
            c
              { chainCommitments =
                  M.insertWith (\_ old' -> old') (unCommitment nrCommitment) now $
                    M.filter (\at -> diffUTCTime now at < commitmentRetention) (chainCommitments c)
              }
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
                | nrTtl > maxNameTtl ->
                    pure $ NRPError NECBadRequest (Just "term is longer than this registry allows") Nothing
                | Just e <- checkGates nrName -> pure e
                | otherwise -> register c now nrName nrOwner nrLink (addUTCTime (fromIntegral nrTtl) now) commitment
        -- A quote names no name: it carries the labelhash, so this answers
        -- only what the chain knows. Charset is unanswerable from a hash and
        -- stays with the client; length is asserted by the caller and re-checked
        -- against the plaintext at registration.
        NRQuote {nrLabelHash, nrLabelLen, nrYears} -> atomically $ do
          c <- readTVar chain
          let -- The mock holds the registry in a Map keyed by full name, so it
              -- scans. A real registrar asks the resolver for this labelhash.
              live = listToMaybe [e | (full, e) <- M.toList (chainNames c), mkLabelHash (T.takeWhile (/= '.') full) == nrLabelHash]
              reserved = any ((nrLabelHash ==) . mkLabelHash) (S.toList reservedLabels)
          pure
            NRPQuote
              { nrLabelHash,
                nrAvailable =
                  maybe
                    (not reserved && fromIntegral nrLabelLen >= minNameLength)
                    (const False)
                    live,
                nrTakenUntil = neExpiry <$> live,
                nrReserved = reserved,
                -- $10/yr for 6+ characters, the only rung reachable while the
                -- minimum length is 6
                nrPriceUsdCents = 1000 * nrYears,
                nrYears
              }
        NRBuy {nrRequestId, nrName, nrOwner, nrCode, nrLink} ->
          idempotent now nrRequestId $ do
            c <- readTVar chain
            let code = unRedemptionCode nrCode
                label = T.takeWhile (/= '.') nrName
            case M.lookup code (chainCodes c) of
              Nothing -> pure $ NRPError NECPaymentRejected (Just "no such code") Nothing
              Just e
                | ceSpent e -> pure $ NRPError NECCodeSpent Nothing Nothing
                | ceExpires e < now -> pure $ NRPError NECCodeExpired Nothing Nothing
                | T.length label < ceMinLength e ->
                    pure $ NRPError NECNameTooShort (Just $ "this code covers names of " <> tshow (ceMinLength e) <> " letters or more") Nothing
                | otherwise -> case checkGates nrName of
                    Just err -> pure err
                    Nothing -> do
                      let expiry = addUTCTime (fromIntegral (ceYears e) * 31536000) now
                      r <- register c now nrName nrOwner nrLink expiry (encodeUtf8 code)
                      case r of
                        NRPRegistered {} -> do
                          modifyTVar' chain $ \c' ->
                            c' {chainCodes = M.insert code e {ceSpent = True} (chainCodes c')}
                          pure r
                        _ -> pure r
        NRVerifyCode {nrCode} -> atomically $ do
          c <- readTVar chain
          pure $ case M.lookup (unRedemptionCode nrCode) (chainCodes c) of
            Nothing -> NRPError NECPaymentRejected (Just "no such code") Nothing
            Just e
              | ceSpent e -> NRPError NECCodeSpent Nothing Nothing
              | ceExpires e < now -> NRPError NECCodeExpired Nothing Nothing
              | otherwise -> NRPCode (fromIntegral (ceMinLength e)) (ceYears e) (ceExpires e)
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
          idempotent now nrRequestId $ do
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
                                RKContact -> entry {neContact = record, neEditsLeft = neEditsLeft entry - 1}
                                RKChannel -> entry {neChannel = record, neEditsLeft = neEditsLeft entry - 1}
                              -- an empty value clears the record rather than
                              -- storing [""], which reads as a target that is
                              -- set to nothing
                              record = filter (not . T.null) [nrValue]
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
    -- One transaction, so two identical requests cannot both run the action.
    -- A relayer that split this would pay twice for one request id.
    idempotent now rid act = atomically $ do
      c <- readTVar chain
      case M.lookup (unRequestId rid) (chainRequests c) of
        Just (_, r) -> pure r
        Nothing -> do
          r <- act
          -- Only settled answers are replayed. Caching a failure would make a
          -- retry with the same id permanently unable to succeed, which is the
          -- opposite of what an idempotency key is for. Answers older than the
          -- retry window are dropped as we go, so the map cannot be grown
          -- without bound by a caller choosing request ids.
          case r of
            NRPError {} -> pure ()
            _ ->
              modifyTVar' chain $ \c' ->
                c'
                  { chainRequests =
                      M.insert (unRequestId rid) (now, r) $
                        M.filter (\(at, _) -> diffUTCTime now at < requestRetention) (chainRequests c')
                  }
          pure r
    checkGates nm =
      let label = T.takeWhile (/= '.') nm
       in if
            -- The registry is per TLD, so a name outside it is not ours to
            -- register. Only the label was checked before, which left the
            -- suffix free for anything a raw request cared to send.
            | nm /= label <> "." <> mockTld ->
                Just $ NRPError NECNameInvalid (Just $ "names end in ." <> mockTld) Nothing
            -- The contract's charset. Without this the reserved set is bypassed
            -- by a capital letter - "Support" is not "support" to a Set, nor to
            -- a Map key - and two names that read alike can both exist.
            | not (validLabel label) ->
                Just $ NRPError NECNameInvalid (Just "names use lowercase letters, digits and hyphens, and cannot start or end with one") Nothing
            | T.length label < minNameLength -> Just $ NRPError NECNameTooShort Nothing Nothing
            | T.length label > maxNameLength ->
                Just $ NRPError NECNameInvalid (Just $ "names are at most " <> tshow maxNameLength <> " characters") Nothing
            | S.member label reservedLabels -> Just $ NRPError NECNameReserved Nothing Nothing
            | otherwise -> Nothing
    -- A live registration is taken, including by its own owner: re-registering
    -- is not an edit. An expired one is available again.
    register c now' nm owner link expiry tag = case M.lookup nm (chainNames c) of
      Just e | neExpiry e >= now' -> pure $ NRPError NECNameTaken Nothing Nothing
      _ -> do
        modifyTVar' chain $ \c' ->
          c' {chainNames = M.insert nm (NameEntry owner (filter (not . T.null) [link]) [] expiry editsPerName) (chainNames c')}
        pure $ NRPRegistered nm expiry (mockTxHash "register" tag)
    -- A NamesResponse always encodes to a JSON object.
    respObj r = case J.toJSON r of J.Object o -> o; _ -> KM.empty
    -- commit and reveal are distinct chain writes, so their hashes differ
    mockTxHash tag = TxHash . keccak256 . (tag <>)

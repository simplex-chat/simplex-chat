{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -fno-warn-ambiguous-fields #-}

module Simplex.Chat.Remote where

import Control.Applicative ((<|>))
import Control.Logger.Simple
import Control.Monad
import Control.Monad.Except
import Control.Monad.IO.Class
import Control.Monad.Reader
import Crypto.Random (getRandomBytes)
import qualified Data.Aeson as J
import qualified Data.Aeson.Types as JT
import Data.ByteString (ByteString)
import qualified Data.ByteString.Base64.URL as B64U
import Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Char8 as B
import Data.Functor (($>))
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import qualified Data.List.NonEmpty as L
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeLatin1, encodeUtf8)
import Data.Word (Word16, Word32)
import qualified Network.HTTP.Types as N
import Network.HTTP2.Server (responseStreaming)
import qualified Paths_simplex_chat as SC
import Simplex.Chat.Archive (archiveFilesFolder)
import Simplex.Chat.Controller
import Simplex.Chat.Files
import Simplex.Chat.Messages (chatNameStr)
import Simplex.Chat.Remote.AppVersion
import Simplex.Chat.Remote.Protocol
import Simplex.Chat.Remote.RevHTTP (attachHTTP2Server, attachRevHTTP2Client)
import Simplex.Chat.Remote.Transport
import Simplex.Chat.Remote.Types
import Simplex.Chat.Store.Files
import Simplex.Chat.Store.Remote
import Simplex.Chat.Store.Shared
import Simplex.Chat.Types
import Simplex.Chat.Util (encryptFile)
import Simplex.FileTransfer.Description (FileDigest (..))
import Simplex.Messaging.Agent
import Simplex.Messaging.Agent.Protocol (AgentErrorType (RCP))
import Simplex.Messaging.Crypto.File (CryptoFile (..), CryptoFileArgs (..))
import qualified Simplex.Messaging.Crypto.File as CF
import Simplex.Messaging.Encoding.String (StrEncoding (..))
import qualified Simplex.Messaging.TMap as TM
import Simplex.Messaging.Transport (TLS, closeConnection, tlsUniq)
import Simplex.Messaging.Transport.HTTP2.Client (HTTP2ClientError, closeHTTP2Client)
import Simplex.Messaging.Transport.HTTP2.Server (HTTP2Request (..))
import Simplex.Messaging.Util
import Simplex.RemoteControl.Client
import Simplex.RemoteControl.Invitation (RCInvitation (..), RCSignedInvitation (..), RCVerifiedInvitation (..), verifySignedInvitation)
import Simplex.RemoteControl.Types
import System.FilePath (takeFileName, (</>))
import UnliftIO
import UnliftIO.Concurrent (forkIO)
import UnliftIO.Directory (copyFile, createDirectoryIfMissing, doesDirectoryExist, removeDirectoryRecursive, renameFile)

-- when acting as host
minRemoteCtrlVersion :: AppVersion
minRemoteCtrlVersion = AppVersion [5, 4, 2, 0]

-- when acting as controller
minRemoteHostVersion :: AppVersion
minRemoteHostVersion = AppVersion [5, 4, 2, 0]

currentAppVersion :: AppVersion
currentAppVersion = AppVersion SC.version

ctrlAppVersionRange :: AppVersionRange
ctrlAppVersionRange = mkAppVersionRange minRemoteHostVersion currentAppVersion

hostAppVersionRange :: AppVersionRange
hostAppVersionRange = mkAppVersionRange minRemoteCtrlVersion currentAppVersion

networkIOTimeout :: Int
networkIOTimeout = 15000000

discoveryTimeout :: Int
discoveryTimeout = 60000000

-- * Desktop side

getRemoteHostClient :: ChatMonad m => RemoteHostId -> m RemoteHostClient
getRemoteHostClient rhId = do
  sessions <- asks remoteHostSessions
  liftIOEither . atomically $
    TM.lookup rhKey sessions >>= \case
      Just (_, RHSessionConnected {rhClient}) -> pure $ Right rhClient
      Just _ -> pure . Left $ ChatErrorRemoteHost rhKey RHEBadState
      Nothing -> pure . Left $ ChatErrorRemoteHost rhKey RHEMissing
  where
    rhKey = RHId rhId

withRemoteHostSession :: ChatMonad m => RHKey -> SessionSeq -> (RemoteHostSession -> Either ChatError (a, RemoteHostSession)) -> m a
withRemoteHostSession rhKey sseq f = do
  sessions <- asks remoteHostSessions
  r <-
    atomically $
      TM.lookup rhKey sessions >>= \case
        Nothing -> pure . Left $ ChatErrorRemoteHost rhKey RHEMissing
        Just (stateSeq, state)
          | stateSeq /= sseq -> pure . Left $ ChatErrorRemoteHost rhKey RHEBadState
          | otherwise -> case f state of
              Right (r, newState) -> Right r <$ TM.insert rhKey (sseq, newState) sessions
              Left ce -> pure $ Left ce
  liftEither r

-- | Transition session state with a 'RHNew' ID to an assigned 'RemoteHostId'
setNewRemoteHostId :: ChatMonad m => SessionSeq -> RemoteHostId -> m ()
setNewRemoteHostId sseq rhId = do
  sessions <- asks remoteHostSessions
  liftIOEither . atomically $ do
    TM.lookup RHNew sessions >>= \case
      Nothing -> err RHEMissing
      Just sess@(stateSeq, _)
        | stateSeq /= sseq -> err RHEBadState
        | otherwise -> do
            TM.delete RHNew sessions
            TM.insert (RHId rhId) sess sessions
            pure $ Right ()
  where
    err = pure . Left . ChatErrorRemoteHost RHNew

startRemoteHost :: ChatMonad m => Maybe (RemoteHostId, Bool) -> Maybe RCCtrlAddress -> Maybe Word16 -> m (NonEmpty RCCtrlAddress, Maybe RemoteHostInfo, RCSignedInvitation)
startRemoteHost rh_ rcAddrPrefs_ port_ = do
  (rhKey, multicast, remoteHost_, pairing) <- case rh_ of
    Just (rhId, multicast) -> do
      rh@RemoteHost {hostPairing} <- withStore $ \db -> getRemoteHost db rhId
      pure (RHId rhId, multicast, Just $ remoteHostInfo rh $ Just RHSStarting, hostPairing) -- get from the database, start multicast if requested
    Nothing -> withAgent $ \a -> (RHNew,False,Nothing,) <$> rcNewHostPairing a
  sseq <- startRemoteHostSession rhKey
  ctrlAppInfo <- mkCtrlAppInfo
  (localAddrs, invitation, rchClient, vars) <- handleConnectError rhKey sseq . withAgent $ \a -> rcConnectHost a pairing (J.toJSON ctrlAppInfo) multicast rcAddrPrefs_ port_
  let rcAddr_ = L.head localAddrs <$ rcAddrPrefs_
  cmdOk <- newEmptyTMVarIO
  rhsWaitSession <- async $ do
    rhKeyVar <- newTVarIO rhKey
    atomically $ takeTMVar cmdOk
    handleHostError sseq rhKeyVar $ waitForHostSession remoteHost_ rhKey sseq rcAddr_ rhKeyVar vars
  let rhs = RHPendingSession {rhKey, rchClient, rhsWaitSession, remoteHost_}
  withRemoteHostSession rhKey sseq $ \case
    RHSessionStarting ->
      let inv = decodeLatin1 $ strEncode invitation
       in Right ((), RHSessionConnecting inv rhs)
    _ -> Left $ ChatErrorRemoteHost rhKey RHEBadState
  (localAddrs, remoteHost_, invitation) <$ atomically (putTMVar cmdOk ())
  where
    mkCtrlAppInfo = do
      deviceName <- chatReadVar localDeviceName
      pure CtrlAppInfo {appVersionRange = ctrlAppVersionRange, deviceName}
    parseHostAppInfo :: RCHostHello -> ExceptT RemoteHostError IO HostAppInfo
    parseHostAppInfo RCHostHello {app = hostAppInfo} = do
      hostInfo@HostAppInfo {appVersion, encoding} <-
        liftEitherWith (RHEProtocolError . RPEInvalidJSON) $ JT.parseEither J.parseJSON hostAppInfo
      unless (isAppCompatible appVersion ctrlAppVersionRange) $ throwError $ RHEBadVersion appVersion
      when (encoding == PEKotlin && localEncoding == PESwift) $ throwError $ RHEProtocolError RPEIncompatibleEncoding
      pure hostInfo
    handleConnectError :: ChatMonad m => RHKey -> SessionSeq -> m a -> m a
    handleConnectError rhKey sessSeq action =
      action `catchChatError` \err -> do
        logError $ "startRemoteHost.rcConnectHost crashed: " <> tshow err
        cancelRemoteHostSession (Just (sessSeq, RHSRConnectionFailed err)) rhKey
        throwError err
    handleHostError :: ChatMonad m => SessionSeq -> TVar RHKey -> m () -> m ()
    handleHostError sessSeq rhKeyVar action =
      action `catchChatError` \err -> do
        logError $ "startRemoteHost.waitForHostSession crashed: " <> tshow err
        readTVarIO rhKeyVar >>= cancelRemoteHostSession (Just (sessSeq, RHSRCrashed err))
    waitForHostSession :: ChatMonad m => Maybe RemoteHostInfo -> RHKey -> SessionSeq -> Maybe RCCtrlAddress -> TVar RHKey -> RCStepTMVar (ByteString, TLS, RCStepTMVar (RCHostSession, RCHostHello, RCHostPairing)) -> m ()
    waitForHostSession remoteHost_ rhKey sseq rcAddr_ rhKeyVar vars = do
      (sessId, tls, vars') <- timeoutThrow (ChatErrorRemoteHost rhKey RHETimeout) 60000000 $ takeRCStep vars
      let sessionCode = verificationCode sessId
      withRemoteHostSession rhKey sseq $ \case
        RHSessionConnecting _inv rhs' -> Right ((), RHSessionPendingConfirmation sessionCode tls rhs')
        _ -> Left $ ChatErrorRemoteHost rhKey RHEBadState
      let rh_' = (\rh -> (rh :: RemoteHostInfo) {sessionState = Just RHSPendingConfirmation {sessionCode}}) <$> remoteHost_
      toView CRRemoteHostSessionCode {remoteHost_ = rh_', sessionCode}
      (RCHostSession {sessionKeys}, rhHello, pairing') <- timeoutThrow (ChatErrorRemoteHost rhKey RHETimeout) 60000000 $ takeRCStep vars'
      hostInfo@HostAppInfo {deviceName = hostDeviceName} <-
        liftError (ChatErrorRemoteHost rhKey) $ parseHostAppInfo rhHello
      withRemoteHostSession rhKey sseq $ \case
        RHSessionPendingConfirmation _ tls' rhs' -> Right ((), RHSessionConfirmed tls' rhs')
        _ -> Left $ ChatErrorRemoteHost rhKey RHEBadState
      rhi@RemoteHostInfo {remoteHostId, storePath} <- upsertRemoteHost pairing' rh_' rcAddr_ hostDeviceName sseq RHSConfirmed {sessionCode}
      let rhKey' = RHId remoteHostId -- rhKey may be invalid after upserting on RHNew
      when (rhKey' /= rhKey) $ do
        atomically $ writeTVar rhKeyVar rhKey'
        toView $ CRNewRemoteHost rhi
      -- set up HTTP transport and remote profile protocol
      disconnected <- toIO $ onDisconnected rhKey' sseq
      httpClient <- liftEitherError (httpError remoteHostId) $ attachRevHTTP2Client disconnected tls
      rhClient <- mkRemoteHostClient httpClient sessionKeys sessId storePath hostInfo
      pollAction <- async $ pollEvents remoteHostId rhClient
      withRemoteHostSession rhKey' sseq $ \case
        RHSessionConfirmed _ RHPendingSession {rchClient} -> Right ((), RHSessionConnected {rchClient, tls, rhClient, pollAction, storePath})
        _ -> Left $ ChatErrorRemoteHost rhKey RHEBadState
      chatWriteVar currentRemoteHost $ Just remoteHostId -- this is required for commands to be passed to remote host
      toView $ CRRemoteHostConnected rhi {sessionState = Just RHSConnected {sessionCode}}
    upsertRemoteHost :: ChatMonad m => RCHostPairing -> Maybe RemoteHostInfo -> Maybe RCCtrlAddress -> Text -> SessionSeq -> RemoteHostSessionState -> m RemoteHostInfo
    upsertRemoteHost pairing'@RCHostPairing {knownHost = kh_} rhi_ rcAddr_ hostDeviceName sseq state = do
      KnownHostPairing {hostDhPubKey = hostDhPubKey'} <- maybe (throwError . ChatError $ CEInternalError "KnownHost is known after verification") pure kh_
      case rhi_ of
        Nothing -> do
          storePath <- liftIO randomStorePath
          rh@RemoteHost {remoteHostId} <- withStore $ \db -> insertRemoteHost db hostDeviceName storePath rcAddr_ port_ pairing' >>= getRemoteHost db
          setNewRemoteHostId sseq remoteHostId
          pure $ remoteHostInfo rh $ Just state
        Just rhi@RemoteHostInfo {remoteHostId} -> do
          withStore' $ \db -> updateHostPairing db remoteHostId hostDeviceName hostDhPubKey' rcAddr_ port_
          pure (rhi :: RemoteHostInfo) {sessionState = Just state}
    onDisconnected :: ChatMonad m => RHKey -> SessionSeq -> m ()
    onDisconnected rhKey sseq = do
      logDebug $ "HTTP2 client disconnected: " <> tshow (rhKey, sseq)
      cancelRemoteHostSession (Just (sseq, RHSRDisconnected)) rhKey
    pollEvents :: ChatMonad m => RemoteHostId -> RemoteHostClient -> m ()
    pollEvents rhId rhClient = do
      oq <- asks outputQ
      forever $ do
        r_ <- liftRH rhId $ remoteRecv rhClient 10000000
        forM r_ $ \r -> atomically $ writeTBQueue oq (Nothing, Just rhId, r)
    httpError :: RemoteHostId -> HTTP2ClientError -> ChatError
    httpError rhId = ChatErrorRemoteHost (RHId rhId) . RHEProtocolError . RPEHTTP2 . tshow

startRemoteHostSession :: ChatMonad m => RHKey -> m SessionSeq
startRemoteHostSession rhKey = do
  sessions <- asks remoteHostSessions
  nextSessionSeq <- asks remoteSessionSeq
  liftIOEither . atomically $
    TM.lookup rhKey sessions >>= \case
      Just _ -> pure . Left $ ChatErrorRemoteHost rhKey RHEBusy
      Nothing -> do
        sessionSeq <- stateTVar nextSessionSeq $ \s -> (s, s + 1)
        Right sessionSeq <$ TM.insert rhKey (sessionSeq, RHSessionStarting) sessions

closeRemoteHost :: ChatMonad m => RHKey -> m ()
closeRemoteHost rhKey = do
  logNote $ "Closing remote host session for " <> tshow rhKey
  cancelRemoteHostSession Nothing rhKey

cancelRemoteHostSession :: ChatMonad m => Maybe (SessionSeq, RemoteHostStopReason) -> RHKey -> m ()
cancelRemoteHostSession handlerInfo_ rhKey = do
  sessions <- asks remoteHostSessions
  crh <- asks currentRemoteHost
  deregistered <-
    atomically $
      TM.lookup rhKey sessions >>= \case
        Nothing -> pure Nothing
        Just (sessSeq, _) | maybe False ((sessSeq /=) . fst) handlerInfo_ -> pure Nothing -- ignore cancel from a ghost session handler
        Just (_, rhs) -> do
          TM.delete rhKey sessions
          modifyTVar' crh $ \cur -> if (RHId <$> cur) == Just rhKey then Nothing else cur -- only wipe the closing RH
          pure $ Just rhs
  forM_ deregistered $ \session -> do
    liftIO $ cancelRemoteHost handlingError session `catchAny` (logError . tshow)
    forM_ (snd <$> handlerInfo_) $ \rhStopReason ->
      toView CRRemoteHostStopped {remoteHostId_, rhsState = rhsSessionState session, rhStopReason}
  where
    handlingError = isJust handlerInfo_
    remoteHostId_ = case rhKey of
      RHNew -> Nothing
      RHId rhId -> Just rhId

cancelRemoteHost :: Bool -> RemoteHostSession -> IO ()
cancelRemoteHost handlingError = \case
  RHSessionStarting -> pure ()
  RHSessionConnecting _inv rhs -> cancelPendingSession rhs
  RHSessionPendingConfirmation _sessCode tls rhs -> do
    cancelPendingSession rhs
    closeConnection tls
  RHSessionConfirmed tls rhs -> do
    cancelPendingSession rhs
    closeConnection tls
  RHSessionConnected {rchClient, tls, rhClient = RemoteHostClient {httpClient}, pollAction} -> do
    uninterruptibleCancel pollAction
    cancelHostClient rchClient `catchAny` (logError . tshow)
    closeConnection tls `catchAny` (logError . tshow)
    unless handlingError $ closeHTTP2Client httpClient `catchAny` (logError . tshow)
  where
    cancelPendingSession RHPendingSession {rchClient, rhsWaitSession} = do
      unless handlingError $ uninterruptibleCancel rhsWaitSession `catchAny` (logError . tshow)
      cancelHostClient rchClient `catchAny` (logError . tshow)

-- | Generate a random 16-char filepath without / in it by using base64url encoding.
randomStorePath :: IO FilePath
randomStorePath = B.unpack . B64U.encode <$> getRandomBytes 12

listRemoteHosts :: ChatMonad m => m [RemoteHostInfo]
listRemoteHosts = do
  sessions <- chatReadVar remoteHostSessions
  map (rhInfo sessions) <$> withStore' getRemoteHosts
  where
    rhInfo sessions rh@RemoteHost {remoteHostId} =
      remoteHostInfo rh $ rhsSessionState . snd <$> M.lookup (RHId remoteHostId) sessions

switchRemoteHost :: ChatMonad m => Maybe RemoteHostId -> m (Maybe RemoteHostInfo)
switchRemoteHost rhId_ = do
  rhi_ <- forM rhId_ $ \rhId -> do
    let rhKey = RHId rhId
    rh <- withStore (`getRemoteHost` rhId)
    sessions <- chatReadVar remoteHostSessions
    case M.lookup rhKey sessions of
      Just (_, RHSessionConnected {tls}) -> pure $ remoteHostInfo rh $ Just RHSConnected {sessionCode = tlsSessionCode tls}
      _ -> throwError $ ChatErrorRemoteHost rhKey RHEInactive
  rhi_ <$ chatWriteVar currentRemoteHost rhId_

remoteHostInfo :: RemoteHost -> Maybe RemoteHostSessionState -> RemoteHostInfo
remoteHostInfo RemoteHost {remoteHostId, storePath, hostDeviceName, bindAddress_, bindPort_} sessionState =
  RemoteHostInfo {remoteHostId, storePath, hostDeviceName, bindAddress_, bindPort_, sessionState}

deleteRemoteHost :: ChatMonad m => RemoteHostId -> m ()
deleteRemoteHost rhId = do
  RemoteHost {storePath} <- withStore (`getRemoteHost` rhId)
  chatReadVar remoteHostsFolder >>= \case
    Just baseDir -> do
      let hostStore = baseDir </> storePath
      logInfo $ "removing host store at " <> tshow hostStore
      whenM (doesDirectoryExist hostStore) $ removeDirectoryRecursive hostStore
    Nothing -> logWarn "Local file store not available while deleting remote host"
  withStore' (`deleteRemoteHostRecord` rhId)

storeRemoteFile :: forall m. ChatMonad m => RemoteHostId -> Maybe Bool -> FilePath -> m CryptoFile
storeRemoteFile rhId encrypted_ localPath = do
  c@RemoteHostClient {encryptHostFiles, storePath} <- getRemoteHostClient rhId
  let encrypt = fromMaybe encryptHostFiles encrypted_
  cf@CryptoFile {filePath} <- if encrypt then encryptLocalFile else pure $ CF.plain localPath
  filePath' <- liftRH rhId $ remoteStoreFile c filePath (takeFileName localPath)
  hf_ <- chatReadVar remoteHostsFolder
  forM_ hf_ $ \hf -> do
    let rhf = hf </> storePath </> archiveFilesFolder
        hPath = rhf </> takeFileName filePath'
    createDirectoryIfMissing True rhf
    (if encrypt then renameFile else copyFile) filePath hPath
  pure (cf :: CryptoFile) {filePath = filePath'}
  where
    encryptLocalFile :: m CryptoFile
    encryptLocalFile = do
      tmpDir <- getChatTempDirectory
      createDirectoryIfMissing True tmpDir
      tmpFile <- tmpDir `uniqueCombine` takeFileName localPath
      cfArgs <- atomically . CF.randomArgs =<< asks random
      liftError (ChatError . CEFileWrite tmpFile) $ encryptFile localPath tmpFile cfArgs
      pure $ CryptoFile tmpFile $ Just cfArgs

getRemoteFile :: ChatMonad m => RemoteHostId -> RemoteFile -> m ()
getRemoteFile rhId rf = do
  c@RemoteHostClient {storePath} <- getRemoteHostClient rhId
  dir <- (</> storePath </> archiveFilesFolder) <$> (maybe getDefaultFilesFolder pure =<< chatReadVar remoteHostsFolder)
  createDirectoryIfMissing True dir
  liftRH rhId $ remoteGetFile c dir rf

processRemoteCommand :: ChatMonad m => RemoteHostId -> RemoteHostClient -> ChatCommand -> ByteString -> m ChatResponse
processRemoteCommand remoteHostId c cmd s = case cmd of
  SendFile chatName f -> sendFile "/f" chatName f
  SendImage chatName f -> sendFile "/img" chatName f
  _ -> liftRH remoteHostId $ remoteSend c s
  where
    sendFile cmdName chatName (CryptoFile path cfArgs) = do
      -- don't encrypt in host if already encrypted locally
      CryptoFile path' cfArgs' <- storeRemoteFile remoteHostId (cfArgs $> False) path
      let f = CryptoFile path' (cfArgs <|> cfArgs') -- use local or host encryption
      liftRH remoteHostId $ remoteSend c $ B.unwords [cmdName, B.pack (chatNameStr chatName), cryptoFileStr f]
    cryptoFileStr CryptoFile {filePath, cryptoArgs} =
      maybe "" (\(CFArgs key nonce) -> "key=" <> strEncode key <> " nonce=" <> strEncode nonce <> " ") cryptoArgs
        <> encodeUtf8 (T.pack filePath)

liftRH :: ChatMonad m => RemoteHostId -> ExceptT RemoteProtocolError IO a -> m a
liftRH rhId = liftError (ChatErrorRemoteHost (RHId rhId) . RHEProtocolError)

-- * Mobile side

-- ** QR/link

-- | Use provided OOB link as an annouce
connectRemoteCtrlURI :: ChatMonad m => RCSignedInvitation -> m (Maybe RemoteCtrlInfo, CtrlAppInfo)
connectRemoteCtrlURI signedInv = do
  verifiedInv <- maybe (throwError $ ChatErrorRemoteCtrl RCEBadInvitation) pure $ verifySignedInvitation signedInv
  sseq <- startRemoteCtrlSession
  connectRemoteCtrl verifiedInv sseq

-- ** Multicast

findKnownRemoteCtrl :: ChatMonad m => m ()
findKnownRemoteCtrl = do
  knownCtrls <- withStore' getRemoteCtrls
  pairings <- case nonEmpty knownCtrls of
    Nothing -> throwError $ ChatErrorRemoteCtrl RCENoKnownControllers
    Just ne -> pure $ fmap (\RemoteCtrl {ctrlPairing} -> ctrlPairing) ne
  sseq <- startRemoteCtrlSession
  foundCtrl <- newEmptyTMVarIO
  cmdOk <- newEmptyTMVarIO
  action <- async $ handleCtrlError sseq RCSRDiscoveryFailed "findKnownRemoteCtrl.discover" $ do
    atomically $ takeTMVar cmdOk
    (RCCtrlPairing {ctrlFingerprint}, inv@(RCVerifiedInvitation RCInvitation {app})) <-
      timeoutThrow (ChatErrorRemoteCtrl RCETimeout) discoveryTimeout . withAgent $ \a -> rcDiscoverCtrl a pairings
    ctrlAppInfo_ <- (Just <$> parseCtrlAppInfo app) `catchChatError` const (pure Nothing)
    rc <-
      withStore' (`getRemoteCtrlByFingerprint` ctrlFingerprint) >>= \case
        Nothing -> throwChatError $ CEInternalError "connecting with a stored ctrl"
        Just rc -> pure rc
    atomically $ putTMVar foundCtrl (rc, inv)
    let compatible = isJust $ compatibleAppVersion hostAppVersionRange . appVersionRange =<< ctrlAppInfo_
    toView CRRemoteCtrlFound {remoteCtrl = remoteCtrlInfo rc (Just RCSSearching), ctrlAppInfo_, appVersion = currentAppVersion, compatible}
  updateRemoteCtrlSession sseq $ \case
    RCSessionStarting -> Right RCSessionSearching {action, foundCtrl}
    _ -> Left $ ChatErrorRemoteCtrl RCEBadState
  atomically $ putTMVar cmdOk ()

confirmRemoteCtrl :: ChatMonad m => RemoteCtrlId -> m (RemoteCtrlInfo, CtrlAppInfo)
confirmRemoteCtrl rcId = do
  session <- asks remoteCtrlSession
  (sseq, listener, found) <- liftIOEither $ atomically $ do
    readTVar session >>= \case
      Just (sseq, RCSessionSearching {action, foundCtrl}) -> do
        writeTVar session $ Just (sseq, RCSessionStarting) -- drop intermediate "Searching" state so connectRemoteCtrl can proceed
        pure $ Right (sseq, action, foundCtrl)
      _ -> pure . Left $ ChatErrorRemoteCtrl RCEBadState
  uninterruptibleCancel listener
  (RemoteCtrl {remoteCtrlId = foundRcId}, verifiedInv) <- atomically $ takeTMVar found
  unless (rcId == foundRcId) $ throwError $ ChatErrorRemoteCtrl RCEBadController
  connectRemoteCtrl verifiedInv sseq >>= \case
    (Nothing, _) -> throwChatError $ CEInternalError "connecting with a stored ctrl"
    (Just rci, appInfo) -> pure (rci, appInfo)

-- ** Common

startRemoteCtrlSession :: ChatMonad m => m SessionSeq
startRemoteCtrlSession = do
  session <- asks remoteCtrlSession
  nextSessionSeq <- asks remoteSessionSeq
  liftIOEither . atomically $
    readTVar session >>= \case
      Just _ -> pure . Left $ ChatErrorRemoteCtrl RCEBusy
      Nothing -> do
        sseq <- stateTVar nextSessionSeq $ \s -> (s, s + 1)
        Right sseq <$ writeTVar session (Just (sseq, RCSessionStarting))

connectRemoteCtrl :: ChatMonad m => RCVerifiedInvitation -> SessionSeq -> m (Maybe RemoteCtrlInfo, CtrlAppInfo)
connectRemoteCtrl verifiedInv@(RCVerifiedInvitation inv@RCInvitation {ca, app}) sseq = handleCtrlError sseq RCSRConnectionFailed "connectRemoteCtrl" $ do
  ctrlInfo@CtrlAppInfo {deviceName = ctrlDeviceName} <- parseCtrlAppInfo app
  v <- checkAppVersion ctrlInfo
  rc_ <- withStore' $ \db -> getRemoteCtrlByFingerprint db ca
  mapM_ (validateRemoteCtrl inv) rc_
  hostAppInfo <- getHostAppInfo v
  (rcsClient, vars) <- timeoutThrow (ChatErrorRemoteCtrl RCETimeout) networkIOTimeout . withAgent $ \a ->
    rcConnectCtrl a verifiedInv (ctrlPairing <$> rc_) (J.toJSON hostAppInfo)
  cmdOk <- newEmptyTMVarIO
  rcsWaitSession <- async $ do
    atomically $ takeTMVar cmdOk
    handleCtrlError sseq RCSRConnectionFailed "waitForCtrlSession" $ waitForCtrlSession rc_ ctrlDeviceName rcsClient vars
  updateRemoteCtrlSession sseq $ \case
    RCSessionStarting -> Right RCSessionConnecting {remoteCtrlId_ = remoteCtrlId' <$> rc_, rcsClient, rcsWaitSession}
    _ -> Left $ ChatErrorRemoteCtrl RCEBadState
  atomically $ putTMVar cmdOk ()
  pure ((`remoteCtrlInfo` Just RCSConnecting) <$> rc_, ctrlInfo)
  where
    validateRemoteCtrl RCInvitation {idkey} RemoteCtrl {ctrlPairing = RCCtrlPairing {idPubKey}} =
      unless (idkey == idPubKey) $ throwError $ ChatErrorRemoteCtrl $ RCEProtocolError $ PRERemoteControl RCEIdentity
    waitForCtrlSession :: ChatMonad m => Maybe RemoteCtrl -> Text -> RCCtrlClient -> RCStepTMVar (ByteString, TLS, RCStepTMVar (RCCtrlSession, RCCtrlPairing)) -> m ()
    waitForCtrlSession rc_ ctrlName rcsClient vars = do
      (uniq, tls, rcsWaitConfirmation) <- timeoutThrow (ChatErrorRemoteCtrl RCETimeout) networkIOTimeout $ takeRCStep vars
      let sessionCode = verificationCode uniq
      updateRemoteCtrlSession sseq $ \case
        RCSessionConnecting {rcsWaitSession} ->
          let remoteCtrlId_ = remoteCtrlId' <$> rc_
           in Right RCSessionPendingConfirmation {remoteCtrlId_, ctrlDeviceName = ctrlName, rcsClient, tls, sessionCode, rcsWaitSession, rcsWaitConfirmation}
        _ -> Left $ ChatErrorRemoteCtrl RCEBadState
      toView CRRemoteCtrlSessionCode {remoteCtrl_ = (`remoteCtrlInfo` Just RCSPendingConfirmation {sessionCode}) <$> rc_, sessionCode}
    checkAppVersion CtrlAppInfo {appVersionRange} =
      case compatibleAppVersion hostAppVersionRange appVersionRange of
        Just (AppCompatible v) -> pure v
        Nothing -> throwError $ ChatErrorRemoteCtrl $ RCEBadVersion $ maxVersion appVersionRange
    getHostAppInfo appVersion = do
      hostDeviceName <- chatReadVar localDeviceName
      encryptFiles <- chatReadVar encryptLocalFiles
      pure HostAppInfo {appVersion, deviceName = hostDeviceName, encoding = localEncoding, encryptFiles}

parseCtrlAppInfo :: ChatMonad m => JT.Value -> m CtrlAppInfo
parseCtrlAppInfo ctrlAppInfo = do
  liftEitherWith (const $ ChatErrorRemoteCtrl RCEBadInvitation) $ JT.parseEither J.parseJSON ctrlAppInfo

handleRemoteCommand :: forall m. ChatMonad m => (ByteString -> m ChatResponse) -> RemoteCrypto -> TBQueue ChatResponse -> HTTP2Request -> m ()
handleRemoteCommand execChatCommand encryption remoteOutputQ HTTP2Request {request, reqBody, sendResponse} = do
  logDebug "handleRemoteCommand"
  liftRC (tryRemoteError parseRequest) >>= \case
    Right (getNext, rc) -> do
      chatReadVar currentUser >>= \case
        Nothing -> replyError $ ChatError CENoActiveUser
        Just user -> processCommand user getNext rc `catchChatError` replyError
    Left e -> reply $ RRProtocolError e
  where
    parseRequest :: ExceptT RemoteProtocolError IO (GetChunk, RemoteCommand)
    parseRequest = do
      (header, getNext) <- parseDecryptHTTP2Body encryption request reqBody
      (getNext,) <$> liftEitherWith RPEInvalidJSON (J.eitherDecode header)
    replyError = reply . RRChatResponse . CRChatCmdError Nothing
    processCommand :: User -> GetChunk -> RemoteCommand -> m ()
    processCommand user getNext = \case
      RCSend {command} -> handleSend execChatCommand command >>= reply
      RCRecv {wait = time} -> handleRecv time remoteOutputQ >>= reply
      RCStoreFile {fileName, fileSize, fileDigest} -> handleStoreFile encryption fileName fileSize fileDigest getNext >>= reply
      RCGetFile {file} -> handleGetFile encryption user file replyWith
    reply :: RemoteResponse -> m ()
    reply = (`replyWith` \_ -> pure ())
    replyWith :: Respond m
    replyWith rr attach = do
      resp <- liftRC $ encryptEncodeHTTP2Body encryption $ J.encode rr
      liftIO . sendResponse . responseStreaming N.status200 [] $ \send flush -> do
        send resp
        attach send
        flush

takeRCStep :: ChatMonad m => RCStepTMVar a -> m a
takeRCStep = liftEitherError (\e -> ChatErrorAgent {agentError = RCP e, connectionEntity_ = Nothing}) . atomically . takeTMVar

type GetChunk = Int -> IO ByteString

type SendChunk = Builder -> IO ()

type Respond m = RemoteResponse -> (SendChunk -> IO ()) -> m ()

liftRC :: ChatMonad m => ExceptT RemoteProtocolError IO a -> m a
liftRC = liftError (ChatErrorRemoteCtrl . RCEProtocolError)

tryRemoteError :: ExceptT RemoteProtocolError IO a -> ExceptT RemoteProtocolError IO (Either RemoteProtocolError a)
tryRemoteError = tryAllErrors (RPEException . tshow)
{-# INLINE tryRemoteError #-}

handleSend :: ChatMonad m => (ByteString -> m ChatResponse) -> Text -> m RemoteResponse
handleSend execChatCommand command = do
  logDebug $ "Send: " <> tshow command
  -- execChatCommand checks for remote-allowed commands
  -- convert errors thrown in ChatMonad into error responses to prevent aborting the protocol wrapper
  RRChatResponse <$> execChatCommand (encodeUtf8 command) `catchError` (pure . CRChatError Nothing)

handleRecv :: MonadUnliftIO m => Int -> TBQueue ChatResponse -> m RemoteResponse
handleRecv time events = do
  logDebug $ "Recv: " <> tshow time
  RRChatEvent <$> (timeout time . atomically $ readTBQueue events)

-- TODO this command could remember stored files and return IDs to allow removing files that are not needed.
-- Also, there should be some process removing unused files uploaded to remote host (possibly, all unused files).
handleStoreFile :: forall m. ChatMonad m => RemoteCrypto -> FilePath -> Word32 -> FileDigest -> GetChunk -> m RemoteResponse
handleStoreFile encryption fileName fileSize fileDigest getChunk =
  either RRProtocolError RRFileStored <$> (chatReadVar filesFolder >>= storeFile)
  where
    storeFile :: Maybe FilePath -> m (Either RemoteProtocolError FilePath)
    storeFile = \case
      Just ff -> takeFileName <$$> storeFileTo ff
      Nothing -> storeFileTo =<< getDefaultFilesFolder
    storeFileTo :: FilePath -> m (Either RemoteProtocolError FilePath)
    storeFileTo dir = liftRC . tryRemoteError $ do
      filePath <- dir `uniqueCombine` fileName
      receiveEncryptedFile encryption getChunk fileSize fileDigest filePath
      pure filePath

handleGetFile :: ChatMonad m => RemoteCrypto -> User -> RemoteFile -> Respond m -> m ()
handleGetFile encryption User {userId} RemoteFile {userId = commandUserId, fileId, sent, fileSource = cf'@CryptoFile {filePath}} reply = do
  logDebug $ "GetFile: " <> tshow filePath
  unless (userId == commandUserId) $ throwChatError $ CEDifferentActiveUser {commandUserId, activeUserId = userId}
  path <- maybe filePath (</> filePath) <$> chatReadVar filesFolder
  withStore $ \db -> do
    cf <- getLocalCryptoFile db commandUserId fileId sent
    unless (cf == cf') $ throwError $ SEFileNotFound fileId
  liftRC (tryRemoteError $ getFileInfo path) >>= \case
    Left e -> reply (RRProtocolError e) $ \_ -> pure ()
    Right (fileSize, fileDigest) ->
      withFile path ReadMode $ \h -> do
        encFile <- liftRC $ prepareEncryptedFile encryption (h, fileSize)
        reply RRFile {fileSize, fileDigest} $ sendEncryptedFile encFile

listRemoteCtrls :: ChatMonad m => m [RemoteCtrlInfo]
listRemoteCtrls = do
  session <- snd <$$> chatReadVar remoteCtrlSession
  let rcId = sessionRcId =<< session
      sessState = rcsSessionState <$> session
  map (rcInfo rcId sessState) <$> withStore' getRemoteCtrls
  where
    rcInfo :: Maybe RemoteCtrlId -> Maybe RemoteCtrlSessionState -> RemoteCtrl -> RemoteCtrlInfo
    rcInfo rcId sessState rc@RemoteCtrl {remoteCtrlId} =
      remoteCtrlInfo rc $ if rcId == Just remoteCtrlId then sessState else Nothing
    sessionRcId = \case
      RCSessionConnecting {remoteCtrlId_} -> remoteCtrlId_
      RCSessionPendingConfirmation {remoteCtrlId_} -> remoteCtrlId_
      RCSessionConnected {remoteCtrlId} -> Just remoteCtrlId
      _ -> Nothing

remoteCtrlInfo :: RemoteCtrl -> Maybe RemoteCtrlSessionState -> RemoteCtrlInfo
remoteCtrlInfo RemoteCtrl {remoteCtrlId, ctrlDeviceName} sessionState =
  RemoteCtrlInfo {remoteCtrlId, ctrlDeviceName, sessionState}

-- | Take a look at emoji of tlsunique, commit pairing, and start session server
verifyRemoteCtrlSession :: ChatMonad m => (ByteString -> m ChatResponse) -> Text -> m RemoteCtrlInfo
verifyRemoteCtrlSession execChatCommand sessCode' = do
  (sseq, client, ctrlName, sessionCode, vars) <-
    chatReadVar remoteCtrlSession >>= \case
      Nothing -> throwError $ ChatErrorRemoteCtrl RCEInactive
      Just (sseq, RCSessionPendingConfirmation {rcsClient, ctrlDeviceName = ctrlName, sessionCode, rcsWaitConfirmation}) -> pure (sseq, rcsClient, ctrlName, sessionCode, rcsWaitConfirmation)
      _ -> throwError $ ChatErrorRemoteCtrl RCEBadState
  handleCtrlError sseq RCSRSetupFailed "verifyRemoteCtrlSession" $ do
    let verified = sameVerificationCode sessCode' sessionCode
    timeoutThrow (ChatErrorRemoteCtrl RCETimeout) networkIOTimeout . liftIO $ confirmCtrlSession client verified -- signal verification result before crashing
    unless verified $ throwError $ ChatErrorRemoteCtrl $ RCEProtocolError PRESessionCode
    (rcsSession@RCCtrlSession {tls, sessionKeys}, rcCtrlPairing) <- timeoutThrow (ChatErrorRemoteCtrl RCETimeout) networkIOTimeout $ takeRCStep vars
    rc@RemoteCtrl {remoteCtrlId} <- upsertRemoteCtrl ctrlName rcCtrlPairing
    remoteOutputQ <- asks (tbqSize . config) >>= newTBQueueIO
    encryption <- mkCtrlRemoteCrypto sessionKeys $ tlsUniq tls
    http2Server <- async $ attachHTTP2Server tls $ handleRemoteCommand execChatCommand encryption remoteOutputQ
    void . forkIO $ monitor sseq http2Server
    updateRemoteCtrlSession sseq $ \case
      RCSessionPendingConfirmation {} -> Right RCSessionConnected {remoteCtrlId, rcsClient = client, rcsSession, tls, http2Server, remoteOutputQ}
      _ -> Left $ ChatErrorRemoteCtrl RCEBadState
    pure $ remoteCtrlInfo rc $ Just RCSConnected {sessionCode = tlsSessionCode tls}
  where
    upsertRemoteCtrl :: ChatMonad m => Text -> RCCtrlPairing -> m RemoteCtrl
    upsertRemoteCtrl ctrlName rcCtrlPairing = withStore $ \db -> do
      rc_ <- liftIO $ getRemoteCtrlByFingerprint db (ctrlFingerprint rcCtrlPairing)
      case rc_ of
        Nothing -> insertRemoteCtrl db ctrlName rcCtrlPairing >>= getRemoteCtrl db
        Just rc@RemoteCtrl {ctrlPairing} -> do
          let dhPrivKey' = dhPrivKey rcCtrlPairing
          liftIO $ updateRemoteCtrl db rc ctrlName dhPrivKey'
          pure rc {ctrlDeviceName = ctrlName, ctrlPairing = ctrlPairing {dhPrivKey = dhPrivKey'}}
    monitor :: ChatMonad m => SessionSeq -> Async () -> m ()
    monitor sseq server = do
      res <- waitCatch server
      logInfo $ "HTTP2 server stopped: " <> tshow res
      cancelActiveRemoteCtrl $ Just (sseq, RCSRDisconnected)

stopRemoteCtrl :: ChatMonad m => m ()
stopRemoteCtrl = cancelActiveRemoteCtrl Nothing

handleCtrlError :: ChatMonad m => SessionSeq -> (ChatError -> RemoteCtrlStopReason) -> Text -> m a -> m a
handleCtrlError sseq mkReason name action =
  action `catchChatError` \e -> do
    logError $ name <> " remote ctrl error: " <> tshow e
    cancelActiveRemoteCtrl $ Just (sseq, mkReason e)
    throwError e

-- | Stop session controller, unless session update key is present but stale
cancelActiveRemoteCtrl :: ChatMonad m => Maybe (SessionSeq, RemoteCtrlStopReason) -> m ()
cancelActiveRemoteCtrl handlerInfo_ = handleAny (logError . tshow) $ do
  var <- asks remoteCtrlSession
  session_ <-
    atomically $
      readTVar var >>= \case
        Nothing -> pure Nothing
        Just (oldSeq, _) | (maybe False ((oldSeq /=) . fst) handlerInfo_) -> pure Nothing
        Just (_, s) -> Just s <$ writeTVar var Nothing
  forM_ session_ $ \session -> do
    liftIO $ cancelRemoteCtrl handlingError session
    forM_ (snd <$> handlerInfo_) $ \rcStopReason ->
      toView CRRemoteCtrlStopped {rcsState = rcsSessionState session, rcStopReason}
  where
    handlingError = isJust handlerInfo_

cancelRemoteCtrl :: Bool -> RemoteCtrlSession -> IO ()
cancelRemoteCtrl handlingError = \case
  RCSessionStarting -> pure ()
  RCSessionSearching {action} ->
    unless handlingError $ uninterruptibleCancel action
  RCSessionConnecting {rcsClient, rcsWaitSession} -> do
    unless handlingError $ uninterruptibleCancel rcsWaitSession
    cancelCtrlClient rcsClient
  RCSessionPendingConfirmation {rcsClient, tls, rcsWaitSession} -> do
    unless handlingError $ uninterruptibleCancel rcsWaitSession
    cancelCtrlClient rcsClient
    closeConnection tls
  RCSessionConnected {rcsClient, tls, http2Server} -> do
    unless handlingError $ uninterruptibleCancel http2Server
    cancelCtrlClient rcsClient
    closeConnection tls

deleteRemoteCtrl :: ChatMonad m => RemoteCtrlId -> m ()
deleteRemoteCtrl rcId = do
  checkNoRemoteCtrlSession
  -- TODO check it exists
  withStore' (`deleteRemoteCtrlRecord` rcId)

checkNoRemoteCtrlSession :: ChatMonad m => m ()
checkNoRemoteCtrlSession =
  chatReadVar remoteCtrlSession >>= maybe (pure ()) (\_ -> throwError $ ChatErrorRemoteCtrl RCEBusy)

-- | Transition controller to a new state, unless session update key is stale
updateRemoteCtrlSession :: ChatMonad m => SessionSeq -> (RemoteCtrlSession -> Either ChatError RemoteCtrlSession) -> m ()
updateRemoteCtrlSession sseq state = do
  session <- asks remoteCtrlSession
  r <- atomically $ do
    readTVar session >>= \case
      Nothing -> pure . Left $ ChatErrorRemoteCtrl RCEInactive
      Just (oldSeq, st)
        | oldSeq /= sseq -> pure . Left $ ChatErrorRemoteCtrl RCEBadState
        | otherwise -> case state st of
            Left ce -> pure $ Left ce
            Right st' -> Right () <$ writeTVar session (Just (sseq, st'))
  liftEither r

utf8String :: [Char] -> ByteString
utf8String = encodeUtf8 . T.pack
{-# INLINE utf8String #-}

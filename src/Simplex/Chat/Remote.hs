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
{-# LANGUAGE TypeApplications #-}
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
import Data.Bifunctor (second)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Base64.URL as B64U
import Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Char8 as B
import Data.Functor (($>))
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, isNothing)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Word (Word16, Word32)
import qualified Network.HTTP.Types as N
import Network.HTTP2.Client (HTTP2Error (..))
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
import qualified Simplex.Messaging.Crypto as C
import Simplex.Messaging.Crypto.File (CryptoFile (..), CryptoFileArgs (..))
import qualified Simplex.Messaging.Crypto.File as CF
import Simplex.Messaging.Encoding.String (StrEncoding (..))
import qualified Simplex.Messaging.TMap as TM
import Simplex.Messaging.Transport (TLS, closeConnection, tlsUniq)
import Simplex.Messaging.Transport.Client (TransportHost (..))
import Simplex.Messaging.Transport.HTTP2.Client (HTTP2ClientError, closeHTTP2Client)
import Simplex.Messaging.Transport.HTTP2.File (hSendFile)
import Simplex.Messaging.Transport.HTTP2.Server (HTTP2Request (..))
import Simplex.Messaging.Util
import Simplex.RemoteControl.Client
import Simplex.RemoteControl.Invitation (RCInvitation (..), RCSignedInvitation (..))
import Simplex.RemoteControl.Types
import System.FilePath (takeFileName, (</>))
import UnliftIO
import UnliftIO.Concurrent (forkIO)
import UnliftIO.Directory (copyFile, createDirectoryIfMissing, renameFile)

-- when acting as host
minRemoteCtrlVersion :: AppVersion
minRemoteCtrlVersion = AppVersion [5, 4, 0, 2]

-- when acting as controller
minRemoteHostVersion :: AppVersion
minRemoteHostVersion = AppVersion [5, 4, 0, 2]

currentAppVersion :: AppVersion
currentAppVersion = AppVersion SC.version

ctrlAppVersionRange :: AppVersionRange
ctrlAppVersionRange = mkAppVersionRange minRemoteHostVersion currentAppVersion

hostAppVersionRange :: AppVersionRange
hostAppVersionRange = mkAppVersionRange minRemoteCtrlVersion currentAppVersion

-- * Desktop side

getRemoteHostClient :: ChatMonad m => RemoteHostId -> m RemoteHostClient
getRemoteHostClient rhId = withRemoteHostSession rhKey $ \case
  s@RHSessionConnected {rhClient} -> Right (rhClient, s)
  _ -> Left $ ChatErrorRemoteHost rhKey RHEBadState
  where
    rhKey = RHId rhId

withRemoteHostSession :: ChatMonad m => RHKey -> (RemoteHostSession -> Either ChatError (a, RemoteHostSession)) -> m a
withRemoteHostSession rhKey state = withRemoteHostSession_ rhKey $ maybe (Left $ ChatErrorRemoteHost rhKey $ RHEMissing) ((second . second) Just . state)

withRemoteHostSession_ :: ChatMonad m => RHKey -> (Maybe RemoteHostSession -> Either ChatError (a, Maybe RemoteHostSession)) -> m a
withRemoteHostSession_ rhKey state = do
  sessions <- asks remoteHostSessions
  r <- atomically $ do
    s <- TM.lookup rhKey sessions
    case state s of
      Left e -> pure $ Left e
      Right (a, s') -> Right a <$ maybe (TM.delete rhKey) (TM.insert rhKey) s' sessions
  liftEither r

setNewRemoteHostId :: ChatMonad m => RHKey -> RemoteHostId -> m ()
setNewRemoteHostId rhKey rhId = do
  sessions <- asks remoteHostSessions
  r <- atomically $ do
    TM.lookupDelete rhKey sessions >>= \case
      Nothing -> pure $ Left $ ChatErrorRemoteHost rhKey RHEMissing
      Just s -> Right () <$ TM.insert (RHId rhId) s sessions
  liftEither r

startRemoteHost' :: ChatMonad m => Maybe (RemoteHostId, Bool) -> m (Maybe RemoteHostInfo, RCSignedInvitation)
startRemoteHost' rh_ = do
  (rhKey, multicast, remoteHost_, pairing) <- case rh_ of
    Just (rhId, multicast) -> do
      rh@RemoteHost {hostPairing} <- withStore $ \db -> getRemoteHost db rhId
      pure (RHId rhId, multicast, Just $ remoteHostInfo rh True, hostPairing) -- get from the database, start multicast if requested
    Nothing -> (RHNew,False,Nothing,) <$> rcNewHostPairing
  withRemoteHostSession_ rhKey $ maybe (Right ((), Just RHSessionStarting)) (\_ -> Left $ ChatErrorRemoteHost rhKey RHEBusy)
  ctrlAppInfo <- mkCtrlAppInfo
  (invitation, rchClient, vars) <- withAgent $ \a -> rcConnectHost a pairing (J.toJSON ctrlAppInfo) multicast
  cmdOk <- newEmptyTMVarIO
  rhsWaitSession <- async $ do
    atomically $ takeTMVar cmdOk
    cleanupOnError rchClient $ waitForSession remoteHost_ vars
  let rhs = RHPendingSession {rhKey, rchClient, rhsWaitSession, remoteHost_}
  withRemoteHostSession rhKey $ \case
    RHSessionStarting -> Right ((), RHSessionConnecting rhs)
    _ -> Left $ ChatErrorRemoteHost rhKey RHEBadState
  (remoteHost_, invitation) <$ atomically (putTMVar cmdOk ())
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
    cleanupOnError :: ChatMonad m => RCHostClient -> (TMVar RHKey -> m ()) -> m ()
    cleanupOnError rchClient action = do
      currentKey <- newEmptyTMVarIO
      action currentKey `catchChatError` \err -> do
        logError $ "startRemoteHost'.waitForSession crashed: " <> tshow err
        sessions <- asks remoteHostSessions
        atomically $ readTMVar currentKey >>= (`TM.delete` sessions)
        liftIO $ cancelHostClient rchClient
    waitForSession :: ChatMonad m => Maybe RemoteHostInfo -> RCStepTMVar (ByteString, RCStepTMVar (RCHostSession, RCHostHello, RCHostPairing)) -> TMVar RHKey -> m ()
    waitForSession remoteHost_ vars currentKey = do
      let rhKey = maybe RHNew (\RemoteHostInfo {remoteHostId} -> RHId remoteHostId) remoteHost_
      atomically $ writeTMVar currentKey rhKey
      (sessId, vars') <- takeRCStep vars
      toView $ CRRemoteHostSessionCode {remoteHost_, sessionCode = verificationCode sessId} -- display confirmation code, wait for mobile to confirm
      (RCHostSession {tls, sessionKeys}, rhHello, pairing') <- takeRCStep vars'
      hostInfo@HostAppInfo {deviceName = hostDeviceName} <-
        liftError (ChatErrorRemoteHost rhKey) $ parseHostAppInfo rhHello
      withRemoteHostSession rhKey $ \case
        RHSessionConnecting rhs' -> Right ((), RHSessionConfirmed tls rhs') -- TODO check it's the same session?
        _ -> Left $ ChatErrorRemoteHost rhKey RHEBadState
      -- update remoteHost with updated pairing
      rhi@RemoteHostInfo {remoteHostId, storePath} <- upsertRemoteHost pairing' remoteHost_ hostDeviceName
      let rhKey' = RHId remoteHostId -- rhKey may be invalid after upserting on RHNew
      atomically $ writeTMVar currentKey rhKey'
      disconnected <- toIO $ onDisconnected remoteHostId
      httpClient <- liftEitherError (httpError rhKey') $ attachRevHTTP2Client disconnected tls
      rhClient <- mkRemoteHostClient httpClient sessionKeys sessId storePath hostInfo
      pollAction <- async $ pollEvents remoteHostId rhClient
      withRemoteHostSession rhKey' $ \case
        RHSessionConfirmed _ RHPendingSession {} -> Right ((), RHSessionConnected {tls, rhClient, pollAction, storePath})
        _ -> Left $ ChatErrorRemoteHost rhKey' RHEBadState
      chatWriteVar currentRemoteHost $ Just remoteHostId -- this is required for commands to be passed to remote host
      toView $ CRRemoteHostConnected rhi
    upsertRemoteHost :: ChatMonad m => RCHostPairing -> Maybe RemoteHostInfo -> Text -> m RemoteHostInfo
    upsertRemoteHost pairing'@RCHostPairing {knownHost = kh_} rhi_ hostDeviceName = do
      KnownHostPairing {hostDhPubKey = hostDhPubKey'} <- maybe (throwError . ChatError $ CEInternalError "KnownHost is known after verification") pure kh_
      case rhi_ of
        Nothing -> do
          storePath <- liftIO randomStorePath
          rh@RemoteHost {remoteHostId} <- withStore $ \db -> insertRemoteHost db hostDeviceName storePath pairing' >>= getRemoteHost db
          setNewRemoteHostId RHNew remoteHostId
          pure $ remoteHostInfo rh True
        Just rhi@RemoteHostInfo {remoteHostId} -> do
          withStore' $ \db -> updateHostPairing db remoteHostId hostDeviceName hostDhPubKey'
          pure rhi
    onDisconnected :: ChatMonad m => RemoteHostId -> m ()
    onDisconnected remoteHostId = do
      logDebug "HTTP2 client disconnected"
      chatModifyVar currentRemoteHost $ \cur -> if cur == Just remoteHostId then Nothing else cur -- only wipe the closing RH
      sessions <- asks remoteHostSessions
      void . atomically $ TM.lookupDelete (RHId remoteHostId) sessions
      toView $ CRRemoteHostStopped remoteHostId
    pollEvents :: ChatMonad m => RemoteHostId -> RemoteHostClient -> m ()
    pollEvents rhId rhClient = do
      oq <- asks outputQ
      forever $ do
        r_ <- liftRH rhId $ remoteRecv rhClient 10000000
        forM r_ $ \r -> atomically $ writeTBQueue oq (Nothing, Just rhId, r)
    httpError :: RHKey -> HTTP2ClientError -> ChatError
    httpError rhKey = ChatErrorRemoteHost rhKey . RHEProtocolError . RPEHTTP2 . tshow

closeRemoteHost :: ChatMonad m => RHKey -> m ()
closeRemoteHost rhKey = do
  logNote $ "Closing remote host session for " <> tshow rhKey
  chatModifyVar currentRemoteHost $ \cur -> if (RHId <$> cur) == Just rhKey then Nothing else cur -- only wipe the closing RH
  join . withRemoteHostSession_ rhKey . maybe (Left $ ChatErrorRemoteCtrl RCEInactive) $
    \s -> Right (liftIO $ cancelRemoteHost s, Nothing)

cancelRemoteHost :: RemoteHostSession -> IO ()
cancelRemoteHost = \case
  RHSessionStarting -> pure ()
  RHSessionConnecting rhs -> cancelPendingSession rhs
  RHSessionConfirmed tls rhs -> do
    cancelPendingSession rhs
    closeConnection tls
  RHSessionConnected {tls, rhClient = RemoteHostClient {httpClient}, pollAction} -> do
    uninterruptibleCancel pollAction
    closeHTTP2Client httpClient
    closeConnection tls
  where
    cancelPendingSession RHPendingSession {rchClient, rhsWaitSession} = do
      uninterruptibleCancel rhsWaitSession
      cancelHostClient rchClient

-- | Generate a random 16-char filepath without / in it by using base64url encoding.
randomStorePath :: IO FilePath
randomStorePath = B.unpack . B64U.encode <$> getRandomBytes 12

listRemoteHosts :: ChatMonad m => m [RemoteHostInfo]
listRemoteHosts = do
  active <- chatReadVar remoteHostSessions
  map (rhInfo active) <$> withStore' getRemoteHosts
  where
    rhInfo active rh@RemoteHost {remoteHostId} =
      remoteHostInfo rh (M.member (RHId remoteHostId) active)

-- XXX: replacing hostPairing replaced with sessionActive, could be a ($>)
remoteHostInfo :: RemoteHost -> Bool -> RemoteHostInfo
remoteHostInfo RemoteHost {remoteHostId, storePath, hostName} sessionActive =
  RemoteHostInfo {remoteHostId, storePath, hostName, sessionActive}

deleteRemoteHost :: ChatMonad m => RemoteHostId -> m ()
deleteRemoteHost rhId = do
  RemoteHost {storePath} <- withStore (`getRemoteHost` rhId)
  chatReadVar filesFolder >>= \case
    Just baseDir -> do
      let hostStore = baseDir </> storePath
      logError $ "TODO: remove " <> tshow hostStore
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
      cfArgs <- liftIO CF.randomArgs
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

findKnownRemoteCtrl :: ChatMonad m => m ()
findKnownRemoteCtrl = undefined -- do

-- | Use provided OOB link as an annouce
connectRemoteCtrl :: ChatMonad m => RCSignedInvitation -> m ()
connectRemoteCtrl inv@RCSignedInvitation {invitation = RCInvitation {ca, app}} = do
  (ctrlDeviceName, v) <- parseCtrlAppInfo app
  withRemoteCtrlSession_ $ maybe (Right ((), Just RCSessionStarting)) (\_ -> Left $ ChatErrorRemoteCtrl RCEBusy)
  rc_ <- withStore' $ \db -> getRemoteCtrlByFingerprint db ca
  hostAppInfo <- getHostAppInfo v
  (rcsClient, vars) <- withAgent $ \a -> rcConnectCtrlURI a inv (ctrlPairing <$> rc_) (J.toJSON hostAppInfo)
  cmdOk <- newEmptyTMVarIO
  rcsWaitSession <- async $ do
    atomically $ takeTMVar cmdOk
    cleanupOnError rcsClient $ waitForSession rc_ ctrlDeviceName rcsClient vars
  cleanupOnError rcsClient . updateRemoteCtrlSession $ \case
    RCSessionStarting -> Right RCSessionConnecting {rcsClient, rcsWaitSession}
    _ -> Left $ ChatErrorRemoteCtrl RCEBadState
  atomically $ putTMVar cmdOk ()
  where
    cleanupOnError :: ChatMonad m => RCCtrlClient -> m () -> m ()
    cleanupOnError rcsClient action = action `catchChatError` \e -> do
      logError $ "connectRemoteCtrl crashed with: " <> tshow e
      chatWriteVar remoteCtrlSession Nothing -- XXX: can only wipe PendingConfirmation or RCSessionConnecting, which only have rcsClient to cancel
      liftIO $ cancelCtrlClient rcsClient
    waitForSession :: ChatMonad m => Maybe RemoteCtrl -> Text -> RCCtrlClient -> RCStepTMVar (ByteString, TLS, RCStepTMVar (RCCtrlSession, RCCtrlPairing)) -> m ()
    waitForSession rc_ ctrlName rcsClient vars = do
      (uniq, tls, rcsWaitConfirmation) <- takeRCStep vars
      let sessionCode = verificationCode uniq
      toView CRRemoteCtrlSessionCode {remoteCtrl_ = (`remoteCtrlInfo` True) <$> rc_, sessionCode}
      updateRemoteCtrlSession $ \case
        RCSessionConnecting {rcsWaitSession} -> Right RCSessionPendingConfirmation {ctrlName, rcsClient, tls, sessionCode, rcsWaitSession, rcsWaitConfirmation}
        _ -> Left $ ChatErrorRemoteCtrl RCEBadState
    parseCtrlAppInfo ctrlAppInfo = do
      CtrlAppInfo {deviceName, appVersionRange} <-
        liftEitherWith (const $ ChatErrorRemoteCtrl RCEBadInvitation) $ JT.parseEither J.parseJSON ctrlAppInfo
      v <- case compatibleAppVersion hostAppVersionRange appVersionRange of
        Just (AppCompatible v) -> pure v
        Nothing -> throwError $ ChatErrorRemoteCtrl $ RCEBadVersion $ maxVersion appVersionRange
      pure (deviceName, v)
    getHostAppInfo appVersion = do
      hostDeviceName <- chatReadVar localDeviceName
      encryptFiles <- chatReadVar encryptLocalFiles
      pure HostAppInfo {appVersion, deviceName = hostDeviceName, encoding = localEncoding, encryptFiles}

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
      RCStoreFile {fileName, fileSize, fileDigest} -> handleStoreFile fileName fileSize fileDigest getNext >>= reply
      RCGetFile {file} -> handleGetFile user file replyWith
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
handleStoreFile :: forall m. ChatMonad m => FilePath -> Word32 -> FileDigest -> GetChunk -> m RemoteResponse
handleStoreFile fileName fileSize fileDigest getChunk =
  either RRProtocolError RRFileStored <$> (chatReadVar filesFolder >>= storeFile)
  where
    storeFile :: Maybe FilePath -> m (Either RemoteProtocolError FilePath)
    storeFile = \case
      Just ff -> takeFileName <$$> storeFileTo ff
      Nothing -> storeFileTo =<< getDefaultFilesFolder
    storeFileTo :: FilePath -> m (Either RemoteProtocolError FilePath)
    storeFileTo dir = liftRC . tryRemoteError $ do
      filePath <- dir `uniqueCombine` fileName
      receiveRemoteFile getChunk fileSize fileDigest filePath
      pure filePath

handleGetFile :: ChatMonad m => User -> RemoteFile -> Respond m -> m ()
handleGetFile User {userId} RemoteFile {userId = commandUserId, fileId, sent, fileSource = cf'@CryptoFile {filePath}} reply = do
  logDebug $ "GetFile: " <> tshow filePath
  unless (userId == commandUserId) $ throwChatError $ CEDifferentActiveUser {commandUserId, activeUserId = userId}
  path <- maybe filePath (</> filePath) <$> chatReadVar filesFolder
  withStore $ \db -> do
    cf <- getLocalCryptoFile db commandUserId fileId sent
    unless (cf == cf') $ throwError $ SEFileNotFound fileId
  liftRC (tryRemoteError $ getFileInfo path) >>= \case
    Left e -> reply (RRProtocolError e) $ \_ -> pure ()
    Right (fileSize, fileDigest) ->
      withFile path ReadMode $ \h ->
        reply RRFile {fileSize, fileDigest} $ \send -> hSendFile h send fileSize

discoverRemoteCtrls :: ChatMonad m => TM.TMap C.KeyHash (TransportHost, Word16) -> m ()
discoverRemoteCtrls discovered = do
  error "TODO: discoverRemoteCtrls"

listRemoteCtrls :: ChatMonad m => m [RemoteCtrlInfo]
listRemoteCtrls = do
  active <- chatReadVar remoteCtrlSession >>= \case
    Just RCSessionConnected {remoteCtrlId} -> pure $ Just remoteCtrlId
    _ -> pure Nothing
  map (rcInfo active) <$> withStore' getRemoteCtrls
  where
    rcInfo activeRcId rc@RemoteCtrl {remoteCtrlId} =
      remoteCtrlInfo rc $ activeRcId == Just remoteCtrlId

remoteCtrlInfo :: RemoteCtrl -> Bool -> RemoteCtrlInfo
remoteCtrlInfo RemoteCtrl {remoteCtrlId, ctrlName} sessionActive =
  RemoteCtrlInfo {remoteCtrlId, ctrlName, sessionActive}

-- XXX: only used for multicast
confirmRemoteCtrl :: ChatMonad m => RemoteCtrlId -> m ()
confirmRemoteCtrl _rcId = do
  -- TODO check it exists, check the ID is the same as in session
  -- RemoteCtrlSession {confirmed} <- getRemoteCtrlSession
  -- withStore' $ \db -> markRemoteCtrlResolution db rcId True
  -- atomically . void $ tryPutTMVar confirmed rcId -- the remote host can now proceed with connection
  undefined

-- | Take a look at emoji of tlsunique, commit pairing, and start session server
verifyRemoteCtrlSession :: ChatMonad m => (ByteString -> m ChatResponse) -> Text -> m RemoteCtrlInfo
verifyRemoteCtrlSession execChatCommand sessCode' = cleanupOnError $ do
  (client, ctrlName, sessionCode, vars) <-
    getRemoteCtrlSession >>= \case
      RCSessionPendingConfirmation {rcsClient, ctrlName, sessionCode, rcsWaitConfirmation} -> pure (rcsClient, ctrlName, sessionCode, rcsWaitConfirmation)
      _ -> throwError $ ChatErrorRemoteCtrl RCEBadState
  let verified = sameVerificationCode sessCode' sessionCode
  liftIO $ confirmCtrlSession client verified
  unless verified $ throwError $ ChatErrorRemoteCtrl $ RCEProtocolError PRESessionCode
  (rcsSession@RCCtrlSession {tls, sessionKeys}, rcCtrlPairing) <- takeRCStep vars
  rc@RemoteCtrl {remoteCtrlId} <- upsertRemoteCtrl ctrlName rcCtrlPairing
  remoteOutputQ <- asks (tbqSize . config) >>= newTBQueueIO
  encryption <- mkCtrlRemoteCrypto sessionKeys $ tlsUniq tls
  http2Server <- async $ attachHTTP2Server tls $ handleRemoteCommand execChatCommand encryption remoteOutputQ
  void . forkIO $ monitor http2Server
  withRemoteCtrlSession $ \case
    RCSessionPendingConfirmation {} -> Right ((), RCSessionConnected {remoteCtrlId, rcsClient = client, rcsSession, tls, http2Server, remoteOutputQ})
    _ -> Left $ ChatErrorRemoteCtrl RCEBadState
  pure $ remoteCtrlInfo rc True
  where
    upsertRemoteCtrl :: ChatMonad m => Text -> RCCtrlPairing -> m RemoteCtrl
    upsertRemoteCtrl ctrlName rcCtrlPairing = withStore $ \db -> do
      rc_ <- liftIO $ getRemoteCtrlByFingerprint db (ctrlFingerprint rcCtrlPairing)
      case rc_ of
        Nothing -> insertRemoteCtrl db ctrlName rcCtrlPairing >>= getRemoteCtrl db
        Just rc@RemoteCtrl {remoteCtrlId} -> do
          liftIO $ updateCtrlPairingKeys db remoteCtrlId (dhPrivKey rcCtrlPairing)
          pure rc
    cleanupOnError :: ChatMonad m => m a -> m a
    cleanupOnError action = action `catchChatError` \e -> do
      logError $ "verifyRemoteCtrlSession crashed with: " <> tshow e
      withRemoteCtrlSession_ (\s -> pure (s, Nothing)) >>= mapM_ (liftIO . cancelRemoteCtrl) -- cancel session threads, if any
      throwError e
    monitor :: ChatMonad m => Async () -> m ()
    monitor server = do
      res <- waitCatch server
      logInfo $ "HTTP2 server stopped: " <> tshow res
      withRemoteCtrlSession_ (\s -> pure (s, Nothing)) >>= mapM_ (liftIO . cancelRemoteCtrl) -- cancel session threads, if any
      toView CRRemoteCtrlStopped

stopRemoteCtrl :: ChatMonad m => m ()
stopRemoteCtrl =
  join . withRemoteCtrlSession_ . maybe (Left $ ChatErrorRemoteCtrl RCEInactive) $
    \s -> Right (liftIO $ cancelRemoteCtrl s, Nothing)

cancelRemoteCtrl :: RemoteCtrlSession -> IO ()
cancelRemoteCtrl = \case
  RCSessionStarting -> pure ()
  RCSessionConnecting {rcsClient, rcsWaitSession} -> do
    uninterruptibleCancel rcsWaitSession
    cancelCtrlClient rcsClient
  RCSessionPendingConfirmation {rcsClient, tls, rcsWaitSession} -> do
    uninterruptibleCancel rcsWaitSession
    cancelCtrlClient rcsClient
    closeConnection tls
  RCSessionConnected {rcsClient, tls, http2Server} -> do
    uninterruptibleCancel http2Server
    cancelCtrlClient rcsClient
    closeConnection tls

deleteRemoteCtrl :: ChatMonad m => RemoteCtrlId -> m ()
deleteRemoteCtrl rcId = do
  checkNoRemoteCtrlSession
  -- TODO check it exists
  withStore' (`deleteRemoteCtrlRecord` rcId)

getRemoteCtrlSession :: ChatMonad m => m RemoteCtrlSession
getRemoteCtrlSession =
  chatReadVar remoteCtrlSession >>= maybe (throwError $ ChatErrorRemoteCtrl RCEInactive) pure

checkNoRemoteCtrlSession :: ChatMonad m => m ()
checkNoRemoteCtrlSession =
  chatReadVar remoteCtrlSession >>= maybe (pure ()) (\_ -> throwError $ ChatErrorRemoteCtrl RCEBusy)

withRemoteCtrlSession :: ChatMonad m => (RemoteCtrlSession -> Either ChatError (a, RemoteCtrlSession)) -> m a
withRemoteCtrlSession state = withRemoteCtrlSession_ $ maybe (Left $ ChatErrorRemoteCtrl RCEInactive) ((second . second) Just . state)

-- | Atomically process controller state wrt. specific remote ctrl session
withRemoteCtrlSession_ :: ChatMonad m => (Maybe RemoteCtrlSession -> Either ChatError (a, Maybe RemoteCtrlSession)) -> m a
withRemoteCtrlSession_ state = do
  session <- asks remoteCtrlSession
  r <-
    atomically $ stateTVar session $ \s ->
      case state s of
        Left e -> (Left e, s)
        Right (a, s') -> (Right a, s')
  liftEither r

updateRemoteCtrlSession :: ChatMonad m => (RemoteCtrlSession -> Either ChatError RemoteCtrlSession) -> m ()
updateRemoteCtrlSession state = withRemoteCtrlSession $ fmap ((),) . state

utf8String :: [Char] -> ByteString
utf8String = encodeUtf8 . T.pack
{-# INLINE utf8String #-}

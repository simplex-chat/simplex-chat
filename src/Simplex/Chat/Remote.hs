{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
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
import Control.Monad.STM (retry)
import Crypto.Random (getRandomBytes)
import qualified Data.Aeson as J
import Data.Bifunctor (first, second)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Base64.URL as B64U
import Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Char8 as B
import Data.Functor (($>))
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Word (Word16, Word32)
import qualified Network.HTTP.Types as N
import Network.HTTP2.Server (responseStreaming)
import Network.Socket (SockAddr (..), hostAddressToTuple)
import Simplex.Chat.Archive (archiveFilesFolder)
import Simplex.Chat.Controller
import Simplex.Chat.Files
import Simplex.Chat.Messages (chatNameStr)
import Simplex.Chat.Remote.Protocol
import Simplex.Chat.Remote.RevHTTP (announceRevHTTP2, attachHTTP2Server)
import Simplex.Chat.Remote.Transport
import Simplex.Chat.Remote.Types
import Simplex.Chat.Store.Files
import Simplex.Chat.Store.Remote
import Simplex.Chat.Store.Shared
import Simplex.Chat.Types
import Simplex.Chat.Util (encryptFile)
import Simplex.FileTransfer.Description (FileDigest (..))
import qualified Simplex.Messaging.Crypto as C
import Simplex.Messaging.Crypto.File (CryptoFile (..), CryptoFileArgs (..))
import qualified Simplex.Messaging.Crypto.File as CF
import Simplex.Messaging.Encoding (smpDecode)
import Simplex.Messaging.Encoding.String (StrEncoding (..))
import qualified Simplex.Messaging.TMap as TM
import Simplex.Messaging.Transport (tlsUniq, TLS (..))
import Simplex.Messaging.Transport.Client (TransportHost (..))
import Simplex.Messaging.Transport.Credentials (genCredentials, tlsCredentials)
import Simplex.Messaging.Transport.HTTP2.File (hSendFile)
import Simplex.Messaging.Transport.HTTP2.Server (HTTP2Request (..))
import Simplex.Messaging.Util (ifM, liftEitherError, liftEitherWith, liftError, liftIOEither, tryAllErrors, tshow, ($>>=), (<$$>))
import qualified Simplex.RemoteControl.Discovery as Discovery
import Simplex.RemoteControl.Client (CtrlSessKeys (..))
import Simplex.RemoteControl.Invitation (RCSignedInvitation (..), RCInvitation (..))
import Simplex.RemoteControl.Types
import System.FilePath (takeFileName, (</>))
import UnliftIO
import UnliftIO.Concurrent (threadDelay)
import UnliftIO.Directory (copyFile, createDirectoryIfMissing, renameFile)
import Simplex.RemoteControl.Client
import Simplex.Messaging.Agent

-- * Desktop side

getRemoteHostSession :: ChatMonad m => Maybe RemoteHostId -> m RemoteHostSession
getRemoteHostSession rhId = withRemoteHostSession rhId $ \_ s -> pure $ Right s

-- withRemoteHostSession :: ChatMonad m => Maybe RemoteHostId -> (TM.TMap (Maybe RemoteHostId) RemoteHostSession -> RemoteHostSession -> STM (Either ChatError a)) -> m a
-- withRemoteHostSession rhId = withRemoteHostSession_ rhId missing
--   where
--     missing _ = pure . Left $ ChatErrorRemoteHost rhId RHMissing

withNoRemoteHostSession :: ChatMonad m => Maybe RemoteHostId -> (TM.TMap (Maybe RemoteHostId) RemoteHostSession -> STM (Either ChatError a)) -> m a
withNoRemoteHostSession rhId action = withRemoteHostSession_ rhId action busy
  where
    busy _ _ = pure . Left $ ChatErrorRemoteHost rhId RHBusy

-- | Atomically process controller state wrt. specific remote host session
-- withRemoteHostSession_ :: ChatMonad m => Maybe RemoteHostId -> (TM.TMap (Maybe RemoteHostId) RemoteHostSession -> STM (Either ChatError a)) -> (TM.TMap RemoteHostId RemoteHostSession -> RemoteHostSession -> STM (Either ChatError a)) -> m a
-- withRemoteHostSession_ rhId missing present = do
--   sessions <- asks remoteHostSessions
--   liftIOEither . atomically $ TM.lookup rhId sessions >>= maybe (missing sessions) (present sessions)

withRemoteHostSession :: ChatMonad m => RHKey -> (RemoteHostSession -> Either ChatError (a, RemoteHostSession)) -> m a
withRemoteHostSession rhKey state = withRemoteHostSession_ rhKey $ maybe (Left $ ChatErrorRemoteHost RHEBusy) ((second . second) Just . state)

withRemoteHostSession_ :: ChatMonad m => RHKey -> (Maybe RemoteHostSession -> Either ChatError (a, Maybe RemoteCtrlSession)) -> m a
withRemoteHostSession_ rhKey state = do
  sessions <- asks remoteHostSession
  r <- atomically $ do
    s <- TM.lookup rhKey sessions
    case state s of
      Left e -> pure $ Left e
      Right (a, s') -> Right a <$ maybe (TM.delete rhKey) (TM.insert rhKey) s' sessions
  liftEither r

startRemoteHost' :: ChatMonad m => Maybe (RemoteHostId, Bool) -> m RCSignedInvitation
startRemoteHost' rh_ = do
  (rhKey, multicast, remoteHost_, pairing) <- case rh_ of
    Just (rhId, multicast) -> pure (RHId rhId, multicast, undefined, undefined) -- get from the database, start multicast if requested
    Nothing -> (RHNew,False,Nothing,) <$> rcNewHostPairing
  withRemoteHostSession_ rhKey $ maybe (Right ((), Just RHSessionStarting)) (\_ -> Left $ ChatErrorRemoteHost RHEBusy)
  let ourApp = J.String "hi"
  -- TMVar (RCHostSession, RCHelloBody, RCHostPairing)
  (invitation, client, vars) <- withAgent $ \a -> rcConnectHost a pairing ourApp multicast
  toView $ CRRemoteHostSessionCode {remoteHost = remoteHostInfo rh True, sessionCode, newCtrl = True}
  rhsWaitSession <- async $ waitForSession client vars
  withRemoteHostSession rhKey $ \case -> 
    RHSessionStarting -> Right ((), RHSessionConnecting {rhsWaitSession, remoteHost_})
    _ -> Left $ ChatErrorRemoteHost RHEBadState
  pure CRRemoteHostStarted {remoteHost_, invitation = encodeUtf8 $ strEncode invitation}
  where
    waitForSession client vars = do
      (rhSession, rhHello, rhPairing) <- atomically $ takeTMVar vars
      remoteHost <- withRemoteHostSession rhKey $ \case ->
        RHSessionConnecting {rhsWaitSession, remoteHost_} ->
          let remoteHost = RemoteHostInfo {active = True} 
           in Right (remoteHost, RHSessionConnected {rhsClient = client, remoteHost})
      -- store remoteHost in DB
      toView $ CRRemoteHostConnected remoteHost

-- startRemoteHost :: ChatMonad m => RemoteHostId -> m ()
-- startRemoteHost rhId = do
--   rh <- withStore (`getRemoteHost` rhId)
--   tasks <- startRemoteHostSession rh
--   logInfo $ "Remote host session starting for " <> tshow rhId
--   asyncRegistered tasks $
--     run rh tasks `catchAny` \err -> do
--       logError $ "Remote host session startup failed for " <> tshow rhId <> ": " <> tshow err
--       cancelTasks tasks
--       chatModifyVar remoteHostSessions $ M.delete rhId
--       throwError $ fromMaybe (mkChatError err) $ fromException err
--   where
--     -- logInfo $ "Remote host session starting for " <> tshow rhId

--     run :: ChatMonad m => RemoteHost -> Tasks -> m ()
--     run rh@RemoteHost {storePath} tasks = do
--       (fingerprint, credentials) <- liftIO $ genSessionCredentials rh
--       cleanupIO <- toIO $ do
--         logNote $ "Remote host session stopping for " <> tshow rhId
--         cancelTasks tasks -- cancel our tasks anyway
--         chatModifyVar currentRemoteHost $ \cur -> if cur == Just rhId then Nothing else cur -- only wipe the closing RH
--         withRemoteHostSession rhId $ \sessions _ -> Right <$> TM.delete rhId sessions
--         toView (CRRemoteHostStopped rhId) -- only signal "stopped" when the session is unregistered cleanly
--         -- block until some client is connected or an error happens
--       logInfo $ "Remote host session connecting for " <> tshow rhId
--       rcName <- chatReadVar localDeviceName
--       localAddr <- asks multicastSubscribers >>= Discovery.getLocalAddressMulticast >>= maybe (throwError . ChatError $ CEInternalError "unable to get local address") pure
--       started <- newEmptyTMVarIO -- XXX: should contain service port to be published
--       (dhKey, sigKey, ann, oob) <- error "TODO: startRemoteHost.run 1" -- Discovery.startSession (if rcName == "" then Nothing else Just rcName) (localAddr, 0) fingerprint
--       toView CRRemoteHostStarted {remoteHost = remoteHostInfo rh True, sessionOOB = decodeUtf8 $ strEncode (oob :: RCSignedInvitation)}
--       httpClient <- error "TODO: startRemoteHost.run 2" -- liftEitherError (ChatErrorRemoteCtrl . RCEHTTP2Error . show) $ announceRevHTTP2 tasks started (sigKey, ann) credentials cleanupIO
--       logInfo $ "Remote host session connected for " <> tshow rhId
--       -- test connection and establish a protocol layer
--       remoteHostClient <- liftRH rhId $ createRemoteHostClient httpClient dhKey rcName
--       -- set up message polling
--       oq <- asks outputQ
--       asyncRegistered tasks . forever $ do
--         liftRH rhId (remoteRecv remoteHostClient 1000000) >>= mapM_ (atomically . writeTBQueue oq . (Nothing,Just rhId,))
--       -- update session state
--       logInfo $ "Remote host session started for " <> tshow rhId
--       chatModifyVar remoteHostSessions $ M.adjust (\rhs -> rhs {remoteHostClient = Just remoteHostClient}) rhId
--       chatWriteVar currentRemoteHost $ Just rhId
--       toView $
--         CRRemoteHostConnected
--           RemoteHostInfo
--             { remoteHostId = rhId,
--               storePath = storePath,
--               displayName = hostDeviceName remoteHostClient,
--               sessionActive = True
--             }

--     genSessionCredentials RemoteHost {caKey, caCert} = do
--       sessionCreds <- genCredentials (Just parent) (0, 24) "Session"
--       pure . tlsCredentials $ sessionCreds :| [parent]
--       where
--         parent = (C.signatureKeyPair caKey, caCert)

-- | Atomically check/register session and prepare its task list
-- startRemoteHostSession :: ChatMonad m => RemoteHost -> m Tasks
-- startRemoteHostSession RemoteHost {remoteHostId, storePath} = withNoRemoteHostSession remoteHostId $ \sessions -> do
--   remoteHostTasks <- newTVar []
--   TM.insert remoteHostId RemoteHostSession {remoteHostTasks, storePath, remoteHostClient = Nothing} sessions
--   pure $ Right remoteHostTasks

closeRemoteHostSession :: ChatMonad m => Maybe RemoteHostId -> m ()
closeRemoteHostSession rhId = do
  logNote $ "Closing remote host session for " <> tshow rhId
  chatModifyVar currentRemoteHost $ \cur -> if cur == rhId then Nothing else cur -- only wipe the closing RH
  session <- withRemoteHostSession rhId $ \sessions rhs -> Right rhs <$ TM.delete rhId sessions
  cancelRemoteHostSession session

cancelRemoteHostSession :: MonadUnliftIO m => RemoteHostSession -> m ()
cancelRemoteHostSession RemoteHostSession {remoteHostTasks, remoteHostClient} = do
  cancelTasks remoteHostTasks
  mapM_ closeRemoteHostClient remoteHostClient


closeRemoteHost :: ChatMonad m => m ()
closeRemoteHost =
  join . withRemoteCtrlSession_ . maybe (Left $ ChatErrorRemoteCtrl RCEInactive) $
    \s -> Right (liftIO $ cancelRemoteCtrl s, Nothing)

cancelRemoteHost :: RemoteHostSession -> IO ()
cancelRemoteHost = \case
  RHSessionStarting -> pure ()
  RHSessionConnecting {rchClient} -> cancelHostClient rchClient
  RHSessionConnected {rhClient = RemoteHostClient {httpClient}} -> closeHTTP2Client httpClient

createRemoteHost :: ChatMonad m => m RemoteHostInfo
createRemoteHost = do
  ((_, caKey), caCert) <- liftIO $ genCredentials Nothing (-25, 24 * 365) "Host"
  storePath <- liftIO randomStorePath
  let remoteName = "" -- will be passed from remote host in hello
  rhId <- withStore' $ \db -> insertRemoteHost db storePath remoteName caKey caCert
  rh <- withStore $ \db -> getRemoteHost db rhId
  pure $ remoteHostInfo rh False

-- | Generate a random 16-char filepath without / in it by using base64url encoding.
randomStorePath :: IO FilePath
randomStorePath = B.unpack . B64U.encode <$> getRandomBytes 12

listRemoteHosts :: ChatMonad m => m [RemoteHostInfo]
listRemoteHosts = do
  active <- chatReadVar remoteHostSessions
  map (rhInfo active) <$> withStore' getRemoteHosts
  where
    rhInfo active rh@RemoteHost {remoteHostId} =
      remoteHostInfo rh (M.member remoteHostId active)

remoteHostInfo :: RemoteHost -> Bool -> RemoteHostInfo
remoteHostInfo RemoteHost {remoteHostId, storePath, displayName} sessionActive =
  RemoteHostInfo {remoteHostId, storePath, displayName, sessionActive}

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
  RemoteHostSession {remoteHostClient, storePath} <- getRemoteHostSession $ Just rhId
  case remoteHostClient of
    Nothing -> throwError $ ChatErrorRemoteHost rhId RHMissing
    Just c@RemoteHostClient {encryptHostFiles} -> do
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
  RemoteHostSession {remoteHostClient, storePath} <- getRemoteHostSession $ Just rhId
  case remoteHostClient of
    Nothing -> throwError $ ChatErrorRemoteHost rhId RHMissing
    Just c -> do
      dir <- (</> storePath </> archiveFilesFolder) <$> (maybe getDefaultFilesFolder pure =<< chatReadVar remoteHostsFolder)
      createDirectoryIfMissing True dir
      liftRH rhId $ remoteGetFile c dir rf

processRemoteCommand :: ChatMonad m => RemoteHostId -> RemoteHostSession -> ChatCommand -> ByteString -> m ChatResponse
processRemoteCommand remoteHostId RemoteHostSession {remoteHostClient = Just rhc} cmd s = case cmd of
  SendFile chatName f -> sendFile "/f" chatName f
  SendImage chatName f -> sendFile "/img" chatName f
  _ -> liftRH remoteHostId $ remoteSend rhc s
  where
    sendFile cmdName chatName (CryptoFile path cfArgs) = do
      -- don't encrypt in host if already encrypted locally
      CryptoFile path' cfArgs' <- storeRemoteFile remoteHostId (cfArgs $> False) path
      let f = CryptoFile path' (cfArgs <|> cfArgs') -- use local or host encryption
      liftRH remoteHostId $ remoteSend rhc $ B.unwords [cmdName, B.pack (chatNameStr chatName), cryptoFileStr f]
    cryptoFileStr CryptoFile {filePath, cryptoArgs} =
      maybe "" (\(CFArgs key nonce) -> "key=" <> strEncode key <> " nonce=" <> strEncode nonce <> " ") cryptoArgs
        <> encodeUtf8 (T.pack filePath)
processRemoteCommand _ _ _ _ = pure $ chatCmdError Nothing "remote command sent before session started"

liftRH :: ChatMonad m => RemoteHostId -> ExceptT RemoteProtocolError IO a -> m a
liftRH rhId = liftError (ChatErrorRemoteHost rhId . RHProtocolError)

-- * Mobile side

findKnownRemoteCtrl :: ChatMonad m => (ByteString -> m ChatResponse) -> m ()
findKnownRemoteCtrl execChatCommand = undefined -- do
  -- checkNoRemoteCtrlSession -- tiny race with the final @chatWriteVar@ until the setup finishes and supervisor spawned
  -- -- TODO: fetch known controllers from DB and pass to discoverRemoteCtrls
  -- discovered <- newTVarIO mempty
  -- discoverer <- async $ discoverRemoteCtrls discovered -- TODO extract to a controller service singleton
  -- confirmed <- newEmptyTMVarIO
  -- verified <- newEmptyTMVarIO
  -- startHost execChatCommand discoverer discovered confirmed verified

-- | Use provided OOB link as an annouce
connectRemoteCtrl :: ChatMonad m => (ByteString -> m ChatResponse) -> RCSignedInvitation -> m ()
connectRemoteCtrl execChatCommand inv@RCSignedInvitation {invitation = RCInvitation {ca, app = theirApp}} = do
  -- TODO parse app and validate version
  withRemoteCtrlSession_ $ maybe (Right ((), Just RCSessionStarting)) (\_ -> Left $ ChatErrorRemoteCtrl RCEBusy)
  -- TODO check new or existing pairing (read from DB)
  let ourApp = J.String "hi"
  (rcsClient, vars) <- withAgent $ \a -> rcConnectCtrlURI a inv Nothing ourApp
  rcsWaitSession <- async $ waitForSession rcsClient vars
  updateRemoteCtrlSession $ \case
    RCSessionStarting -> Right RCSessionConnecting {rcsClient, rcsWaitSession}
    _ -> Left $ ChatErrorRemoteCtrl RCEBadState -- TODO kill rcsClient
  where
    waitForSession rcsClient vars = do
      (rcsSession@RCCtrlSession {tls = TLS {tlsUniq}}, rcsPairing) <- atomically $ takeTMVar vars
      let remoteCtrl = RemoteCtrlInfo -- TODO use Pairing or something
            { remoteCtrlId = 1,
              displayName = "from app",
              fingerprint = ca,
              accepted = Just True,
              sessionActive = True
            }
      let sessionCode = verificationCode tlsUniq
      toView CRRemoteCtrlSessionCode {remoteCtrl, sessionCode, newCtrl = True}
      updateRemoteCtrlSession $ \case
        RCSessionConnecting {} -> Right RCSessionPendingConfirmation {rcsClient, rcsSession, sessionCode, rcsPairing}
        _ -> Left $ ChatErrorRemoteCtrl RCEBadState -- TODO kill rcsClient

-- startHost :: ChatMonad m => (ByteString -> m ChatResponse) -> Async () -> TM.TMap C.KeyHash (TransportHost, Word16) -> TMVar RemoteCtrlId -> TMVar (RemoteCtrlId, Text) -> m ()
-- startHost execChatCommand discoverer discovered confirmed verified = do
--   remoteOutputQ <- asks (tbqSize . config) >>= newTBQueueIO
--   supervisor <- async $ do
--     threadDelay 500000 -- give chat controller a chance to reply with "ok" to prevent flaking tests
--     logInfo "Starting remote host"
--     runHost discovered confirmed verified $ handleRemoteCommand execChatCommand remoteOutputQ
--   chatWriteVar remoteCtrlSession $ Just RemoteCtrlSession {discoverer, supervisor, hostServer = Nothing, discovered, confirmed, verified, remoteOutputQ}

-- | Track remote host lifecycle in controller session state and signal UI on its progress
-- runHost :: ChatMonad m => TM.TMap C.KeyHash (TransportHost, Word16) -> TMVar RemoteCtrlId -> TMVar (RemoteCtrlId, Text) -> (HTTP2Request -> m ()) -> m ()
-- runHost discovered confirmed verified handleHttp = do
--   remoteCtrlId <- atomically (readTMVar confirmed) -- wait for discoverRemoteCtrls.process or confirmRemoteCtrl to confirm fingerprint as a known RC
--   rc@RemoteCtrl {fingerprint} <- withStore (`getRemoteCtrl` remoteCtrlId)
--   serviceAddress <- atomically $ TM.lookup fingerprint discovered >>= maybe retry pure -- wait for location of the matching fingerprint
--   -- XXX: the part above is only needed for discovery, can be streamlined
--   toView $ CRRemoteCtrlConnecting $ remoteCtrlInfo rc False
--   atomically $ writeTVar discovered mempty -- flush unused sources
--   server <- async $ do
--     let hsk = HostSessionKeys {ca = fingerprint}
--     -- spawn server for remote protocol commands
--     Discovery.connectTLSClient serviceAddress hsk $ \HostCryptoHandle tls -> do
--       let sessionCode = decodeUtf8 . strEncode $ tlsUniq tls
--       toView $ CRRemoteCtrlSessionCode {remoteCtrl = remoteCtrlInfo rc True, sessionCode, newCtrl = False}
--       userInfo <- atomically $ readTMVar verified
--       if userInfo == (remoteCtrlId, sessionCode)
--         then do
--           toView $ CRRemoteCtrlConnected $ remoteCtrlInfo rc True
--           -- attachHTTP2Server handleHttp tls
--           error "TODO: runHost"
--         else do
--           toView $ CRChatCmdError Nothing $ ChatErrorRemoteCtrl RCEBadVerificationCode
--           -- the server doesn't enter its loop and waitCatch below falls through
--   chatModifyVar remoteCtrlSession $ fmap $ \s -> s {hostServer = Just server}
--   waitCatch server >>= either (logDebug . tshow) pure -- wait for the server to finish
--   chatWriteVar remoteCtrlSession Nothing
--   toView CRRemoteCtrlStopped

handleRemoteCommand :: forall m. ChatMonad m => (ByteString -> m ChatResponse) -> CtrlSessKeys -> TBQueue ChatResponse -> HTTP2Request -> m ()
handleRemoteCommand execChatCommand _sessionKeys remoteOutputQ HTTP2Request {request, reqBody, sendResponse} = do
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
      (header, getNext) <- parseHTTP2Body request reqBody
      (getNext,) <$> liftEitherWith (RPEInvalidJSON . T.pack) (J.eitherDecodeStrict' header)
    replyError = reply . RRChatResponse . CRChatCmdError Nothing
    processCommand :: User -> GetChunk -> RemoteCommand -> m ()
    processCommand user getNext = \case
      RCHello {deviceName = desktopName} -> handleHello desktopName >>= reply
      RCSend {command} -> handleSend execChatCommand command >>= reply
      RCRecv {wait = time} -> handleRecv time remoteOutputQ >>= reply
      RCStoreFile {fileName, fileSize, fileDigest} -> handleStoreFile fileName fileSize fileDigest getNext >>= reply
      RCGetFile {file} -> handleGetFile user file replyWith
    reply :: RemoteResponse -> m ()
    reply = (`replyWith` \_ -> pure ())
    replyWith :: Respond m
    replyWith rr attach =
      liftIO . sendResponse . responseStreaming N.status200 [] $ \send flush -> do
        send $ sizePrefixedEncode rr
        attach send
        flush

type GetChunk = Int -> IO ByteString

type SendChunk = Builder -> IO ()

type Respond m = RemoteResponse -> (SendChunk -> IO ()) -> m ()

liftRC :: ChatMonad m => ExceptT RemoteProtocolError IO a -> m a
liftRC = liftError (ChatErrorRemoteCtrl . RCEProtocolError)

tryRemoteError :: ExceptT RemoteProtocolError IO a -> ExceptT RemoteProtocolError IO (Either RemoteProtocolError a)
tryRemoteError = tryAllErrors (RPEException . tshow)
{-# INLINE tryRemoteError #-}

handleHello :: ChatMonad m => Text -> m RemoteResponse
handleHello desktopName = do
  logInfo $ "Hello from " <> tshow desktopName
  mobileName <- chatReadVar localDeviceName
  encryptFiles <- chatReadVar encryptLocalFiles
  pure RRHello {encoding = localEncoding, deviceName = mobileName, encryptFiles}

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
  -- subscribers <- asks multicastSubscribers
  -- Discovery.withListener subscribers run
  -- where
  --   run sock = receive sock >>= process sock

  --   receive sock =
  --     Discovery.recvAnnounce sock >>= \case
  --       (SockAddrInet _sockPort sockAddr, sigAnnBytes) -> case smpDecode sigAnnBytes of
  --         Right (SignedAnnounce ann _sig) -> pure (sockAddr, ann)
  --         Left _ -> receive sock -- TODO it is probably better to report errors to view here
  --       _nonV4 -> receive sock

  --   process sock (sockAddr, Announce {caFingerprint, serviceAddress = (annAddr, port)}) = do
  --     unless (annAddr == sockAddr) $ logError "Announced address doesn't match socket address"
  --     let addr = THIPv4 (hostAddressToTuple sockAddr)
  --     ifM
  --       (atomically $ TM.member caFingerprint discovered)
  --       (logDebug $ "Fingerprint already known: " <> tshow (addr, caFingerprint))
  --       ( do
  --           logInfo $ "New fingerprint announced: " <> tshow (addr, caFingerprint)
  --           atomically $ TM.insert caFingerprint (addr, port) discovered
  --       )
  --     -- TODO we check fingerprint for duplicate where id doesn't matter - to prevent re-insert - and don't check to prevent duplicate events,
  --     -- so UI now will have to check for duplicates again
  --     withStore' (`getRemoteCtrlByFingerprint` caFingerprint) >>= \case
  --       Nothing -> toView $ CRRemoteCtrlAnnounce caFingerprint -- unknown controller, ui "register" action required
  --       -- TODO Maybe Bool is very confusing - the intent is very unclear here
  --       Just found@RemoteCtrl {remoteCtrlId, accepted = storedChoice} -> case storedChoice of
  --         Nothing -> toView $ CRRemoteCtrlFound $ remoteCtrlInfo found False -- first-time controller, ui "accept" action required
  --         Just False -> run sock -- restart, skipping a rejected item
  --         Just True ->
  --           chatReadVar remoteCtrlSession >>= \case
  --             Nothing -> toView . CRChatError Nothing . ChatError $ CEInternalError "Remote host found without running a session"
  --             Just RemoteCtrlSession {confirmed} -> atomically $ void $ tryPutTMVar confirmed remoteCtrlId -- previously accepted controller, connect automatically

listRemoteCtrls :: ChatMonad m => m [RemoteCtrlInfo]
listRemoteCtrls = pure [] -- do
  -- active <-
  --   chatReadVar remoteCtrlSession $>>= \RemoteCtrlSession {confirmed} ->
  --     atomically $ tryReadTMVar confirmed
  -- map (rcInfo active) <$> withStore' getRemoteCtrls
  -- where
  --   rcInfo activeRcId rc@RemoteCtrl {remoteCtrlId} =
  --     remoteCtrlInfo rc $ activeRcId == Just remoteCtrlId

remoteCtrlInfo :: RemoteCtrl -> Bool -> RemoteCtrlInfo
remoteCtrlInfo RemoteCtrl {remoteCtrlId, displayName, fingerprint, accepted} sessionActive =
  RemoteCtrlInfo {remoteCtrlId, displayName, fingerprint, accepted, sessionActive}

-- XXX: only used for multicast
confirmRemoteCtrl :: ChatMonad m => RemoteCtrlId -> m ()
confirmRemoteCtrl rcId = do
  -- TODO check it exists, check the ID is the same as in session
  -- RemoteCtrlSession {confirmed} <- getRemoteCtrlSession
  -- withStore' $ \db -> markRemoteCtrlResolution db rcId True
  -- atomically . void $ tryPutTMVar confirmed rcId -- the remote host can now proceed with connection
  undefined

-- Take a look at emoji of tlsunique
verifyRemoteCtrlSession :: ChatMonad m => (ByteString -> m ChatResponse) -> Text -> m ()
verifyRemoteCtrlSession execChatCommand sessCode' = do
  (client, sessionCode, _rcsPairing) <- withRemoteCtrlSession $ \case
    RCSessionPendingConfirmation {rcsClient, rcsSession, sessionCode, rcsPairing} ->
      Right ((rcsClient, sessionCode, rcsPairing), RCSessionConfirmed {rcsClient, rcsSession})
    _ -> Left $ ChatErrorRemoteCtrl RCEBadState
  let verified = sameVerificationCode sessCode' sessionCode
  liftIO $ confirmCtrlSession client verified
  -- TODO: Store new rcsPairing
  remoteOutputQ <- asks (tbqSize . config) >>= newTBQueueIO
  RCCtrlSession {tls, sessionKeys} <- withRemoteCtrlSession $ \case
    RCSessionConfirmed {rcsClient, rcsSession} ->
      Right (rcsSession, RCSessionConnected {rcsClient, rcsSession, remoteOutputQ})
    _ -> Left $ ChatErrorRemoteCtrl RCEBadState
  attachHTTP2Server tls $ handleRemoteCommand execChatCommand sessionKeys remoteOutputQ

stopRemoteCtrl :: ChatMonad m => m ()
stopRemoteCtrl =
  join . withRemoteCtrlSession_ . maybe (Left $ ChatErrorRemoteCtrl RCEInactive) $
    \s -> Right (liftIO $ cancelRemoteCtrl s, Nothing)

cancelRemoteCtrl :: RemoteCtrlSession -> IO ()
cancelRemoteCtrl = \case
  RCSessionStarting -> pure ()
  RCSessionConnecting {rcsClient, rcsWaitSession} -> do
    cancelCtrlClient rcsClient
    uninterruptibleCancel rcsWaitSession
  RCSessionPendingConfirmation {rcsClient} -> cancelCtrlClient rcsClient
  RCSessionConfirmed {rcsClient} -> cancelCtrlClient rcsClient
  RCSessionConnected {rcsClient} -> cancelCtrlClient rcsClient

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

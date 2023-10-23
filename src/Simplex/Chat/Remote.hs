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

import Control.Logger.Simple
import Control.Monad
import Control.Monad.Except
import Control.Monad.IO.Class
import Control.Monad.Reader (asks)
import Control.Monad.STM (retry)
import Crypto.Random (getRandomBytes)
import qualified Data.Aeson as J
import Data.ByteString (ByteString)
import qualified Data.ByteString.Base64.URL as B64U
import Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Char8 as B
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Word (Word32)
import Network.HTTP2.Server (responseStreaming)
import qualified Network.HTTP.Types as N
import Network.Socket (SockAddr (..), hostAddressToTuple)
import Simplex.Chat.Controller
import qualified Simplex.Chat.Remote.Discovery as Discovery
import Simplex.Chat.Remote.Protocol
import Simplex.Chat.Remote.Types
import Simplex.Chat.Store.Remote
import qualified Simplex.Messaging.Crypto as C
import Simplex.Messaging.Encoding.String (StrEncoding (..))
import Simplex.Messaging.Transport.HTTP2.Server (HTTP2Request (..))
import qualified Simplex.Messaging.TMap as TM
import Simplex.Messaging.Transport.Client (TransportHost (..))
import Simplex.Messaging.Transport.Credentials (genCredentials, tlsCredentials)
import Simplex.Messaging.Transport.HTTP2.File (hSendFile)
import Simplex.Messaging.Util (ifM, liftEitherError, liftEitherWith, liftError, liftIOEither, tryAllErrors, tshow, ($>>=))
import System.FilePath ((</>))
import UnliftIO

-- * Desktop side

getRemoteHostSession :: ChatMonad m => RemoteHostId -> m RemoteHostSession
getRemoteHostSession rhId = withRemoteHostSession rhId $ \_ s -> pure $ Right s

withRemoteHostSession :: ChatMonad m => RemoteHostId -> (TM.TMap RemoteHostId RemoteHostSession -> RemoteHostSession -> STM (Either ChatError a)) -> m a
withRemoteHostSession rhId = withRemoteHostSession_ rhId missing
  where
    missing _ = pure . Left $ ChatErrorRemoteHost rhId RHMissing

withNoRemoteHostSession :: ChatMonad m => RemoteHostId -> (TM.TMap RemoteHostId RemoteHostSession -> STM (Either ChatError a)) -> m a
withNoRemoteHostSession rhId action = withRemoteHostSession_ rhId action busy
  where
    busy _ _ = pure . Left $ ChatErrorRemoteHost rhId RHBusy

-- | Atomically process controller state wrt. specific remote host session
withRemoteHostSession_ :: ChatMonad m => RemoteHostId -> (TM.TMap RemoteHostId RemoteHostSession -> STM (Either ChatError a)) -> (TM.TMap RemoteHostId RemoteHostSession -> RemoteHostSession -> STM (Either ChatError a)) -> m a
withRemoteHostSession_ rhId missing present = do
  sessions <- asks remoteHostSessions
  liftIOEither . atomically $ TM.lookup rhId sessions >>= maybe (missing sessions) (present sessions)

startRemoteHost :: ChatMonad m => RemoteHostId -> m ()
startRemoteHost rhId = do
  rh <- withStore (`getRemoteHost` rhId)
  tasks <- startRemoteHostSession rh
  logInfo $ "Remote host session starting for " <> tshow rhId
  asyncRegistered tasks $ run rh tasks `catchAny` \err -> do
    logError $ "Remote host session startup failed for " <> tshow rhId <> ": " <> tshow err
    cancelTasks tasks
    chatModifyVar remoteHostSessions $ M.delete rhId
    throwError $ fromMaybe (mkChatError err) $ fromException err
  -- logInfo $ "Remote host session starting for " <> tshow rhId
  where
    run :: ChatMonad m => RemoteHost -> Tasks -> m ()
    run rh@RemoteHost {storePath} tasks = do
      (fingerprint, credentials) <- liftIO $ genSessionCredentials rh
      cleanupIO <- toIO $ do
        logNote $ "Remote host session stopping for " <> tshow rhId
        cancelTasks tasks -- cancel our tasks anyway
        chatModifyVar currentRemoteHost $ \cur -> if cur == Just rhId then Nothing else cur -- only wipe the closing RH
        withRemoteHostSession rhId $ \sessions _ -> Right <$> TM.delete rhId sessions
        toView (CRRemoteHostStopped rhId) -- only signal "stopped" when the session is unregistered cleanly
      -- block until some client is connected or an error happens
      logInfo $ "Remote host session connecting for " <> tshow rhId
      httpClient <- liftEitherError (ChatErrorRemoteCtrl . RCEHTTP2Error . show) $ Discovery.announceRevHTTP2 tasks fingerprint credentials cleanupIO
      logInfo $ "Remote host session connected for " <> tshow rhId
      rcName <- chatReadVar localDeviceName
      -- test connection and establish a protocol layer
      remoteHostClient <- liftRH rhId $ createRemoteHostClient httpClient rcName
      withStore' $ \db -> setRemoteHostName db rhId (remoteDeviceName remoteHostClient)
      -- set up message polling
      oq <- asks outputQ
      asyncRegistered tasks . forever $ do
        liftRH rhId (remoteRecv remoteHostClient 1000000) >>= mapM_ (atomically . writeTBQueue oq . (Nothing,Just rhId,))
      -- update session state
      logInfo $ "Remote host session started for " <> tshow rhId
      chatModifyVar remoteHostSessions $ M.adjust (\rhs -> rhs {remoteHostClient = Just remoteHostClient}) rhId
      chatWriteVar currentRemoteHost $ Just rhId
      toView $ CRRemoteHostConnected RemoteHostInfo
        { remoteHostId = rhId,
          storePath = storePath,
          displayName = remoteDeviceName remoteHostClient,
          remoteCtrlOOB = RemoteCtrlOOB {fingerprint, displayName=rcName},
          sessionActive = True
        }

    genSessionCredentials RemoteHost {caKey, caCert} = do
      sessionCreds <- genCredentials (Just parent) (0, 24) "Session"
      pure . tlsCredentials $ sessionCreds :| [parent]
      where
        parent = (C.signatureKeyPair caKey, caCert)

-- | Atomically check/register session and prepare its task list
startRemoteHostSession :: ChatMonad m => RemoteHost -> m Tasks
startRemoteHostSession RemoteHost {remoteHostId, storePath} = withNoRemoteHostSession remoteHostId $ \sessions -> do
  remoteHostTasks <- newTVar []
  TM.insert remoteHostId RemoteHostSession {remoteHostTasks, storePath, remoteHostClient = Nothing} sessions
  pure $ Right remoteHostTasks

closeRemoteHostSession :: ChatMonad m => RemoteHostId -> m ()
closeRemoteHostSession rhId = do
  logNote $ "Closing remote host session for " <> tshow rhId
  chatModifyVar currentRemoteHost $ \cur -> if cur == Just rhId then Nothing else cur -- only wipe the closing RH
  session <- withRemoteHostSession rhId $ \sessions rhs -> Right rhs <$ TM.delete rhId sessions
  cancelRemoteHostSession session

cancelRemoteHostSession :: MonadUnliftIO m => RemoteHostSession -> m ()
cancelRemoteHostSession RemoteHostSession {remoteHostTasks, remoteHostClient} = do
  cancelTasks remoteHostTasks
  mapM_ closeRemoteHostClient remoteHostClient

createRemoteHost :: ChatMonad m => m RemoteHostInfo
createRemoteHost = do
  ((_, caKey), caCert) <- liftIO $ genCredentials Nothing (-25, 24 * 365) "Host"
  storePath <- liftIO randomStorePath
  let remoteName = "" -- will be passed from remote host in hello
  remoteHostId <- withStore' $ \db -> insertRemoteHost db storePath remoteName caKey caCert
  localName <- chatReadVar localDeviceName
  let remoteCtrlOOB = RemoteCtrlOOB {fingerprint = C.certificateFingerprint caCert, displayName = localName}
  pure RemoteHostInfo {remoteHostId, storePath, displayName = remoteName, remoteCtrlOOB, sessionActive = False}

-- | Generate a random 16-char filepath without / in it by using base64url encoding.
randomStorePath :: IO FilePath
randomStorePath = B.unpack . B64U.encode <$> getRandomBytes 12

listRemoteHosts :: ChatMonad m => m [RemoteHostInfo]
listRemoteHosts = do
  active <- chatReadVar remoteHostSessions
  rcName <- chatReadVar localDeviceName
  map (rhInfo active rcName) <$> withStore' getRemoteHosts
  where
    rhInfo active rcName rh@RemoteHost {remoteHostId} =
      remoteHostInfo rh (M.member remoteHostId active) rcName

remoteHostInfo :: RemoteHost -> Bool -> Text -> RemoteHostInfo
remoteHostInfo RemoteHost {remoteHostId, storePath, displayName, caCert} sessionActive rcName =
  let remoteCtrlOOB = RemoteCtrlOOB {fingerprint = C.certificateFingerprint caCert, displayName = rcName}
   in RemoteHostInfo {remoteHostId, storePath, displayName, remoteCtrlOOB, sessionActive}

deleteRemoteHost :: ChatMonad m => RemoteHostId -> m ()
deleteRemoteHost rhId = do
  RemoteHost {storePath} <- withStore (`getRemoteHost` rhId)
  chatReadVar filesFolder >>= \case
    Just baseDir -> do
      let hostStore = baseDir </> storePath
      logError $ "TODO: remove " <> tshow hostStore
    Nothing -> logWarn "Local file store not available while deleting remote host"
  withStore' (`deleteRemoteHostRecord` rhId)

processRemoteCommand :: ChatMonad m => RemoteHostId -> RemoteHostSession -> ByteString -> m ChatResponse
processRemoteCommand remoteHostId RemoteHostSession {remoteHostClient = Just rhc} s = liftRH remoteHostId $ remoteSend rhc s
processRemoteCommand _ _ _ = pure $ chatCmdError Nothing "remote command sent before session started"

liftRH :: ChatMonad m => RemoteHostId -> ExceptT RemoteProtocolError IO a -> m a
liftRH rhId = liftError (ChatErrorRemoteHost rhId . RHProtocolError)

-- * Mobile side

startRemoteCtrl :: forall m . ChatMonad m => (ByteString -> m ChatResponse) -> m ()
startRemoteCtrl execChatCommand = do
  logInfo "Starting remote host"
  checkNoRemoteCtrlSession -- tiny race with the final @chatWriteVar@ until the setup finishes and supervisor spawned
  discovered <- newTVarIO mempty
  discoverer <- async $ discoverRemoteCtrls discovered -- TODO extract to a controller service singleton
  size <- asks $ tbqSize . config
  remoteOutputQ <- newTBQueueIO size
  accepted <- newEmptyTMVarIO
  supervisor <- async $ runHost discovered accepted $ handleRemoteCommand execChatCommand remoteOutputQ
  chatWriteVar remoteCtrlSession $ Just RemoteCtrlSession {discoverer, supervisor, hostServer = Nothing, discovered, accepted, remoteOutputQ}

-- | Track remote host lifecycle in controller session state and signal UI on its progress
runHost :: ChatMonad m => TM.TMap C.KeyHash TransportHost -> TMVar RemoteCtrlId -> (HTTP2Request -> m ()) -> m ()
runHost discovered accepted handleHttp = do
  remoteCtrlId <- atomically (readTMVar accepted) -- wait for ???
  rc@RemoteCtrl {fingerprint} <- withStore (`getRemoteCtrl` remoteCtrlId)
  source <- atomically $ TM.lookup fingerprint discovered >>= maybe retry pure -- wait for location of the matching fingerprint
  toView $ CRRemoteCtrlConnecting $ remoteCtrlInfo rc False
  atomically $ writeTVar discovered mempty -- flush unused sources
  server <- async $ Discovery.connectRevHTTP2 source fingerprint handleHttp -- spawn server for remote protocol commands
  chatModifyVar remoteCtrlSession $ fmap $ \s -> s {hostServer = Just server}
  toView $ CRRemoteCtrlConnected $ remoteCtrlInfo rc True
  _ <- waitCatch server -- wait for the server to finish
  chatWriteVar remoteCtrlSession Nothing
  toView CRRemoteCtrlStopped

handleRemoteCommand :: forall m . ChatMonad m => (ByteString -> m ChatResponse) -> TBQueue ChatResponse -> HTTP2Request -> m ()
handleRemoteCommand execChatCommand remoteOutputQ HTTP2Request {request, reqBody, sendResponse} = do
  logDebug "handleRemoteCommand"
  liftRC (tryRemoteError parseRequest) >>= \case
    Right (getNext, rc) -> processCommand getNext rc `catchAny` (reply . RRProtocolError . RPEException . tshow)
    Left e -> reply $ RRProtocolError e
  where
    parseRequest :: ExceptT RemoteProtocolError IO (GetChunk, RemoteCommand)
    parseRequest = do
      (header, getNext) <- parseHTTP2Body request reqBody
      (getNext,) <$> liftEitherWith (RPEInvalidJSON . T.pack) (J.eitherDecodeStrict' header)
    processCommand :: GetChunk -> RemoteCommand -> m ()
    processCommand getNext = \case
      RCHello {deviceName = desktopName} -> handleHello desktopName >>= reply
      RCSend {command} -> handleSend execChatCommand command >>= reply
      RCRecv {wait = time} -> handleRecv time remoteOutputQ >>= reply
      RCStoreFile {fileSize, encrypt} -> handleStoreFile fileSize encrypt getNext >>= reply
      RCGetFile {filePath} -> handleGetFile filePath replyWith
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
  ctrlId <- getRemoteCtrlSessionId
  withStore' $ \db -> setRemoteCtrlName db ctrlId desktopName
  mobileName <- chatReadVar localDeviceName
  pure RRHello {encoding = localEncoding, deviceName = mobileName}

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

handleStoreFile :: ChatMonad m => Word32 -> Maybe Bool -> GetChunk -> m RemoteResponse
handleStoreFile _fileSize _encrypt _getNext = error "TODO" <$ logError "TODO: handleStoreFile"

handleGetFile :: ChatMonad m => FilePath -> Respond m -> m ()
handleGetFile path reply = do
  logDebug $ "GetFile: " <> tshow path
  withFile path ReadMode $ \h -> do
    fileSize' <- hFileSize h
    when (fileSize' > toInteger (maxBound :: Word32)) $ throwIO RPEFileTooLarge
    let fileSize = fromInteger fileSize'
    reply RRFile {fileSize} $ \send -> hSendFile h send fileSize

-- TODO the problem with this code was that it wasn't clear where the recursion can happen,
-- by splitting receiving and processing to two functions it becomes clear
discoverRemoteCtrls :: ChatMonad m => TM.TMap C.KeyHash TransportHost -> m ()
discoverRemoteCtrls discovered = Discovery.withListener $ receive >=> process
  where
    -- TODO how would it receive more than one fingerprint?
    receive sock =
      Discovery.recvAnnounce sock >>= \case
        (SockAddrInet _sockPort sockAddr, invite) -> case strDecode invite of
          -- TODO it is probably better to report errors to view here
          Left _ -> receive sock
          Right fingerprint -> pure (sockAddr, fingerprint)
        _nonV4 -> receive sock
    process (sockAddr, fingerprint) = do
      let addr = THIPv4 (hostAddressToTuple sockAddr)
      ifM
        (atomically $ TM.member fingerprint discovered)
        (logDebug $ "Fingerprint already known: " <> tshow (addr, fingerprint))
        ( do
            logInfo $ "New fingerprint announced: " <> tshow (addr, fingerprint)
            atomically $ TM.insert fingerprint addr discovered
        )
      -- TODO we check fingerprint for duplicate where id doesn't matter - to prevent re-insert - and don't check to prevent duplicate events,
      -- so UI now will have to check for duplicates again
      withStore' (`getRemoteCtrlByFingerprint` fingerprint) >>= \case
        Nothing -> toView $ CRRemoteCtrlAnnounce fingerprint -- unknown controller, ui "register" action required
        -- TODO Maybe Bool is very confusing - the intent is very unclear here
        Just found@RemoteCtrl {remoteCtrlId, accepted = storedChoice} -> case storedChoice of
          Nothing -> toView $ CRRemoteCtrlFound $ remoteCtrlInfo found False -- first-time controller, ui "accept" action required
          Just False -> pure () -- skipping a rejected item
          Just True ->
            chatReadVar remoteCtrlSession >>= \case
              Nothing -> toView . CRChatError Nothing . ChatError $ CEInternalError "Remote host found without running a session"
              Just RemoteCtrlSession {accepted} -> atomically $ void $ tryPutTMVar accepted remoteCtrlId -- previously accepted controller, connect automatically

listRemoteCtrls :: ChatMonad m => m [RemoteCtrlInfo]
listRemoteCtrls = do
  active <-
    chatReadVar remoteCtrlSession
      $>>= \RemoteCtrlSession {accepted} -> atomically $ tryReadTMVar accepted
  map (rcInfo active) <$> withStore' getRemoteCtrls
  where
    rcInfo activeRcId rc@RemoteCtrl {remoteCtrlId} =
      remoteCtrlInfo rc $ activeRcId == Just remoteCtrlId

remoteCtrlInfo :: RemoteCtrl -> Bool -> RemoteCtrlInfo
remoteCtrlInfo RemoteCtrl {remoteCtrlId, displayName, fingerprint, accepted} sessionActive =
  RemoteCtrlInfo {remoteCtrlId, displayName, fingerprint, accepted, sessionActive}

acceptRemoteCtrl :: ChatMonad m => RemoteCtrlId -> m ()
acceptRemoteCtrl rcId = do
  -- TODO check it exists, check the ID is the same as in session
  RemoteCtrlSession {accepted} <- getRemoteCtrlSession
  withStore' $ \db -> markRemoteCtrlResolution db rcId True
  atomically . void $ tryPutTMVar accepted rcId -- the remote host can now proceed with connection

rejectRemoteCtrl :: ChatMonad m => RemoteCtrlId -> m ()
rejectRemoteCtrl rcId = do
  withStore' $ \db -> markRemoteCtrlResolution db rcId False
  RemoteCtrlSession {discoverer, supervisor} <- getRemoteCtrlSession
  cancel discoverer
  cancel supervisor

stopRemoteCtrl :: ChatMonad m => m ()
stopRemoteCtrl = do
  rcs <- getRemoteCtrlSession
  cancelRemoteCtrlSession rcs $ chatWriteVar remoteCtrlSession Nothing

cancelRemoteCtrlSession_ :: MonadUnliftIO m => RemoteCtrlSession -> m ()
cancelRemoteCtrlSession_ rcs = cancelRemoteCtrlSession rcs $ pure ()

cancelRemoteCtrlSession :: MonadUnliftIO m => RemoteCtrlSession -> m () -> m ()
cancelRemoteCtrlSession RemoteCtrlSession {discoverer, supervisor, hostServer} cleanup = do
  cancel discoverer -- may be gone by now
  case hostServer of
    Just host -> cancel host -- supervisor will clean up
    Nothing -> do
      cancel supervisor -- supervisor is blocked until session progresses
      cleanup

deleteRemoteCtrl :: ChatMonad m => RemoteCtrlId -> m ()
deleteRemoteCtrl rcId = do
  checkNoRemoteCtrlSession
  -- TODO check it exists
  withStore' (`deleteRemoteCtrlRecord` rcId)

getRemoteCtrlSession :: ChatMonad m => m RemoteCtrlSession
getRemoteCtrlSession =
  chatReadVar remoteCtrlSession >>= maybe (throwError $ ChatErrorRemoteCtrl RCEInactive) pure

getRemoteCtrlSessionId :: ChatMonad m => m RemoteCtrlId
getRemoteCtrlSessionId = do
  RemoteCtrlSession {accepted} <- getRemoteCtrlSession
  atomically (tryReadTMVar accepted) >>= maybe (throwError $ ChatErrorRemoteCtrl RCEInactive) pure

checkNoRemoteCtrlSession :: ChatMonad m => m ()
checkNoRemoteCtrlSession =
  chatReadVar remoteCtrlSession >>= maybe (pure ()) (\_ -> throwError $ ChatErrorRemoteCtrl RCEBusy)

utf8String :: [Char] -> ByteString
utf8String = encodeUtf8 . T.pack
{-# INLINE utf8String #-}

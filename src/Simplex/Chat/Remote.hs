{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Simplex.Chat.Remote where

import Control.Monad
import Control.Monad.Except
import Control.Monad.IO.Class
import Control.Monad.Reader (asks)
import Control.Monad.STM (retry)
import Crypto.Random (getRandomBytes)
import qualified Data.Aeson as J
import qualified Data.Binary.Builder as Binary
import Data.ByteString (ByteString)
import qualified Data.ByteString.Base64.URL as B64U
import qualified Data.ByteString.Char8 as B
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map.Strict as M
import qualified Network.HTTP.Types as HTTP
import Network.HTTP.Types.Status (ok200, badRequest400)
import qualified Network.HTTP2.Client as HTTP2Client
import qualified Network.HTTP2.Server as HTTP2Server
import Network.Socket (SockAddr (..), hostAddressToTuple)
import Simplex.Chat.Controller
import qualified Simplex.Chat.Remote.Discovery as Discovery
import Simplex.Messaging.Protocol (CorrId)
import Simplex.Chat.Remote.Types
import Simplex.Chat.Store.Remote
import Simplex.Chat.Types
import qualified Simplex.Messaging.Crypto as C
import Simplex.Messaging.Encoding.String (StrEncoding (..))
import qualified Simplex.Messaging.TMap as TM
import Simplex.Messaging.Transport.Client (TransportHost (..))
import Simplex.Messaging.Transport.Credentials (genCredentials, tlsCredentials)
import Simplex.Messaging.Transport.HTTP2 (HTTP2Body (..))
import Simplex.Messaging.Transport.HTTP2.Client (HTTP2Client)
import qualified Simplex.Messaging.Transport.HTTP2.Client as HTTP2
import qualified Simplex.Messaging.Transport.HTTP2.Server as HTTP2
import Simplex.Messaging.Util (bshow)
import System.Directory (getFileSize)
import UnliftIO

withRemoteHostSession :: (ChatMonad m) => RemoteHostId -> (RemoteHostSession -> m a) -> m a
withRemoteHostSession remoteHostId action = do
  chatReadVar remoteHostSessions >>= maybe err action . M.lookup remoteHostId
  where
    err = throwError $ ChatErrorRemoteHost remoteHostId RHMissing

withRemoteHost :: (ChatMonad m) => RemoteHostId -> (RemoteHost -> m a) -> m a
withRemoteHost remoteHostId action =
  withStore' (`getRemoteHost` remoteHostId) >>= \case
    Nothing -> throwError $ ChatErrorRemoteHost remoteHostId RHMissing
    Just rh -> action rh

startRemoteHost :: (ChatMonad m) => RemoteHostId -> m ChatResponse
startRemoteHost remoteHostId = do
  M.lookup remoteHostId <$> chatReadVar remoteHostSessions >>= \case
    Just _ -> throwError $ ChatErrorRemoteHost remoteHostId RHBusy
    Nothing -> withRemoteHost remoteHostId run
  where
    run RemoteHost {storePath, caKey, caCert} = do
      finished <- newTVarIO False
      announcer <- async $ do
        cleanup <- toIO $ do
          atomically $ writeTVar finished True
          closeRemoteHostSession remoteHostId >>= toView
        let parent = (C.signatureKeyPair caKey, caCert)
        sessionCreds <- liftIO $ genCredentials (Just parent) (0, 24) "Session"
        let (fingerprint, credentials) = tlsCredentials $ sessionCreds :| [parent]
        Discovery.announceRevHTTP2 cleanup fingerprint credentials >>= \case
          Left todo'err -> liftIO cleanup -- TODO: log error
          Right ctrlClient -> do
            chatModifyVar remoteHostSessions $ M.insert remoteHostId RemoteHostSessionStarted {storePath, ctrlClient}
            chatWriteVar currentRemoteHost $ Just remoteHostId
            sendHello ctrlClient >>= \case
              Left todo'err -> liftIO cleanup
              Right todo'ok -> do
                _ <- asks outputQ >>= async . pumpRemote finished ctrlClient "/output" (\(ci, _noHost :: Maybe (), cr) -> (ci, Just remoteHostId, cr))
                _ <- asks notifyQ >>= async . pumpRemote finished ctrlClient "/notify" id
                toView CRRemoteHostConnected {remoteHostId}
      chatModifyVar remoteHostSessions $ M.insert remoteHostId RemoteHostSessionStarting {announcer}
      pure CRRemoteHostStarted {remoteHostId}

sendHello :: (ChatMonad m) => HTTP2Client -> m (Either HTTP2.HTTP2ClientError HTTP2.HTTP2Response)
sendHello http = liftIO (HTTP2.sendRequest http req Nothing)
  where
    req = HTTP2Client.requestNoBody "GET" "/" mempty

pumpRemote :: (ChatMonad m, J.FromJSON a) => TVar Bool -> HTTP2Client -> ByteString -> (a -> b) -> TBQueue b -> m ()
pumpRemote finished http path f queue = loop
  where
    loop = do
      liftIO (HTTP2.sendRequest http req Nothing) >>= \case
        Left e -> pure ()
        Right HTTP2.HTTP2Response {respBody=HTTP2Body{bodyHead}} ->
          case J.eitherDecodeStrict' bodyHead of
            Left e -> pure ()
            Right o -> atomically $ writeTBQueue queue (f o)
      readTVarIO finished >>= (`unless` loop)
    req = HTTP2Client.requestNoBody "GET" path mempty

closeRemoteHostSession :: (ChatMonad m) => RemoteHostId -> m ChatResponse
closeRemoteHostSession remoteHostId = withRemoteHostSession remoteHostId $ \session -> do
  case session of
    RemoteHostSessionStarting {announcer} -> cancel announcer
    RemoteHostSessionStarted {ctrlClient} -> liftIO (HTTP2.closeHTTP2Client ctrlClient)
  chatWriteVar currentRemoteHost Nothing
  chatModifyVar remoteHostSessions $ M.delete remoteHostId
  pure CRRemoteHostStopped { remoteHostId }

createRemoteHost :: (ChatMonad m) => m ChatResponse
createRemoteHost = do
  let displayName = "TODO" -- you don't have remote host name here, it will be passed from remote host
  ((_, caKey), caCert) <- liftIO $ genCredentials Nothing (-25, 24 * 365) displayName
  storePath <- liftIO randomStorePath
  remoteHostId <- withStore' $ \db -> insertRemoteHost db storePath displayName caKey caCert
  let oobData =
        RemoteCtrlOOB
          { caFingerprint = C.certificateFingerprint caCert
          }
  pure CRRemoteHostCreated {remoteHostId, oobData}

-- | Generate a random 16-char filepath without / in it by using base64url encoding.
randomStorePath :: IO FilePath
randomStorePath = B.unpack . B64U.encode <$> getRandomBytes 12

listRemoteHosts :: (ChatMonad m) => m ChatResponse
listRemoteHosts = do
  stored <- withStore' getRemoteHosts
  active <- chatReadVar remoteHostSessions
  pure $ CRRemoteHostList $ do
    RemoteHost {remoteHostId, storePath, displayName} <- stored
    let sessionActive = M.member remoteHostId active
    pure RemoteHostInfo {remoteHostId, storePath, displayName, sessionActive}

deleteRemoteHost :: (ChatMonad m) => RemoteHostId -> m ChatResponse
deleteRemoteHost remoteHostId = withRemoteHost remoteHostId $ \rh -> do
  -- TODO: delete files
  withStore' $ \db -> deleteRemoteHostRecord db remoteHostId
  pure CRRemoteHostDeleted {remoteHostId}

processRemoteCommand :: (ChatMonad m) => RemoteHostSession -> (ByteString, ChatCommand) -> m ChatResponse
processRemoteCommand RemoteHostSessionStarting {} _ = error "TODO: sending remote commands before session started"
processRemoteCommand RemoteHostSessionStarted {ctrlClient} (s, cmd) =
  -- XXX: intercept and filter some commands
  -- TODO: store missing files on remote host
  relayCommand ctrlClient s

relayCommand :: (ChatMonad m) => HTTP2Client -> ByteString -> m ChatResponse
relayCommand http s =
  postBytestring Nothing http "/command" mempty s >>= \case
    Left e -> error "TODO: http2chatError"
    Right HTTP2.HTTP2Response {respBody = HTTP2Body {bodyHead}} -> do
      remoteChatResponse <-
        if iTax
          then case J.eitherDecodeStrict bodyHead of -- XXX: large JSONs can overflow into buffered chunks
            Left e -> error "TODO: json2chatError" e
            Right (raw :: J.Value) -> case J.fromJSON (sum2tagged raw) of
              J.Error e -> error "TODO: json2chatError" e
              J.Success cr -> pure cr
          else case J.eitherDecodeStrict bodyHead of -- XXX: large JSONs can overflow into buffered chunks
            Left e -> error "TODO: json2chatError" e
            Right cr -> pure cr
      case remoteChatResponse of
        -- TODO: intercept file responses and fetch files when needed
        -- XXX: is that even possible, to have a file response to a command?
        _ -> pure remoteChatResponse
  where
    iTax = True -- TODO: get from RemoteHost
    -- XXX: extract to http2 transport
    postBytestring timeout c path hs body = liftIO $ HTTP2.sendRequest c req timeout
      where
        req = HTTP2Client.requestBuilder "POST" path hs (Binary.fromByteString body)

-- | Convert swift single-field sum encoding into tagged/discriminator-field
sum2tagged :: J.Value -> J.Value
sum2tagged = \case
  J.Object todo'convert -> J.Object todo'convert
  skip -> skip

storeRemoteFile :: (ChatMonad m) => HTTP2Client -> FilePath -> m ChatResponse
storeRemoteFile http localFile = do
  postFile Nothing http "/store" mempty localFile >>= \case
    Left todo'err -> error "TODO: http2chatError"
    Right HTTP2.HTTP2Response {response} -> case HTTP.statusCode <$> HTTP2Client.responseStatus response of
      Just 200 -> pure $ CRCmdOk Nothing
      todo'notOk -> error "TODO: http2chatError"
  where
    postFile timeout c path hs file = liftIO $ do
      fileSize <- fromIntegral <$> getFileSize file
      HTTP2.sendRequest c (req fileSize) timeout
      where
        req size = HTTP2Client.requestFile "POST" path hs (HTTP2Client.FileSpec file 0 size)

fetchRemoteFile :: (ChatMonad m) => HTTP2Client -> FilePath -> FileTransferId -> m ChatResponse
fetchRemoteFile http storePath remoteFileId = do
  liftIO (HTTP2.sendRequest http req Nothing) >>= \case
    Left e -> error "TODO: http2chatError"
    Right HTTP2.HTTP2Response {respBody} -> do
      error "TODO: stream body into a local file" -- XXX: consult headers for a file name?
  where
    req = HTTP2Client.requestNoBody "GET" path mempty
    path = "/fetch/" <> bshow remoteFileId

processControllerRequest :: forall m . (ChatMonad m) => (ByteString -> m ChatResponse) -> RemoteCtrlId -> HTTP2.HTTP2Request -> m ()
processControllerRequest execChatCommand rc HTTP2.HTTP2Request {request, reqBody = HTTP2Body {bodyHead}, sendResponse} = case (HTTP2Server.requestMethod request, HTTP2Server.requestPath request) of
  (Just "GET", Just "/") -> getHello
  (Just "POST", Just "/command") -> postCommand
  (Just "GET", Just "/output") -> getOutput
  (Just "GET", Just "/notify") -> getNotify
  (Just "PUT", Just "/store") -> putStore
  (Just "GET", Just "/fetch") -> getFetch
  unexpected -> respondWith badRequest400 $ "unexpected method/path: " <> Binary.putStringUtf8 (show unexpected)
  where
    getHello = respond "TODO: getHello"
    postCommand = execChatCommand bodyHead >>= respondJSON
    getOutput = respond "TODO: getOutput"
    getNotify = asks notifyQ >>= atomically . readTBQueue >>= respondJSON
    putStore = respond "TODO: putStore"
    getFetch = respond "TODO: getFetch"

    respondJSON :: J.ToJSON a => a -> m ()
    respondJSON = respond . Binary.fromLazyByteString . J.encode
    respond = respondWith ok200
    respondWith status = liftIO . sendResponse . HTTP2Server.responseBuilder status []


-- * ChatRequest handlers

startRemoteCtrl :: (ChatMonad m) => (ByteString -> m ChatResponse) -> m ChatResponse
startRemoteCtrl execChatCommand =
  chatReadVar remoteCtrlSession >>= \case
    Just _busy -> throwError $ ChatErrorRemoteCtrl RCEBusy
    Nothing -> do
      size <- asks $ tbqSize . config
      remoteOutputQ <- newTBQueueIO size
      remoteNotifyQ <- newTBQueueIO size
      discovered <- newTVarIO mempty
      discoverer <- async $ discoverRemoteCtrls discovered
      accepted <- newEmptyTMVarIO
      supervisor <- async $ do
        remoteCtrlId <- atomically (readTMVar accepted)
        withRemoteCtrl remoteCtrlId $ \RemoteCtrl {displayName, fingerprint} -> do
          source <- atomically $ TM.lookup fingerprint discovered >>= maybe retry pure
          toView $ CRRemoteCtrlConnecting {remoteCtrlId, displayName}
          atomically $ writeTVar discovered mempty -- flush unused sources
          server <- async $ Discovery.connectRevHTTP2 source fingerprint (processControllerRequest execChatCommand remoteCtrlId)
          chatModifyVar remoteCtrlSession $ fmap $ \s -> s {hostServer = Just server}
          toView $ CRRemoteCtrlConnected {remoteCtrlId, displayName}
          _ <- waitCatch server
          chatWriteVar remoteCtrlSession Nothing
          toView $ CRRemoteCtrlStopped {remoteCtrlId}
      chatWriteVar remoteCtrlSession $ Just RemoteCtrlSession {discoverer, supervisor, hostServer = Nothing, discovered, accepted, remoteOutputQ, remoteNotifyQ}
      pure $ CRRemoteCtrlStarted Nothing

discoverRemoteCtrls :: (ChatMonad m) => TM.TMap C.KeyHash TransportHost -> m ()
discoverRemoteCtrls discovered = Discovery.openListener >>= go
  where
    go sock =
      Discovery.recvAnnounce sock >>= \case
        (SockAddrInet _port addr, invite) -> case strDecode invite of
          Left _ -> go sock -- ignore malformed datagrams
          Right fingerprint -> do
            atomically $ TM.insert fingerprint (THIPv4 $ hostAddressToTuple addr) discovered
            withStore' (`getRemoteCtrlByFingerprint` fingerprint) >>= \case
              Nothing -> toView $ CRRemoteCtrlAnnounce fingerprint -- unknown controller, ui action required
              Just found@RemoteCtrl {remoteCtrlId, accepted=storedChoice} -> case storedChoice of
                Nothing -> toView $ CRRemoteCtrlFound found -- first-time controller, ui action required
                Just False -> pure () -- skipping a rejected item
                Just True -> chatReadVar remoteCtrlSession >>= \case
                  Nothing -> toView . CRChatError Nothing . ChatError $ CEInternalError "Remote host found without running a session"
                  Just RemoteCtrlSession {accepted} -> atomically $ void $ tryPutTMVar accepted remoteCtrlId -- previously accepted controller, connect automatically
        _nonV4 -> go sock

registerRemoteCtrl :: (ChatMonad m) => RemoteCtrlOOB -> m ChatResponse
registerRemoteCtrl RemoteCtrlOOB {caFingerprint} = do
  let displayName = "TODO" -- maybe include into OOB data
  remoteCtrlId <- withStore' $ \db -> insertRemoteCtrl db displayName caFingerprint
  pure $ CRRemoteCtrlRegistered {remoteCtrlId}

listRemoteCtrls :: (ChatMonad m) => m ChatResponse
listRemoteCtrls = do
  stored <- withStore' getRemoteCtrls
  active <-
    chatReadVar remoteCtrlSession >>= \case
      Nothing -> pure Nothing
      Just RemoteCtrlSession {accepted} -> atomically (tryReadTMVar accepted)
  pure $ CRRemoteCtrlList $ do
    RemoteCtrl {remoteCtrlId, displayName} <- stored
    let sessionActive = active == Just remoteCtrlId
    pure RemoteCtrlInfo {remoteCtrlId, displayName, sessionActive}

acceptRemoteCtrl :: (ChatMonad m) => RemoteCtrlId -> m ChatResponse
acceptRemoteCtrl remoteCtrlId = do
  withStore' $ \db -> markRemoteCtrlResolution db remoteCtrlId True
  chatReadVar remoteCtrlSession >>= \case
    Nothing -> throwError $ ChatErrorRemoteCtrl RCEInactive
    Just RemoteCtrlSession {accepted} -> atomically . void $ tryPutTMVar accepted remoteCtrlId -- the remote host can now proceed with connection
  pure $ CRRemoteCtrlAccepted {remoteCtrlId}

rejectRemoteCtrl :: (ChatMonad m) => RemoteCtrlId -> m ChatResponse
rejectRemoteCtrl remoteCtrlId = do
  withStore' $ \db -> markRemoteCtrlResolution db remoteCtrlId False
  chatReadVar remoteCtrlSession >>= \case
    Nothing -> throwError $ ChatErrorRemoteCtrl RCEInactive
    Just RemoteCtrlSession {discoverer, supervisor} -> do
      cancel discoverer
      cancel supervisor
  pure $ CRRemoteCtrlRejected {remoteCtrlId}

stopRemoteCtrl :: (ChatMonad m) => RemoteCtrlId -> m ChatResponse
stopRemoteCtrl remoteCtrlId =
  chatReadVar remoteCtrlSession >>= \case
    Nothing -> throwError $ ChatErrorRemoteCtrl RCEInactive
    Just RemoteCtrlSession {discoverer, supervisor, hostServer} -> do
      cancel discoverer -- may be gone by now
      case hostServer of
        Just host -> cancel host -- supervisor will clean up
        Nothing -> do
          cancel supervisor -- supervisor is blocked until session progresses
          chatWriteVar remoteCtrlSession Nothing
          toView $ CRRemoteCtrlStopped {remoteCtrlId}
      pure $ CRCmdOk Nothing

deleteRemoteCtrl :: (ChatMonad m) => RemoteCtrlId -> m ChatResponse
deleteRemoteCtrl remoteCtrlId =
  chatReadVar remoteCtrlSession >>= \case
    Nothing -> do
      withStore' $ \db -> deleteRemoteCtrlRecord db remoteCtrlId
      pure $ CRRemoteCtrlDeleted {remoteCtrlId}
    Just _ -> throwError $ ChatErrorRemoteCtrl RCEBusy

withRemoteCtrl :: (ChatMonad m) => RemoteCtrlId -> (RemoteCtrl -> m a) -> m a
withRemoteCtrl remoteCtrlId action =
  withStore' (`getRemoteCtrl` remoteCtrlId) >>= \case
    Nothing -> throwError $ ChatErrorRemoteCtrl RCEMissing {remoteCtrlId}
    Just rc -> action rc

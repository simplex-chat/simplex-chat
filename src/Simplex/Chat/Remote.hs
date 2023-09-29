{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Simplex.Chat.Remote where

import Control.Monad.Except
import Control.Monad.IO.Class
import qualified Data.Aeson as J
import qualified Data.Binary.Builder as Binary
import Data.ByteString.Char8 (ByteString)
import qualified Data.Map.Strict as M
import qualified Network.HTTP.Types as HTTP
import qualified Network.HTTP2.Client as HTTP2Client
import Network.Socket (SockAddr (..), hostAddressToTuple)
import Simplex.Chat.Controller
import qualified Simplex.Chat.Remote.Discovery as Discovery
import Simplex.Chat.Remote.Types
import Simplex.Chat.Store.Remote
import Simplex.Chat.Types
import qualified Simplex.Messaging.Agent.Store.SQLite.DB as DB
import qualified Simplex.Messaging.Crypto as C
import Simplex.Messaging.Encoding.String (StrEncoding (..))
import qualified Simplex.Messaging.TMap as TM
import Simplex.Messaging.Transport.Client (TransportHost (..))
import Simplex.Messaging.Transport.HTTP2 (HTTP2Body (..))
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

startRemoteHost :: (ChatMonad m) => RemoteHostId -> m ChatResponse
startRemoteHost remoteHostId = do
  RemoteHost {displayName = _, storePath, caKey, caCert} <- error "TODO: get from DB"
  (fingerprint :: ByteString, sessionCreds) <- error "TODO: derive session creds" (caKey, caCert)
  cleanup <- toIO $ chatModifyVar remoteHostSessions (M.delete remoteHostId)
  Discovery.runAnnouncer cleanup fingerprint sessionCreds >>= \case
    Left todo'err -> pure $ chatCmdError Nothing "TODO: Some HTTP2 error"
    Right ctrlClient -> do
      chatModifyVar remoteHostSessions $ M.insert remoteHostId RemoteHostSession {storePath, ctrlClient}
      pure $ CRRemoteHostStarted remoteHostId

closeRemoteHostSession :: (ChatMonad m) => RemoteHostId -> m ()
closeRemoteHostSession rh = withRemoteHostSession rh (liftIO . HTTP2.closeHTTP2Client . ctrlClient)

processRemoteCommand :: (ChatMonad m) => RemoteHostSession -> (ByteString, ChatCommand) -> m ChatResponse
processRemoteCommand rhs = \case
  -- XXX: intercept and filter some commands
  -- TODO: store missing files on remote host
  (s, _cmd) -> relayCommand rhs s

relayCommand :: (ChatMonad m) => RemoteHostSession -> ByteString -> m ChatResponse
relayCommand RemoteHostSession {ctrlClient} s =
  postBytestring Nothing ctrlClient "/relay" mempty s >>= \case
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

storeRemoteFile :: (ChatMonad m) => RemoteHostSession -> FilePath -> m ChatResponse
storeRemoteFile RemoteHostSession {ctrlClient} localFile = do
  postFile Nothing ctrlClient "/store" mempty localFile >>= \case
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

fetchRemoteFile :: (ChatMonad m) => RemoteHostSession -> FileTransferId -> m ChatResponse
fetchRemoteFile RemoteHostSession {ctrlClient, storePath} remoteFileId = do
  liftIO (HTTP2.sendRequest ctrlClient req Nothing) >>= \case
    Left e -> error "TODO: http2chatError"
    Right HTTP2.HTTP2Response {respBody} -> do
      error "TODO: stream body into a local file" -- XXX: consult headers for a file name?
  where
    req = HTTP2Client.requestNoBody "GET" path mempty
    path = "/fetch/" <> bshow remoteFileId

-- | Convert swift single-field sum encoding into tagged/discriminator-field
sum2tagged :: J.Value -> J.Value
sum2tagged = \case
  J.Object todo'convert -> J.Object todo'convert
  skip -> skip

processControllerCommand :: (ChatMonad m) => RemoteCtrlId -> HTTP2.HTTP2Request -> m ()
processControllerCommand rc req = error "TODO: processControllerCommand"

-- * ChatRequest handlers

startRemoteCtrl :: (ChatMonad m) => m ChatResponse
startRemoteCtrl =
  chatReadVar remoteCtrlSession >>= \case
    Just _busy -> throwError $ ChatErrorRemoteCtrl RCEBusy
    Nothing -> do
      accepted <- newEmptyTMVarIO
      discovered <- newTVarIO mempty
      listener <- async $ discoverRemoteCtrls discovered
      _supervisor <- async $ do
        uiEvent <- async $ atomically $ readTMVar accepted
        waitEitherCatchCancel listener uiEvent >>= \case
          Left _ -> pure () -- discover got cancelled or crashed on some UDP error
          Right (Left _) -> toView . CRChatError Nothing . ChatError $ CEException "Crashed while waiting for remote session confirmation"
          Right (Right remoteCtrlId) ->
            -- got connection confirmation
            atomically (TM.lookup remoteCtrlId discovered) >>= \case
              Nothing -> toView . CRChatError Nothing . ChatError $ CEInternalError "Remote session accepted without getting discovered first"
              Just (source, fingerprint) -> do
                atomically $ writeTVar discovered mempty -- flush unused sources
                host <- async $ runRemoteHost remoteCtrlId source fingerprint
                chatWriteVar remoteCtrlSession $ Just RemoteCtrlSession {ctrlAsync = host, accepted}
                _ <- waitCatch host
                chatWriteVar remoteCtrlSession Nothing
                toView $ CRRemoteCtrlStopped {remoteCtrlId}
      chatWriteVar remoteCtrlSession $ Just RemoteCtrlSession {ctrlAsync = listener, accepted}
      pure CRRemoteCtrlStarted

discoverRemoteCtrls :: (ChatMonad m) => TM.TMap RemoteCtrlId (TransportHost, C.KeyHash) -> m ()
discoverRemoteCtrls discovered = Discovery.openListener >>= go
  where
    go sock =
      Discovery.recvAnnounce sock >>= \case
        (SockAddrInet _port addr, invite) -> case strDecode invite of
          Left _ -> go sock -- ignore malformed datagrams
          Right fingerprint -> do
            withStore' (\db -> getRemoteCtrlByFingerprint (DB.conn db) fingerprint) >>= \case
              Nothing -> toView $ CRRemoteCtrlAnnounce fingerprint
              Just found@RemoteCtrl {remoteCtrlId} -> do
                atomically $ TM.insert remoteCtrlId (THIPv4 (hostAddressToTuple addr), fingerprint) discovered
                toView $ CRRemoteCtrlFound found
        _nonV4 -> go sock

runRemoteHost :: (ChatMonad m) => RemoteCtrlId -> TransportHost -> C.KeyHash -> m ()
runRemoteHost remoteCtrlId remoteCtrlHost fingerprint =
  Discovery.connectSessionHost remoteCtrlHost fingerprint $ Discovery.attachServer (processControllerCommand remoteCtrlId)

confirmRemoteCtrl :: (ChatMonad m) => RemoteCtrlId -> m ChatResponse
confirmRemoteCtrl remoteCtrlId =
  chatReadVar remoteCtrlSession >>= \case
    Nothing -> throwError $ ChatErrorRemoteCtrl RCEInactive
    Just RemoteCtrlSession {accepted} -> do
      withStore' $ \db -> markRemoteCtrlResolution (DB.conn db) remoteCtrlId True
      atomically $ putTMVar accepted remoteCtrlId -- the remote host can now proceed with connection
      pure $ CRRemoteCtrlAccepted {remoteCtrlId}

rejectRemoteCtrl :: (ChatMonad m) => RemoteCtrlId -> m ChatResponse
rejectRemoteCtrl remoteCtrlId =
  chatReadVar remoteCtrlSession >>= \case
    Nothing -> throwError $ ChatErrorRemoteCtrl RCEInactive
    Just RemoteCtrlSession {ctrlAsync} -> do
      withStore' $ \db -> markRemoteCtrlResolution (DB.conn db) remoteCtrlId False
      cancel ctrlAsync
      pure $ CRRemoteCtrlRejected {remoteCtrlId}

stopRemoteCtrl :: (ChatMonad m) => RemoteCtrlId -> m ChatResponse
stopRemoteCtrl remoteCtrlId =
  chatReadVar remoteCtrlSession >>= \case
    Nothing -> throwError $ ChatErrorRemoteCtrl RCEInactive
    Just RemoteCtrlSession {ctrlAsync} -> do
      cancel ctrlAsync
      pure CRRemoteCtrlStopped {remoteCtrlId}

disposeRemoteCtrl :: (ChatMonad m) => RemoteCtrlId -> m ChatResponse
disposeRemoteCtrl remoteCtrlId =
  chatReadVar remoteCtrlSession >>= \case
    Nothing -> do
      withStore' $ \db -> deleteRemoteCtrl (DB.conn db) remoteCtrlId
      pure $ CRRemoteCtrlDisposed {remoteCtrlId}
    Just _ -> throwError $ ChatErrorRemoteCtrl RCEBusy

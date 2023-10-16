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
import Data.ByteString (ByteString)
import qualified Data.ByteString.Base64.URL as B64U
import qualified Data.ByteString.Char8 as B
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Network.Socket (SockAddr (..), hostAddressToTuple)
import Simplex.Chat.Controller
import qualified Simplex.Chat.Remote.Discovery as Discovery
import Simplex.Chat.Remote.Types
import Simplex.Chat.Store.Remote
import qualified Simplex.Messaging.Crypto as C
import Simplex.Messaging.Encoding.String (StrEncoding (..))
import qualified Simplex.Messaging.TMap as TM
import Simplex.Messaging.Transport.Client (TransportHost (..))
import Simplex.Messaging.Transport.Credentials (genCredentials, tlsCredentials)
import Simplex.Messaging.Util (ifM, tshow, ($>>=), liftError, liftEitherError)
import System.FilePath ((</>))
import UnliftIO
import Simplex.Chat.Remote.Protocol

getRemoteHostSession :: ChatMonad m => RemoteHostId -> m RemoteHostSession
getRemoteHostSession rhId = chatReadVar remoteHostSessions >>= maybe err pure . M.lookup rhId
  where
    err = throwError $ ChatErrorRemoteHost rhId RHMissing

-- BUG: must be atomic with update or doesn't mean a thing
checkNoRemoteHostSession :: ChatMonad m => RemoteHostId -> m ()
checkNoRemoteHostSession rhId = chatReadVar remoteHostSessions >>= maybe (pure ()) err . M.lookup rhId
  where
    err _ = throwError $ ChatErrorRemoteHost rhId RHBusy

startRemoteHost :: ChatMonad m => RemoteHostId -> m ()
startRemoteHost rhId = do
  checkNoRemoteHostSession rhId
  rh <- withStore (`getRemoteHost` rhId)
  setupAsync <- async $ do
    finished <- newTVarIO False
    run rh finished `onException` cleanup finished
  chatModifyVar remoteHostSessions $ M.insert rhId RemoteHostSessionConnecting {setupAsync}
  where
    cleanup :: ChatMonad m => TVar Bool -> m ()
    cleanup finished = do
      atomically $ writeTVar finished True -- signal looped asyncs to stop
      -- TODO why this is not an error?
      M.lookup rhId <$> chatReadVar remoteHostSessions >>= \case
        Nothing -> logInfo $ "Session already closed for remote host " <> tshow rhId
        Just _ -> closeRemoteHostSession rhId >> toView (CRRemoteHostStopped rhId)
    run :: ChatMonad m => RemoteHost -> TVar Bool -> m ()
    run rh@RemoteHost {storePath} finished  = do
      (fingerprint, credentials) <- liftIO $ genSessionCredentials rh
      u <- askUnliftIO
      httpClient <- liftEitherError (ChatErrorRemoteCtrl . RCEHTTP2Error . show) $ Discovery.announceRevHTTP2 fingerprint credentials $ unliftIO u (cleanup finished)
      -- block until some client is connected or an error happens
      rcName <- chatReadVar localDeviceName
      remoteHostClient <- liftRH rhId $ createRemoteHostClient httpClient rcName
      -- update session state
      chatModifyVar remoteHostSessions $ M.insert rhId RemoteHostSessionStarted {remoteHostClient, storePath}
      chatWriteVar currentRemoteHost $ Just rhId
      -- set up message polling
      oq <- asks outputQ
      let toViewRemote = atomically . writeTBQueue oq . (Nothing,Just rhId,)
      pollRemote rhId finished remoteHostClient toViewRemote -- $ handleFileResponse remoteHostClient >=> toViewRemote

    genSessionCredentials RemoteHost {caKey, caCert} = do
      sessionCreds <- genCredentials (Just parent) (0, 24) "Session"
      pure . tlsCredentials $ sessionCreds :| [parent]
      where
        parent = (C.signatureKeyPair caKey, caCert)

pollRemote :: ChatMonad m => RemoteHostId -> TVar Bool -> RemoteHostClient -> (ChatResponse -> m ()) -> m ()
pollRemote rhId finished rhc action = loop `catchChatError` \e -> action (CRChatError Nothing e) >> loop
  where
    loop = do
      liftRH rhId (remoteRecv rhc pollTimeout) >>= mapM_ action
      readTVarIO finished >>= (`unless` loop)
    pollTimeout = 1000000

closeRemoteHostSession :: ChatMonad m => RemoteHostId -> m ()
closeRemoteHostSession remoteHostId = do
  session <- getRemoteHostSession remoteHostId
  logInfo $ "Closing remote host session for " <> tshow remoteHostId
  liftIO $ cancelRemoteHostSession session
  chatWriteVar currentRemoteHost Nothing
  chatModifyVar remoteHostSessions $ M.delete remoteHostId

cancelRemoteHostSession :: MonadUnliftIO m => RemoteHostSession -> m ()
cancelRemoteHostSession = \case
  -- TODO: RemoteHostSessionStarting -> pure ()
  RemoteHostSessionConnecting {setupAsync} -> cancel setupAsync
  RemoteHostSessionStarted {remoteHostClient} -> closeRemoteHostClient remoteHostClient

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
processRemoteCommand _ RemoteHostSessionConnecting {} _ = pure $ chatCmdError Nothing "remote command sent before session started"
processRemoteCommand remoteHostId RemoteHostSessionStarted {remoteHostClient = rhc} s = liftRH remoteHostId $ remoteSend rhc s
  -- liftRH remoteHostId $ uploadFile cmd >>= remoteSend rhc
  -- where
  --   uploadFile = \case
  --     -- TODO APISendMessage should only be used with host path already, and UI has to upload file first.
  --     -- The problem is that we cannot have different file names in host and controller, because it simply won't be able to show files.
  --     -- So we need to ask the host to store files BEFORE storing them in the app storage and use host names in the command and to store the file locally if it has to be shown,
  --     -- or don't even store it if it's not image/video.
  --     -- The current approach won't work.
  --     -- It also does not account for local file encryption.
  --     -- Also, local file encryption setting should be tracked in the controller, as otherwise host won't be able to decide what to do having received the upload command.
  --     APISendMessage {composedMessage = cm@ComposedMessage {fileSource = Just CryptoFile {filePath = localPath}}} -> do
  --       let encrypt = Nothing -- TODO
  --       source <- remoteStoreFile rhc localPath encrypt
  --       let cm' = cm {fileSource = Just source} :: ComposedMessage
  --       -- TODO we shouldn't manipulate commands like that
  --       pure $ B.takeWhile (/= '{') s <> B.toStrict (J.encode cm')
  --     -- SendFile cn ctrlPath -> fileCmd "/file" cn <$> remoteStoreFile remoteHostClient ctrlPath
  --     -- SendImage cn ctrlPath -> fileCmd "/image" cn <$> remoteStoreFile remoteHostClient ctrlPath
  --     _ -> pure s
  --   -- fileCmd cmdPfx cn hostPath = utf8String $ unwords [cmdPfx, chatNameStr cn, hostPath]

liftRH :: (MonadIO m, MonadError ChatError m) => RemoteHostId -> ExceptT RemoteClientError IO a -> m a
liftRH rhId = liftError (ChatErrorRemoteHost rhId . RHClientError)

-- -- TODO command/response pattern, remove REST conventions
-- processControllerRequest :: forall m. ChatMonad m => (ByteString -> m ChatResponse) -> HTTP2.HTTP2Request -> m ()
-- processControllerRequest execChatCommand HTTP2.HTTP2Request {request, reqBody, sendResponse} = do
--   logDebug $ "Remote controller request: " <> tshow (method <> " " <> path)
--   res <- tryChatError $ case (method, ps) of
--     ("GET", []) -> getHello
--     ("POST", ["send"]) -> sendCommand
--     ("GET", ["recv"]) -> recvMessage
--     ("PUT", ["store"]) -> storeFile
--     ("GET", ["fetch"]) -> fetchFile
--     unexpected -> respondWith Status.badRequest400 $ "unexpected method/path: " <> Binary.putStringUtf8 (show unexpected)
--   case res of
--     Left e -> logError $ "Error handling remote controller request: (" <> tshow (method <> " " <> path) <> "): " <> tshow e
--     Right () -> logDebug $ "Remote controller request: " <> tshow (method <> " " <> path) <> " OK"
--   where
--     method = fromMaybe "" $ HS.requestMethod request
--     path = fromMaybe "/" $ HS.requestPath request
--     (ps, query) = HTTP.decodePath path
--     getHello = respond "OK"
--     sendCommand = execChatCommand (bodyHead reqBody) >>= respondJSON
--     recvMessage =
--       chatReadVar remoteCtrlSession >>= \case
--         Nothing -> respondWith Status.internalServerError500 "session not active"
--         Just rcs -> atomically (readTBQueue $ remoteOutputQ rcs) >>= respondJSON
--     -- TODO liftEither storeFileQuery
--     storeFile = case storeFileQuery of
--       Left err -> respondWith Status.badRequest400 (Binary.putStringUtf8 err)
--       Right fileName -> do
--         baseDir <- fromMaybe "." <$> chatReadVar filesFolder
--         localPath <- uniqueCombine baseDir fileName
--         logDebug $ "Storing controller file to " <> tshow (baseDir, localPath)
--         writeBodyToFile localPath reqBody
--         let storeRelative = takeFileName localPath
--         respond $ Binary.putStringUtf8 storeRelative
--       where
--         storeFileQuery = parseField "file_name" $ A.many1 (A.satisfy $ not . isPathSeparator)
--     -- TODO move to ExceptT monad, catch errors in one place, convert errors to responses
--     fetchFile = case fetchFileQuery of
--       Left err -> respondWith Status.badRequest400 (Binary.putStringUtf8 err)
--       Right (userId, fileId) -> do
--         logInfo $ "Fetching file " <> tshow fileId <> " from user " <> tshow userId
--         x <- withStore' $ \db -> runExceptT $ do
--           user <- getUser db userId
--           getRcvFileTransfer db user fileId
--         -- TODO this error handling is very ad-hoc, there is no separation between Chat errors and responses
--         case x of
--           Right RcvFileTransfer {fileStatus = RFSComplete RcvFileInfo {filePath}} -> do
--             baseDir <- fromMaybe "." <$> chatReadVar filesFolder
--             let fullPath = baseDir </> filePath
--             size <- fromInteger <$> getFileSize fullPath
--             liftIO . sendResponse . HS.responseFile Status.ok200 mempty $ HS.FileSpec fullPath 0 size
--           Right _ -> respondWith Status.internalServerError500 "The requested file is not complete"
--           Left SEUserNotFound {} -> respondWith Status.notFound404 "User not found"
--           Left SERcvFileNotFound {} -> respondWith Status.notFound404 "File not found"
--           _ -> respondWith Status.internalServerError500 "Store error"
--       where
--         fetchFileQuery =
--           (,)
--             <$> parseField "user_id" A.decimal
--             <*> parseField "file_id" A.decimal

--     parseField :: ByteString -> A.Parser a -> Either String a
--     parseField field p = maybe (Left $ "missing " <> B.unpack field) (A.parseOnly $ p <* A.endOfInput) (join $ lookup field query)

--     respondJSON :: (J.ToJSON a) => a -> m ()
--     respondJSON = respond . Binary.fromLazyByteString . J.encode

--     respond = respondWith Status.ok200
--     respondWith status = liftIO . sendResponse . HS.responseBuilder status []

-- * ChatRequest handlers

startRemoteCtrl :: ChatMonad m => (ByteString -> m ChatResponse) -> m ()
startRemoteCtrl execChatCommand = do
  checkNoRemoteCtrlSession
  size <- asks $ tbqSize . config
  remoteOutputQ <- newTBQueueIO size
  discovered <- newTVarIO mempty
  discoverer <- async $ discoverRemoteCtrls discovered
  accepted <- newEmptyTMVarIO
  supervisor <- async $ runSupervisor discovered accepted
  chatWriteVar remoteCtrlSession $ Just RemoteCtrlSession {discoverer, supervisor, hostServer = Nothing, discovered, accepted, remoteOutputQ}
  where
    runSupervisor discovered accepted = do
      remoteCtrlId <- atomically (readTMVar accepted)
      rc@RemoteCtrl {fingerprint} <- withStore (`getRemoteCtrl` remoteCtrlId)
      source <- atomically $ TM.lookup fingerprint discovered >>= maybe retry pure
      toView $ CRRemoteCtrlConnecting $ remoteCtrlInfo rc False
      atomically $ writeTVar discovered mempty -- flush unused sources
      server <- async $ Discovery.connectRevHTTP2 source fingerprint (processControllerRequest execChatCommand)
      chatModifyVar remoteCtrlSession $ fmap $ \s -> s {hostServer = Just server}
      toView $ CRRemoteCtrlConnected $ remoteCtrlInfo rc True
      _ <- waitCatch server
      chatWriteVar remoteCtrlSession Nothing
      toView CRRemoteCtrlStopped

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

checkNoRemoteCtrlSession :: ChatMonad m => m ()
checkNoRemoteCtrlSession =
  chatReadVar remoteCtrlSession >>= maybe (pure ()) (\_ -> throwError $ ChatErrorRemoteCtrl RCEBusy)

utf8String :: [Char] -> ByteString
utf8String = encodeUtf8 . T.pack
{-# INLINE utf8String #-}

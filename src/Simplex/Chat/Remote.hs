{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
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
import Data.Aeson ((.=))
import qualified Data.Aeson as J
import qualified Data.Aeson.Key as JK
import qualified Data.Aeson.KeyMap as JM
import qualified Data.Attoparsec.ByteString.Char8 as A
import qualified Data.Binary.Builder as Binary
import Data.ByteString (ByteString, hPut)
import qualified Data.ByteString.Base64.URL as B64U
import qualified Data.ByteString.Char8 as B
import Data.Int (Int64)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import qualified Network.HTTP.Types as HTTP
import qualified Network.HTTP.Types.Status as Status
import qualified Network.HTTP2.Client as HTTP2Client
import qualified Network.HTTP2.Server as HTTP2Server
import Network.Socket (SockAddr (..), hostAddressToTuple)
import Simplex.Chat.Controller
import Simplex.Chat.Messages (AChatItem (..), CIFile (..), CIFileStatus (..), ChatItem (..), chatNameStr)
import Simplex.Chat.Messages.CIContent (MsgDirection (..), SMsgDirection (..))
import qualified Simplex.Chat.Remote.Discovery as Discovery
import Simplex.Chat.Remote.Types
import Simplex.Chat.Store.Files (getRcvFileTransfer)
import Simplex.Chat.Store.Profiles (getUser)
import Simplex.Chat.Store.Remote
import Simplex.Chat.Store.Shared (StoreError (..))
import Simplex.Chat.Types
import Simplex.FileTransfer.Util (uniqueCombine)
import qualified Simplex.Messaging.Crypto as C
import Simplex.Messaging.Crypto.File (CryptoFile (..))
import Simplex.Messaging.Encoding.String (StrEncoding (..))
import Simplex.Messaging.Parsers (pattern SingleFieldJSONTag, pattern TaggedObjectJSONData, pattern TaggedObjectJSONTag)
import qualified Simplex.Messaging.TMap as TM
import Simplex.Messaging.Transport.Client (TransportHost (..))
import Simplex.Messaging.Transport.Credentials (genCredentials, tlsCredentials)
import Simplex.Messaging.Transport.HTTP2 (HTTP2Body (..), defaultHTTP2BufferSize)
import Simplex.Messaging.Transport.HTTP2.Client (HTTP2Client)
import qualified Simplex.Messaging.Transport.HTTP2.Client as HTTP2
import qualified Simplex.Messaging.Transport.HTTP2.Server as HTTP2
import Simplex.Messaging.Util (bshow, ifM, tshow, ($>>=))
import System.FilePath (isPathSeparator, takeFileName, (</>))
import UnliftIO
import UnliftIO.Directory (createDirectoryIfMissing, getFileSize)

getRemoteHostSession :: ChatMonad m => RemoteHostId -> m RemoteHostSession
getRemoteHostSession rhId = chatReadVar remoteHostSessions >>= maybe err pure . M.lookup rhId
  where
    err = throwError $ ChatErrorRemoteHost rhId RHMissing

checkNoRemoteHostSession :: ChatMonad m => RemoteHostId -> m ()
checkNoRemoteHostSession rhId = chatReadVar remoteHostSessions >>= maybe (pure ()) err . M.lookup rhId
  where
    err _ = throwError $ ChatErrorRemoteHost rhId RHBusy

startRemoteHost :: ChatMonad m => RemoteHostId -> m ()
startRemoteHost rhId = do
  checkNoRemoteHostSession rhId
  rh <- withStore (`getRemoteHost` rhId)
  announcer <- async $ run rh
  chatModifyVar remoteHostSessions $ M.insert rhId RemoteHostSessionStarting {announcer}
  where
    cleanup finished = do
      logInfo "Remote host http2 client fininshed"
      atomically $ writeTVar finished True
      -- TODO why this is not an error?
      M.lookup rhId <$> chatReadVar remoteHostSessions >>= \case
        Nothing -> logInfo $ "Session already closed for remote host " <> tshow rhId
        Just _ -> closeRemoteHostSession rhId >> toView (CRRemoteHostStopped rhId)
    run rh@RemoteHost {storePath, caKey, caCert} = do
      finished <- newTVarIO False
      let parent = (C.signatureKeyPair caKey, caCert)
      sessionCreds <- liftIO $ genCredentials (Just parent) (0, 24) "Session"
      let (fingerprint, credentials) = tlsCredentials $ sessionCreds :| [parent]
      -- TODO ExceptT
      Discovery.announceRevHTTP2 (cleanup finished) fingerprint credentials >>= \case
        Left h2ce -> do
          logError $ "Failed to set up remote host connection: " <> tshow h2ce
          cleanup finished
        Right ctrlClient -> do
          chatModifyVar remoteHostSessions $ M.insert rhId RemoteHostSessionStarted {storePath, ctrlClient}
          chatWriteVar currentRemoteHost $ Just rhId
          -- TODO ExceptT
          sendHello ctrlClient >>= \case
            Left h2ce -> do
              logError $ "Failed to send initial remote host request: " <> tshow h2ce
              cleanup finished
            Right HTTP2.HTTP2Response {respBody = HTTP2Body {bodyHead}} -> do
              logDebug $ "Got initial from remote host: " <> tshow bodyHead
              oq <- asks outputQ
              let toViewRemote = atomically . writeTBQueue oq . (Nothing,Just rhId,)
              void . async $ pollRemote finished ctrlClient "/recv" $ \chatResponse -> do
                -- TODO move to view / terminal
                case chatResponse of
                  CRRcvFileComplete {user = ru, chatItem = AChatItem c d@SMDRcv i ci@ChatItem {file = Just ciFile}} -> do
                    handleRcvFileComplete ctrlClient storePath ru ciFile >>= \case
                      Nothing -> toViewRemote chatResponse
                      Just localFile -> toViewRemote CRRcvFileComplete {user = ru, chatItem = AChatItem c d i ci {file = Just localFile}}
                  _ -> toViewRemote chatResponse
              rcName <- chatReadVar localDeviceName
              -- TODO what sets session active?
              toView CRRemoteHostConnected {remoteHost = remoteHostInfo rh True rcName}

sendHello :: ChatMonad m => HTTP2Client -> m (Either HTTP2.HTTP2ClientError HTTP2.HTTP2Response)
sendHello http = liftIO (HTTP2.sendRequestDirect http req Nothing)
  where
    req = HTTP2Client.requestNoBody "GET" "/" mempty

pollRemote :: (ChatMonad m, J.FromJSON a) => TVar Bool -> HTTP2Client -> ByteString -> (a -> m ()) -> m ()
pollRemote finished http path action = loop
  where
    loop = do
      -- TODO ExceptT
      liftIO (HTTP2.sendRequestDirect http req Nothing) >>= \case
        Left e -> logError $ "pollRemote: " <> tshow (path, e)
        Right HTTP2.HTTP2Response {respBody = HTTP2Body {bodyHead}} -> do
          logDebug $ "Got /recv response: " <> decodeUtf8 bodyHead
          -- TODO ExceptT (liftEither)
          case J.eitherDecodeStrict' bodyHead of
            -- send error toView
            Left e -> logError $ "pollRemote/decode: " <> tshow (path, e)
            Right o -> action o
      readTVarIO finished >>= (`unless` loop)
    req = HTTP2Client.requestNoBody "GET" path mempty

closeRemoteHostSession :: ChatMonad m => RemoteHostId -> m ()
closeRemoteHostSession remoteHostId = do
  session <- getRemoteHostSession remoteHostId
  logInfo $ "Closing remote host session for " <> tshow remoteHostId
  liftIO $ cancelRemoteHostSession session
  chatWriteVar currentRemoteHost Nothing
  chatModifyVar remoteHostSessions $ M.delete remoteHostId

cancelRemoteHostSession :: MonadUnliftIO m => RemoteHostSession -> m ()
cancelRemoteHostSession = \case
  RemoteHostSessionStarting {announcer} -> cancel announcer
  RemoteHostSessionStarted {ctrlClient} -> liftIO $ HTTP2.closeHTTP2Client ctrlClient

createRemoteHost :: ChatMonad m => m RemoteHostInfo
createRemoteHost = do
  let rhName = "TODO" -- you don't have remote host name here, it will be passed from remote host
  ((_, caKey), caCert) <- liftIO $ genCredentials Nothing (-25, 24 * 365) rhName
  storePath <- liftIO randomStorePath
  remoteHostId <- withStore' $ \db -> insertRemoteHost db storePath rhName caKey caCert
  rcName <- chatReadVar localDeviceName
  let remoteCtrlOOB = RemoteCtrlOOB {fingerprint = C.certificateFingerprint caCert, displayName = rcName}
  pure RemoteHostInfo {remoteHostId, storePath, displayName = rhName, remoteCtrlOOB, sessionActive = False}

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

processRemoteCommand :: ChatMonad m => RemoteHostSession -> (ByteString, ChatCommand) -> m ChatResponse
processRemoteCommand RemoteHostSessionStarting {} _ = pure . CRChatError Nothing . ChatError $ CEInternalError "sending remote commands before session started"
processRemoteCommand RemoteHostSessionStarted {ctrlClient} (s, cmd) = do
  logDebug $ "processRemoteCommand: " <> tshow (s, cmd)
  case cmd of
    SendFile cn ctrlPath -> do
      -- TODO this would look likg this:
      -- hostPath <- storeRemoteFile ctrlClient ctrlPath
      -- relayCommand ctrlClient $ utf8String $ "/file " <> chatNameStr cn <> " " <> hostPath
      storeRemoteFile ctrlClient ctrlPath >>= \case
        -- TODO: There are file specific errors, they are not internal, should be thrown in storeRemoteFile
        Nothing -> pure . CRChatError Nothing . ChatError $ CEInternalError "failed to store file on remote host"
        Just hostPath -> relayCommand ctrlClient $ utf8String $ "/file " <> chatNameStr cn <> " " <> hostPath
    SendImage cn ctrlPath -> do
      storeRemoteFile ctrlClient ctrlPath >>= \case
        Nothing -> pure . CRChatError Nothing . ChatError $ CEInternalError "failed to store image on remote host"
        Just hostPath -> relayCommand ctrlClient $ utf8String $ "/image " <> chatNameStr cn <> " " <> hostPath
    APISendMessage {composedMessage = cm@ComposedMessage {fileSource = Just CryptoFile {filePath = ctrlPath, cryptoArgs}}} -> do
      storeRemoteFile ctrlClient ctrlPath >>= \case
        Nothing -> pure . CRChatError Nothing . ChatError $ CEInternalError "failed to store file on remote host"
        Just hostPath -> do
          -- TODO something is wrong here. What if file encryption is disabled?
          let cm' = cm {fileSource = Just CryptoFile {filePath = hostPath, cryptoArgs}} :: ComposedMessage
          -- TODO we shouldn't manipulate JSON like that
          relayCommand ctrlClient $ B.takeWhile (/= '{') s <> B.toStrict (J.encode cm')
    _ -> relayCommand ctrlClient s

relayCommand :: ChatMonad m => HTTP2Client -> ByteString -> m ChatResponse
relayCommand http s =
  -- TODO ExceptT
  postBytestring Nothing http "/send" mempty s >>= \case
    Left e -> err $ "relayCommand/post: " <> show e
    Right HTTP2.HTTP2Response {respBody = HTTP2Body {bodyHead}} -> do
      logDebug $ "Got /send response: " <> decodeUtf8 bodyHead
      -- TODO liftEither
      remoteChatResponse <- case J.eitherDecodeStrict bodyHead of -- XXX: large JSONs can overflow into buffered chunks
        Left e -> err $ "relayCommand/decodeValue: " <> show e
        Right json -> case J.fromJSON $ toTaggedJSON json of
          J.Error e -> err $ "relayCommand/fromJSON: " <> show e
          J.Success cr -> pure cr
      case remoteChatResponse of
        -- TODO: intercept file responses and fetch files when needed
        -- XXX: is that even possible, to have a file response to a command?
        _ -> pure remoteChatResponse
  where
    err = pure . CRChatError Nothing . ChatError . CEInternalError
    toTaggedJSON :: J.Value -> J.Value
    toTaggedJSON = id -- owsf2tagged TODO: get from RemoteHost
    -- XXX: extract to http2 transport
    postBytestring timeout' c path hs body = liftIO $ HTTP2.sendRequestDirect c req timeout'
      where
        req = HTTP2Client.requestBuilder "POST" path hs (Binary.fromByteString body)

handleRcvFileComplete :: ChatMonad m => HTTP2Client -> FilePath -> User -> CIFile 'MDRcv -> m (Maybe (CIFile 'MDRcv))
handleRcvFileComplete http storePath remoteUser cif@CIFile {fileId, fileName, fileStatus} = case fileStatus of
  CIFSRcvComplete ->
    chatReadVar filesFolder >>= \case
      Just baseDir -> do
        let hostStore = baseDir </> storePath
        createDirectoryIfMissing True hostStore
        -- TODO the problem here is that the name may turn out to be different and nothing will work
        -- file processing seems to work "accidentally", not "by design"
        localPath <- uniqueCombine hostStore fileName
        ok <- fetchRemoteFile http remoteUser fileId localPath
        if ok
          then pure $ Just (cif {fileName = localPath} :: CIFile 'MDRcv)
          else Nothing <$ logError "fetchRemoteFile failed"
      Nothing -> Nothing <$ logError "Local file store not available while fetching remote file"
  _ -> Nothing <$ logDebug ("Ingoring invalid file notification for file (" <> tshow fileId <> ") " <> tshow fileName)

-- | Convert swift single-field sum encoding into tagged/discriminator-field
owsf2tagged :: J.Value -> J.Value
owsf2tagged = fst . convert
  where
    convert val = case val of
      J.Object o
        | JM.size o == 2 ->
            case JM.toList o of
              [OwsfTag, o'] -> tagged o'
              [o', OwsfTag] -> tagged o'
              _ -> props
        | otherwise -> props
        where
          props = (J.Object $ fmap owsf2tagged o, False)
      J.Array a -> (J.Array $ fmap owsf2tagged a, False)
      _ -> (val, False)
    -- `tagged` converts the pair of single-field object encoding to tagged encoding.
    -- It sets innerTag returned by `convert` to True to prevent the tag being overwritten.
    tagged (k, v) = (J.Object pairs, True)
      where
        (v', innerTag) = convert v
        pairs = case v' of
          -- `innerTag` indicates that internal object already has tag,
          -- so the current tag cannot be inserted into it.
          J.Object o
            | innerTag -> pair
            | otherwise -> JM.insert TaggedObjectJSONTag tag o
          _ -> pair
        tag = J.String $ JK.toText k
        pair = JM.fromList [TaggedObjectJSONTag .= tag, TaggedObjectJSONData .= v']

pattern OwsfTag :: (JK.Key, J.Value)
pattern OwsfTag = (SingleFieldJSONTag, J.Bool True)

-- TODO storeRemoteFile :: ChatMonad m => HTTP2Client -> FilePath -> m FilePath
storeRemoteFile :: MonadUnliftIO m => HTTP2Client -> FilePath -> m (Maybe FilePath)
storeRemoteFile http localFile = do
  -- TODO use ExceptT / ChatMonad m
  putFile Nothing http uri mempty localFile >>= \case
    Left h2ce -> Nothing <$ logError (tshow h2ce)
    Right HTTP2.HTTP2Response {response, respBody = HTTP2Body {bodyHead}} ->
      case HTTP.statusCode <$> HTTP2Client.responseStatus response of
        Just 200 -> pure . Just $ B.unpack bodyHead
        -- TODO errors should be sent to view, this logging won't help debugging on devices
        notOk -> Nothing <$ logError ("Bad response status: " <> tshow notOk)
  where
    uri = "/store?" <> HTTP.renderSimpleQuery False [("file_name", utf8String $ takeFileName localFile)]
    putFile timeout' c path hs file = liftIO $ do
      fileSize <- fromIntegral <$> getFileSize file
      HTTP2.sendRequestDirect c (req fileSize) timeout'
      where
        req size = HTTP2Client.requestFile "PUT" path hs (HTTP2Client.FileSpec file 0 size)

-- TODO fetchRemoteFile :: ChatMonad m => HTTP2Client -> User -> Int64 -> FilePath -> m ()
fetchRemoteFile :: MonadUnliftIO m => HTTP2Client -> User -> Int64 -> FilePath -> m Bool
fetchRemoteFile http User {userId = remoteUserId} remoteFileId localPath = do
  -- TODO use ExceptT / ChatMonad m
  liftIO (HTTP2.sendRequestDirect http req Nothing) >>= \case
    Left h2ce -> False <$ logError (tshow h2ce)
    Right HTTP2.HTTP2Response {response, respBody} ->
      if HTTP2Client.responseStatus response == Just Status.ok200
        then True <$ writeBodyToFile localPath respBody
        -- TODO throwError
        else False <$ (logError $ "Request failed: " <> maybe "(??)" tshow (HTTP2Client.responseStatus response) <> " " <> decodeUtf8 (bodyHead respBody))
  where
    req = HTTP2Client.requestNoBody "GET" path mempty
    path = "/fetch?" <> HTTP.renderSimpleQuery False [("user_id", bshow remoteUserId), ("file_id", bshow remoteFileId)]

-- XXX: extract to Transport.HTTP2 ?
writeBodyToFile :: MonadUnliftIO m => FilePath -> HTTP2Body -> m ()
writeBodyToFile path HTTP2Body {bodyHead, bodySize, bodyPart} = do
  logInfo $ "Receiving " <> tshow bodySize <> " bytes to " <> tshow path
  liftIO . withFile path WriteMode $ \h -> do
    hPut h bodyHead
    mapM_ (hPutBodyChunks h) bodyPart

hPutBodyChunks :: Handle -> (Int -> IO ByteString) -> IO ()
hPutBodyChunks h getChunk = do
  chunk <- getChunk defaultHTTP2BufferSize
  unless (B.null chunk) $ do
    hPut h chunk
    hPutBodyChunks h getChunk

processControllerRequest :: forall m. ChatMonad m => (ByteString -> m ChatResponse) -> HTTP2.HTTP2Request -> m ()
processControllerRequest execChatCommand HTTP2.HTTP2Request {request, reqBody, sendResponse} = do
  logDebug $ "Remote controller request: " <> tshow (method <> " " <> path)
  res <- tryChatError $ case (method, ps) of
    ("GET", []) -> getHello
    ("POST", ["send"]) -> sendCommand
    ("GET", ["recv"]) -> recvMessage
    ("PUT", ["store"]) -> storeFile
    ("GET", ["fetch"]) -> fetchFile
    unexpected -> respondWith Status.badRequest400 $ "unexpected method/path: " <> Binary.putStringUtf8 (show unexpected)
  case res of
    Left e -> logError $ "Error handling remote controller request: (" <> tshow (method <> " " <> path) <> "): " <> tshow e
    Right () -> logDebug $ "Remote controller request: " <> tshow (method <> " " <> path) <> " OK"
  where
    method = fromMaybe "" $ HTTP2Server.requestMethod request
    path = fromMaybe "/" $ HTTP2Server.requestPath request
    (ps, query) = HTTP.decodePath path
    getHello = respond "OK"
    sendCommand = execChatCommand (bodyHead reqBody) >>= respondJSON
    recvMessage =
      chatReadVar remoteCtrlSession >>= \case
        Nothing -> respondWith Status.internalServerError500 "session not active"
        Just rcs -> atomically (readTBQueue $ remoteOutputQ rcs) >>= respondJSON
    -- TODO liftEither storeFileQuery
    storeFile = case storeFileQuery of
      Left err -> respondWith Status.badRequest400 (Binary.putStringUtf8 err)
      Right fileName -> do
        baseDir <- fromMaybe "." <$> chatReadVar filesFolder
        localPath <- uniqueCombine baseDir fileName
        logDebug $ "Storing controller file to " <> tshow (baseDir, localPath)
        writeBodyToFile localPath reqBody
        let storeRelative = takeFileName localPath
        respond $ Binary.putStringUtf8 storeRelative
      where
        storeFileQuery = parseField "file_name" $ A.many1 (A.satisfy $ not . isPathSeparator)
    -- TODO move to ExceptT monad, catch errors in one place, convert errors to responses
    fetchFile = case fetchFileQuery of
      Left err -> respondWith Status.badRequest400 (Binary.putStringUtf8 err)
      Right (userId, fileId) -> do
        logInfo $ "Fetching file " <> tshow fileId <> " from user " <> tshow userId
        x <- withStore' $ \db -> runExceptT $ do
          user <- getUser db userId
          getRcvFileTransfer db user fileId
        -- TODO this error handling is very ad-hoc, there is no separation between Chat errors and responses
        case x of
          Right RcvFileTransfer {fileStatus = RFSComplete RcvFileInfo {filePath}} -> do
            baseDir <- fromMaybe "." <$> chatReadVar filesFolder
            let fullPath = baseDir </> filePath
            size <- fromInteger <$> getFileSize fullPath
            liftIO . sendResponse . HTTP2Server.responseFile Status.ok200 mempty $ HTTP2Server.FileSpec fullPath 0 size
          Right _ -> respondWith Status.internalServerError500 "The requested file is not complete"
          Left SEUserNotFound {} -> respondWith Status.notFound404 "User not found"
          Left SERcvFileNotFound {} -> respondWith Status.notFound404 "File not found"
          _ -> respondWith Status.internalServerError500 "Store error"
      where
        fetchFileQuery =
          (,)
            <$> parseField "user_id" A.decimal
            <*> parseField "file_id" A.decimal

    parseField :: ByteString -> A.Parser a -> Either String a
    parseField field p = maybe (Left $ "missing " <> B.unpack field) (A.parseOnly $ p <* A.endOfInput) (join $ lookup field query)

    respondJSON :: (J.ToJSON a) => a -> m ()
    respondJSON = respond . Binary.fromLazyByteString . J.encode

    respond = respondWith Status.ok200
    respondWith status = liftIO . sendResponse . HTTP2Server.responseBuilder status []

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

discoverRemoteCtrls :: ChatMonad m => TM.TMap C.KeyHash TransportHost -> m ()
discoverRemoteCtrls discovered = Discovery.withListener go
  where
    go sock =
      Discovery.recvAnnounce sock >>= \case
        (SockAddrInet _sockPort sockAddr, invite) -> case strDecode invite of
          Left _ -> go sock -- ignore malformed datagrams
          Right fingerprint -> do
            let addr = THIPv4 (hostAddressToTuple sockAddr)
            ifM
              (atomically $ TM.member fingerprint discovered)
              (logDebug $ "Fingerprint announce already knwon: " <> tshow (addr, invite))
              ( do
                  logInfo $ "New fingerprint announce: " <> tshow (addr, invite)
                  atomically $ TM.insert fingerprint addr discovered
              )
            withStore' (`getRemoteCtrlByFingerprint` fingerprint) >>= \case
              Nothing -> toView $ CRRemoteCtrlAnnounce fingerprint -- unknown controller, ui "register" action required
              Just found@RemoteCtrl {remoteCtrlId, accepted = storedChoice} -> case storedChoice of
                Nothing -> toView $ CRRemoteCtrlFound $ remoteCtrlInfo found False -- first-time controller, ui "accept" action required
                Just False -> pure () -- skipping a rejected item
                Just True ->
                  chatReadVar remoteCtrlSession >>= \case
                    Nothing -> toView . CRChatError Nothing . ChatError $ CEInternalError "Remote host found without running a session"
                    Just RemoteCtrlSession {accepted} -> atomically $ void $ tryPutTMVar accepted remoteCtrlId -- previously accepted controller, connect automatically
        _nonV4 -> go sock

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

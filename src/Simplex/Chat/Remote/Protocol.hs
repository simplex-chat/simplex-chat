{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}

module Simplex.Chat.Remote.Protocol where

import Control.Logger.Simple
import Control.Monad
import Control.Monad.Except
import Data.Aeson ((.=))
import qualified Data.Aeson as J
import qualified Data.Aeson.Key as JK
import qualified Data.Aeson.KeyMap as JM
import Data.Aeson.TH (deriveJSON)
import qualified Data.Aeson.Types as JT
import Data.Binary.Get (getInt64le, runGetOrFail)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.ByteString.Builder (Builder, int64LE, lazyByteString)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Internal as BLI
import Data.Maybe (isNothing)
import Data.String (fromString)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import Data.Word (Word32)
import qualified Network.HTTP.Types as N
import qualified Network.HTTP2.Client as H
import Network.HTTP2.Server (responseStreaming)
import Simplex.Chat.Controller (ChatResponse)
import Simplex.Chat.Remote.Types
import Simplex.Messaging.Crypto.File (CryptoFile)
import Simplex.Messaging.Parsers (dropPrefix, taggedObjectJSON, pattern SingleFieldJSONTag, pattern TaggedObjectJSONData, pattern TaggedObjectJSONTag)
import Simplex.Messaging.Transport.Buffer (getBuffered)
import Simplex.Messaging.Transport.HTTP2 (HTTP2Body (..), HTTP2BodyChunk, defaultHTTP2BufferSize, getBodyChunk)
import Simplex.Messaging.Transport.HTTP2.Client (HTTP2Client, HTTP2Response (..), closeHTTP2Client, sendRequestDirect)
import Simplex.Messaging.Transport.HTTP2.File (hSendFile)
import Simplex.Messaging.Transport.HTTP2.Server (HTTP2Request (..))
import Simplex.Messaging.Util (liftEitherError, liftEitherWith, tshow, whenM)
import System.FilePath ((</>))
import System.IO.Unsafe (unsafeInterleaveIO)
import UnliftIO
import UnliftIO.Directory (doesFileExist, getFileSize)

data RemoteCommand
  = RCHello {deviceName :: Text}
  | RCSend {command :: Text} -- TODO maybe ChatCommand here?
  | RCRecv {wait :: Int} -- this wait should be less than HTTP timeout
  | -- local file encryption is determined by the host, but can be overridden for videos
    RCStoreFile {fileSize :: Word32, encrypt :: Maybe Bool} -- requires attachment
  | RCGetFile {filePath :: FilePath}
  deriving (Show)

data RemoteResponse
  = RRHello {encoding :: PlatformEncoding, deviceName :: Text}
  | RRChatResponse {chatResponse :: ChatResponse}
  | RRChatEvent {chatEvent :: Maybe ChatResponse} -- ^ 'Nothing' on poll timeout
  | RRFileStored {fileSource :: CryptoFile}
  | RRFile {fileSize :: Word32} -- provides attachment
  | RRServerError {serverError :: RemoteProtocolError} -- ^ The protocol error happened on the server side
  | RRException {someException :: Text} -- ^ Handler crashed
  deriving (Show)

-- Force platform-independent encoding as the types aren't UI-visible
$(deriveJSON (taggedObjectJSON $ dropPrefix "RC") ''RemoteCommand)
$(deriveJSON (taggedObjectJSON $ dropPrefix "RR") ''RemoteResponse)

-- * Client side / desktop

createRemoteHostClient :: HTTP2Client -> Text -> ExceptT RemoteProtocolError IO RemoteHostClient
createRemoteHostClient httpClient desktopName = do
  logInfo "Sending initial hello"
  let dummyRemoteEncoding = localEncoding
  sendRemoteCommand httpClient dummyRemoteEncoding Nothing RCHello {deviceName = desktopName} >>= \case
    (Nothing, rrh@RRHello {encoding, deviceName = mobileName}) -> do
      logInfo $ "Got initial hello: " <> tshow rrh
      when (encoding == PEKotlin && localEncoding == PESwift) $ throwError RPEIncompatibleEncoding
      pure RemoteHostClient {remoteEncoding = encoding, remoteDeviceName = mobileName, httpClient}
    (Just _, _) -> throwError RPEUnexpectedFile
    _ -> throwError RPEBadResponse

closeRemoteHostClient :: MonadIO m => RemoteHostClient -> m ()
closeRemoteHostClient RemoteHostClient {httpClient} = liftIO $ closeHTTP2Client httpClient

-- ** Commands

remoteSend :: RemoteHostClient -> ByteString -> ExceptT RemoteProtocolError IO ChatResponse
remoteSend RemoteHostClient {httpClient, remoteEncoding} cmd = sendRemoteCommand httpClient remoteEncoding Nothing RCSend {command = decodeUtf8 cmd} >>= \case
  (Nothing, RRChatResponse cr) -> pure cr
  (Just _, RRChatEvent{}) -> throwError RPEUnexpectedFile
  (_, _) -> throwError RPEBadResponse

remoteRecv :: RemoteHostClient -> Int -> ExceptT RemoteProtocolError IO (Maybe ChatResponse)
remoteRecv RemoteHostClient {httpClient, remoteEncoding} ms = sendRemoteCommand httpClient remoteEncoding Nothing RCRecv {wait=ms} >>= \case
  (Nothing, RRChatEvent cr_) -> pure cr_
  (Just _, RRChatEvent{}) -> throwError RPEUnexpectedFile
  (_, _) -> throwError RPEBadResponse

sendRemoteCommand :: HTTP2Client -> PlatformEncoding -> Maybe (Handle, Word32) -> RemoteCommand -> ExceptT RemoteProtocolError IO (Maybe BL.ByteString, RemoteResponse)
sendRemoteCommand http remoteEncoding attachment_ rc = do
  HTTP2Response{response, respBody} <- liftEitherError (RPEHTTP2 . tshow) $ sendRequestDirect http httpRequest Nothing
  jsonBody (convertJSON remoteEncoding localEncoding) response respBody
  where
    httpRequest = H.requestStreaming N.methodPost "/" mempty $ \send flush -> do
      send $ sizePrefixedEncode rc
      case attachment_ of
        Nothing -> pure ()
        Just (h, sz) -> hSendFile h send sz
      flush

remoteStoreFile :: RemoteHostClient -> FilePath -> Maybe Bool -> ExceptT RemoteProtocolError IO CryptoFile
remoteStoreFile RemoteHostClient {httpClient, remoteEncoding} localPath encrypt = do
  (attachment_, reply) <- withFileHSize localPath $ \h fileSize -> sendRemoteCommand httpClient remoteEncoding (Just (h, fileSize)) RCStoreFile {encrypt, fileSize}
  unless (isNothing attachment_) $ throwError RPEUnexpectedFile
  case reply of
    RRFileStored {fileSource} -> pure fileSource
    _ -> throwError RPEBadResponse

-- TODO this should work differently for CLI and UI clients
-- CLI - potentially, create new unique names and report them as created
-- UI - always use the same names and report error if file already exists
-- alternatively, CLI should also use a fixed folder for remote session
-- Possibly, path in the database should be optional and CLI commands should allow configuring it per session or use temp or download folder
remoteGetFile :: RemoteHostClient -> FilePath -> FilePath -> ExceptT RemoteProtocolError IO FilePath
remoteGetFile RemoteHostClient {httpClient, remoteEncoding} baseDir filePath = do
  (attachment_, reply) <- sendRemoteCommand httpClient remoteEncoding Nothing RCGetFile {filePath}
  -- XXX: this should use the body-to-file writer, but header/JSON
  expectedSize <- case reply of
    RRFile {fileSize} -> pure fileSize
    _ -> throwError RPEBadResponse
  whenM (liftIO $ doesFileExist localFile) $ throwError RPEStoredFileExists
  maybe (throwError RPENoFile) (liftIO . BL.writeFile localFile) attachment_
  whenM ((== expectedSize) . fromIntegral <$> getFileSize localFile) $ throwError RPEInvalidSize
  pure localFile
  where
    localFile = baseDir </> filePath

-- * Server side / mobile

handleRemoteCommand :: MonadUnliftIO m => ((Maybe BL.ByteString, RemoteCommand) -> m RemoteResponse) -> HTTP2Request -> m ()
handleRemoteCommand handler HTTP2Request{request, reqBody, sendResponse} = do
  logDebug "handleRemoteCommand"
  result <- tryAny $ -- prevent exception leaking out to the HTTP2 so the client can skip checking status
    withRunInIO $ \io -> runExceptT $ do -- discard parent context and run remote protocol
      acmd <- jsonBody id request reqBody
      ExceptT . try . io $ handler acmd -- return to handler context, intercepting RemoteProtocolError thrown as exceptions
  liftIO . sendResponse . responseStreaming N.status200 [] $ \send flush -> do
    send $ sizePrefixedEncode $ case result of
      Left err -> RRException (tshow err)
      Right (Left rpe) -> RRServerError rpe
      Right (Right ok) -> ok
    -- todo'unhandled <- runExceptT . withFileHSize "localPath" $ \h fileSize -> liftIO $ hSendFile h send fileSize
    flush

withFileHSize :: (MonadUnliftIO m, MonadError RemoteProtocolError m) => FilePath -> (Handle -> Word32 -> m a) -> m a
withFileHSize path action =
  withFile path ReadMode $ \h -> do
    fileSize' <- hFileSize h
    when (fileSize' > toInteger (maxBound :: Word32)) $ throwError RPEFileTooLarge
    action h (fromInteger fileSize')

-- * Transport-level wrappers

convertJSON :: PlatformEncoding -> PlatformEncoding -> J.Value -> J.Value
convertJSON _remote@PEKotlin _local@PEKotlin = id
convertJSON PESwift PESwift = id
convertJSON PESwift PEKotlin = owsf2tagged
convertJSON PEKotlin PESwift = error "unsupported convertJSON: K/S" -- guarded by createRemoteHostClient

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

-- | Get size-prefixed JSON and decode, performing platform transcoding if needed.
-- The remains are returned as lazy bytestring and don't get streamed until requested.
jsonBody :: (HTTP2BodyChunk a, J.FromJSON b) => (J.Value -> J.Value) -> a -> HTTP2Body -> ExceptT RemoteProtocolError IO (Maybe BL.ByteString, b)
jsonBody transcode r body = do
  chunks <- liftIO (getHTTP2BodyChunks r body)
  (json, attachment) <- withExceptT (const RPEInvalidSize) $ sizedChunk chunks
  -- logDebug $ decodeUtf8 $ B.toStrict json
  res <- liftEitherWith (RPEInvalidJSON . fromString) $ J.eitherDecode json >>= JT.parseEither J.parseJSON . transcode
  pure (if BL.null attachment then Nothing else Just attachment, res)

-- | Convert a command or a response into 'Builder'
sizePrefixedEncode :: J.ToJSON a => a -> Builder
sizePrefixedEncode value = int64LE size <> lazyByteString json
  where
    size = BL.length json
    json = J.encode value

sizedChunk :: BL.ByteString -> ExceptT () IO (BL.ByteString, BL.ByteString)
sizedChunk chunks = do
  (next, _offset, headerSize) <- liftEitherWith (const ()) $ runGetOrFail getInt64le chunks
  when (headerSize > BL.length next) $ throwError ()
  pure $ BL.splitAt headerSize next

getHTTP2BodyChunks :: HTTP2BodyChunk a => a -> HTTP2Body -> IO BL.ByteString
getHTTP2BodyChunks r HTTP2Body {bodyHead, bodyBuffer} =
  if B.null bodyHead
    then lazyGet -- skip head chunk
    else BLI.Chunk bodyHead <$> lazyGet -- attach head as a first chunk
  where
    getNext :: IO ByteString
    getNext = getBuffered bodyBuffer defaultHTTP2BufferSize Nothing (getBodyChunk r)

    -- | Delay IO effect until it is demanded
    --
    -- cf. https://hackage.haskell.org/package/bytestring-0.12.0.2/docs/src/Data.ByteString.Lazy.html#hGetContentsN
    lazyGet :: IO BL.ByteString
    lazyGet = unsafeInterleaveIO next

    next :: IO BL.ByteString
    next = do
      chunk <- getNext
      if B.null chunk
        then pure BLI.Empty -- done, terminate with a "nil" chunk
        else BLI.Chunk chunk <$> lazyGet -- produce a chunk with an on-demand body

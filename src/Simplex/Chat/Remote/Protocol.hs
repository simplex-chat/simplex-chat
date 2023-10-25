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
import Data.ByteString (ByteString)
import Data.ByteString.Builder (Builder, word32BE, lazyByteString)
import qualified Data.ByteString.Lazy as BL
import Data.String (fromString)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import Data.Word (Word32)
import qualified Network.HTTP.Types as N
import qualified Network.HTTP2.Client as H
import Network.Transport.Internal (decodeWord32)
import Simplex.Chat.Controller (ChatResponse)
import Simplex.Chat.Remote.Types
import Simplex.Messaging.Crypto.File (CryptoFile)
import Simplex.Messaging.Parsers (dropPrefix, taggedObjectJSON, pattern SingleFieldJSONTag, pattern TaggedObjectJSONData, pattern TaggedObjectJSONTag)
import Simplex.Messaging.Transport.Buffer (getBuffered)
import Simplex.Messaging.Transport.HTTP2 (HTTP2Body (..), HTTP2BodyChunk, getBodyChunk)
import Simplex.Messaging.Transport.HTTP2.Client (HTTP2Client, HTTP2Response (..), closeHTTP2Client, sendRequestDirect)
import Simplex.Messaging.Transport.HTTP2.File (hReceiveFile, hSendFile)
import Simplex.Messaging.Util (liftEitherError, liftEitherWith, tshow, whenM)
import System.FilePath ((</>))
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
  | RRProtocolError {remoteProcotolError :: RemoteProtocolError} -- ^ The protocol error happened on the server side
  deriving (Show)

-- Force platform-independent encoding as the types aren't UI-visible
$(deriveJSON (taggedObjectJSON $ dropPrefix "RC") ''RemoteCommand)
$(deriveJSON (taggedObjectJSON $ dropPrefix "RR") ''RemoteResponse)

-- * Client side / desktop

createRemoteHostClient :: HTTP2Client -> dh -> Text -> ExceptT RemoteProtocolError IO RemoteHostClient
createRemoteHostClient httpClient todo'dhKey desktopName = do
  logInfo "Sending initial hello"
  (_getNext, rr) <- sendRemoteCommand httpClient localEncoding Nothing RCHello {deviceName = desktopName}
  case rr of
    rrh@RRHello {encoding, deviceName = mobileName} -> do
      logInfo $ "Got initial hello: " <> tshow rrh
      when (encoding == PEKotlin && localEncoding == PESwift) $ throwError RPEIncompatibleEncoding
      pure RemoteHostClient {remoteEncoding = encoding, remoteDeviceName = mobileName, httpClient}
    _ -> throwError $ RPEUnexpectedResponse $ tshow rr

closeRemoteHostClient :: MonadIO m => RemoteHostClient -> m ()
closeRemoteHostClient RemoteHostClient {httpClient} = liftIO $ closeHTTP2Client httpClient

-- ** Commands

remoteSend :: RemoteHostClient -> ByteString -> ExceptT RemoteProtocolError IO ChatResponse
remoteSend RemoteHostClient {httpClient, remoteEncoding} cmd = do
  (_getNext, rr) <- sendRemoteCommand httpClient remoteEncoding Nothing RCSend {command = decodeUtf8 cmd}
  case rr of
    RRChatResponse cr -> pure cr
    _ -> throwError $ RPEUnexpectedResponse $ tshow rr

remoteRecv :: RemoteHostClient -> Int -> ExceptT RemoteProtocolError IO (Maybe ChatResponse)
remoteRecv RemoteHostClient {httpClient, remoteEncoding} ms = do
  (_getNext, rr) <- sendRemoteCommand httpClient remoteEncoding Nothing RCRecv {wait=ms}
  case rr of
    RRChatEvent cr_ -> pure cr_
    _ -> throwError $ RPEUnexpectedResponse $ tshow rr

remoteStoreFile :: RemoteHostClient -> FilePath -> Maybe Bool -> ExceptT RemoteProtocolError IO CryptoFile
remoteStoreFile RemoteHostClient {httpClient, remoteEncoding} localPath encrypt = do
  (_getNext, rr) <- withFile localPath ReadMode $ \h -> do
    fileSize' <- hFileSize h
    when (fileSize' > toInteger (maxBound :: Word32)) $ throwError RPEFileTooLarge
    let fileSize = fromInteger fileSize'
    sendRemoteCommand httpClient remoteEncoding (Just (h, fileSize)) RCStoreFile {encrypt, fileSize}
  case rr of
    RRFileStored {fileSource} -> pure fileSource
    _ -> throwError $ RPEUnexpectedResponse $ tshow rr

-- TODO this should work differently for CLI and UI clients
-- CLI - potentially, create new unique names and report them as created
-- UI - always use the same names and report error if file already exists
-- alternatively, CLI should also use a fixed folder for remote session
-- Possibly, path in the database should be optional and CLI commands should allow configuring it per session or use temp or download folder
remoteGetFile :: RemoteHostClient -> FilePath -> FilePath -> ExceptT RemoteProtocolError IO FilePath
remoteGetFile RemoteHostClient {httpClient, remoteEncoding} baseDir filePath = do
  (getNext, rr) <- sendRemoteCommand httpClient remoteEncoding Nothing RCGetFile {filePath}
  expectedSize <- case rr of
    RRFile {fileSize} -> pure fileSize
    _ -> throwError $ RPEUnexpectedResponse $ tshow rr
  whenM (liftIO $ doesFileExist localFile) $ throwError RPEStoredFileExists
  rc <- liftIO $ withFile localFile WriteMode $ \h -> hReceiveFile getNext h expectedSize
  when (rc /= 0) $ throwError RPEInvalidSize
  whenM ((== expectedSize) . fromIntegral <$> getFileSize localFile) $ throwError RPEInvalidSize
  pure localFile
  where
    localFile = baseDir </> filePath

sendRemoteCommand :: HTTP2Client -> PlatformEncoding -> Maybe (Handle, Word32) -> RemoteCommand -> ExceptT RemoteProtocolError IO (Int -> IO ByteString, RemoteResponse)
sendRemoteCommand http remoteEncoding attachment_ rc = do
  HTTP2Response {response, respBody} <- liftEitherError (RPEHTTP2 . tshow) $ sendRequestDirect http httpRequest Nothing
  (header, getNext) <- parseHTTP2Body response respBody
  rr <- liftEitherWith (RPEInvalidJSON . fromString) $ J.eitherDecodeStrict header >>= JT.parseEither J.parseJSON . convertJSON remoteEncoding localEncoding
  pure (getNext, rr)
  where
    httpRequest = H.requestStreaming N.methodPost "/" mempty $ \send flush -> do
      send $ sizePrefixedEncode rc
      case attachment_ of
        Nothing -> pure ()
        Just (h, sz) -> hSendFile h send sz
      flush

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

-- | Convert a command or a response into 'Builder'.
sizePrefixedEncode :: J.ToJSON a => a -> Builder
sizePrefixedEncode value = word32BE (fromIntegral $ BL.length json) <> lazyByteString json
  where
    json = J.encode value

-- | Parse HTTP request or response to a size-prefixed chunk and a function to read more.
parseHTTP2Body :: HTTP2BodyChunk a => a -> HTTP2Body -> ExceptT RemoteProtocolError IO (ByteString, Int -> IO ByteString)
parseHTTP2Body hr HTTP2Body {bodyBuffer} = do
  rSize <- liftIO $ decodeWord32 <$> getNext 4
  when (rSize > fromIntegral (maxBound :: Int)) $ throwError RPEInvalidSize
  r <- liftIO $ getNext $ fromIntegral rSize
  pure (r, getNext)
  where
    getNext sz = getBuffered bodyBuffer sz Nothing $ getBodyChunk hr

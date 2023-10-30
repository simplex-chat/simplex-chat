{-# LANGUAGE DeriveAnyClass #-}
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
import qualified Data.ByteString.Lazy as LB
import Data.String (fromString)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import Data.Word (Word32)
import qualified Network.HTTP.Types as N
import qualified Network.HTTP2.Client as H
import Network.Transport.Internal (decodeWord32)
import Simplex.Chat.Controller
import Simplex.Chat.Remote.Transport
import Simplex.Chat.Remote.Types
import Simplex.FileTransfer.Description (FileDigest (..))
import Simplex.Messaging.Crypto.File (CryptoFile (..))
import Simplex.Messaging.Parsers (dropPrefix, taggedObjectJSON, pattern SingleFieldJSONTag, pattern TaggedObjectJSONData, pattern TaggedObjectJSONTag)
import Simplex.Messaging.Transport.Buffer (getBuffered)
import Simplex.Messaging.Transport.HTTP2 (HTTP2Body (..), HTTP2BodyChunk, getBodyChunk)
import Simplex.Messaging.Transport.HTTP2.Client (HTTP2Client, HTTP2Response (..), closeHTTP2Client, sendRequestDirect)
import Simplex.Messaging.Transport.HTTP2.File (hSendFile)
import Simplex.Messaging.Util (liftEitherError, liftEitherWith, tshow)
import System.FilePath ((</>), takeFileName)
import UnliftIO

data RemoteCommand
  = RCHello {deviceName :: Text}
  | RCSend {command :: Text} -- TODO maybe ChatCommand here?
  | RCRecv {wait :: Int} -- this wait should be less than HTTP timeout
  | -- local file encryption is determined by the host, but can be overridden for videos
    RCStoreFile {fileName :: String, fileSize :: Word32, fileDigest :: FileDigest} -- requires attachment
  | RCGetFile {file :: RemoteFile}
  deriving (Show)

data RemoteResponse
  = RRHello {encoding :: PlatformEncoding, deviceName :: Text, encryptFiles :: Bool}
  | RRChatResponse {chatResponse :: ChatResponse}
  | RRChatEvent {chatEvent :: Maybe ChatResponse} -- ^ 'Nothing' on poll timeout
  | RRFileStored {filePath :: String}
  | RRFile {fileSize :: Word32, fileDigest :: FileDigest} -- provides attachment , fileDigest :: FileDigest
  | RRProtocolError {remoteProcotolError :: RemoteProtocolError} -- ^ The protocol error happened on the server side
  deriving (Show)

-- Force platform-independent encoding as the types aren't UI-visible
$(deriveJSON (taggedObjectJSON $ dropPrefix "RC") ''RemoteCommand)
$(deriveJSON (taggedObjectJSON $ dropPrefix "RR") ''RemoteResponse)

-- * Client side / desktop

createRemoteHostClient :: HTTP2Client -> dh -> Text -> ExceptT RemoteProtocolError IO RemoteHostClient
createRemoteHostClient httpClient todo'dhKey desktopName = do
  logDebug "Sending initial hello"
  sendRemoteCommand' httpClient localEncoding Nothing RCHello {deviceName = desktopName} >>= \case
    RRHello {encoding, deviceName = mobileName, encryptFiles} -> do
      logDebug "Got initial hello"
      when (encoding == PEKotlin && localEncoding == PESwift) $ throwError RPEIncompatibleEncoding
      pure RemoteHostClient {hostEncoding = encoding, hostDeviceName = mobileName, httpClient, encryptHostFiles = encryptFiles}
    r -> badResponse r

closeRemoteHostClient :: MonadIO m => RemoteHostClient -> m ()
closeRemoteHostClient RemoteHostClient {httpClient} = liftIO $ closeHTTP2Client httpClient

-- ** Commands

remoteSend :: RemoteHostClient -> ByteString -> ExceptT RemoteProtocolError IO ChatResponse
remoteSend RemoteHostClient {httpClient, hostEncoding} cmd =
  sendRemoteCommand' httpClient hostEncoding Nothing RCSend {command = decodeUtf8 cmd} >>= \case
    RRChatResponse cr -> pure cr
    r -> badResponse r

remoteRecv :: RemoteHostClient -> Int -> ExceptT RemoteProtocolError IO (Maybe ChatResponse)
remoteRecv RemoteHostClient {httpClient, hostEncoding} ms =
  sendRemoteCommand' httpClient hostEncoding Nothing RCRecv {wait = ms} >>= \case
    RRChatEvent cr_ -> pure cr_
    r -> badResponse r

remoteStoreFile :: RemoteHostClient -> FilePath -> FilePath -> ExceptT RemoteProtocolError IO FilePath
remoteStoreFile RemoteHostClient {httpClient, hostEncoding} localPath fileName = do
  (fileSize, fileDigest) <- getFileInfo localPath
  let send h = sendRemoteCommand' httpClient hostEncoding (Just (h, fileSize)) RCStoreFile {fileName, fileSize, fileDigest}
  withFile localPath ReadMode send >>= \case
    RRFileStored {filePath = filePath'} -> pure filePath'
    r -> badResponse r

remoteGetFile :: RemoteHostClient -> FilePath -> RemoteFile -> ExceptT RemoteProtocolError IO ()
remoteGetFile RemoteHostClient {httpClient, hostEncoding} destDir rf@RemoteFile {fileSource = CryptoFile {filePath}} =
  sendRemoteCommand httpClient hostEncoding Nothing RCGetFile {file = rf} >>= \case
    (getChunk, RRFile {fileSize, fileDigest}) -> do
      -- TODO we could optimize by checking size and hash before receiving the file
      let localPath = destDir </> takeFileName filePath
      receiveRemoteFile getChunk fileSize fileDigest localPath
    (_, r) -> badResponse r

-- TODO validate there is no attachment
sendRemoteCommand' :: HTTP2Client -> PlatformEncoding -> Maybe (Handle, Word32) -> RemoteCommand -> ExceptT RemoteProtocolError IO RemoteResponse
sendRemoteCommand' http remoteEncoding attachment_ rc = snd <$> sendRemoteCommand http remoteEncoding attachment_ rc

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

badResponse :: RemoteResponse -> ExceptT RemoteProtocolError IO a
badResponse = \case
  RRProtocolError e -> throwError e
  -- TODO handle chat errors?
  r -> throwError $ RPEUnexpectedResponse $ tshow r

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
sizePrefixedEncode value = word32BE (fromIntegral $ LB.length json) <> lazyByteString json
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

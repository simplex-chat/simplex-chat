{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}

module Simplex.Chat.Remote.Protocol where

import Control.Logger.Simple
import Control.Monad
import Control.Monad.Except
import Data.Aeson ((.=))
import qualified Data.Aeson as J
import qualified Data.Aeson.Key as JK
import qualified Data.Aeson.KeyMap as JM
import Data.Aeson.TH (deriveJSON)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.ByteString.Builder (lazyByteString, int64LE, Builder)
import qualified Data.ByteString.Lazy as BL
import Data.String (fromString)
import Data.Text (Text)
import Data.Word (Word32)
import qualified Network.HTTP.Types as N
import qualified Network.HTTP2.Client as H
import Network.HTTP2.Server (responseBuilder)
import Simplex.Chat.Controller (ChatResponse)
import Simplex.Chat.Remote.Types
import Simplex.Messaging.Crypto.File (CryptoFile)
import Simplex.Messaging.Parsers (dropPrefix, sumTypeJSON, pattern SingleFieldJSONTag, pattern TaggedObjectJSONData, pattern TaggedObjectJSONTag)
import Simplex.Messaging.Transport.HTTP2 (HTTP2Body (..), HTTP2BodyChunk, getBodyChunk, defaultHTTP2BufferSize)
import Simplex.Messaging.Transport.HTTP2.Client (HTTP2Client, HTTP2ClientError, HTTP2Response (..), closeHTTP2Client, sendRequestDirect)
import Simplex.Messaging.Transport.HTTP2.File
import Simplex.Messaging.Transport.HTTP2.Server (HTTP2Request (..))
import Simplex.Messaging.Util (liftEitherError, liftEitherWith, tshow, liftIOEither)
import UnliftIO
import Simplex.Messaging.Transport.Buffer (getBuffered)
import Data.Binary.Get (getInt64le, runGetOrFail)
import Data.Maybe (isNothing)
import UnliftIO.Concurrent (threadDelay)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)

data RemoteCommand
  = RCSend {command :: Text} -- TODO maybe ChatCommand here?
  | RCRecv {wait :: Int} -- this wait should be less than HTTP timeout
  -- local file encryption is determined by the host, but can be overridden for videos
  | RCStoreFile {fileSize :: Word32, encrypt :: Maybe Bool} -- requires attachment
  | RCGetFile {filePath :: FilePath}
  deriving (Show)

data RemoteResponse
  = RRChatResponse {chatResponse :: ChatResponse}
  | RRChatEvent {chatEvent :: Maybe ChatResponse} -- ^ 'Nothing' on poll timeout
  | RRFileStored {fileSource :: CryptoFile}
  | RRFile {fileSize :: Word32} -- provides attachment
  | RRServerError {serverError :: RemoteProtocolError} -- ^ The protocol error happened on the server side
  | RRException {someException :: Text} -- ^ Handler crashed

$(deriveJSON (sumTypeJSON $ dropPrefix "RC") ''RemoteCommand)

$(deriveJSON (sumTypeJSON $ dropPrefix "RR") ''RemoteResponse)

createRemoteHostClient :: HTTP2Client -> Text -> ExceptT RemoteProtocolError IO RemoteHostClient
createRemoteHostClient httpClient localDeviceName = do
  rh@RemoteHello {encoding, deviceName} <- sendHello httpClient localDeviceName
  logInfo $ tshow rh
  when (encoding == PEKotlin && localEncoding == PESwift) $ throwError RPEIncompatibleEncoding
  pure RemoteHostClient {remoteEncoding = encoding, remoteDeviceName = deviceName, httpClient}

closeRemoteHostClient :: MonadIO m => RemoteHostClient -> m ()
closeRemoteHostClient RemoteHostClient {httpClient} = liftIO $ closeHTTP2Client httpClient

convertJSON :: PlatformEncoding -> PlatformEncoding -> J.Value -> J.Value
convertJSON _remote@PEKotlin _local@PEKotlin = id
convertJSON PESwift PESwift = id
convertJSON PESwift PEKotlin = owsf2tagged
convertJSON PEKotlin PESwift = error "unsupported convertJSON: K/S" -- guarded by createRemoteHostClient

-- | Initial query to coordinate codec etc.
sendHello :: HTTP2Client -> Text -> ExceptT RemoteProtocolError IO RemoteHello
sendHello c localDeviceName = do
  logInfo "Sending initial hello"
  (attachment_, hello) <- sendRemoteJSON c RemoteHello {encoding = localEncoding, deviceName = localDeviceName}
  unless (isNothing attachment_) $ throwError RPEUnexpectedFile
  pure hello

-- | Send chat command an
remoteSend :: RemoteHostClient -> ByteString -> ExceptT RemoteProtocolError IO ChatResponse
remoteSend RemoteHostClient {httpClient} cmd = sendRemoteJSON httpClient RCSend {command = decodeUtf8 cmd} >>= \case
  (Nothing, RRChatResponse cr) -> pure cr
  (Just _, RRChatEvent{}) -> throwError RPEUnexpectedFile
  (_, _) -> throwError RPEBadResponse

remoteRecv :: RemoteHostClient -> Int -> ExceptT RemoteProtocolError IO (Maybe ChatResponse)
remoteRecv RemoteHostClient {httpClient} ms = sendRemoteJSON httpClient RCRecv {wait=ms} >>= \case
  (Nothing, RRChatEvent cr_) -> pure cr_
  (Just _, RRChatEvent{}) -> throwError RPEUnexpectedFile
  (_, _) -> throwError RPEBadResponse

-- TODO: file attachments
sendRemoteJSON :: (J.ToJSON a, J.FromJSON b) => HTTP2Client -> a -> ExceptT RemoteProtocolError IO (Maybe BL.ByteString, b)
sendRemoteJSON http req = do
  HTTP2Response{response, respBody} <- liftEitherError (RPEHTTP2 . tshow) $ sendRequestDirect http httpRequest (Just 1000000)
  jsonBody response respBody
  where
    httpRequest = H.requestBuilder N.methodPost "/" mempty $  sizePrefixedEncode req

jsonBody :: (HTTP2BodyChunk a, J.FromJSON b) => a -> HTTP2Body -> ExceptT RemoteProtocolError IO (Maybe BL.ByteString, b)
jsonBody r body = do
  chunks <- liftIO (getHTTP2BodyChunks r body)
  (json, attachment) <- withExceptT (const RPEInvalidSize) $ sizedChunk chunks
  -- logDebug $ decodeUtf8 $ B.toStrict json
  res <- liftEitherWith (RPEInvalidJSON . fromString) $ J.eitherDecode' json
  pure (if BL.null attachment then Nothing else Just attachment, res)

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
getHTTP2BodyChunks r HTTP2Body {bodyHead, bodyBuffer} = go (BL.fromStrict bodyHead)
  where
    go :: BL.ByteString -> IO BL.ByteString
    go acc = do
      part <- getBuffered bodyBuffer defaultHTTP2BufferSize Nothing (getBodyChunk r)
      if B.null part
        then pure acc
        else go (acc <> BL.fromStrict part)

{- TODO:
\* Encode request
\* Issue HTTP request with a Builder body (http errors)
\* Stream body into a JSON parser (parsing errors)
-}

remoteStoreFile :: RemoteHostClient -> FilePath -> Maybe Bool -> ExceptT RemoteProtocolError IO CryptoFile
remoteStoreFile c localPath encrypt = error "remoteStoreFile" -- sendRemoteCommand c RCStoreFile {encrypt} (Just localPath)

{- TODO:
\* Get necessary info from FS
\* Encode request with a file metadata
\* Stream body as `len(json) <> json <> fileBody`
-}

-- TODO this should work differently for CLI and UI clients
-- CLI - potentially, create new unique names and report them as created
-- UI - always use the same names and report error if file already exists
-- alternatively, CLI should also use a fixed folder for remote session
-- Possibly, path in the database should be optional and CLI commands should allow configuring it per session or use temp or download folder
remoteGetFile :: RemoteHostClient -> FilePath -> ExceptT RemoteProtocolError IO FilePath
remoteGetFile c baseDir = error "remoteGetFile"

processControllerHello :: HTTP2Request -> ExceptT RemoteProtocolError IO ()
processControllerHello HTTP2Request{request, reqBody, sendResponse} = do
  logDebug "processControllerRequest"
  (attachment_, RemoteHello {deviceName=desktopDeviceName}) <- jsonBody request reqBody
  let reply = RemoteHello {encoding = localEncoding, deviceName = "mobileDeviceName"}
  liftIO . sendResponse $ responseBuilder N.status200 [] (sizePrefixedEncode reply)

getControllerCommand :: HTTP2Request -> ExceptT RemoteProtocolError IO (RemoteResponse -> IO (), (Maybe BL.ByteString, RemoteCommand))
getControllerCommand HTTP2Request{request, reqBody, sendResponse} = do
  logDebug "processControllerCommand"
  cmd <- jsonBody request reqBody
  let respond = liftIO . sendResponse . responseBuilder N.status200 [] . sizePrefixedEncode
  pure (respond, cmd)
{- TODO:
\* Get full chat command body
\* Run handler in a host controller context
\* Stream ChatResponse JSON with sendResponse (responseBuilder)
-}

{- TODO:
  * Encode request to locate the file
  * Stream body into a local file
    - Alternatively: receive length-prefixed json with metadata, then stream remaining data into a file
  -}

-- * Transport-level wrappers

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

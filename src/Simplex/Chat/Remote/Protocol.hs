{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}

module Simplex.Chat.Remote.Protocol where

import Control.Monad.Except
import Data.Int (Int64)
import Data.Aeson ((.=))
import qualified Data.Aeson as J
import qualified Data.Aeson.Key as JK
import qualified Data.Aeson.KeyMap as JM
import qualified Data.Aeson.TH as JQ
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Word (Word32)
import Simplex.Chat.Controller (ChatResponse)
import Simplex.Chat.Remote.Types
import Simplex.Messaging.Crypto.File (CryptoFile)
import Simplex.Messaging.Parsers (dropPrefix, enumJSON, pattern SingleFieldJSONTag, pattern TaggedObjectJSONData, pattern TaggedObjectJSONTag)
import Simplex.Messaging.Transport.HTTP2.Client (HTTP2Client, HTTP2ClientError, closeHTTP2Client)
import Simplex.Messaging.Transport.HTTP2.Server (HTTP2Request (..))
import Simplex.Messaging.Util (catchAllErrors, liftEitherError)
import Simplex.Chat.Types (User)
import UnliftIO

createRemoteHostClient :: HTTP2Client -> Text -> ExceptT RemoteClientError IO RemoteHostClient
createRemoteHostClient httpClient localDeviceName = do
  -- TODO this should fail if remote encoding is Kotlin and local is Swift
  RemoteHello {encoding, deviceName} <- sendHello httpClient localDeviceName
  pure RemoteHostClient {encoding, remoteDeviceName = deviceName, httpClient}

closeRemoteHostClient :: MonadIO m => RemoteHostClient -> m ()
closeRemoteHostClient RemoteHostClient {httpClient} = liftIO $ closeHTTP2Client httpClient

liftHTTP2 :: IO (Either HTTP2ClientError a) -> ExceptT RemoteClientError IO a
liftHTTP2 = liftEitherError $ RCEHTTP2 . show

convertJSON :: PlatformEncoding -> PlatformEncoding -> J.Value -> J.Value
convertJSON _remote@PEKotlin _local@PEKotlin = id
convertJSON PESwift PESwift = id
convertJSON PESwift PEKotlin = undefined -- owsf2tagged move here?
convertJSON PEKotlin PESwift = undefined -- unsupported

-- Initial query to coordinate codec etc.
sendHello :: HTTP2Client -> Text -> ExceptT RemoteClientError IO RemoteHello
sendHello c localDeviceName = undefined -- sendRemoteCommand c RCHello {deviceName = localDeviceName} Nothing

remoteSend :: RemoteHostClient -> ByteString -> ExceptT RemoteClientError IO ChatResponse
remoteSend c cmd = undefined -- sendRemoteCommand c RCSend {command = decodeUtf8 cmd} Nothing

remoteRecv :: RemoteHostClient -> Int -> ExceptT RemoteClientError IO (Maybe ChatResponse)
remoteRecv c wait = undefined -- sendRemoteCommand c RCRecv {wait} Nothing

sendRemoteCommand :: RemoteHostClient -> RemoteCommand -> Maybe FilePath -> ExceptT RemoteClientError IO (RemoteResponse, Maybe FilePath)
sendRemoteCommand RemoteHostClient {httpClient} cmd filePath_ = liftHTTP2 $ undefined

{- TODO:
\* Encode request
\* Issue HTTP request with a Builder body (http errors)
\* Stream body into a JSON parser (parsing errors)
-}

remoteStoreFile :: RemoteHostClient -> FilePath -> Maybe Bool -> ExceptT RemoteClientError IO CryptoFile
remoteStoreFile c localPath encrypt = undefined -- sendRemoteCommand c RCStoreFile {encrypt} (Just localPath)

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
remoteGetFile :: RemoteHostClient -> FilePath -> ExceptT RemoteClientError IO FilePath
remoteGetFile c baseDir = undefined

-- processControllerRequest :: forall m. ChatMonad m => (ByteString -> m ChatResponse) -> HTTP2.HTTP2Request -> m ()
processControllerRequest execChatCommand HTTP2Request {request, reqBody, sendResponse} = undefined
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

{- | Session initiation message.

Can't be a part of the 'RemoteRequest' sum as the sum encoding types needs to be coordinated first.
Also, must be used exactly once.
-}
data RemoteHello = RemoteHello
  { encoding :: PlatformEncoding,
    deviceName :: Text
  }
  deriving (Show)

-- * Transport-level wrappers

data RemoteCommand
  = RCSend {command :: Text} -- TODO maybe ChatCommand here?
  | RCRecv {wait :: Int} -- this wait should be less than HTTP timeout
  -- local file encryption is determined by the host, but can be overridden for videos
  | RCStoreFile {encrypt :: Maybe Bool}
  | RCGetFile {filePath :: FilePath}
  deriving (Show)

data RemoteResponse
  = RRChatResponse {chatResponse :: ChatResponse}
  | RRFileStored {fileSource :: CryptoFile}
  | RRFile {fileSize :: Word32}
  | RRError {error :: RemoteServerError} -- XXX: part of ExceptT already

data RemoteServerError = RemoteServerError

-- XXX: extract to Transport.HTTP2 ?
-- writeBodyToFile :: MonadUnliftIO m => FilePath -> HTTP2Body -> m ()
-- writeBodyToFile path HTTP2Body {bodyHead, bodySize, bodyPart} = do
--   logInfo $ "Receiving " <> tshow bodySize <> " bytes to " <> tshow path
--   liftIO . withFile path WriteMode $ \h -> do
--     hPut h bodyHead
--     mapM_ (hPutBodyChunks h) bodyPart

-- hPutBodyChunks :: Handle -> (Int -> IO ByteString) -> IO ()
-- hPutBodyChunks h getChunk = do
--   chunk <- getChunk defaultHTTP2BufferSize
--   unless (B.null chunk) $ do
--     hPut h chunk
--     hPutBodyChunks h getChunk

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

$(JQ.deriveJSON J.defaultOptions ''RemoteHello)

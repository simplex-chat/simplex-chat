{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Simplex.Chat.Remote.Protocol where

import Control.Monad.Except
import qualified Data.Aeson as J
import qualified Data.Aeson.TH as JQ
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Text (Text)
import Data.Word (Word32)
import Simplex.Chat.Controller (ChatResponse)
import Simplex.Chat.Remote.Types (RemoteHost (..), RemoteHostId)
import Simplex.Messaging.Crypto.File (CryptoFile)
import Simplex.Messaging.Parsers (dropPrefix, enumJSON)
import Simplex.Messaging.Transport.HTTP2.Client (HTTP2Client, HTTP2ClientError)
import Simplex.Messaging.Util (liftEitherError)

data RemoteHostClient = RemoteHostClient
  { remoteHostId :: RemoteHostId,
    encoding :: PlatformEncoding,
    deviceName :: Text,
    storePath :: FilePath,
    httpClient :: HTTP2Client
  }

createRemoteHostClient :: RemoteHost -> Text -> ExceptT RemoteError IO RemoteHostClient
createRemoteHostClient rh@RemoteHost {remoteHostId, storePath} localDeviceName = do
  c <- connectRemoteClient
  -- TODO this should fail if remote encoding is Kotlin and local is Swift
  RemoteHello {encoding, deviceName} <- sendHello c localDeviceName
  pure (c :: RemoteHostClient) {encoding, deviceName}
  where
    connectRemoteClient :: ExceptT RemoteError IO RemoteHostClient
    connectRemoteClient = do
      -- httpClient <- _
      pure
        RemoteHostClient
          { remoteHostId,
            encoding = localEncoding,
            deviceName = "",
            storePath,
            httpClient = undefined
          }

liftHTTP2 :: IO (Either HTTP2ClientError a) -> ExceptT RemoteError IO a
liftHTTP2 = liftEitherError $ REHTTP2 . show

convertJSON :: PlatformEncoding -> PlatformEncoding -> J.Value -> J.Value
convertJSON _remote@PEKotlin _local@PEKotlin = id
convertJSON PESwift PESwift = id
convertJSON PESwift PEKotlin = undefined -- owsf2tagged move here?
convertJSON PEKotlin PESwift = undefined -- unsupported

-- Initial query to coordinate codec etc.
sendHello :: RemoteHostClient -> Text -> ExceptT RemoteError IO RemoteHello
sendHello c localDeviceName = undefined -- sendRemoteCommand c RCHello Nothing

remoteSend :: RemoteHostClient -> ByteString -> ExceptT RemoteError IO ChatResponse
remoteSend c cmd = undefined -- sendRemoteCommand c (RCSend $ B.unpack cmd) Nothing

remoteRecv :: RemoteHostClient -> Int -> ExceptT RemoteError IO ChatResponse
remoteRecv c wait = undefined -- sendRemoteCommand c (RCRecv wait) Nothing

sendRemoteCommand :: RemoteHostClient -> RemoteCommand -> Maybe FilePath -> ExceptT RemoteError IO (RemoteResponse, Maybe FilePath)
sendRemoteCommand RemoteHostClient {httpClient} cmd filePath_ = liftHTTP2 $ undefined

{- TODO:
\* Encode request
\* Issue HTTP request with a Builder body (http errors)
\* Stream body into a JSON parser (parsing errors)
-}

remoteStoreFile :: RemoteHostClient -> FilePath -> Maybe Bool -> ExceptT RemoteError IO CryptoFile
remoteStoreFile c localPath encrypt = undefined

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
remoteGetFile :: RemoteHostClient -> FilePath -> ExceptT RemoteError IO (Maybe FilePath)
remoteGetFile c remotePath = undefined

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

-- TODO: put into a proper place
data PlatformEncoding
  = PESwift
  | PEKotlin
  deriving (Show)

localEncoding :: PlatformEncoding
#if defined(darwin_HOST_OS) && defined(swiftJSON)
localEncoding = PESwift
#else
localEncoding = PEKotlin
#endif

-- * Transport-level wrappers

data RemoteCommand
  = RCHello {deviceName :: Text}
  | RCSend {command :: String} -- TODO maybe ChatCommand here?
  | RCRecv {wait :: Int} -- this wait should be less than HTTP timeout
  -- local file encryption is determined by the host, but can be overridden for videos
  | RCStoreFile {encrypt :: Maybe Bool}
  | RCGetFile {filePath :: FilePath}
  deriving (Show)

data RemoteResponse
  = RRHello {encoding :: PlatformEncoding, deviceName :: Text}
  | RRChatResponse {chatResponse :: ChatResponse}
  | RRFileStored {fileSource :: CryptoFile}
  | RRFile {fileSize :: Word32}
  | RRError {error :: RemoteError}

data RemoteError
  = REInvalid -- failed to parse RemoteCommand or RemoteResponse
  | REUnexpected -- unexpected response
  | RENoChatResponse -- returned on timeout, the client would re-send the request
  | RENoFile
  | REHTTP2 String

$(JQ.deriveJSON (enumJSON $ dropPrefix "PE") ''PlatformEncoding)

$(JQ.deriveJSON J.defaultOptions ''RemoteHello)

{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}

module Simplex.Chat.Remote.Protocol where

import Control.Monad.Except (ExceptT)
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as J
import Data.ByteString (ByteString)
import Data.Int (Int64)
import Data.Text (Text)
import GHC.Generics (Generic)
import Simplex.Chat.Controller (ChatResponse, RemoteHostError)
import Simplex.Chat.Remote.Types (RemoteHostId)
import Simplex.Chat.Store.Remote (getRemoteHost)
import Simplex.Messaging.Transport.HTTP2.Client (HTTP2Client, HTTP2ClientError)
import Simplex.Messaging.Util (safeDecodeUtf8)

data RemoteHostClient = RemoteHostClient
  { remoteHostId :: RemoteHostId
  , storePath :: FilePath
  , http :: HTTP2Client
  , transcodeSwift :: J.Value -> J.Value
  }

createRemoteHostClient :: RemoteHostId -> ExceptT RemoteHostError m RemoteHostClient
createRemoteHostClient remoteHostId = do
  rh <- withStore' $ \db -> getRemoteHost db remoteHostId
  ourName <- getDeviceName
  http <- connectHttpClient rh
  theirHello <- sendHello http RemoteHello{encoding = localEncoding, displayName = ourName}
  pure
    RemoteHostClient
      { remoteHostId
      , storePath = rh.storePath
      , http = http
      , transcodeSwift = shouldConvert localEncoding (encoding theirHello)
      }

convertHttp2RHError :: RemoteHostError -> ExceptT HTTP2ClientError m RemoteHello -> ExceptT RemoteHostError m RemoteHello
convertHttp2RHError tag = _

getDeviceName :: ExceptT RemoteHostError m Text
getDeviceName = _

shouldConvert :: PlatformEncoding -> PlatformEncoding -> J.Value -> J.Value
shouldConvert PEKotlin PEKotlin = id
shouldConvert PESwift PESwift = id
shouldConvert PESwift PEKotlin = owsf2tagged
shouldConvert PEKotlin PESwift = _

-- Initial query to coordinate codec etc.
sendHello :: HTTP2Client -> RemoteHello -> ExceptT HTTP2ClientError m RemoteHello
sendHello http ours = convertHttp2RHError "Hello" $ httpRequest http ours

remoteSend :: RemoteHostClient -> ByteString -> ExceptT RemoteHostError m ChatResponse
remoteSend RemoteHostClient{http} cmd = convertHttp2RHError "Send" . httpRequest http $ RReqSend (safeDecodeUtf8 cmd)

remoteRecv :: RemoteHostClient -> ExceptT RemoteHostError m ChatResponse
remoteRecv RemoteHostClient{http} = convertHttp2RHError "Recv" . httpRequest http RReqRecv

httpRequest :: (J.ToJSON req, J.FromJSON res) => HTTP2Client -> req -> ExceptT HTTP2ClientError m res
httpRequest = _

{- TODO:
\* Encode request
\* Issue HTTP request with a Builder body (http errors)
\* Stream body into a JSON parser (parsing errors)
-}

remoteStoreFile :: RemoteHostClient -> FilePath -> ExceptT RemoteHostError m ()
remoteStoreFile = _

{- TODO:
\* Get necessary info from FS
\* Encode request with a file metadata
\* Stream body as `len(json) <> json <> fileBody`
-}

remoteFetchFile :: RemoteHostClient -> UserId -> FileId -> ExceptT RemoteHostError m ()
remoteFetchFile = _

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
  { encoding :: PlatformEncoding
  , displayName :: Text
  }
  deriving (Show, Generic, FromJSON, ToJSON)

-- TODO: put into a proper place
data PlatformEncoding
  = PESwift
  | PEKotlin
  deriving (Show, Generic, FromJSON, ToJSON) -- XXX: Not an UI type, no platform codecs needed

localEncoding :: PlatformEncoding
#if defined(darwin_HOST_OS) && defined(swiftJSON)
localEncoding = PESwift
#else
localEncoding = PEKotlin
#endif

-- * Transport-level wrappers

data RemoteRequest
  = RReqSend Text
  | RReqRecv
  | RReqStore RemoteStoreFile
  | RReqFetch RemoteFetchFile
  deriving (Show, Generic, FromJSON, ToJSON)

data RemoteFetchFile = RemoteFetchFile
  { userId :: Int64
  , fileId :: Int64
  }
  deriving (Show, Generic, FromJSON, ToJSON)

data RemoteStoreFile = RemoteStoreFile
  { todo'fileMetadata :: FilePath
  }
  deriving (Show, Generic, FromJSON, ToJSON)

-- XXX: No need for a separate sum wrapper for response.
-- Response are all generic types and their parser is known at the request call site (`remoteSend` etc.)

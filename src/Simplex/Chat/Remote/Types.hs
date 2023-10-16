{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

module Simplex.Chat.Remote.Types where

import Control.Concurrent.Async (Async)
import Control.Exception
import qualified Data.Aeson.TH as J
import Data.Int (Int64)
import Data.Text (Text)
import qualified Simplex.Messaging.Crypto as C
import Simplex.Messaging.Transport.HTTP2.Client (HTTP2Client)
import Simplex.Messaging.Parsers (dropPrefix, enumJSON, sumTypeJSON)

data RemoteHostClient = RemoteHostClient
  { encoding :: PlatformEncoding,
    remoteDeviceName :: Text,
    httpClient :: HTTP2Client
  }

data RemoteHostSession
  = RemoteHostSessionConnecting {setupAsync :: Async ()}
  | RemoteHostSessionStarted {remoteHostClient :: RemoteHostClient, storePath :: FilePath}

data RemoteClientError
  = RCEInvalid -- ^ failed to parse RemoteCommand or RemoteResponse
  | RCEUnexpected -- ^ unexpected response
  -- | RCENoChatResponse -- returned on timeout, the client would re-send the request
  -- RCE: doesn't look like an exceptional situation/error, but also distorts consumer into sorting through exceptions pattern where a traversal would work
  | RCENoFile
  | RCEHTTP2 Text
  deriving (Show, Exception)

type RemoteHostId = Int64

data RemoteHost = RemoteHost
  { remoteHostId :: RemoteHostId,
    storePath :: FilePath,
    displayName :: Text,
    -- | Credentials signing key for root and session certs
    caKey :: C.APrivateSignKey,
    -- | A stable part of TLS credentials used in remote session
    caCert :: C.SignedCertificate,
    contacted :: Bool
  }
  deriving (Show)

data RemoteCtrlOOB = RemoteCtrlOOB
  { fingerprint :: C.KeyHash,
    displayName :: Text
  }
  deriving (Show)

data RemoteHostInfo = RemoteHostInfo
  { remoteHostId :: RemoteHostId,
    storePath :: FilePath,
    displayName :: Text,
    remoteCtrlOOB :: RemoteCtrlOOB,
    sessionActive :: Bool
  }
  deriving (Show)

type RemoteCtrlId = Int64

data RemoteCtrl = RemoteCtrl
  { remoteCtrlId :: RemoteCtrlId,
    displayName :: Text,
    fingerprint :: C.KeyHash,
    accepted :: Maybe Bool
  }
  deriving (Show)

data RemoteCtrlInfo = RemoteCtrlInfo
  { remoteCtrlId :: RemoteCtrlId,
    displayName :: Text,
    fingerprint :: C.KeyHash,
    accepted :: Maybe Bool,
    sessionActive :: Bool
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

$(J.deriveJSON (sumTypeJSON $ dropPrefix "RCE") ''RemoteClientError)

$(J.deriveJSON (enumJSON $ dropPrefix "PE") ''PlatformEncoding)

$(J.deriveJSON J.defaultOptions ''RemoteCtrlOOB)

$(J.deriveJSON J.defaultOptions ''RemoteHostInfo)

$(J.deriveJSON J.defaultOptions {J.omitNothingFields = True} ''RemoteCtrl)

$(J.deriveJSON J.defaultOptions {J.omitNothingFields = True} ''RemoteCtrlInfo)

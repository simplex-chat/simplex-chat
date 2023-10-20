{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

module Simplex.Chat.Remote.Types where

import Control.Exception
import qualified Data.Aeson.TH as J
import Data.Int (Int64)
import Data.Text (Text)
import qualified Simplex.Messaging.Crypto as C
import Simplex.Messaging.Transport.HTTP2.Client (HTTP2Client)
import Simplex.Messaging.Parsers (dropPrefix, enumJSON, sumTypeJSON)
import UnliftIO

data RemoteHostClient = RemoteHostClient
  { remoteEncoding :: PlatformEncoding,
    remoteDeviceName :: Text,
    httpClient :: HTTP2Client
  }

data RemoteHostSession = RemoteHostSession
  { remoteHostTasks :: Tasks,
    remoteHostClient :: Maybe RemoteHostClient,
    storePath :: FilePath
  }

data RemoteProtocolError
  = RPEInvalidSize -- ^ size prefix is malformed
  | RPEInvalidJSON Text -- ^ failed to parse RemoteCommand or RemoteResponse
  | RPEIncompatibleEncoding
  | RPEUnexpectedFile
  | RPENoFile
  | RPEFileTooLarge
  | RPEBadResponse -- ^ Wrong response received for the command sent
  | RPEStoredFileExists -- ^ A file already exists in the destination position
  | RPEHTTP2 Text
  | RPEIOError Text
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
  deriving (Show, Eq)

localEncoding :: PlatformEncoding
#if defined(darwin_HOST_OS) && defined(swiftJSON)
localEncoding = PESwift
#else
localEncoding = PEKotlin
#endif

type Tasks = TVar [Async ()]

asyncRegistered :: MonadUnliftIO m => Tasks -> m () -> m ()
asyncRegistered tasks action = async action >>= registerAsync tasks

registerAsync :: MonadIO m => Tasks -> Async () -> m ()
registerAsync tasks = atomically . modifyTVar tasks . (:)

cancelTasks :: (MonadIO m) => Tasks -> m ()
cancelTasks tasks = readTVarIO tasks >>= mapM_ cancel

$(J.deriveJSON (sumTypeJSON $ dropPrefix "RPE") ''RemoteProtocolError)

$(J.deriveJSON (enumJSON $ dropPrefix "PE") ''PlatformEncoding)

$(J.deriveJSON J.defaultOptions ''RemoteCtrlOOB)

$(J.deriveJSON J.defaultOptions ''RemoteHostInfo)

$(J.deriveJSON J.defaultOptions {J.omitNothingFields = True} ''RemoteCtrl)

$(J.deriveJSON J.defaultOptions {J.omitNothingFields = True} ''RemoteCtrlInfo)

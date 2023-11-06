{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Simplex.Chat.Remote.Types where

import Control.Exception (Exception)
import qualified Data.Aeson.TH as J
import Data.Int (Int64)
import Data.Text (Text)
import qualified Simplex.Messaging.Crypto as C
import Simplex.Messaging.Parsers (defaultJSON, dropPrefix, enumJSON, sumTypeJSON)
import Simplex.Messaging.Transport.HTTP2.Client (HTTP2Client)
import Simplex.RemoteControl.Types (Tasks)
import Simplex.Messaging.Crypto.File (CryptoFile)

data RemoteHostClient = RemoteHostClient
  { hostEncoding :: PlatformEncoding,
    hostDeviceName :: Text,
    httpClient :: HTTP2Client,
    encryptHostFiles :: Bool
  }

-- data RemoteHostSession = RemoteHostSession
--   { remoteHostTasks :: Tasks,
--     remoteHostClient :: Maybe RemoteHostClient,
--     storePath :: FilePath,
--   }

data RemoteHostSession
  = RHSessionStarting
  | RHSessionConnecting {rchClient :: RCHostClient}
  | RHSessionConnected {rhClient :: RemoteHostClient, storePath :: FilePath}

data RemoteProtocolError
  = -- | size prefix is malformed
    RPEInvalidSize
  | -- | failed to parse RemoteCommand or RemoteResponse
    RPEInvalidJSON {invalidJSON :: Text}
  | RPEIncompatibleEncoding
  | RPEUnexpectedFile
  | RPENoFile
  | RPEFileSize
  | RPEFileDigest
  | -- | Wrong response received for the command sent
    RPEUnexpectedResponse {response :: Text}
  | -- | A file already exists in the destination position
    RPEStoredFileExists
  | RPEHTTP2 {http2Error :: Text}
  | RPEException {someException :: Text}
  deriving (Show, Exception)

type RemoteHostId = Int64

data RHKey = RHNew | RHId RemoteHostId

data RemoteHost = RemoteHost
  { remoteHostId :: RHId,
    -- caFingerprint :: C.KeyHash,
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

data RemoteFile = RemoteFile
  { userId :: Int64,
    fileId :: Int64,
    sent :: Bool,
    fileSource :: CryptoFile
  }
  deriving (Show)

$(J.deriveJSON defaultJSON ''RemoteFile)

$(J.deriveJSON (sumTypeJSON $ dropPrefix "RPE") ''RemoteProtocolError)

$(J.deriveJSON (enumJSON $ dropPrefix "PE") ''PlatformEncoding)

$(J.deriveJSON defaultJSON ''RemoteHostInfo)

$(J.deriveJSON defaultJSON ''RemoteCtrl)

$(J.deriveJSON defaultJSON ''RemoteCtrlInfo)

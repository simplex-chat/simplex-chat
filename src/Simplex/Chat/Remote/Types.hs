{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

module Simplex.Chat.Remote.Types where

import qualified Data.Aeson.TH as J
import Data.Int (Int64)
import Data.Text (Text)
import qualified Simplex.Messaging.Crypto as C

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

$(J.deriveJSON J.defaultOptions ''RemoteCtrlOOB)

data RemoteHostInfo = RemoteHostInfo
  { remoteHostId :: RemoteHostId,
    storePath :: FilePath,
    displayName :: Text,
    remoteCtrlOOB :: RemoteCtrlOOB,
    sessionActive :: Bool
  }
  deriving (Show)

$(J.deriveJSON J.defaultOptions ''RemoteHostInfo)

type RemoteCtrlId = Int64

data RemoteCtrl = RemoteCtrl
  { remoteCtrlId :: RemoteCtrlId,
    displayName :: Text,
    fingerprint :: C.KeyHash,
    accepted :: Maybe Bool
  }
  deriving (Show)

$(J.deriveJSON J.defaultOptions {J.omitNothingFields = True} ''RemoteCtrl)

data RemoteCtrlInfo = RemoteCtrlInfo
  { remoteCtrlId :: RemoteCtrlId,
    displayName :: Text,
    fingerprint :: C.KeyHash,
    accepted :: Maybe Bool,
    sessionActive :: Bool
  }
  deriving (Show)

$(J.deriveJSON J.defaultOptions {J.omitNothingFields = True} ''RemoteCtrlInfo)

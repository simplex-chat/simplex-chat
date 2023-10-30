{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

module Simplex.Chat.Types.Remote where

import qualified Data.Aeson.TH as JQ
import Data.Int (Int64)
import Data.Text (Text)
import qualified Simplex.Messaging.Crypto as C
import Simplex.Messaging.Parsers (defaultJSON)

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

$(JQ.deriveJSON defaultJSON ''RemoteHostInfo)

$(JQ.deriveJSON defaultJSON ''RemoteCtrlInfo)

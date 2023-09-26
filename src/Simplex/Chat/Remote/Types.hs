{-# LANGUAGE DuplicateRecordFields #-}

module Simplex.Chat.Remote.Types where

import Control.Concurrent.Async (Async)
import Data.ByteString.Char8 (ByteString)
import Data.Int (Int64)
import Data.Text (Text)
import qualified Simplex.Messaging.Crypto as C
import Simplex.Messaging.Transport.HTTP2.Client (HTTP2Client)

type RemoteHostId = Int64

data RemoteHost = RemoteHost
  { remoteHostId :: RemoteHostId,
    displayName :: Text,
    -- | Path to store replicated files
    storePath :: FilePath,
    -- | A stable part of X509 credentials used to access the host
    caCert :: ByteString,
    -- | Credentials signing key for root and session certs
    caKey :: C.Key
  }

type RemoteCtrlId = Int

data RemoteCtrl = RemoteCtrl
  { remoteCtrlId :: RemoteCtrlId,
    displayName :: Text,
    fingerprint :: Text
  }

data RemoteHostSession = RemoteHostSession
  { -- | process to communicate with the host
    hostAsync :: Async (),
    -- | Path for local resources to be synchronized with host
    storePath :: FilePath,
    ctrlClient :: HTTP2Client
  }

-- | Host-side dual to RemoteHostSession, on-methods represent HTTP API.
data RemoteCtrlSession = RemoteCtrlSession
  { -- | process to communicate with the remote controller
    ctrlAsync :: Async ()
    -- server :: HTTP2Server
  }

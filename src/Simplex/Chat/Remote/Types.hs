{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Simplex.Chat.Remote.Types where

import Control.Concurrent.Async (Async)
import Data.Aeson (ToJSON (..), FromJSON)
import qualified Data.Aeson as J
import Data.Int (Int64)
import Data.Text (Text)
import GHC.Generics (Generic)
import qualified Simplex.Messaging.Crypto as C
import Simplex.Messaging.TMap (TMap)
import Simplex.Messaging.Transport.Client (TransportHost)
import Simplex.Messaging.Transport.HTTP2.Client (HTTP2Client)
import UnliftIO.STM

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

type RemoteCtrlId = Int64

data RemoteCtrl = RemoteCtrl
  { remoteCtrlId :: RemoteCtrlId,
    displayName :: Text,
    fingerprint :: C.KeyHash,
    accepted :: Maybe Bool
  }
  deriving (Show, Generic, FromJSON)

instance ToJSON RemoteCtrl where toEncoding = J.genericToEncoding J.defaultOptions

data RemoteHostSession
  = RemoteHostSessionStarting
      { announcer :: Async ()
      }
  | RemoteHostSessionStarted
      { -- | Path for local resources to be synchronized with host
        storePath :: FilePath,
        ctrlClient :: HTTP2Client
      }

data RemoteCtrlSession = RemoteCtrlSession
  { -- | Server side of transport to process remote commands and forward notifications
    discoverer :: Async (),
    supervisor :: Async (),
    hostServer :: Maybe (Async ()),
    discovered :: TMap C.KeyHash TransportHost,
    accepted :: TMVar RemoteCtrlId
  }

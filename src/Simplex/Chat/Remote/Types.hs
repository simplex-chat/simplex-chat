{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Simplex.Chat.Remote.Types where

import Control.Concurrent.Async (Async)
import Control.Concurrent.STM (TVar)
import Control.Exception (Exception)
import Crypto.Random (ChaChaDRG)
import qualified Data.Aeson.TH as J
import Data.ByteString (ByteString)
import Data.IORef (IORef)
import Data.Int (Int64)
import Data.Text (Text)
import Data.Word (Word16)
import Simplex.Chat.Remote.AppVersion
import Simplex.Chat.Types (verificationCode)
import qualified Simplex.Messaging.Crypto as C
import Simplex.Messaging.Crypto.File (CryptoFile)
import Simplex.Messaging.Crypto.SNTRUP761 (KEMHybridSecret)
import Simplex.Messaging.Parsers (defaultJSON, dropPrefix, enumJSON, sumTypeJSON)
import Simplex.Messaging.Transport (TLS (..))
import Simplex.Messaging.Transport.HTTP2.Client (HTTP2Client)
import Simplex.RemoteControl.Client
import Simplex.RemoteControl.Types

data RemoteHostClient = RemoteHostClient
  { hostEncoding :: PlatformEncoding,
    hostDeviceName :: Text,
    httpClient :: HTTP2Client,
    encryption :: RemoteCrypto,
    encryptHostFiles :: Bool,
    storePath :: FilePath
  }

data RemoteCrypto = RemoteCrypto
  { drg :: IORef ChaChaDRG,
    counter :: TVar Int64,
    sessionCode :: ByteString,
    hybridKey :: KEMHybridSecret,
    signatures :: RemoteSignatures
  }

data RemoteSignatures
  = RSSign
      { idPrivKey :: C.PrivateKeyEd25519,
        sessPrivKey :: C.PrivateKeyEd25519
      }
  | RSVerify
      { idPubKey :: C.PublicKeyEd25519,
        sessPubKey :: C.PublicKeyEd25519
      }

type SessionSeq = Int

data RHPendingSession = RHPendingSession
  { rhKey :: RHKey,
    rchClient :: RCHostClient,
    rhsWaitSession :: Async (),
    remoteHost_ :: Maybe RemoteHostInfo
  }

data RemoteHostSession
  = RHSessionStarting
  | RHSessionConnecting {invitation :: Text, rhPendingSession :: RHPendingSession}
  | RHSessionPendingConfirmation {sessionCode :: Text, tls :: TLS, rhPendingSession :: RHPendingSession}
  | RHSessionConfirmed {tls :: TLS, rhPendingSession :: RHPendingSession}
  | RHSessionConnected
      { rchClient :: RCHostClient,
        tls :: TLS,
        rhClient :: RemoteHostClient,
        pollAction :: Async (),
        storePath :: FilePath
      }

data RemoteHostSessionState
  = RHSStarting
  | RHSConnecting {invitation :: Text}
  | RHSPendingConfirmation {sessionCode :: Text}
  | RHSConfirmed {sessionCode :: Text}
  | RHSConnected {sessionCode :: Text}
  deriving (Show)

rhsSessionState :: RemoteHostSession -> RemoteHostSessionState
rhsSessionState = \case
  RHSessionStarting -> RHSStarting
  RHSessionConnecting {invitation} -> RHSConnecting {invitation}
  RHSessionPendingConfirmation {tls} -> RHSPendingConfirmation {sessionCode = tlsSessionCode tls}
  RHSessionConfirmed {tls} -> RHSConfirmed {sessionCode = tlsSessionCode tls}
  RHSessionConnected {tls} -> RHSConnected {sessionCode = tlsSessionCode tls}

tlsSessionCode :: TLS -> Text
tlsSessionCode = verificationCode . tlsUniq

data RemoteProtocolError
  = -- | size prefix is malformed
    RPEInvalidSize
  | -- | failed to parse RemoteCommand or RemoteResponse
    RPEInvalidJSON {invalidJSON :: String}
  | RPEInvalidBody {invalidBody :: String}
  | PRESessionCode
  | RPEIncompatibleEncoding
  | RPEUnexpectedFile
  | RPENoFile
  | RPEFileSize
  | RPEFileDigest
  | -- | Wrong response received for the command sent
    RPEUnexpectedResponse {response :: Text}
  | -- | A file already exists in the destination position
    RPEStoredFileExists
  | PRERemoteControl {rcError :: RCErrorType}
  | RPEHTTP2 {http2Error :: Text}
  | RPEException {someException :: Text}
  deriving (Show, Exception)

type RemoteHostId = Int64

data RHKey = RHNew | RHId {remoteHostId :: RemoteHostId}
  deriving (Eq, Ord, Show)

-- | Storable/internal remote host data
data RemoteHost = RemoteHost
  { remoteHostId :: RemoteHostId,
    hostDeviceName :: Text,
    storePath :: FilePath,
    bindAddress_ :: Maybe RCCtrlAddress,
    bindPort_ :: Maybe Word16,
    hostPairing :: RCHostPairing
  }

-- | UI-accessible remote host information
data RemoteHostInfo = RemoteHostInfo
  { remoteHostId :: RemoteHostId,
    hostDeviceName :: Text,
    storePath :: FilePath,
    bindAddress_ :: Maybe RCCtrlAddress,
    bindPort_ :: Maybe Word16,
    sessionState :: Maybe RemoteHostSessionState
  }
  deriving (Show)

type RemoteCtrlId = Int64

-- | Storable/internal remote controller data
data RemoteCtrl = RemoteCtrl
  { remoteCtrlId :: RemoteCtrlId,
    ctrlDeviceName :: Text,
    ctrlPairing :: RCCtrlPairing
  }

remoteCtrlId' :: RemoteCtrl -> RemoteCtrlId
remoteCtrlId' = remoteCtrlId

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

data CtrlAppInfo = CtrlAppInfo
  { appVersionRange :: AppVersionRange,
    deviceName :: Text
  }
  deriving (Show)

data HostAppInfo = HostAppInfo
  { appVersion :: AppVersion,
    deviceName :: Text,
    encoding :: PlatformEncoding,
    encryptFiles :: Bool -- if the host encrypts files in app storage
  }

$(J.deriveJSON defaultJSON ''RemoteFile)

$(J.deriveJSON (sumTypeJSON $ dropPrefix "RPE") ''RemoteProtocolError)

$(J.deriveJSON (sumTypeJSON $ dropPrefix "RH") ''RHKey)

$(J.deriveJSON (enumJSON $ dropPrefix "PE") ''PlatformEncoding)

$(J.deriveJSON (sumTypeJSON $ dropPrefix "RHS") ''RemoteHostSessionState)

$(J.deriveJSON defaultJSON ''RemoteHostInfo)

$(J.deriveJSON defaultJSON ''CtrlAppInfo)

$(J.deriveJSON defaultJSON ''HostAppInfo)

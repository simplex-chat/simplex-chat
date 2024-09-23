{-# LANGUAGE BangPatterns #-}
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
import Control.Concurrent.STM
import Control.Exception (Exception)
import Control.Monad (when)
import qualified Data.Aeson.TH as J
import Data.ByteString (ByteString)
import Data.Int (Int64)
import Data.Text (Text)
import Data.Word (Word16, Word32)
import Simplex.Chat.Remote.AppVersion
import Simplex.Chat.Types (verificationCode)
import qualified Simplex.Messaging.Crypto as C
import Simplex.Messaging.Crypto.File (CryptoFile)
import Simplex.Messaging.Parsers (defaultJSON, dropPrefix, enumJSON, sumTypeJSON)
import Simplex.Messaging.Transport (TLS (..), TSbChainKeys (..))
import Simplex.Messaging.Transport.HTTP2.Client (HTTP2Client)
import qualified Simplex.Messaging.TMap as TM
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
  { sessionCode :: ByteString,
    sndCounter :: TVar Word32,
    rcvCounter :: TVar Word32,
    chainKeys :: TSbChainKeys,
    skippedKeys :: TM.TMap Word32 (C.SbKeyNonce, C.SbKeyNonce),
    signatures :: RemoteSignatures
  }

getRemoteSndKeys :: RemoteCrypto -> STM (Word32, C.SbKeyNonce, C.SbKeyNonce)
getRemoteSndKeys RemoteCrypto {sndCounter, chainKeys = TSbChainKeys {sndKey}} = do
  corrId <- stateTVar sndCounter $ \c -> let !c' = c + 1 in (c', c')
  cmdKN <- stateTVar sndKey C.sbcHkdf
  fileKN <- stateTVar sndKey C.sbcHkdf
  pure (corrId, cmdKN, fileKN)

getRemoteRcvKeys :: RemoteCrypto -> Word32 -> STM (Either RemoteProtocolError (C.SbKeyNonce, C.SbKeyNonce))
getRemoteRcvKeys RemoteCrypto {rcvCounter, chainKeys = TSbChainKeys {rcvKey}, skippedKeys} !corrId =
  readTVar rcvCounter >>= getRcvKeys
  where
    getRcvKeys prevCorrId
      | prevCorrId > corrId =
          let err = PREEarlierId $ prevCorrId - corrId
           in maybe (Left err) Right <$> TM.lookupDelete corrId skippedKeys
      | prevCorrId == corrId =
          pure $ Left PREDuplicateId
      | prevCorrId + maxSkip < corrId =
          pure $ Left $ RPEManySkippedIds (corrId - prevCorrId)
      | otherwise = do -- prevCorrId < corrId
          writeTVar rcvCounter corrId
          skipKeys (prevCorrId + 1)
          Right <$> getKeys
    maxSkip = 256
    getKeys = (,) <$> stateTVar rcvKey C.sbcHkdf <*> stateTVar rcvKey C.sbcHkdf
    skipKeys !cId =
      when (cId < corrId) $ do
        keys <- getKeys
        TM.insert cId keys skippedKeys
        skipKeys (cId + 1)

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
  | RPEManySkippedIds Word32
  | PREEarlierId Word32
  | PREDuplicateId
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

{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Simplex.Chat.Remote.Transport where

import Control.Monad
import Control.Monad.Except
import Data.ByteString (ByteString)
import Data.ByteString.Builder (Builder, byteString)
import qualified Data.ByteString.Lazy as LB
import Data.Word (Word32)
import Simplex.Chat.Remote.Types
import Simplex.FileTransfer.Description (FileDigest (..))
import Simplex.FileTransfer.Transport (ReceiveFileError (..), receiveSbFile, sendEncFile)
import qualified Simplex.Messaging.Crypto as C
import qualified Simplex.Messaging.Crypto.Lazy as LC
import Simplex.Messaging.Encoding
import Simplex.Messaging.Util (liftEitherError, liftEitherWith)
import Simplex.RemoteControl.Types (RCErrorType (..))
import UnliftIO
import UnliftIO.Directory (getFileSize)

type EncryptedFile = ((Handle, Word32), C.CbNonce, LC.SbState)

prepareEncryptedFile :: RemoteCrypto -> (Handle, Word32) -> ExceptT RemoteProtocolError IO EncryptedFile
prepareEncryptedFile RemoteCrypto {drg, hybridKey} f = do
  nonce <- atomically $ C.randomCbNonce drg
  sbState <- liftEitherWith (const $ PRERemoteControl RCEEncrypt) $ LC.kcbInit hybridKey nonce
  pure (f, nonce, sbState)

sendEncryptedFile :: EncryptedFile -> (Builder -> IO ()) -> IO ()
sendEncryptedFile ((h, sz), nonce, sbState) send = do
  send $ byteString $ smpEncode ('\x01', nonce, sz + fromIntegral C.authTagSize)
  sendEncFile h send sbState sz

receiveEncryptedFile :: RemoteCrypto -> (Int -> IO ByteString) -> Word32 -> FileDigest -> FilePath -> ExceptT RemoteProtocolError IO ()
receiveEncryptedFile RemoteCrypto {hybridKey} getChunk fileSize fileDigest toPath = do
  c <- liftIO $ getChunk 1
  unless (c == "\x01") $ throwError RPENoFile
  nonce <- liftEitherError RPEInvalidBody $ smpDecode <$> getChunk 24
  size <- liftEitherError RPEInvalidBody $ smpDecode <$> getChunk 4
  unless (size == fileSize + fromIntegral C.authTagSize) $ throwError RPEFileSize
  sbState <- liftEitherWith (const $ PRERemoteControl RCEDecrypt) $ LC.kcbInit hybridKey nonce
  liftEitherError fErr $ withFile toPath WriteMode $ \h -> receiveSbFile getChunk h sbState fileSize
  digest <- liftIO $ LC.sha512Hash <$> LB.readFile toPath
  unless (FileDigest digest == fileDigest) $ throwError RPEFileDigest
  where
    fErr RFESize = RPEFileSize
    fErr RFECrypto = PRERemoteControl RCEDecrypt

getFileInfo :: FilePath -> ExceptT RemoteProtocolError IO (Word32, FileDigest)
getFileInfo filePath = do
  fileDigest <- liftIO $ FileDigest . LC.sha512Hash <$> LB.readFile filePath
  fileSize' <- getFileSize filePath
  when (fileSize' > toInteger (maxBound :: Word32)) $ throwError RPEFileSize
  pure (fromInteger fileSize', fileDigest)

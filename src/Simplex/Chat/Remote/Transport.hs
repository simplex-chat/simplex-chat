module Simplex.Chat.Remote.Transport where

import Control.Monad
import Control.Monad.Except
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LB
import Data.Word (Word32)
import Simplex.FileTransfer.Description (FileDigest (..))
import Simplex.Chat.Remote.Types
import qualified Simplex.Messaging.Crypto.Lazy as LC
import Simplex.Messaging.Transport.HTTP2.File (hReceiveFile)
import UnliftIO
import UnliftIO.Directory (getFileSize)

receiveRemoteFile :: (Int -> IO ByteString) -> Word32 -> FileDigest -> FilePath -> ExceptT RemoteProtocolError IO ()
receiveRemoteFile getChunk fileSize fileDigest toPath = do
  diff <- liftIO $ withFile toPath WriteMode $ \h -> hReceiveFile getChunk h fileSize
  unless (diff == 0) $ throwError RPEFileSize
  digest <- liftIO $ LC.sha512Hash <$> LB.readFile toPath
  unless (FileDigest digest == fileDigest) $ throwError RPEFileDigest

getFileInfo :: FilePath -> ExceptT RemoteProtocolError IO (Word32, FileDigest)
getFileInfo filePath = do
  fileDigest <- liftIO $ FileDigest . LC.sha512Hash <$> LB.readFile filePath
  fileSize' <- getFileSize filePath
  when (fileSize' > toInteger (maxBound :: Word32)) $ throwError RPEFileSize
  pure (fromInteger fileSize', fileDigest)

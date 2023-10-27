module Simplex.Chat.Remote.Transport where

import Control.Monad
import Control.Monad.Except
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LB
import Data.Word (Word32)
import Simplex.FileTransfer.Description (FileDigest (..))
import Simplex.Chat.Remote.Types
import Simplex.Messaging.Crypto.Lazy as LC
import Simplex.Messaging.Transport.HTTP2.File (hReceiveFile)
import UnliftIO

receiveRemoteFile :: (Int -> IO ByteString) -> Word32 -> FileDigest -> FilePath -> ExceptT RemoteProtocolError IO ()
receiveRemoteFile getChunk fileSize fileDigest toPath = do
  diff <- liftIO $ withFile toPath WriteMode $ \h -> hReceiveFile getChunk h fileSize
  unless (diff == 0) $ throwError RPEFileSize
  digest <- liftIO $ LC.sha512Hash <$> LB.readFile toPath
  unless (FileDigest digest == fileDigest) $ throwError RPEFileDigest

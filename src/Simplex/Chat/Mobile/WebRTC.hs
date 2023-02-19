module Simplex.Chat.Mobile.WebRTC where

import Control.Monad.Except
import qualified Crypto.Cipher.Types as AES
import Crypto.Random (getRandomBytes)
import qualified Data.ByteArray as BA
import qualified Data.ByteString as B
import qualified Data.ByteString.Base64.URL as U
import Data.ByteString.Internal (ByteString (PS), memcpy)
import Data.Either (fromRight)
import Data.Word (Word8)
import Foreign.C (CInt, CString)
import Foreign.ForeignPtr (newForeignPtr_)
import Foreign.ForeignPtr.Unsafe (unsafeForeignPtrToPtr)
import Foreign.Ptr (Ptr, plusPtr)
import qualified Simplex.Messaging.Crypto as C

cChatEncryptMedia :: CString -> Ptr Word8 -> CInt -> IO ()
cChatEncryptMedia = cTransformMedia chatEncryptMedia

cChatDecryptMedia :: CString -> Ptr Word8 -> CInt -> IO ()
cChatDecryptMedia = cTransformMedia chatDecryptMedia

cTransformMedia :: (ByteString -> ByteString -> IO ByteString) -> CString -> Ptr Word8 -> CInt -> IO ()
cTransformMedia f cKey cFrame cFrameLen = do
  key <- B.packCString cKey
  frame <- getByteString cFrame cFrameLen
  frame' <- f key frame
  putByteString frame' cFrame cFrameLen
{-# INLINE cTransformMedia #-}

chatEncryptMedia :: ByteString -> ByteString -> IO ByteString
chatEncryptMedia keyStr frame = fromRight frame <$> encrypt
  where
    encrypt = runExceptT $ do
      key <- liftEither $ U.decode keyStr
      iv <- liftIO $ getRandomBytes ivSize
      let (frame', _) = B.splitAt (B.length frame - C.authTagSize - ivSize) frame
      (tag, frame'') <- withExceptT show $ C.encryptAESNoPad (C.Key key) (C.IV iv) frame'
      let authTag = BA.convert $ C.unAuthTag tag
      pure $ frame'' <> authTag <> iv

chatDecryptMedia :: ByteString -> ByteString -> IO ByteString
chatDecryptMedia keyStr frame = fromRight frame <$> decrypt
  where
    decrypt = runExceptT $ do
      key <- liftEither $ U.decode keyStr
      let (rest, iv) = B.splitAt (B.length frame - ivSize) frame
          (frame', tag) = B.splitAt (B.length rest - C.authTagSize) rest
          authTag = C.AuthTag $ AES.AuthTag $ BA.convert tag
      withExceptT show $ C.decryptAESNoPad (C.Key key) (C.IV iv) frame' authTag

authTagSize :: Int
authTagSize = C.authTagSize
{-# INLINE authTagSize #-}

ivSize :: Int
ivSize = 12
{-# INLINE ivSize #-}

getByteString :: Ptr Word8 -> CInt -> IO ByteString
getByteString p size = do
  fp <- newForeignPtr_ p
  pure $ PS fp 0 $ fromIntegral size
{-# INLINE getByteString #-}

putByteString :: ByteString -> Ptr Word8 -> CInt -> IO ()
putByteString bs@(PS fp offset _) to size = do
  let len = B.length bs
      p = unsafeForeignPtrToPtr fp `plusPtr` offset
  when (len <= fromIntegral size) $ memcpy to p len
{-# INLINE putByteString #-}

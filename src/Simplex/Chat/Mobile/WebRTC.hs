module Simplex.Chat.Mobile.WebRTC where

import qualified Data.ByteString as B
import Data.ByteString.Internal (ByteString(PS), memcpy)
import Foreign.C (CInt, CString)
import Foreign.Ptr (Ptr, plusPtr)
import Foreign.ForeignPtr (newForeignPtr_)
import Foreign.ForeignPtr.Unsafe (unsafeForeignPtrToPtr)
import Control.Monad (when)
import Data.Word (Word8)

cChatEncryptMedia :: CString -> Ptr Word8 -> CInt -> IO ()
cChatEncryptMedia = cTransformMedia chatEncryptMedia

cChatDecryptMedia :: CString -> Ptr Word8 -> CInt -> IO ()
cChatDecryptMedia = cTransformMedia chatDecryptMedia

cTransformMedia :: (ByteString -> ByteString -> ByteString) -> CString -> Ptr Word8 -> CInt -> IO ()
cTransformMedia f cKey cFrame cFrameLen = do
  key <- B.packCStringLen (cKey, 32)
  frame <- getByteString cFrame cFrameLen
  let frame' = f key frame
  putByteString frame' cFrame cFrameLen
{-# INLINE cTransformMedia #-}

chatEncryptMedia :: ByteString -> ByteString -> ByteString
chatEncryptMedia _key frame = frame

chatDecryptMedia :: ByteString -> ByteString -> ByteString
chatDecryptMedia _key frame = frame

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

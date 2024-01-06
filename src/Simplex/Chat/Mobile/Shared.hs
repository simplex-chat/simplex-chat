{-# LANGUAGE LambdaCase #-}

module Simplex.Chat.Mobile.Shared where

import qualified Data.ByteString as B
import Data.ByteString.Internal (ByteString (..))
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Lazy.Internal as LB
import Foreign
import Foreign.C (CInt, CString)

type CJSONString = CString

type JSONByteString = LB.ByteString

getByteString :: Ptr Word8 -> CInt -> IO ByteString
getByteString ptr len = do
  fp <- newForeignPtr_ ptr
  pure $ PS fp 0 (fromIntegral len)
{-# INLINE getByteString #-}

putByteString :: Ptr Word8 -> ByteString -> IO ()
putByteString ptr (PS fp offset len) =
  withForeignPtr fp $ \p -> copyBytes ptr (p `plusPtr` offset) len
{-# INLINE putByteString #-}

putLazyByteString :: Ptr Word8 -> LB.ByteString -> IO ()
putLazyByteString ptr = \case
  LB.Empty -> pure ()
  LB.Chunk ch s -> do
    putByteString ptr ch
    putLazyByteString (ptr `plusPtr` B.length ch) s

newCStringFromBS :: ByteString -> IO CString
newCStringFromBS s = do
  let len = B.length s
  buf <- mallocBytes (len + 1)
  putByteString buf s
  pokeByteOff buf len (0 :: Word8)
  pure $ castPtr buf

newCStringFromLazyBS :: LB.ByteString -> IO CString
newCStringFromLazyBS s = do
  let len = fromIntegral $ LB.length s
  buf <- mallocBytes (len + 1)
  putLazyByteString buf s
  pokeByteOff buf len (0 :: Word8)
  pure $ castPtr buf

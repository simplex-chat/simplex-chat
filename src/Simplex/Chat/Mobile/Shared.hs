module Simplex.Chat.Mobile.Shared where

import Data.ByteString.Internal (ByteString (..), memcpy)
import Foreign.C (CInt, CString)
import Foreign

type CJSONString = CString

type JSONByteString = ByteString

getByteString :: Ptr Word8 -> CInt -> IO ByteString
getByteString ptr len = do
  fp <- newForeignPtr_ ptr
  pure $ BS fp $ fromIntegral len

putByteString :: Ptr Word8 -> ByteString -> IO ()
putByteString ptr (BS fp len) =
  withForeignPtr fp $ \p -> memcpy ptr p len

newCStringFromBS :: ByteString -> IO CString
newCStringFromBS (BS fp len) = do
  buf <- mallocBytes (len + 1)
  withForeignPtr fp $ \p -> do
    memcpy buf p len
    pokeByteOff buf len (0 :: Word8)
    pure $ castPtr buf

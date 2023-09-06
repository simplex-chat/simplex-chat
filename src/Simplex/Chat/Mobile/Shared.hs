module Simplex.Chat.Mobile.Shared where

import qualified Data.ByteString as B
import Data.ByteString.Internal (ByteString (PS), memcpy)
import Foreign.C (CInt, CString)
import Foreign (Ptr, Word8, newForeignPtr_, plusPtr)
import Foreign.ForeignPtr.Unsafe

type CJSONString = CString

getByteString :: Ptr Word8 -> CInt -> IO ByteString
getByteString ptr len = do
  fp <- newForeignPtr_ ptr
  pure $ PS fp 0 $ fromIntegral len

putByteString :: Ptr Word8 -> ByteString -> IO ()
putByteString ptr bs@(PS fp offset _)  = do
  let p = unsafeForeignPtrToPtr fp `plusPtr` offset
  memcpy ptr p $ B.length bs

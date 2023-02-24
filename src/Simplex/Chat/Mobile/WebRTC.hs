{-# LANGUAGE FlexibleContexts #-}

module Simplex.Chat.Mobile.WebRTC where

import Control.Monad.Except
import qualified Crypto.Cipher.Types as AES
import Crypto.Random (getRandomBytes)
import qualified Data.ByteArray as BA
import qualified Data.ByteString as B
import qualified Data.ByteString.Base64.URL as U
import Data.ByteString.Internal (ByteString (PS), memcpy)
import Data.Either (fromLeft)
import Data.Word (Word8)
import Foreign.C (CInt, CString, newCAString)
import Foreign.ForeignPtr (newForeignPtr_)
import Foreign.ForeignPtr.Unsafe (unsafeForeignPtrToPtr)
import Foreign.Ptr (Ptr, plusPtr)
import qualified Simplex.Messaging.Crypto as C

cChatEncryptMedia :: CString -> Ptr Word8 -> CInt -> IO CString
cChatEncryptMedia = cTransformMedia chatEncryptMedia

cChatDecryptMedia :: CString -> Ptr Word8 -> CInt -> IO CString
cChatDecryptMedia = cTransformMedia chatDecryptMedia

cTransformMedia :: (ByteString -> ByteString -> ExceptT String IO ByteString) -> CString -> Ptr Word8 -> CInt -> IO CString
cTransformMedia f cKey cFrame cFrameLen = do
  key <- B.packCString cKey
  frame <- getByteString cFrame cFrameLen
  runExceptT (checkFrameSize frame >> f key frame >>= liftIO . putByteString) >>= newCAString . fromLeft ""
  where
    checkFrameSize frame =
      when (B.length frame < reservedSize) $ throwError "frame has no [reserved space] IV and/or auth tag"
    getByteString p size = do
      fp <- newForeignPtr_ p
      pure $ PS fp 0 $ fromIntegral size
    putByteString bs@(PS fp offset _) = do
      let len = B.length bs
          p = unsafeForeignPtrToPtr fp `plusPtr` offset
      when (len <= fromIntegral cFrameLen) $ memcpy cFrame p len
{-# INLINE cTransformMedia #-}

chatEncryptMedia :: ByteString -> ByteString -> ExceptT String IO ByteString
chatEncryptMedia keyStr frame = do
  key <- liftEither $ U.decode keyStr
  iv <- liftIO $ getRandomBytes ivSize
  let (frame', _) = B.splitAt (B.length frame - reservedSize) frame
  (tag, frame'') <- withExceptT show $ C.encryptAESNoPad (C.Key key) (C.IV $ iv <> ivPad) frame'
  let authTag = BA.convert $ C.unAuthTag tag
  pure $ frame'' <> authTag <> iv

chatDecryptMedia :: ByteString -> ByteString -> ExceptT String IO ByteString
chatDecryptMedia keyStr frame = do
  key <- liftEither $ U.decode keyStr
  let (rest, iv) = B.splitAt (B.length frame - ivSize) frame
      (frame', tag) = B.splitAt (B.length rest - C.authTagSize) rest
      authTag = C.AuthTag $ AES.AuthTag $ BA.convert tag
  frame'' <- withExceptT show $ C.decryptAESNoPad (C.Key key) (C.IV $ iv <> ivPad) frame' authTag
  pure $ frame'' <> B.replicate reservedSize 0

authTagSize :: Int
authTagSize = C.authTagSize
{-# INLINE authTagSize #-}

ivSize :: Int
ivSize = 12
{-# INLINE ivSize #-}

ivPad :: ByteString
ivPad = B.replicate 4 0

reservedSize :: Int
reservedSize = authTagSize + ivSize
{-# INLINE reservedSize #-}

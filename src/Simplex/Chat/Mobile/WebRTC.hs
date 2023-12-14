{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}

module Simplex.Chat.Mobile.WebRTC
  ( cChatEncryptMedia,
    cChatDecryptMedia,
    chatEncryptMedia,
    chatDecryptMedia,
    reservedSize,
  ) where

import Control.Monad
import Control.Monad.Except
import Control.Monad.IO.Class
import qualified Crypto.Cipher.Types as AES
import Data.Bifunctor (bimap)
import qualified Data.ByteArray as BA
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Base64.URL as U
import Data.Either (fromLeft)
import Data.Word (Word8)
import Foreign.C (CInt, CString, newCAString)
import Foreign.Ptr (Ptr)
import Foreign.StablePtr
import Simplex.Chat.Controller (ChatController (..))
import Simplex.Chat.Mobile.Shared
import qualified Simplex.Messaging.Crypto as C
import UnliftIO (atomically)

cChatEncryptMedia :: StablePtr ChatController -> CString -> Ptr Word8 -> CInt -> IO CString
cChatEncryptMedia = cTransformMedia . chatEncryptMedia

cChatDecryptMedia :: CString -> Ptr Word8 -> CInt -> IO CString
cChatDecryptMedia = cTransformMedia chatDecryptMedia

cTransformMedia :: (ByteString -> ByteString -> ExceptT String IO ByteString) -> CString -> Ptr Word8 -> CInt -> IO CString
cTransformMedia f cKey cFrame cFrameLen = do
  key <- B.packCString cKey
  frame <- getByteString cFrame cFrameLen
  runExceptT (f key frame >>= liftIO . putFrame) >>= newCAString . fromLeft ""
  where
    putFrame s = when (B.length s <= fromIntegral cFrameLen) $ putByteString cFrame s
{-# INLINE cTransformMedia #-}

chatEncryptMedia :: StablePtr ChatController -> ByteString -> ByteString -> ExceptT String IO ByteString
chatEncryptMedia cc keyStr frame = do
  ChatController {random} <- liftIO $ deRefStablePtr cc
  len <- checkFrameLen frame
  key <- decodeKey keyStr
  iv <- atomically $ C.randomGCMIV random
  (tag, frame') <- withExceptT show $ C.encryptAESNoPad key iv $ B.take len frame
  pure $ frame' <> BA.convert (C.unAuthTag tag) <> C.unGCMIV iv

chatDecryptMedia :: ByteString -> ByteString -> ExceptT String IO ByteString
chatDecryptMedia keyStr frame = do
  len <- checkFrameLen frame
  key <- decodeKey keyStr
  let (frame', rest) = B.splitAt len frame
      (tag, iv) = B.splitAt C.authTagSize rest
      authTag = C.AuthTag $ AES.AuthTag $ BA.convert tag
  withExceptT show $ do
    iv' <- liftEither $ C.gcmIV iv
    frame'' <- C.decryptAESNoPad key iv' frame' authTag
    pure $ frame'' <> framePad

checkFrameLen :: ByteString -> ExceptT String IO Int
checkFrameLen frame = do
  let len = B.length frame - reservedSize
  when (len < 0) $ throwError "frame has no [reserved space for] IV and/or auth tag"
  pure len
{-# INLINE checkFrameLen #-}

decodeKey :: ByteString -> ExceptT String IO C.Key
decodeKey = liftEither . bimap ("invalid key: " <>) C.Key . U.decode
{-# INLINE decodeKey #-}

reservedSize :: Int
reservedSize = C.authTagSize + C.gcmIVSize

framePad :: ByteString
framePad = B.replicate reservedSize 0

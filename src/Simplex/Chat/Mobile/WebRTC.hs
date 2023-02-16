module Simplex.Chat.Mobile.WebRTC where

import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import Foreign.C (CInt, CString)

cChatEncryptMedia :: CString -> CString -> CInt -> IO ()
cChatEncryptMedia cKey cFrame cFrameLen = do
  key <- B.packCStringLen (cKey, 32)
  str <- B.packCStringLen (cFrame, fromIntegral cFrameLen)
  let _ = B.unpack $ chatEncryptMedia key str
  -- TODO write bytes back to cFrame
  pure ()

cChatDecryptMedia :: CString -> CString -> CInt -> IO ()
cChatDecryptMedia cKey cFrame cFrameLen = do
  key <- B.packCStringLen (cKey, 32)
  str <- B.packCStringLen (cFrame, fromIntegral cFrameLen)
  let _ = B.unpack $ chatDecryptMedia key str
  -- TODO write bytes back to cFrame
  pure ()

chatEncryptMedia :: ByteString -> ByteString -> ByteString
chatEncryptMedia _key frame = frame

chatDecryptMedia :: ByteString -> ByteString -> ByteString
chatDecryptMedia _key frame = frame

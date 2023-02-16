module Simplex.Chat.Mobile.WebRTC where

import Data.ByteString.Char8 (ByteString)

chatEncryptMedia :: ByteString -> ByteString -> ByteString
chatEncryptMedia _key frame = frame

-- chatDecryptMedia :: ByteString -> ByteString -> ByteString
-- chatDecryptMedia _key frame = frame

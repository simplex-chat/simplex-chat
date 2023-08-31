{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Simplex.Chat.Mobile.File
  ( cChatReadFile,
    cChatWriteFile,
  )
where

import Control.Monad.Except
import Data.Aeson (ToJSON)
import qualified Data.Aeson as J
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Lazy.Char8 as LB'
import Data.Int (Int64)
import Data.Word (Word8)
import Foreign.C
import Foreign.Marshal.Alloc (mallocBytes)
import Foreign.Ptr
import GHC.Generics (Generic)
import Simplex.Chat.Mobile.Shared
import Simplex.Messaging.Crypto.File (CryptoFile (..), CryptoFileArgs (..))
import qualified Simplex.Messaging.Crypto.File as CF
import Simplex.Messaging.Encoding.String
import Simplex.Messaging.Parsers (dropPrefix, sumTypeJSON)

data WriteFileResult
  = WFResult {cryptoArgs :: CryptoFileArgs}
  | WFError {writeError :: String}
  deriving (Generic)

instance ToJSON WriteFileResult where toEncoding = J.genericToEncoding . sumTypeJSON $ dropPrefix "WF"

cChatWriteFile :: CString -> Ptr Word8 -> CInt -> IO CJSONString
cChatWriteFile cPath ptr len = do
  path <- peekCAString cPath
  s <- getByteString ptr len
  r <- chatWriteFile path s
  newCAString $ LB'.unpack $ J.encode r

chatWriteFile :: FilePath -> ByteString -> IO WriteFileResult
chatWriteFile path s = do
  cfArgs <- CF.randomArgs
  let file = CryptoFile path $ Just cfArgs
  either (WFError . show) (\_ -> WFResult cfArgs)
    <$> runExceptT (CF.writeFile file $ LB.fromStrict s)

data ReadFileResult
  = RFResult {fileSize :: Int64}
  | RFError {readError :: String}
  deriving (Generic)

instance ToJSON ReadFileResult where toEncoding = J.genericToEncoding . sumTypeJSON $ dropPrefix "RF"

cChatReadFile :: CString -> CString -> CString -> IO (Ptr Word8)
cChatReadFile cPath cKey cNonce = do
  path <- peekCAString cPath
  key <- B.packCString cKey
  nonce <- B.packCString cNonce
  (r, s) <- chatReadFile path key nonce
  let r' = LB.toStrict (J.encode r) <> "\NUL"
  ptr <- mallocBytes $ B.length r' + B.length s
  putByteString ptr r'
  putByteString (ptr `plusPtr` B.length r') s
  pure ptr

chatReadFile :: FilePath -> ByteString -> ByteString -> IO (ReadFileResult, ByteString)
chatReadFile path keyStr nonceStr = do
  either ((,"") . RFError) (\s -> (RFResult $ LB.length s, LB.toStrict s)) <$> runExceptT readFile_
  where
    readFile_ :: ExceptT String IO LB.ByteString
    readFile_ = do
      key <- liftEither $ strDecode keyStr
      nonce <- liftEither $ strDecode nonceStr
      let file = CryptoFile path $ Just $ CFArgs key nonce
      withExceptT show $ CF.readFile file

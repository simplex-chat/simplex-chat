{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Simplex.Chat.Mobile.File
  ( cChatWriteFile,
    cChatReadFile,
    cChatEncryptFile,
    cChatDecryptFile,
    WriteFileResult (..),
    ReadFileResult (..),
    chatWriteFile,
    chatReadFile,
  )
where

import Control.Monad.Except
import Data.Aeson (ToJSON)
import qualified Data.Aeson as J
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Lazy.Char8 as LB'
import Data.Either (fromLeft)
import Data.Word (Word8)
import Foreign.C
import Foreign.Marshal.Alloc (mallocBytes)
import Foreign.Ptr
import GHC.Generics (Generic)
import Simplex.Chat.Mobile.Shared
import Simplex.Messaging.Crypto.File (CryptoFile (..), CryptoFileArgs (..), CryptoFileHandle, FTCryptoError (..))
import qualified Simplex.Messaging.Crypto.File as CF
import Simplex.Messaging.Encoding.String
import Simplex.Messaging.Parsers (dropPrefix, sumTypeJSON)
import UnliftIO (Handle, IOMode (..), withFile)

data WriteFileResult
  = WFResult {cryptoArgs :: CryptoFileArgs}
  | WFError {writeError :: String}
  deriving (Generic)

instance ToJSON WriteFileResult where toEncoding = J.genericToEncoding . sumTypeJSON $ dropPrefix "WF"

cChatWriteFile :: CString -> Ptr Word8 -> CInt -> IO CJSONString
cChatWriteFile cPath ptr len = do
  path <- peekCString cPath
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
  = RFResult {fileSize :: Int}
  | RFError {readError :: String}
  deriving (Generic)

instance ToJSON ReadFileResult where toEncoding = J.genericToEncoding . sumTypeJSON $ dropPrefix "RF"

cChatReadFile :: CString -> CString -> CString -> IO (Ptr Word8)
cChatReadFile cPath cKey cNonce = do
  path <- peekCString cPath
  key <- B.packCString cKey
  nonce <- B.packCString cNonce
  (r, s) <- chatReadFile path key nonce
  let r' = LB.toStrict $ J.encode r <> "\NUL"
  ptr <- mallocBytes $ B.length r' + B.length s
  putByteString ptr r'
  unless (B.null s) $ putByteString (ptr `plusPtr` B.length r') s
  pure ptr

chatReadFile :: FilePath -> ByteString -> ByteString -> IO (ReadFileResult, ByteString)
chatReadFile path keyStr nonceStr = do
  either ((,"") . RFError) result <$> runExceptT readFile_
  where
    result s = let s' = LB.toStrict s in (RFResult $ B.length s', s')
    readFile_ :: ExceptT String IO LB.ByteString
    readFile_ = do
      key <- liftEither $ strDecode keyStr
      nonce <- liftEither $ strDecode nonceStr
      let file = CryptoFile path $ Just $ CFArgs key nonce
      withExceptT show $ CF.readFile file

cChatEncryptFile :: CString -> CString -> IO CJSONString
cChatEncryptFile cFromPath cToPath = do
  fromPath <- peekCString cFromPath
  toPath <- peekCString cToPath
  r <- chatEncryptFile fromPath toPath
  newCAString . LB'.unpack $ J.encode r

chatEncryptFile :: FilePath -> FilePath -> IO WriteFileResult
chatEncryptFile fromPath toPath =
  either WFError WFResult <$> runExceptT encrypt
  where
    encrypt = do
      cfArgs <- liftIO $ CF.randomArgs
      let toFile = CryptoFile toPath $ Just cfArgs
      withExceptT show $
        withFile fromPath ReadMode $ \r -> CF.withFile toFile WriteMode $ \w -> do
          encryptChunks r w
          liftIO $ CF.hPutTag w
      pure cfArgs
    encryptChunks r w = do
      ch <- liftIO $ LB.hGet r chunkSize
      unless (LB.null ch) $ liftIO $ CF.hPut w ch
      unless (LB.length ch < chunkSize) $ encryptChunks r w

cChatDecryptFile :: CString -> CString -> CString -> CString -> IO CString
cChatDecryptFile cFromPath cKey cNonce cToPath = do
  fromPath <- peekCString cFromPath
  key <- B.packCString cKey
  nonce <- B.packCString cNonce
  toPath <- peekCString cToPath
  r <- chatDecryptFile fromPath key nonce toPath
  newCAString r

chatDecryptFile :: FilePath -> ByteString -> ByteString -> FilePath -> IO String
chatDecryptFile fromPath keyStr nonceStr toPath = fromLeft "" <$> runExceptT decrypt
  where
    decrypt = do
      key <- liftEither $ strDecode keyStr
      nonce <- liftEither $ strDecode nonceStr
      let fromFile = CryptoFile fromPath $ Just $ CFArgs key nonce
      size <- liftIO $ CF.getFileContentsSize fromFile
      withExceptT show $
        CF.withFile fromFile ReadMode $ \r -> withFile toPath WriteMode $ \w -> do
          decryptChunks r w size
          CF.hGetTag r
    decryptChunks :: CryptoFileHandle -> Handle -> Integer -> ExceptT FTCryptoError IO ()
    decryptChunks r w !size = do
      let chSize = min size chunkSize
          chSize' = fromIntegral chSize
          size' = size - chSize
      ch <- liftIO $ CF.hGet r chSize'
      when (B.length ch /= chSize') $ throwError $ FTCEFileIOError "encrypting file: unexpected EOF"
      liftIO $ B.hPut w ch
      when (size' > 0) $ decryptChunks r w size'

chunkSize :: Num a => a
chunkSize = 65536
{-# INLINE chunkSize #-}

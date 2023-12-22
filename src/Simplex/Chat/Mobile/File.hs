{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
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

import Control.Monad
import Control.Monad.Except
import Control.Monad.IO.Class
import qualified Data.Aeson as J
import qualified Data.Aeson.TH as JQ
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Lazy.Char8 as LB'
import Data.Char (chr)
import Data.Either (fromLeft)
import Data.Word (Word32, Word8)
import Foreign.C
import Foreign.Marshal.Alloc (mallocBytes)
import Foreign.Ptr
import Foreign.StablePtr
import Foreign.Storable (poke, pokeByteOff)
import Simplex.Chat.Controller (ChatController (..))
import Simplex.Chat.Mobile.Shared
import Simplex.Chat.Util (chunkSize, encryptFile)
import Simplex.Messaging.Crypto.File (CryptoFile (..), CryptoFileArgs (..), CryptoFileHandle, FTCryptoError (..))
import qualified Simplex.Messaging.Crypto.File as CF
import Simplex.Messaging.Encoding.String
import Simplex.Messaging.Parsers (dropPrefix, sumTypeJSON)
import Simplex.Messaging.Util (catchAll)
import UnliftIO (Handle, IOMode (..), atomically, withFile)

data WriteFileResult
  = WFResult {cryptoArgs :: CryptoFileArgs}
  | WFError {writeError :: String}

$(JQ.deriveToJSON (sumTypeJSON $ dropPrefix "WF") ''WriteFileResult)

cChatWriteFile :: StablePtr ChatController -> CString -> Ptr Word8 -> CInt -> IO CJSONString
cChatWriteFile cc cPath ptr len = do
  c <- deRefStablePtr cc
  path <- peekCString cPath
  s <- getByteString ptr len
  r <- chatWriteFile c path s
  newCStringFromLazyBS $ J.encode r

chatWriteFile :: ChatController -> FilePath -> ByteString -> IO WriteFileResult
chatWriteFile ChatController {random} path s = do
  cfArgs <- atomically $ CF.randomArgs random
  let file = CryptoFile path $ Just cfArgs
  either WFError (\_ -> WFResult cfArgs)
    <$> runCatchExceptT (withExceptT show $ CF.writeFile file $ LB.fromStrict s)

data ReadFileResult
  = RFResult {fileSize :: Int}
  | RFError {readError :: String}

cChatReadFile :: CString -> CString -> CString -> IO (Ptr Word8)
cChatReadFile cPath cKey cNonce = do
  path <- peekCString cPath
  key <- B.packCString cKey
  nonce <- B.packCString cNonce
  chatReadFile path key nonce >>= \case
    Left e -> castPtr <$> newCString (chr 1 : e)
    Right s -> do
      let len = fromIntegral $ LB.length s
      ptr <- mallocBytes $ len + 5
      poke ptr (0 :: Word8)
      pokeByteOff ptr 1 (fromIntegral len :: Word32)
      putLazyByteString (ptr `plusPtr` 5) s
      pure ptr

chatReadFile :: FilePath -> ByteString -> ByteString -> IO (Either String LB.ByteString)
chatReadFile path keyStr nonceStr = runCatchExceptT $ do
  key <- liftEither $ strDecode keyStr
  nonce <- liftEither $ strDecode nonceStr
  let file = CryptoFile path $ Just $ CFArgs key nonce
  withExceptT show $ CF.readFile file

cChatEncryptFile :: StablePtr ChatController -> CString -> CString -> IO CJSONString
cChatEncryptFile cc cFromPath cToPath = do
  c <- deRefStablePtr cc
  fromPath <- peekCString cFromPath
  toPath <- peekCString cToPath
  r <- chatEncryptFile c fromPath toPath
  newCAString . LB'.unpack $ J.encode r

chatEncryptFile :: ChatController -> FilePath -> FilePath -> IO WriteFileResult
chatEncryptFile ChatController {random} fromPath toPath =
  either WFError WFResult <$> runCatchExceptT encrypt
  where
    encrypt = do
      cfArgs <- atomically $ CF.randomArgs random
      encryptFile fromPath toPath cfArgs
      pure cfArgs

cChatDecryptFile :: CString -> CString -> CString -> CString -> IO CString
cChatDecryptFile cFromPath cKey cNonce cToPath = do
  fromPath <- peekCString cFromPath
  key <- B.packCString cKey
  nonce <- B.packCString cNonce
  toPath <- peekCString cToPath
  r <- chatDecryptFile fromPath key nonce toPath
  newCAString r

chatDecryptFile :: FilePath -> ByteString -> ByteString -> FilePath -> IO String
chatDecryptFile fromPath keyStr nonceStr toPath = fromLeft "" <$> runCatchExceptT decrypt
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

runCatchExceptT :: ExceptT String IO a -> IO (Either String a)
runCatchExceptT action = runExceptT action `catchAll` (pure . Left . show)

$(JQ.deriveToJSON (sumTypeJSON $ dropPrefix "RF") ''ReadFileResult)

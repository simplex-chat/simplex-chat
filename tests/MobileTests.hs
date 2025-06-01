{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module MobileTests where

import ChatTests.DBUtils
import ChatTests.Utils
import Control.Concurrent.STM
import Control.Monad.Except
import Data.Aeson (FromJSON)
import qualified Data.Aeson as J
import qualified Data.Aeson.TH as JQ
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BS
import Data.ByteString.Internal (create)
import qualified Data.ByteString.Lazy.Char8 as LB
import Data.Word (Word8, Word32)
import Foreign.C
import Foreign.Marshal.Alloc (mallocBytes)
import Foreign.Marshal.Utils (copyBytes)
import Foreign.Ptr
import Foreign.StablePtr
import Foreign.Storable (peek)
import GHC.IO.Encoding (setLocaleEncoding, setFileSystemEncoding, setForeignEncoding)
import JSONFixtures
import Simplex.Chat.Controller (ChatController (..))
import Simplex.Chat.Mobile hiding (error)
import Simplex.Chat.Mobile.File
import Simplex.Chat.Mobile.Shared
import Simplex.Chat.Mobile.WebRTC
import Simplex.Chat.Options.DB
import Simplex.Chat.Store
import Simplex.Chat.Store.Profiles
import Simplex.Chat.Types (AgentUserId (..), Profile (..))
import Simplex.Messaging.Agent.Store.Interface
import Simplex.Messaging.Agent.Store.Shared (MigrationConfirmation (..))
import qualified Simplex.Messaging.Agent.Store.SQLite.DB as DB
import qualified Simplex.Messaging.Crypto as C
import Simplex.Messaging.Crypto.File (CryptoFile(..), CryptoFileArgs (..))
import qualified Simplex.Messaging.Crypto.File as CF
import Simplex.Messaging.Encoding.String
import Simplex.Messaging.Parsers (dropPrefix, sumTypeJSON)
import System.Directory (copyFile)
import System.FilePath ((</>))
import System.IO (utf8)
import Test.Hspec hiding (it)

mobileTests :: HasCallStack => SpecWith TestParams
mobileTests = do
  describe "mobile API" $ do
    runIO $ do
      setLocaleEncoding utf8
      setFileSystemEncoding utf8
      setForeignEncoding utf8
    it "start new chat without user" testChatApiNoUser
    it "start new chat with existing user" testChatApi
    it "should encrypt/decrypt WebRTC frames" testMediaApi
    it "should encrypt/decrypt WebRTC frames via C API" testMediaCApi
    describe "should read/write encrypted files via C API" $ do
      it "latin1 name" $ testFileCApi "test"
      it "utf8 name 1" $ testFileCApi "тест"
      it "utf8 name 2" $ testFileCApi "👍"
      it "no exception on missing file" testMissingFileCApi
    describe "should encrypt/decrypt files via C API" $ do
      it "latin1 name" $ testFileEncryptionCApi "test"
      it "utf8 name 1" $ testFileEncryptionCApi "тест"
      it "utf8 name 2" $ testFileEncryptionCApi "👍"
      it "no exception on missing file" testMissingFileEncryptionCApi
    describe "validate name" $ do
      it "should convert invalid name to a valid name" testValidNameCApi
    describe "JSON length" $ do
      it "should compute length of JSON encoded string" testChatJsonLengthCApi

noActiveUser :: LB.ByteString
noActiveUser =
#if defined(darwin_HOST_OS) && defined(swiftJSON)
  noActiveUserSwift
#else
  noActiveUserTagged
#endif

activeUserExists :: LB.ByteString
activeUserExists =
#if defined(darwin_HOST_OS) && defined(swiftJSON)
  activeUserExistsSwift
#else
  activeUserExistsTagged
#endif

activeUser :: LB.ByteString
activeUser =
#if defined(darwin_HOST_OS) && defined(swiftJSON)
  activeUserSwift
#else
  activeUserTagged
#endif

chatStarted :: LB.ByteString
chatStarted =
#if defined(darwin_HOST_OS) && defined(swiftJSON)
  chatStartedSwift
#else
  chatStartedTagged
#endif

networkStatuses :: LB.ByteString
networkStatuses =
#if defined(darwin_HOST_OS) && defined(swiftJSON)
  networkStatusesSwift
#else
  networkStatusesTagged
#endif

memberSubSummary :: LB.ByteString
memberSubSummary =
#if defined(darwin_HOST_OS) && defined(swiftJSON)
  memberSubSummarySwift
#else
  memberSubSummaryTagged
#endif

userContactSubSummary :: LB.ByteString
userContactSubSummary =
#if defined(darwin_HOST_OS) && defined(swiftJSON)
  userContactSubSummarySwift
#else
  userContactSubSummaryTagged
#endif

pendingSubSummary :: LB.ByteString
pendingSubSummary =
#if defined(darwin_HOST_OS) && defined(swiftJSON)
  pendingSubSummarySwift
#else
  pendingSubSummaryTagged
#endif

parsedMarkdown :: LB.ByteString
parsedMarkdown =
#if defined(darwin_HOST_OS) && defined(swiftJSON)
  parsedMarkdownSwift
#else
  parsedMarkdownTagged
#endif

testChatApiNoUser :: TestParams -> IO ()
testChatApiNoUser ps = do
  let tmp = tmpPath ps
      dbPrefix = tmp </> "1"
  Right cc <- chatMigrateInit dbPrefix "" "yesUp"
  Left (DBMErrorNotADatabase _) <- chatMigrateInit dbPrefix "myKey" "yesUp"
  chatSendCmd cc "/u" `shouldReturn` noActiveUser
  chatSendCmd cc "/_start" `shouldReturn` noActiveUser
  chatSendCmd cc "/create user alice Alice" `shouldReturn` activeUser
  chatSendCmd cc "/_start" `shouldReturn` chatStarted

testChatApi :: TestParams -> IO ()
testChatApi ps = do
  let tmp = tmpPath ps
      dbPrefix = tmp </> "1"
      f = dbPrefix <> chatSuffix
  Right st <- createChatStore (DBOpts f "myKey" False True DB.TQOff) MCYesUp
  Right _ <- withTransaction st $ \db -> runExceptT $ createUserRecord db (AgentUserId 1) aliceProfile {preferences = Nothing} True
  Right cc <- chatMigrateInit dbPrefix "myKey" "yesUp"
  Left (DBMErrorNotADatabase _) <- chatMigrateInit dbPrefix "" "yesUp"
  Left (DBMErrorNotADatabase _) <- chatMigrateInit dbPrefix "anotherKey" "yesUp"
  chatSendCmd cc "/u" `shouldReturn` activeUser
  chatSendCmd cc "/create user alice Alice" `shouldReturn` activeUserExists
  chatSendCmd cc "/_start" `shouldReturn` chatStarted
  chatRecvMsg cc `shouldReturn` networkStatuses
  chatRecvMsgWait cc 10000 `shouldReturn` ""
  chatParseMarkdown "hello" `shouldBe` "{}"
  chatParseMarkdown "*hello*" `shouldBe` parsedMarkdown

testMediaApi :: HasCallStack => TestParams -> IO ()
testMediaApi ps = do
  let tmp = tmpPath ps
  Right c@ChatController {random = g} <- chatMigrateInit (tmp </> "1") "" "yesUp"
  cc <- newStablePtr c
  key <- atomically $ C.randomBytes 32 g
  frame <- atomically $ C.randomBytes 100 g
  let keyStr = strEncode key
      reserved = B.replicate (C.authTagSize + C.gcmIVSize) 0
      frame' = frame <> reserved
  Right encrypted <- runExceptT $ chatEncryptMedia cc keyStr frame'
  encrypted `shouldNotBe` frame'
  B.length encrypted `shouldBe` B.length frame'
  runExceptT (chatDecryptMedia keyStr encrypted) `shouldReturn` Right frame'

testMediaCApi :: HasCallStack => TestParams -> IO ()
testMediaCApi ps = do
  let tmp = tmpPath ps
  Right c@ChatController {random = g} <- chatMigrateInit (tmp </> "1") "" "yesUp"
  cc <- newStablePtr c
  key <- atomically $ C.randomBytes 32 g
  frame <- atomically $ C.randomBytes 100 g
  let keyStr = strEncode key
      reserved = B.replicate (C.authTagSize + C.gcmIVSize) 0
      frame' = frame <> reserved
  encrypted <- test (cChatEncryptMedia cc) keyStr frame'
  encrypted `shouldNotBe` frame'
  test cChatDecryptMedia keyStr encrypted `shouldReturn` frame'
  where
    test :: HasCallStack => (CString -> Ptr Word8 -> CInt -> IO CString) -> ByteString -> ByteString -> IO ByteString
    test f keyStr frame = do
      let len = B.length frame
          cLen = fromIntegral len
      ptr <- mallocBytes len
      putByteString ptr frame
      cKeyStr <- newCAString $ BS.unpack keyStr
      (f cKeyStr ptr cLen >>= peekCAString) `shouldReturn` ""
      getByteString ptr cLen

instance FromJSON WriteFileResult where
  parseJSON = $(JQ.mkParseJSON (sumTypeJSON $ dropPrefix "WF") ''WriteFileResult)

instance FromJSON ReadFileResult where
  parseJSON = $(JQ.mkParseJSON (sumTypeJSON $ dropPrefix "RF") ''ReadFileResult)

testFileCApi :: FilePath -> TestParams -> IO ()
testFileCApi fileName ps = do
  let tmp = tmpPath ps
  cc <- mkCCPtr tmp
  src <- B.readFile "./tests/fixtures/test.pdf"
  let path = tmp </> (fileName <> ".pdf")
  cPath <- newCString path
  let len = B.length src
      cLen = fromIntegral len
  ptr <- mallocBytes $ B.length src
  putByteString ptr src
  r <- peekCAString =<< cChatWriteFile cc cPath ptr cLen
  Just (WFResult cfArgs@(CFArgs key nonce)) <- jDecode r
  let encryptedFile = CryptoFile path $ Just cfArgs
  CF.getFileContentsSize encryptedFile `shouldReturn` fromIntegral (B.length src)
  cKey <- encodedCString key
  cNonce <- encodedCString nonce
  -- the returned pointer contains 0, buffer length as Word32, then buffer
  ptr' <- cChatReadFile cPath cKey cNonce
  peek ptr' `shouldReturn` (0 :: Word8)
  sz :: Word32 <- peek (ptr' `plusPtr` 1)
  let sz' = fromIntegral sz
  contents <- create sz' $ \toPtr -> copyBytes toPtr (ptr' `plusPtr` 5) sz'
  contents `shouldBe` src
  sz' `shouldBe` len

testMissingFileCApi :: TestParams -> IO ()
testMissingFileCApi ps = do
  let tmp = tmpPath ps
  let path = tmp </> "missing_file"
  cPath <- newCString path
  CFArgs key nonce <- atomically . CF.randomArgs =<< C.newRandom
  cKey <- encodedCString key
  cNonce <- encodedCString nonce
  ptr <- cChatReadFile cPath cKey cNonce
  peek ptr `shouldReturn` 1
  err <- peekCAString (ptr `plusPtr` 1)
  err `shouldContain` "missing_file: openBinaryFile: does not exist"

testFileEncryptionCApi :: FilePath -> TestParams -> IO ()
testFileEncryptionCApi fileName ps = do
  let tmp = tmpPath ps
  cc <- mkCCPtr tmp
  let fromPath = tmp </> (fileName <> ".source.pdf")
  copyFile "./tests/fixtures/test.pdf" fromPath
  src <- B.readFile fromPath
  cFromPath <- newCString fromPath
  let toPath = tmp </> (fileName <> ".encrypted.pdf")
  cToPath <- newCString toPath
  r <- peekCAString =<< cChatEncryptFile cc cFromPath cToPath
  Just (WFResult cfArgs@(CFArgs key nonce)) <- jDecode r
  CF.getFileContentsSize (CryptoFile toPath $ Just cfArgs) `shouldReturn` fromIntegral (B.length src)
  cKey <- encodedCString key
  cNonce <- encodedCString nonce
  let toPath' = tmp </> (fileName <> ".decrypted.pdf")
  cToPath' <- newCString toPath'
  "" <- peekCAString =<< cChatDecryptFile cToPath cKey cNonce cToPath'
  B.readFile toPath' `shouldReturn` src

testMissingFileEncryptionCApi :: TestParams -> IO ()
testMissingFileEncryptionCApi ps = do
  let tmp = tmpPath ps
  cc <- mkCCPtr tmp
  let fromPath = tmp </> "missing_file.source.pdf"
      toPath = tmp </> "missing_file.encrypted.pdf"
  cFromPath <- newCString fromPath
  cToPath <- newCString toPath
  r <- peekCAString =<< cChatEncryptFile cc cFromPath cToPath
  Just (WFError err) <- jDecode r
  err `shouldContain` fromPath
  CFArgs key nonce <- atomically . CF.randomArgs =<< C.newRandom
  cKey <- encodedCString key
  cNonce <- encodedCString nonce
  let toPath' = tmp </> "missing_file.decrypted.pdf"
  cToPath' <- newCString toPath'
  err' <- peekCAString =<< cChatDecryptFile cToPath cKey cNonce cToPath'
  err' `shouldContain` toPath

mkCCPtr :: FilePath -> IO (StablePtr ChatController)
mkCCPtr tmp = either (error . show) newStablePtr =<< chatMigrateInit (tmp </> "1") "" "yesUp"

testValidNameCApi :: TestParams -> IO ()
testValidNameCApi _ = do
  let goodName = "Джон Доу 👍"
  cName1 <- cChatValidName =<< newCString goodName
  peekCString cName1 `shouldReturn` goodName
  cName2 <- cChatValidName =<< newCString " @'Джон'  Доу   👍 "
  peekCString cName2 `shouldReturn` goodName

testChatJsonLengthCApi :: TestParams -> IO ()
testChatJsonLengthCApi _ = do
  cInt1 <- cChatJsonLength =<< newCString "Hello!"
  cInt1 `shouldBe` 6
  cInt2 <- cChatJsonLength =<< newCString "こんにちは！"
  cInt2 `shouldBe` 18

jDecode :: FromJSON a => String -> IO (Maybe a)
jDecode = pure . J.decode . LB.pack

encodedCString :: StrEncoding a => a -> IO CString
encodedCString = newCAString . BS.unpack . strEncode

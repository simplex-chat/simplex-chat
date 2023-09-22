{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module MobileTests where

import ChatTests.Utils
import Control.Monad.Except
import Crypto.Random (getRandomBytes)
import Data.Aeson (FromJSON (..))
import qualified Data.Aeson as J
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BS
import Data.ByteString.Internal (create, memcpy)
import qualified Data.ByteString.Lazy.Char8 as LB
import Data.Word (Word8, Word32)
import Foreign.C
import Foreign.Marshal.Alloc (mallocBytes)
import Foreign.Ptr
import Foreign.Storable (peek)
import GHC.IO.Encoding (setLocaleEncoding, setFileSystemEncoding, setForeignEncoding)
import Simplex.Chat.Mobile
import Simplex.Chat.Mobile.File
import Simplex.Chat.Mobile.Shared
import Simplex.Chat.Mobile.WebRTC
import Simplex.Chat.Store
import Simplex.Chat.Store.Profiles
import Simplex.Chat.Types (AgentUserId (..), Profile (..))
import Simplex.Messaging.Agent.Store.SQLite (MigrationConfirmation (..))
import qualified Simplex.Messaging.Crypto as C
import Simplex.Messaging.Crypto.File (CryptoFile(..), CryptoFileArgs (..))
import qualified Simplex.Messaging.Crypto.File as CF
import Simplex.Messaging.Encoding.String
import Simplex.Messaging.Parsers (dropPrefix, sumTypeJSON)
import System.Directory (copyFile)
import System.FilePath ((</>))
import System.IO (utf8)
import Test.Hspec

mobileTests :: HasCallStack => SpecWith FilePath
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

noActiveUser :: LB.ByteString
#if defined(darwin_HOST_OS) && defined(swiftJSON)
noActiveUser = "{\"resp\":{\"chatCmdError\":{\"chatError\":{\"error\":{\"errorType\":{\"noActiveUser\":{}}}}}}}"
#else
noActiveUser = "{\"resp\":{\"type\":\"chatCmdError\",\"chatError\":{\"type\":\"error\",\"errorType\":{\"type\":\"noActiveUser\"}}}}"
#endif

activeUserExists :: LB.ByteString
#if defined(darwin_HOST_OS) && defined(swiftJSON)
activeUserExists = "{\"resp\":{\"chatCmdError\":{\"user_\":{\"userId\":1,\"agentUserId\":\"1\",\"userContactId\":1,\"localDisplayName\":\"alice\",\"profile\":{\"profileId\":1,\"displayName\":\"alice\",\"fullName\":\"Alice\",\"localAlias\":\"\"},\"fullPreferences\":{\"timedMessages\":{\"allow\":\"yes\"},\"fullDelete\":{\"allow\":\"no\"},\"reactions\":{\"allow\":\"yes\"},\"voice\":{\"allow\":\"yes\"},\"calls\":{\"allow\":\"yes\"}},\"activeUser\":true,\"showNtfs\":true,\"sendRcptsContacts\":true,\"sendRcptsSmallGroups\":true},\"chatError\":{\"error\":{\"errorType\":{\"userExists\":{\"contactName\":\"alice\"}}}}}}}"
#else
activeUserExists = "{\"resp\":{\"type\":\"chatCmdError\",\"user_\":{\"userId\":1,\"agentUserId\":\"1\",\"userContactId\":1,\"localDisplayName\":\"alice\",\"profile\":{\"profileId\":1,\"displayName\":\"alice\",\"fullName\":\"Alice\",\"localAlias\":\"\"},\"fullPreferences\":{\"timedMessages\":{\"allow\":\"yes\"},\"fullDelete\":{\"allow\":\"no\"},\"reactions\":{\"allow\":\"yes\"},\"voice\":{\"allow\":\"yes\"},\"calls\":{\"allow\":\"yes\"}},\"activeUser\":true,\"showNtfs\":true,\"sendRcptsContacts\":true,\"sendRcptsSmallGroups\":true},\"chatError\":{\"type\":\"error\",\"errorType\":{\"type\":\"userExists\",\"contactName\":\"alice\"}}}}"
#endif

activeUser :: LB.ByteString
#if defined(darwin_HOST_OS) && defined(swiftJSON)
activeUser = "{\"resp\":{\"activeUser\":{\"user\":{\"userId\":1,\"agentUserId\":\"1\",\"userContactId\":1,\"localDisplayName\":\"alice\",\"profile\":{\"profileId\":1,\"displayName\":\"alice\",\"fullName\":\"Alice\",\"localAlias\":\"\"},\"fullPreferences\":{\"timedMessages\":{\"allow\":\"yes\"},\"fullDelete\":{\"allow\":\"no\"},\"reactions\":{\"allow\":\"yes\"},\"voice\":{\"allow\":\"yes\"},\"calls\":{\"allow\":\"yes\"}},\"activeUser\":true,\"showNtfs\":true,\"sendRcptsContacts\":true,\"sendRcptsSmallGroups\":true}}}}"
#else
activeUser = "{\"resp\":{\"type\":\"activeUser\",\"user\":{\"userId\":1,\"agentUserId\":\"1\",\"userContactId\":1,\"localDisplayName\":\"alice\",\"profile\":{\"profileId\":1,\"displayName\":\"alice\",\"fullName\":\"Alice\",\"localAlias\":\"\"},\"fullPreferences\":{\"timedMessages\":{\"allow\":\"yes\"},\"fullDelete\":{\"allow\":\"no\"},\"reactions\":{\"allow\":\"yes\"},\"voice\":{\"allow\":\"yes\"},\"calls\":{\"allow\":\"yes\"}},\"activeUser\":true,\"showNtfs\":true,\"sendRcptsContacts\":true,\"sendRcptsSmallGroups\":true}}}"
#endif

chatStarted :: LB.ByteString
#if defined(darwin_HOST_OS) && defined(swiftJSON)
chatStarted = "{\"resp\":{\"chatStarted\":{}}}"
#else
chatStarted = "{\"resp\":{\"type\":\"chatStarted\"}}"
#endif

contactSubSummary :: LB.ByteString
#if defined(darwin_HOST_OS) && defined(swiftJSON)
contactSubSummary = "{\"resp\":{\"contactSubSummary\":{" <> userJSON <> ",\"contactSubscriptions\":[]}}}"
#else
contactSubSummary = "{\"resp\":{\"type\":\"contactSubSummary\"," <> userJSON <> ",\"contactSubscriptions\":[]}}"
#endif

memberSubSummary :: LB.ByteString
#if defined(darwin_HOST_OS) && defined(swiftJSON)
memberSubSummary = "{\"resp\":{\"memberSubSummary\":{" <> userJSON <> ",\"memberSubscriptions\":[]}}}"
#else
memberSubSummary = "{\"resp\":{\"type\":\"memberSubSummary\"," <> userJSON <> ",\"memberSubscriptions\":[]}}"
#endif

userContactSubSummary :: LB.ByteString
#if defined(darwin_HOST_OS) && defined(swiftJSON)
userContactSubSummary = "{\"resp\":{\"userContactSubSummary\":{" <> userJSON <> ",\"userContactSubscriptions\":[]}}}"
#else
userContactSubSummary = "{\"resp\":{\"type\":\"userContactSubSummary\"," <> userJSON <> ",\"userContactSubscriptions\":[]}}"
#endif

pendingSubSummary :: LB.ByteString
#if defined(darwin_HOST_OS) && defined(swiftJSON)
pendingSubSummary = "{\"resp\":{\"pendingSubSummary\":{" <> userJSON <> ",\"pendingSubscriptions\":[]}}}"
#else
pendingSubSummary = "{\"resp\":{\"type\":\"pendingSubSummary\"," <> userJSON <> ",\"pendingSubscriptions\":[]}}"
#endif

userJSON :: LB.ByteString
userJSON = "\"user\":{\"userId\":1,\"agentUserId\":\"1\",\"userContactId\":1,\"localDisplayName\":\"alice\",\"profile\":{\"profileId\":1,\"displayName\":\"alice\",\"fullName\":\"Alice\",\"localAlias\":\"\"},\"fullPreferences\":{\"timedMessages\":{\"allow\":\"yes\"},\"fullDelete\":{\"allow\":\"no\"},\"reactions\":{\"allow\":\"yes\"},\"voice\":{\"allow\":\"yes\"},\"calls\":{\"allow\":\"yes\"}},\"activeUser\":true,\"showNtfs\":true,\"sendRcptsContacts\":true,\"sendRcptsSmallGroups\":true}"

parsedMarkdown :: LB.ByteString
#if defined(darwin_HOST_OS) && defined(swiftJSON)
parsedMarkdown = "{\"formattedText\":[{\"format\":{\"bold\":{}},\"text\":\"hello\"}]}"
#else
parsedMarkdown = "{\"formattedText\":[{\"format\":{\"type\":\"bold\"},\"text\":\"hello\"}]}"
#endif

testChatApiNoUser :: FilePath -> IO ()
testChatApiNoUser tmp = do
  let dbPrefix = tmp </> "1"
  Right cc <- chatMigrateInit dbPrefix "" "yesUp"
  Left (DBMErrorNotADatabase _) <- chatMigrateInit dbPrefix "myKey" "yesUp"
  chatSendCmd cc "/u" `shouldReturn` noActiveUser
  chatSendCmd cc "/_start" `shouldReturn` noActiveUser
  chatSendCmd cc "/create user alice Alice" `shouldReturn` activeUser
  chatSendCmd cc "/_start" `shouldReturn` chatStarted

testChatApi :: FilePath -> IO ()
testChatApi tmp = do
  let dbPrefix = tmp </> "1"
      f = chatStoreFile dbPrefix
  Right st <- createChatStore f "myKey" MCYesUp
  Right _ <- withTransaction st $ \db -> runExceptT $ createUserRecord db (AgentUserId 1) aliceProfile {preferences = Nothing} True
  Right cc <- chatMigrateInit dbPrefix "myKey" "yesUp"
  Left (DBMErrorNotADatabase _) <- chatMigrateInit dbPrefix "" "yesUp"
  Left (DBMErrorNotADatabase _) <- chatMigrateInit dbPrefix "anotherKey" "yesUp"
  chatSendCmd cc "/u" `shouldReturn` activeUser
  chatSendCmd cc "/create user alice Alice" `shouldReturn` activeUserExists
  chatSendCmd cc "/_start" `shouldReturn` chatStarted
  chatRecvMsg cc `shouldReturn` contactSubSummary
  chatRecvMsg cc `shouldReturn` userContactSubSummary
  chatRecvMsg cc `shouldReturn` memberSubSummary
  chatRecvMsgWait cc 10000 `shouldReturn` pendingSubSummary
  chatRecvMsgWait cc 10000 `shouldReturn` ""
  chatParseMarkdown "hello" `shouldBe` "{}"
  chatParseMarkdown "*hello*" `shouldBe` parsedMarkdown

testMediaApi :: HasCallStack => FilePath -> IO ()
testMediaApi _ = do
  key :: ByteString <- getRandomBytes 32
  frame <- getRandomBytes 100
  let keyStr = strEncode key
      reserved = B.replicate (C.authTagSize + C.gcmIVSize) 0
      frame' = frame <> reserved
  Right encrypted <- runExceptT $ chatEncryptMedia keyStr frame'
  encrypted `shouldNotBe` frame'
  B.length encrypted `shouldBe` B.length frame'
  runExceptT (chatDecryptMedia keyStr encrypted) `shouldReturn` Right frame'

testMediaCApi :: HasCallStack => FilePath -> IO ()
testMediaCApi _ = do
  key :: ByteString <- getRandomBytes 32
  frame <- getRandomBytes 100
  let keyStr = strEncode key
      reserved = B.replicate (C.authTagSize + C.gcmIVSize) 0
      frame' = frame <> reserved
  encrypted <- test cChatEncryptMedia keyStr frame'
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

instance FromJSON WriteFileResult where parseJSON = J.genericParseJSON . sumTypeJSON $ dropPrefix "WF"

instance FromJSON ReadFileResult where parseJSON = J.genericParseJSON . sumTypeJSON $ dropPrefix "RF"

testFileCApi :: FilePath -> FilePath -> IO ()
testFileCApi fileName tmp = do
  src <- B.readFile "./tests/fixtures/test.pdf"
  let path = tmp </> (fileName <> ".pdf")
  cPath <- newCString path
  let len = B.length src
      cLen = fromIntegral len
  ptr <- mallocBytes $ B.length src
  putByteString ptr src
  r <- peekCAString =<< cChatWriteFile cPath ptr cLen
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
  contents <- create sz' $ \toPtr -> memcpy toPtr (ptr' `plusPtr` 5) sz'
  contents `shouldBe` src
  sz' `shouldBe` fromIntegral len

testMissingFileCApi :: FilePath -> IO ()
testMissingFileCApi tmp = do
  let path = tmp </> "missing_file"
  cPath <- newCString path
  CFArgs key nonce <- CF.randomArgs
  cKey <- encodedCString key
  cNonce <- encodedCString nonce
  ptr <- cChatReadFile cPath cKey cNonce
  peek ptr `shouldReturn` 1
  err <- peekCAString (ptr `plusPtr` 1)
  err `shouldContain` "missing_file: openBinaryFile: does not exist"

testFileEncryptionCApi :: FilePath -> FilePath -> IO ()
testFileEncryptionCApi fileName tmp = do
  let fromPath = tmp </> (fileName <> ".source.pdf")
  copyFile "./tests/fixtures/test.pdf" fromPath
  src <- B.readFile fromPath
  cFromPath <- newCString fromPath
  let toPath = tmp </> (fileName <> ".encrypted.pdf")
  cToPath <- newCString toPath
  r <- peekCAString =<< cChatEncryptFile cFromPath cToPath
  Just (WFResult cfArgs@(CFArgs key nonce)) <- jDecode r
  CF.getFileContentsSize (CryptoFile toPath $ Just cfArgs) `shouldReturn` fromIntegral (B.length src)
  cKey <- encodedCString key
  cNonce <- encodedCString nonce
  let toPath' = tmp </> (fileName <> ".decrypted.pdf")
  cToPath' <- newCString toPath'
  "" <- peekCAString =<< cChatDecryptFile cToPath cKey cNonce cToPath'
  B.readFile toPath' `shouldReturn` src

testMissingFileEncryptionCApi :: FilePath -> IO ()
testMissingFileEncryptionCApi tmp = do
  let fromPath = tmp </> "missing_file.source.pdf"
      toPath = tmp </> "missing_file.encrypted.pdf"
  cFromPath <- newCString fromPath
  cToPath <- newCString toPath
  r <- peekCAString =<< cChatEncryptFile cFromPath cToPath
  Just (WFError err) <- jDecode r
  err `shouldContain` fromPath
  CFArgs key nonce <- CF.randomArgs
  cKey <- encodedCString key
  cNonce <- encodedCString nonce
  let toPath' = tmp </> "missing_file.decrypted.pdf"
  cToPath' <- newCString toPath'
  err' <- peekCAString =<< cChatDecryptFile cToPath cKey cNonce cToPath'
  err' `shouldContain` toPath

jDecode :: FromJSON a => String -> IO (Maybe a)
jDecode = pure . J.decode . LB.pack

encodedCString :: StrEncoding a => a -> IO CString
encodedCString = newCAString . BS.unpack . strEncode

{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module MobileTests where

import ChatTests.Utils
import Control.Concurrent.STM
import Control.Monad.Except
import Data.Aeson (FromJSON)
import qualified Data.Aeson as J
import qualified Data.Aeson.TH as JQ
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BS
import Data.ByteString.Internal (create, memcpy)
import qualified Data.ByteString.Lazy.Char8 as LB
import Data.Word (Word8, Word32)
import Foreign.C
import Foreign.Marshal.Alloc (mallocBytes)
import Foreign.Ptr
import Foreign.StablePtr
import Foreign.Storable (peek)
import GHC.IO.Encoding (setLocaleEncoding, setFileSystemEncoding, setForeignEncoding)
import Simplex.Chat.Controller (ChatController (..))
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
    describe "validate name" $ do
      it "should convert invalid name to a valid name" testValidNameCApi

noActiveUser :: LB.ByteString
noActiveUser =
#if defined(darwin_HOST_OS) && defined(swiftJSON)
  noActiveUserSwift
#else
  noActiveUserTagged
#endif

noActiveUserSwift :: LB.ByteString
noActiveUserSwift = "{\"resp\":{\"_owsf\":true,\"chatCmdError\":{\"chatError\":{\"_owsf\":true,\"error\":{\"errorType\":{\"_owsf\":true,\"noActiveUser\":{}}}}}}}"

noActiveUserTagged :: LB.ByteString
noActiveUserTagged = "{\"resp\":{\"type\":\"chatCmdError\",\"chatError\":{\"type\":\"error\",\"errorType\":{\"type\":\"noActiveUser\"}}}}"

activeUserExists :: LB.ByteString
activeUserExists =
#if defined(darwin_HOST_OS) && defined(swiftJSON)
  activeUserExistsSwift
#else
  activeUserExistsTagged
#endif

activeUserExistsSwift :: LB.ByteString
activeUserExistsSwift = "{\"resp\":{\"_owsf\":true,\"chatCmdError\":{\"user_\":{\"userId\":1,\"agentUserId\":\"1\",\"userContactId\":1,\"localDisplayName\":\"alice\",\"profile\":{\"profileId\":1,\"displayName\":\"alice\",\"fullName\":\"Alice\",\"localAlias\":\"\"},\"fullPreferences\":{\"timedMessages\":{\"allow\":\"yes\"},\"fullDelete\":{\"allow\":\"no\"},\"reactions\":{\"allow\":\"yes\"},\"voice\":{\"allow\":\"yes\"},\"calls\":{\"allow\":\"yes\"}},\"activeUser\":true,\"showNtfs\":true,\"sendRcptsContacts\":true,\"sendRcptsSmallGroups\":true},\"chatError\":{\"_owsf\":true,\"error\":{\"errorType\":{\"_owsf\":true,\"userExists\":{\"contactName\":\"alice\"}}}}}}}"

activeUserExistsTagged :: LB.ByteString
activeUserExistsTagged = "{\"resp\":{\"type\":\"chatCmdError\",\"user_\":{\"userId\":1,\"agentUserId\":\"1\",\"userContactId\":1,\"localDisplayName\":\"alice\",\"profile\":{\"profileId\":1,\"displayName\":\"alice\",\"fullName\":\"Alice\",\"localAlias\":\"\"},\"fullPreferences\":{\"timedMessages\":{\"allow\":\"yes\"},\"fullDelete\":{\"allow\":\"no\"},\"reactions\":{\"allow\":\"yes\"},\"voice\":{\"allow\":\"yes\"},\"calls\":{\"allow\":\"yes\"}},\"activeUser\":true,\"showNtfs\":true,\"sendRcptsContacts\":true,\"sendRcptsSmallGroups\":true},\"chatError\":{\"type\":\"error\",\"errorType\":{\"type\":\"userExists\",\"contactName\":\"alice\"}}}}"

activeUser :: LB.ByteString
activeUser =
#if defined(darwin_HOST_OS) && defined(swiftJSON)
  activeUserSwift
#else
  activeUserTagged
#endif

activeUserSwift :: LB.ByteString
activeUserSwift = "{\"resp\":{\"_owsf\":true,\"activeUser\":{\"user\":{\"userId\":1,\"agentUserId\":\"1\",\"userContactId\":1,\"localDisplayName\":\"alice\",\"profile\":{\"profileId\":1,\"displayName\":\"alice\",\"fullName\":\"Alice\",\"localAlias\":\"\"},\"fullPreferences\":{\"timedMessages\":{\"allow\":\"yes\"},\"fullDelete\":{\"allow\":\"no\"},\"reactions\":{\"allow\":\"yes\"},\"voice\":{\"allow\":\"yes\"},\"calls\":{\"allow\":\"yes\"}},\"activeUser\":true,\"showNtfs\":true,\"sendRcptsContacts\":true,\"sendRcptsSmallGroups\":true}}}}"

activeUserTagged :: LB.ByteString
activeUserTagged = "{\"resp\":{\"type\":\"activeUser\",\"user\":{\"userId\":1,\"agentUserId\":\"1\",\"userContactId\":1,\"localDisplayName\":\"alice\",\"profile\":{\"profileId\":1,\"displayName\":\"alice\",\"fullName\":\"Alice\",\"localAlias\":\"\"},\"fullPreferences\":{\"timedMessages\":{\"allow\":\"yes\"},\"fullDelete\":{\"allow\":\"no\"},\"reactions\":{\"allow\":\"yes\"},\"voice\":{\"allow\":\"yes\"},\"calls\":{\"allow\":\"yes\"}},\"activeUser\":true,\"showNtfs\":true,\"sendRcptsContacts\":true,\"sendRcptsSmallGroups\":true}}}"

chatStarted :: LB.ByteString
chatStarted =
#if defined(darwin_HOST_OS) && defined(swiftJSON)
  chatStartedSwift
#else
  chatStartedTagged
#endif

chatStartedSwift :: LB.ByteString
chatStartedSwift = "{\"resp\":{\"_owsf\":true,\"chatStarted\":{}}}"

chatStartedTagged :: LB.ByteString
chatStartedTagged = "{\"resp\":{\"type\":\"chatStarted\"}}"

networkStatuses :: LB.ByteString
networkStatuses =
#if defined(darwin_HOST_OS) && defined(swiftJSON)
  networkStatusesSwift
#else
  networkStatusesTagged
#endif

networkStatusesSwift :: LB.ByteString
networkStatusesSwift = "{\"resp\":{\"_owsf\":true,\"networkStatuses\":{\"user_\":" <> userJSON <> ",\"networkStatuses\":[]}}}"

networkStatusesTagged :: LB.ByteString
networkStatusesTagged = "{\"resp\":{\"type\":\"networkStatuses\",\"user_\":" <> userJSON <> ",\"networkStatuses\":[]}}"

memberSubSummary :: LB.ByteString
memberSubSummary =
#if defined(darwin_HOST_OS) && defined(swiftJSON)
  memberSubSummarySwift
#else
  memberSubSummaryTagged
#endif

memberSubSummarySwift :: LB.ByteString
memberSubSummarySwift = "{\"resp\":{\"_owsf\":true,\"memberSubSummary\":{\"user\":" <> userJSON <> ",\"memberSubscriptions\":[]}}}"

memberSubSummaryTagged :: LB.ByteString
memberSubSummaryTagged = "{\"resp\":{\"type\":\"memberSubSummary\",\"user\":" <> userJSON <> ",\"memberSubscriptions\":[]}}"

userContactSubSummary :: LB.ByteString
userContactSubSummary =
#if defined(darwin_HOST_OS) && defined(swiftJSON)
  userContactSubSummarySwift
#else
  userContactSubSummaryTagged
#endif

userContactSubSummarySwift :: LB.ByteString
userContactSubSummarySwift = "{\"resp\":{\"_owsf\":true,\"userContactSubSummary\":{\"user\":" <> userJSON <> ",\"userContactSubscriptions\":[]}}}"

userContactSubSummaryTagged :: LB.ByteString
userContactSubSummaryTagged = "{\"resp\":{\"type\":\"userContactSubSummary\",\"user\":" <> userJSON <> ",\"userContactSubscriptions\":[]}}"

pendingSubSummary :: LB.ByteString
pendingSubSummary =
#if defined(darwin_HOST_OS) && defined(swiftJSON)
  pendingSubSummarySwift
#else
  pendingSubSummaryTagged
#endif

pendingSubSummarySwift :: LB.ByteString
pendingSubSummarySwift = "{\"resp\":{\"_owsf\":true,\"pendingSubSummary\":{\"user\":" <> userJSON <> ",\"pendingSubscriptions\":[]}}}"

pendingSubSummaryTagged :: LB.ByteString
pendingSubSummaryTagged = "{\"resp\":{\"type\":\"pendingSubSummary\",\"user\":" <> userJSON <> ",\"pendingSubscriptions\":[]}}"

userJSON :: LB.ByteString
userJSON = "{\"userId\":1,\"agentUserId\":\"1\",\"userContactId\":1,\"localDisplayName\":\"alice\",\"profile\":{\"profileId\":1,\"displayName\":\"alice\",\"fullName\":\"Alice\",\"localAlias\":\"\"},\"fullPreferences\":{\"timedMessages\":{\"allow\":\"yes\"},\"fullDelete\":{\"allow\":\"no\"},\"reactions\":{\"allow\":\"yes\"},\"voice\":{\"allow\":\"yes\"},\"calls\":{\"allow\":\"yes\"}},\"activeUser\":true,\"showNtfs\":true,\"sendRcptsContacts\":true,\"sendRcptsSmallGroups\":true}"

parsedMarkdown :: LB.ByteString
parsedMarkdown =
#if defined(darwin_HOST_OS) && defined(swiftJSON)
  parsedMarkdownSwift
#else
  parsedMarkdownTagged
#endif

parsedMarkdownSwift :: LB.ByteString
parsedMarkdownSwift = "{\"formattedText\":[{\"format\":{\"_owsf\":true,\"bold\":{}},\"text\":\"hello\"}]}"

parsedMarkdownTagged :: LB.ByteString
parsedMarkdownTagged = "{\"formattedText\":[{\"format\":{\"type\":\"bold\"},\"text\":\"hello\"}]}"

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
  Right st <- createChatStore f "myKey" False MCYesUp
  Right _ <- withTransaction st $ \db -> runExceptT $ createUserRecord db (AgentUserId 1) aliceProfile {preferences = Nothing} True
  Right cc <- chatMigrateInit dbPrefix "myKey" "yesUp"
  Left (DBMErrorNotADatabase _) <- chatMigrateInit dbPrefix "" "yesUp"
  Left (DBMErrorNotADatabase _) <- chatMigrateInit dbPrefix "anotherKey" "yesUp"
  chatSendCmd cc "/u" `shouldReturn` activeUser
  chatSendCmd cc "/create user alice Alice" `shouldReturn` activeUserExists
  chatSendCmd cc "/_start" `shouldReturn` chatStarted
  chatRecvMsg cc `shouldReturn` networkStatuses
  chatRecvMsg cc `shouldReturn` userContactSubSummary
  chatRecvMsg cc `shouldReturn` memberSubSummary
  chatRecvMsgWait cc 10000 `shouldReturn` pendingSubSummary
  chatRecvMsgWait cc 10000 `shouldReturn` ""
  chatParseMarkdown "hello" `shouldBe` "{}"
  chatParseMarkdown "*hello*" `shouldBe` parsedMarkdown

testMediaApi :: HasCallStack => FilePath -> IO ()
testMediaApi tmp = do
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

testMediaCApi :: HasCallStack => FilePath -> IO ()
testMediaCApi tmp = do
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

testFileCApi :: FilePath -> FilePath -> IO ()
testFileCApi fileName tmp = do
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
  contents <- create sz' $ \toPtr -> memcpy toPtr (ptr' `plusPtr` 5) sz'
  contents `shouldBe` src
  sz' `shouldBe` fromIntegral len

testMissingFileCApi :: FilePath -> IO ()
testMissingFileCApi tmp = do
  let path = tmp </> "missing_file"
  cPath <- newCString path
  CFArgs key nonce <- atomically . CF.randomArgs =<< C.newRandom
  cKey <- encodedCString key
  cNonce <- encodedCString nonce
  ptr <- cChatReadFile cPath cKey cNonce
  peek ptr `shouldReturn` 1
  err <- peekCAString (ptr `plusPtr` 1)
  err `shouldContain` "missing_file: openBinaryFile: does not exist"

testFileEncryptionCApi :: FilePath -> FilePath -> IO ()
testFileEncryptionCApi fileName tmp = do
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

testMissingFileEncryptionCApi :: FilePath -> IO ()
testMissingFileEncryptionCApi tmp = do
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

testValidNameCApi :: FilePath -> IO ()
testValidNameCApi _ = do
  let goodName = "Джон Доу 👍"
  cName1 <- cChatValidName =<< newCString goodName
  peekCString cName1 `shouldReturn` goodName
  cName2 <- cChatValidName =<< newCString " @'Джон'  Доу   👍 "
  peekCString cName2 `shouldReturn` goodName

jDecode :: FromJSON a => String -> IO (Maybe a)
jDecode = pure . J.decode . LB.pack

encodedCString :: StrEncoding a => a -> IO CString
encodedCString = newCAString . BS.unpack . strEncode

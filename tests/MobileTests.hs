{-# LANGUAGE CPP #-}
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
import qualified Data.ByteString.Lazy.Char8 as LB
import Data.Word (Word8)
import Foreign.C
import Foreign.Marshal.Alloc (mallocBytes)
import Foreign.Ptr
import Simplex.Chat.Mobile
import Simplex.Chat.Mobile.File
import Simplex.Chat.Mobile.Shared
import Simplex.Chat.Mobile.WebRTC
import Simplex.Chat.Store
import Simplex.Chat.Store.Profiles
import Simplex.Chat.Types (AgentUserId (..), Profile (..))
import Simplex.Messaging.Agent.Store.SQLite (MigrationConfirmation (..))
import qualified Simplex.Messaging.Crypto as C
import Simplex.Messaging.Crypto.File (CryptoFile(..), CryptoFileArgs (..), getFileContentsSize)
import Simplex.Messaging.Encoding.String
import Simplex.Messaging.Parsers (dropPrefix, sumTypeJSON)
import System.FilePath ((</>))
import Test.Hspec

mobileTests :: HasCallStack => SpecWith FilePath
mobileTests = do
  describe "mobile API" $ do
    it "start new chat without user" testChatApiNoUser
    it "start new chat with existing user" testChatApi
    it "should encrypt/decrypt WebRTC frames" testMediaApi
    it "should encrypt/decrypt WebRTC frames via C API" testMediaCApi
    it "should read/write encrypted files via C API" testFileCApi
    it "should encrypt/decrypt files via C API" testFileEncryptionCApi

noActiveUser :: String
#if defined(darwin_HOST_OS) && defined(swiftJSON)
noActiveUser = "{\"resp\":{\"chatCmdError\":{\"chatError\":{\"error\":{\"errorType\":{\"noActiveUser\":{}}}}}}}"
#else
noActiveUser = "{\"resp\":{\"type\":\"chatCmdError\",\"chatError\":{\"type\":\"error\",\"errorType\":{\"type\":\"noActiveUser\"}}}}"
#endif

activeUserExists :: String
#if defined(darwin_HOST_OS) && defined(swiftJSON)
activeUserExists = "{\"resp\":{\"chatCmdError\":{\"user_\":{\"userId\":1,\"agentUserId\":\"1\",\"userContactId\":1,\"localDisplayName\":\"alice\",\"profile\":{\"profileId\":1,\"displayName\":\"alice\",\"fullName\":\"Alice\",\"localAlias\":\"\"},\"fullPreferences\":{\"timedMessages\":{\"allow\":\"yes\"},\"fullDelete\":{\"allow\":\"no\"},\"reactions\":{\"allow\":\"yes\"},\"voice\":{\"allow\":\"yes\"},\"calls\":{\"allow\":\"yes\"}},\"activeUser\":true,\"showNtfs\":true,\"sendRcptsContacts\":true,\"sendRcptsSmallGroups\":true},\"chatError\":{\"error\":{\"errorType\":{\"userExists\":{\"contactName\":\"alice\"}}}}}}}"
#else
activeUserExists = "{\"resp\":{\"type\":\"chatCmdError\",\"user_\":{\"userId\":1,\"agentUserId\":\"1\",\"userContactId\":1,\"localDisplayName\":\"alice\",\"profile\":{\"profileId\":1,\"displayName\":\"alice\",\"fullName\":\"Alice\",\"localAlias\":\"\"},\"fullPreferences\":{\"timedMessages\":{\"allow\":\"yes\"},\"fullDelete\":{\"allow\":\"no\"},\"reactions\":{\"allow\":\"yes\"},\"voice\":{\"allow\":\"yes\"},\"calls\":{\"allow\":\"yes\"}},\"activeUser\":true,\"showNtfs\":true,\"sendRcptsContacts\":true,\"sendRcptsSmallGroups\":true},\"chatError\":{\"type\":\"error\",\"errorType\":{\"type\":\"userExists\",\"contactName\":\"alice\"}}}}"
#endif

activeUser :: String
#if defined(darwin_HOST_OS) && defined(swiftJSON)
activeUser = "{\"resp\":{\"activeUser\":{\"user\":{\"userId\":1,\"agentUserId\":\"1\",\"userContactId\":1,\"localDisplayName\":\"alice\",\"profile\":{\"profileId\":1,\"displayName\":\"alice\",\"fullName\":\"Alice\",\"localAlias\":\"\"},\"fullPreferences\":{\"timedMessages\":{\"allow\":\"yes\"},\"fullDelete\":{\"allow\":\"no\"},\"reactions\":{\"allow\":\"yes\"},\"voice\":{\"allow\":\"yes\"},\"calls\":{\"allow\":\"yes\"}},\"activeUser\":true,\"showNtfs\":true,\"sendRcptsContacts\":true,\"sendRcptsSmallGroups\":true}}}}"
#else
activeUser = "{\"resp\":{\"type\":\"activeUser\",\"user\":{\"userId\":1,\"agentUserId\":\"1\",\"userContactId\":1,\"localDisplayName\":\"alice\",\"profile\":{\"profileId\":1,\"displayName\":\"alice\",\"fullName\":\"Alice\",\"localAlias\":\"\"},\"fullPreferences\":{\"timedMessages\":{\"allow\":\"yes\"},\"fullDelete\":{\"allow\":\"no\"},\"reactions\":{\"allow\":\"yes\"},\"voice\":{\"allow\":\"yes\"},\"calls\":{\"allow\":\"yes\"}},\"activeUser\":true,\"showNtfs\":true,\"sendRcptsContacts\":true,\"sendRcptsSmallGroups\":true}}}"
#endif

chatStarted :: String
#if defined(darwin_HOST_OS) && defined(swiftJSON)
chatStarted = "{\"resp\":{\"chatStarted\":{}}}"
#else
chatStarted = "{\"resp\":{\"type\":\"chatStarted\"}}"
#endif

contactSubSummary :: String
#if defined(darwin_HOST_OS) && defined(swiftJSON)
contactSubSummary = "{\"resp\":{\"contactSubSummary\":{" <> userJSON <> ",\"contactSubscriptions\":[]}}}"
#else
contactSubSummary = "{\"resp\":{\"type\":\"contactSubSummary\"," <> userJSON <> ",\"contactSubscriptions\":[]}}"
#endif

memberSubSummary :: String
#if defined(darwin_HOST_OS) && defined(swiftJSON)
memberSubSummary = "{\"resp\":{\"memberSubSummary\":{" <> userJSON <> ",\"memberSubscriptions\":[]}}}"
#else
memberSubSummary = "{\"resp\":{\"type\":\"memberSubSummary\"," <> userJSON <> ",\"memberSubscriptions\":[]}}"
#endif

userContactSubSummary :: String
#if defined(darwin_HOST_OS) && defined(swiftJSON)
userContactSubSummary = "{\"resp\":{\"userContactSubSummary\":{" <> userJSON <> ",\"userContactSubscriptions\":[]}}}"
#else
userContactSubSummary = "{\"resp\":{\"type\":\"userContactSubSummary\"," <> userJSON <> ",\"userContactSubscriptions\":[]}}"
#endif

pendingSubSummary :: String
#if defined(darwin_HOST_OS) && defined(swiftJSON)
pendingSubSummary = "{\"resp\":{\"pendingSubSummary\":{" <> userJSON <> ",\"pendingSubscriptions\":[]}}}"
#else
pendingSubSummary = "{\"resp\":{\"type\":\"pendingSubSummary\"," <> userJSON <> ",\"pendingSubscriptions\":[]}}"
#endif

userJSON :: String
userJSON = "\"user\":{\"userId\":1,\"agentUserId\":\"1\",\"userContactId\":1,\"localDisplayName\":\"alice\",\"profile\":{\"profileId\":1,\"displayName\":\"alice\",\"fullName\":\"Alice\",\"localAlias\":\"\"},\"fullPreferences\":{\"timedMessages\":{\"allow\":\"yes\"},\"fullDelete\":{\"allow\":\"no\"},\"reactions\":{\"allow\":\"yes\"},\"voice\":{\"allow\":\"yes\"},\"calls\":{\"allow\":\"yes\"}},\"activeUser\":true,\"showNtfs\":true,\"sendRcptsContacts\":true,\"sendRcptsSmallGroups\":true}"

parsedMarkdown :: String
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

testFileCApi :: FilePath -> IO ()
testFileCApi tmp = do
  src <- B.readFile "./tests/fixtures/test.pdf"
  cPath <- newCAString $ tmp </> "test.pdf"
  let len = B.length src
      cLen = fromIntegral len
  ptr <- mallocBytes $ B.length src
  putByteString ptr src
  r <- peekCAString =<< cChatWriteFile cPath ptr cLen
  Just (WFResult (CFArgs key nonce)) <- jDecode r
  cKey <- encodedCString key
  cNonce <- encodedCString nonce
  ptr' <- cChatReadFile cPath cKey cNonce
  -- the returned pointer contains NUL-terminated JSON string of ReadFileResult followed by the file contents
  r' <- peekCAString $ castPtr ptr'
  Just (RFResult sz) <- jDecode r'
  contents <- getByteString (ptr' `plusPtr` (length r' + 1)) $ fromIntegral sz
  contents `shouldBe` src
  sz `shouldBe` len

testFileEncryptionCApi :: FilePath -> IO ()
testFileEncryptionCApi tmp = do
  src <- B.readFile "./tests/fixtures/test.pdf"
  cFromPath <- newCAString "./tests/fixtures/test.pdf"
  let toPath = tmp </> "test.encrypted.pdf"
  cToPath <- newCAString toPath
  r <- peekCAString =<< cChatEncryptFile cFromPath cToPath
  Just (WFResult cfArgs@(CFArgs key nonce)) <- jDecode r
  getFileContentsSize (CryptoFile toPath $ Just cfArgs) `shouldReturn` fromIntegral (B.length src)
  cKey <- encodedCString key
  cNonce <- encodedCString nonce
  let toPath' = tmp </> "test.decrypted.pdf"
  cToPath' <- newCAString toPath'
  "" <- peekCAString =<< cChatDecryptFile cToPath cKey cNonce cToPath'
  B.readFile toPath' `shouldReturn` src

jDecode :: FromJSON a => String -> IO (Maybe a)
jDecode = pure . J.decode . LB.pack

encodedCString :: StrEncoding a => a -> IO CString
encodedCString = newCAString . BS.unpack . strEncode

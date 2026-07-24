{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module MobileTests (mobileTests) where

import ChatClient
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
import Data.Text.Encoding (encodeUtf8)
import Data.Time.Clock (getCurrentTime)
import Data.Word (Word8, Word32)
import Foreign.C
import Foreign.Marshal.Alloc (mallocBytes)
import Foreign.Marshal.Utils (copyBytes)
import Foreign.Ptr
import Foreign.StablePtr
import Foreign.Storable (peek)
import GHC.IO.Encoding (setLocaleEncoding, setFileSystemEncoding, setForeignEncoding)
import JSONFixtures
import Simplex.Chat
import Simplex.Chat.Badges (BadgeInfo (..), BadgeRequest (..), BadgeType (..), generateMasterKey, verifyCredential)
import Simplex.Chat.Controller (ChatController (..), ChatDatabase (..), CheckedLink (..), ScannedLinkType (..))
import Simplex.Chat.Markdown (SimplexLinkType (..))
import Simplex.Chat.Mobile hiding (error)
import Simplex.Chat.Mobile.Badges hiding (error)
import Simplex.Chat.Mobile.File
import Simplex.Chat.Mobile.Shared
import Simplex.Chat.Mobile.WebRTC
import Simplex.Chat.Options.DB
import Simplex.Chat.Store
import Simplex.Chat.Store.Profiles
import Simplex.Chat.Types (AgentUserId (..), Profile (..), verificationCode)
import Simplex.FileTransfer.Description (ChunkReplicaId (..), FileChunk (..), FileChunkReplica (..), FileDescription (..), FileDescriptionURI (..), FileDigest (..), FileSize (..), validateFileDescription)
import Simplex.FileTransfer.Protocol (SFileParty (..))
import Simplex.Messaging.Agent.Store.Shared (MigrationConfig (..), MigrationConfirmation (..))
import qualified Simplex.Messaging.Agent.Store.SQLite.DB as DB
import qualified Simplex.Messaging.Crypto as C
import Simplex.Messaging.Crypto.File (CryptoFile(..), CryptoFileArgs (..))
import qualified Simplex.Messaging.Crypto.File as CF
import Simplex.Messaging.Encoding.String
import Simplex.Messaging.Protocol (EntityId (..))
import Simplex.Messaging.ServiceScheme (ServiceScheme (..), SrvLoc (..))
import Simplex.RemoteControl.Invitation (RCInvitation (..), signInvitation)
import Simplex.RemoteControl.Types (supportedRCPVRange)
import Data.Time.Clock.System (SystemTime (..))
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
    describe "Parsers" $ do
      it "should parse server address" testChatParseServer
      it "should parse and sanitize URI" testChatParseUri
      it "should classify scanned links" testChatCheckLink
    describe "Badges" $ do
      it "should generate key and issue badge via C API, verify credential" testBadgeKeygenIssueCApi

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

connectionsDiff :: LB.ByteString
connectionsDiff =
#if defined(darwin_HOST_OS) && defined(swiftJSON)
  connectionsDiffSwift
#else
  connectionsDiffTagged
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
  Right ChatDatabase {chatStore, agentStore} <- createChatDatabase (ChatDbOpts dbPrefix "myKey" DB.TQOff True) (MigrationConfig MCYesUp Nothing)
  insertUser agentStore
  ts <- getCurrentTime
  Right _ <- withTransaction chatStore $ \db -> runExceptT $ createUserRecordAt db (AgentUserId 1) False False aliceProfile {preferences = Nothing} True ts
  Right cc <- chatMigrateInit dbPrefix "myKey" "yesUp"
  Left (DBMErrorNotADatabase _) <- chatMigrateInit dbPrefix "" "yesUp"
  Left (DBMErrorNotADatabase _) <- chatMigrateInit dbPrefix "anotherKey" "yesUp"
  chatSendCmd cc "/u" `shouldReturn` activeUser
  chatSendCmd cc "/create user alice Alice" `shouldReturn` activeUserExists
  chatSendCmd cc "/_start" `shouldReturn` chatStarted
  chatRecvMsg cc `shouldReturn` connectionsDiff
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

testChatParseServer :: TestParams -> IO ()
testChatParseServer _ = do
  pure ()

testChatParseUri :: TestParams -> IO ()
testChatParseUri _ = do
  pure ()

testChatCheckLink :: TestParams -> IO ()
testChatCheckLink _ = do
  -- connection links: full URIs and short links, all five subtypes reach simplexLinkType
  checkLink invFull `shouldBe` Just (SLTConnection XLInvitation)
  checkLink ctShort `shouldBe` Just (SLTConnection XLContact)
  checkLink grFull `shouldBe` Just (SLTConnection XLGroup)
  checkLink grShort `shouldBe` Just (SLTConnection XLGroup)
  checkLink chShort `shouldBe` Just (SLTConnection XLChannel)
  checkLink rlShort `shouldBe` Just (SLTConnection XLRelay)
  -- SMP server address
  checkLink smpServer `shouldBe` Just SLTServer
  -- migration / standalone file link (built from a valid file description)
  checkLink fileLinkSimplex `shouldBe` Just SLTFileDescription
  -- mandatory: a file link on a non-simplex.chat https host still classifies as
  -- FileDescription (the widening half of the strHasSimplexFileLink deletion)
  checkLink fileLinkOtherHost `shouldBe` Just SLTFileDescription
  -- desktop session address (a valid signed xrcp invitation, generated here)
  drg <- C.newRandom
  (skPub, skPriv) <- atomically $ C.generateKeyPair @'C.Ed25519 drg
  (idPub, idPriv) <- atomically $ C.generateKeyPair @'C.Ed25519 drg
  (dhPub, _) <- atomically $ C.generateKeyPair @'C.X25519 drg
  let inv =
        RCInvitation
          { ca = C.KeyHash "test-ca",
            host = either error id $ strDecode "localhost",
            port = 5223,
            v = supportedRCPVRange,
            app = J.String "app",
            ts = MkSystemTime 0 0,
            skey = skPub,
            idkey = idPub,
            dh = dhPub
          }
  checkLink (strEncode $ signInvitation skPriv idPriv inv) `shouldBe` Just SLTDesktopCtrl
  -- contact security code (generated by verificationCode); recognised by shape
  checkLink (encodeUtf8 $ verificationCode $ C.sha256Hash "check-link-test") `shouldBe` Just SLTVerificationCode
  -- defensive ASCII trim: surrounding whitespace does not change classification
  checkLink ("  " <> ctShort <> "\n") `shouldBe` Just (SLTConnection XLContact)
  -- unknown / not a SimpleX code
  checkLink "simplex:/file#/?desc=not-a-valid-file-description" `shouldBe` Nothing -- mandatory: corrupt file link, guards the narrowing delta
  checkLink "xrcp:/not-a-signed-invitation" `shouldBe` Nothing -- mandatory: unsigned/malformed xrcp fails RCSignedInvitation
  checkLink "https://example.com/page" `shouldBe` Nothing
  checkLink "just some scanned text" `shouldBe` Nothing
  checkLink "123456789012345678901234567890123456" `shouldBe` Nothing -- bare number: not the grouped verification-code shape
  checkLink "" `shouldBe` Nothing
  where
    checkLink :: ByteString -> Maybe ScannedLinkType
    checkLink s = case J.decode (chatCheckLink s) of
      Just (CheckedLink t) -> t
      Nothing -> error "chatCheckLink produced undecodable JSON"
    -- fixtures reused from MarkdownTests; short-link type char a/c/g/r selects the subtype
    invFull = "simplex:/invitation#/?v=1&smp=smp%3A%2F%2F1234-w%3D%3D%40smp.simplex.im%3A5223%2F3456-w%3D%3D%23%2F%3Fv%3D1-2%26dh%3DMCowBQYDK2VuAyEAjiswwI3O_NlS8Fk3HJUW870EY2bAwmttMBsvRB9eV3o%253D&e2e=v%3D2%26x3dh%3DMEIwBQYDK2VvAzkAmKuSYeQ_m0SixPDS8Wq8VBaTS1cW-Lp0n0h4Diu-kUpR-qXx4SDJ32YGEFoGFGSbGPry5Ychr6U%3D%2CMEIwBQYDK2VvAzkAmKuSYeQ_m0SixPDS8Wq8VBaTS1cW-Lp0n0h4Diu-kUpR-qXx4SDJ32YGEFoGFGSbGPry5Ychr6U%3D"
    ctShort = "simplex:/a#lrdvu2d8A1GumSmoKb2krQmtKhWXq-tyGpHuM7aMwsw?h=smp6.simplex.im"
    chShort = "simplex:/c#lrdvu2d8A1GumSmoKb2krQmtKhWXq-tyGpHuM7aMwsw?h=smp6.simplex.im"
    grShort = "simplex:/g#lrdvu2d8A1GumSmoKb2krQmtKhWXq-tyGpHuM7aMwsw?h=smp6.simplex.im"
    rlShort = "simplex:/r#lrdvu2d8A1GumSmoKb2krQmtKhWXq-tyGpHuM7aMwsw?h=smp6.simplex.im"
    grFull = "simplex:/contact#/?v=2&smp=smp%3A%2F%2Fu2dS9sG8nMNURyZwqASV4yROM28Er0luVTx5X1CsMrU%3D%40smp4.simplex.im%2FWHV0YU1sYlU7NqiEHkHDB6gxO1ofTync%23%2F%3Fv%3D1-2%26dh%3DMCowBQYDK2VuAyEAWbebOqVYuBXaiqHcXYjEHCpYi6VzDlu6CVaijDTmsQU%253D%26srv%3Do5vmywmrnaxalvz6wi3zicyftgio6psuvyniis6gco6bp6ekl4cqj4id.onion&data=%7B%22type%22%3A%22group%22%2C%22groupLinkId%22%3A%22mL-7Divb94GGmGmRBef5Dg%3D%3D%22%7D"
    smpServer = "smp://0YuTwO05YJWS8rkjn9eLJDjQhFKvIYd8d4xG8X1blIU=@smp8.simplex.im"
    -- a valid file link, built from a minimal 1-chunk file description (crypto
    -- literals reused from simplexmq's FileDescriptionTests); the URI host is
    -- decorative, so the same description encodes under either scheme.
    fileLinkSimplex = strEncode $ FileDescriptionURI SSSimplex validFd Nothing
    fileLinkOtherHost = strEncode $ FileDescriptionURI (SSAppServer (SrvLoc "example.com" "")) validFd Nothing
    validFd = either error id $ validateFileDescription fileDesc
    fileDesc =
      FileDescription
        { party = SFRecipient,
          size = FileSize (8 * 1024 * 1024),
          digest = FileDigest "abc",
          key = either error id $ strDecode "00n8p1tJq5E-SGnHcYTOrS4A9I07gTA_WFD6MTFFFOY=",
          nonce = either error id $ strDecode "dPSF-wrQpDiK_K6sYv0BDBZ9S4dg-jmu",
          chunkSize = FileSize (8 * 1024 * 1024),
          chunks =
            [ FileChunk
                { chunkNo = 1,
                  chunkSize = FileSize (8 * 1024 * 1024),
                  digest = FileDigest "ghi",
                  replicas =
                    [ FileChunkReplica
                        { server = "xftp://abc=@example1.com",
                          replicaId = ChunkReplicaId (EntityId "abc"),
                          replicaKey = C.APrivateAuthKey C.SEd25519 "MC4CAQAwBQYDK2VwBCIEIDfEfevydXXfKajz3sRkcQ7RPvfWUPoq6pu1TYHV1DEe"
                        }
                    ]
                }
            ],
          redirect = Nothing
        }

-- Generate a server keypair and issue a badge credential via the C FFI,
-- constructing the request from the typed records, then verify the issued
-- credential's BBS signature on the Haskell side.
testBadgeKeygenIssueCApi :: TestParams -> IO ()
testBadgeKeygenIssueCApi _ = do
  g <- C.newRandom
  IssuerKeyPair {publicKey, secretKey} <- ffiResult =<< (peekCString =<< cChatBadgeKeygen)
  mk <- generateMasterKey g
  let req = BadgeIssueReq {badgeKeyIdx = 1, secretKey, request = BadgeRequest {masterKey = mk, badgeInfo = BadgeInfo {badgeType = BTSupporter, badgeExpiry = Nothing, badgeExtra = ""}}}
  cred <- ffiResult =<< (peekCString =<< cChatBadgeIssue =<< newCString (LB.unpack (J.encode req)))
  verifyCredential publicKey cred `shouldReturn` True

-- Decode an FFI `BadgeResult` envelope, returning the result or failing on error.
ffiResult :: FromJSON r => String -> IO r
ffiResult s = case J.eitherDecode (LB.pack s) of
  Right (BadgeResult r) -> pure r
  Right (BadgeError e) -> error $ "badge FFI error: " <> show e
  Left e -> error $ "badge FFI decode failed: " <> e <> " in " <> s

jDecode :: FromJSON a => String -> IO (Maybe a)
jDecode = pure . J.decode . LB.pack

encodedCString :: StrEncoding a => a -> IO CString
encodedCString = newCAString . BS.unpack . strEncode

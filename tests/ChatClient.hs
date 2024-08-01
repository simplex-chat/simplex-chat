{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-ambiguous-fields #-}

module ChatClient where

import Control.Concurrent (forkIOWithUnmask, killThread, threadDelay)
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Exception (bracket, bracket_)
import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader
import Data.ByteArray (ScrubbedBytes)
import Data.Functor (($>))
import Data.List (dropWhileEnd, find)
import Data.Maybe (isNothing)
import qualified Data.Text as T
import Network.Socket
import Simplex.Chat
import Simplex.Chat.Controller (ChatCommand (..), ChatConfig (..), ChatController (..), ChatDatabase (..), ChatLogLevel (..), defaultSimpleNetCfg)
import Simplex.Chat.Core
import Simplex.Chat.Options
import Simplex.Chat.Protocol (currentChatVersion, pqEncryptionCompressionVersion)
import Simplex.Chat.Store
import Simplex.Chat.Store.Profiles
import Simplex.Chat.Terminal
import Simplex.Chat.Terminal.Output (newChatTerminal)
import Simplex.Chat.Types
import Simplex.FileTransfer.Description (kb, mb)
import Simplex.FileTransfer.Server (runXFTPServerBlocking)
import Simplex.FileTransfer.Server.Env (XFTPServerConfig (..), defaultFileExpiration, supportedXFTPhandshakes)
import Simplex.FileTransfer.Transport (supportedFileServerVRange)
import Simplex.Messaging.Agent (disposeAgentClient)
import Simplex.Messaging.Agent.Env.SQLite
import Simplex.Messaging.Agent.Protocol (currentSMPAgentVersion, duplexHandshakeSMPAgentVersion, pqdrSMPAgentVersion, supportedSMPAgentVRange)
import Simplex.Messaging.Agent.RetryInterval
import Simplex.Messaging.Agent.Store.SQLite (MigrationConfirmation (..), closeSQLiteStore)
import qualified Simplex.Messaging.Agent.Store.SQLite.DB as DB
import Simplex.Messaging.Client (ProtocolClientConfig (..))
import Simplex.Messaging.Client.Agent (defaultSMPClientAgentConfig)
import Simplex.Messaging.Crypto.Ratchet (supportedE2EEncryptVRange)
import qualified Simplex.Messaging.Crypto.Ratchet as CR
import Simplex.Messaging.Protocol (srvHostnamesSMPClientVersion)
import Simplex.Messaging.Server (runSMPServerBlocking)
import Simplex.Messaging.Server.Env.STM
import Simplex.Messaging.Transport
import Simplex.Messaging.Transport.Server (TransportServerConfig (..), defaultTransportServerConfig)
import Simplex.Messaging.Version
import Simplex.Messaging.Version.Internal
import System.Directory (createDirectoryIfMissing, removeDirectoryRecursive)
import System.FilePath ((</>))
import qualified System.Terminal as C
import System.Terminal.Internal (VirtualTerminal (..), VirtualTerminalSettings (..), withVirtualTerminal)
import System.Timeout (timeout)
import Test.Hspec (Expectation, HasCallStack, shouldReturn)

testDBPrefix :: FilePath
testDBPrefix = "tests/tmp/test"

serverPort :: ServiceName
serverPort = "7001"

testOpts :: ChatOpts
testOpts =
  ChatOpts
    { coreOptions = testCoreOpts,
      deviceName = Nothing,
      chatCmd = "",
      chatCmdDelay = 3,
      chatCmdLog = CCLNone,
      chatServerPort = Nothing,
      optFilesFolder = Nothing,
      optTempDirectory = Nothing,
      showReactions = True,
      allowInstantFiles = True,
      autoAcceptFileSize = 0,
      muteNotifications = True,
      markRead = True,
      maintenance = False
    }

testCoreOpts :: CoreChatOpts
testCoreOpts =
  CoreChatOpts
    { dbFilePrefix = "./simplex_v1",
      dbKey = "",
      -- dbKey = "this is a pass-phrase to encrypt the database",
      smpServers = ["smp://LcJUMfVhwD8yxjAiSaDzzGF3-kLG4Uh0Fl_ZIjrRwjI=:server_password@localhost:7001"],
      xftpServers = ["xftp://LcJUMfVhwD8yxjAiSaDzzGF3-kLG4Uh0Fl_ZIjrRwjI=:server_password@localhost:7002"],
      simpleNetCfg = defaultSimpleNetCfg,
      logLevel = CLLImportant,
      logConnections = False,
      logServerHosts = False,
      logAgent = Nothing,
      logFile = Nothing,
      tbqSize = 16,
      highlyAvailable = False,
      yesToUpMigrations = False
    }

getTestOpts :: Bool -> ScrubbedBytes -> ChatOpts
getTestOpts maintenance dbKey = testOpts {maintenance, coreOptions = testCoreOpts {dbKey}}

termSettings :: VirtualTerminalSettings
termSettings =
  VirtualTerminalSettings
    { virtualType = "xterm",
      virtualWindowSize = pure C.Size {height = 24, width = 2250},
      virtualEvent = retry,
      virtualInterrupt = retry
    }

data TestCC = TestCC
  { chatController :: ChatController,
    virtualTerminal :: VirtualTerminal,
    chatAsync :: Async (),
    termAsync :: Async (),
    termQ :: TQueue String,
    printOutput :: Bool
  }

aCfg :: AgentConfig
aCfg = (agentConfig defaultChatConfig) {tbqSize = 16}

testAgentCfg :: AgentConfig
testAgentCfg =
  aCfg
    { reconnectInterval = (reconnectInterval aCfg) {initialInterval = 50000}
    }

testAgentCfgSlow :: AgentConfig
testAgentCfgSlow =
  testAgentCfg
    { smpClientVRange = mkVersionRange (Version 1) srvHostnamesSMPClientVersion, -- v2
      smpAgentVRange = mkVersionRange duplexHandshakeSMPAgentVersion pqdrSMPAgentVersion, -- v5
      smpCfg = (smpCfg testAgentCfg) {serverVRange = mkVersionRange batchCmdsSMPVersion sendingProxySMPVersion} -- v8
    }

testCfg :: ChatConfig
testCfg =
  defaultChatConfig
    { agentConfig = testAgentCfg,
      showReceipts = False,
      testView = True,
      tbqSize = 16
    }

testCfgSlow :: ChatConfig
testCfgSlow = testCfg {agentConfig = testAgentCfgSlow}

testAgentCfgVPrev :: AgentConfig
testAgentCfgVPrev =
  testAgentCfg
    { smpClientVRange = prevRange $ smpClientVRange testAgentCfg,
      smpAgentVRange = prevRange supportedSMPAgentVRange,
      e2eEncryptVRange = prevRange supportedE2EEncryptVRange,
      smpCfg = (smpCfg testAgentCfg) {serverVRange = prevRange $ serverVRange $ smpCfg testAgentCfg}
    }

testAgentCfgVNext :: AgentConfig
testAgentCfgVNext =
  testAgentCfg
    { smpClientVRange = nextRange $ smpClientVRange testAgentCfg,
      smpAgentVRange = mkVersionRange duplexHandshakeSMPAgentVersion $ max pqdrSMPAgentVersion currentSMPAgentVersion,
      e2eEncryptVRange = mkVersionRange CR.kdfX3DHE2EEncryptVersion $ max CR.pqRatchetE2EEncryptVersion CR.currentE2EEncryptVersion,
      smpCfg = (smpCfg testAgentCfg) {serverVRange = nextRange $ serverVRange $ smpCfg testAgentCfg}
    }

testAgentCfgV1 :: AgentConfig
testAgentCfgV1 =
  testAgentCfg
    { smpClientVRange = v1Range,
      smpAgentVRange = versionToRange duplexHandshakeSMPAgentVersion,
      e2eEncryptVRange = versionToRange CR.kdfX3DHE2EEncryptVersion,
      smpCfg = (smpCfg testAgentCfg) {serverVRange = versionToRange batchCmdsSMPVersion}
    }

testCfgVPrev :: ChatConfig
testCfgVPrev =
  testCfg
    { chatVRange = prevRange $ chatVRange testCfg,
      agentConfig = testAgentCfgVPrev
    }

testCfgVNext :: ChatConfig
testCfgVNext =
  testCfg
    { chatVRange = mkVersionRange initialChatVersion $ max pqEncryptionCompressionVersion currentChatVersion,
      agentConfig = testAgentCfgVNext
    }

testCfgV1 :: ChatConfig
testCfgV1 =
  testCfg
    { chatVRange = v1Range,
      agentConfig = testAgentCfgV1
    }

prevRange :: VersionRange v -> VersionRange v
prevRange vr = vr {maxVersion = max (minVersion vr) (prevVersion $ maxVersion vr)}

nextRange :: VersionRange v -> VersionRange v
nextRange vr = vr {maxVersion = max (minVersion vr) (nextVersion $ maxVersion vr)}

v1Range :: VersionRange v
v1Range = mkVersionRange (Version 1) (Version 1)

prevVersion :: Version v -> Version v
prevVersion (Version v) = Version (v - 1)

nextVersion :: Version v -> Version v
nextVersion (Version v) = Version (v + 1)

testCfgCreateGroupDirect :: ChatConfig
testCfgCreateGroupDirect =
  mkCfgCreateGroupDirect testCfg

mkCfgCreateGroupDirect :: ChatConfig -> ChatConfig
mkCfgCreateGroupDirect cfg =
  cfg
    { chatVRange = groupCreateDirectVRange,
      agentConfig = testAgentCfgSlow
    }

groupCreateDirectVRange :: VersionRangeChat
groupCreateDirectVRange = mkVersionRange (VersionChat 1) (VersionChat 1)

testCfgGroupLinkViaContact :: ChatConfig
testCfgGroupLinkViaContact =
  mkCfgGroupLinkViaContact testCfg

mkCfgGroupLinkViaContact :: ChatConfig -> ChatConfig
mkCfgGroupLinkViaContact cfg = cfg {chatVRange = groupLinkViaContactVRange}

groupLinkViaContactVRange :: VersionRangeChat
groupLinkViaContactVRange = mkVersionRange (VersionChat 1) (VersionChat 2)

createTestChat :: FilePath -> ChatConfig -> ChatOpts -> String -> Profile -> IO TestCC
createTestChat tmp cfg opts@ChatOpts {coreOptions = CoreChatOpts {dbKey}} dbPrefix profile = do
  Right db@ChatDatabase {chatStore, agentStore} <- createChatDatabase (tmp </> dbPrefix) dbKey False MCError
  withTransaction agentStore (`DB.execute_` "INSERT INTO users (user_id) VALUES (1);")
  Right user <- withTransaction chatStore $ \db' -> runExceptT $ createUserRecord db' (AgentUserId 1) profile True
  startTestChat_ db cfg opts user

startTestChat :: FilePath -> ChatConfig -> ChatOpts -> String -> IO TestCC
startTestChat tmp cfg opts@ChatOpts {coreOptions = CoreChatOpts {dbKey}} dbPrefix = do
  Right db@ChatDatabase {chatStore} <- createChatDatabase (tmp </> dbPrefix) dbKey False MCError
  Just user <- find activeUser <$> withTransaction chatStore getUsers
  startTestChat_ db cfg opts user

startTestChat_ :: ChatDatabase -> ChatConfig -> ChatOpts -> User -> IO TestCC
startTestChat_ db cfg opts user = do
  t <- withVirtualTerminal termSettings pure
  ct <- newChatTerminal t opts
  cc <- newChatController db (Just user) cfg opts False
  void $ execChatCommand' (SetTempFolder "tests/tmp/tmp") `runReaderT` cc
  chatAsync <- async . runSimplexChat opts user cc $ \_u cc' -> runChatTerminal ct cc' opts
  atomically . unless (maintenance opts) $ readTVar (agentAsync cc) >>= \a -> when (isNothing a) retry
  termQ <- newTQueueIO
  termAsync <- async $ readTerminalOutput t termQ
  pure TestCC {chatController = cc, virtualTerminal = t, chatAsync, termAsync, termQ, printOutput = False}

stopTestChat :: TestCC -> IO ()
stopTestChat TestCC {chatController = cc@ChatController {smpAgent, chatStore}, chatAsync, termAsync} = do
  stopChatController cc
  uninterruptibleCancel termAsync
  uninterruptibleCancel chatAsync
  liftIO $ disposeAgentClient smpAgent
  closeSQLiteStore chatStore
  threadDelay 200000

withNewTestChat :: HasCallStack => FilePath -> String -> Profile -> (HasCallStack => TestCC -> IO a) -> IO a
withNewTestChat tmp = withNewTestChatCfgOpts tmp testCfg testOpts

withNewTestChatV1 :: HasCallStack => FilePath -> String -> Profile -> (HasCallStack => TestCC -> IO a) -> IO a
withNewTestChatV1 tmp = withNewTestChatCfg tmp testCfgV1

withNewTestChatCfg :: HasCallStack => FilePath -> ChatConfig -> String -> Profile -> (HasCallStack => TestCC -> IO a) -> IO a
withNewTestChatCfg tmp cfg = withNewTestChatCfgOpts tmp cfg testOpts

withNewTestChatOpts :: HasCallStack => FilePath -> ChatOpts -> String -> Profile -> (HasCallStack => TestCC -> IO a) -> IO a
withNewTestChatOpts tmp = withNewTestChatCfgOpts tmp testCfg

withNewTestChatCfgOpts :: HasCallStack => FilePath -> ChatConfig -> ChatOpts -> String -> Profile -> (HasCallStack => TestCC -> IO a) -> IO a
withNewTestChatCfgOpts tmp cfg opts dbPrefix profile runTest =
  bracket
    (createTestChat tmp cfg opts dbPrefix profile)
    stopTestChat
    (\cc -> runTest cc >>= ((cc <// 100000) $>))

withTestChatV1 :: HasCallStack => FilePath -> String -> (HasCallStack => TestCC -> IO a) -> IO a
withTestChatV1 tmp = withTestChatCfg tmp testCfgV1

withTestChat :: HasCallStack => FilePath -> String -> (HasCallStack => TestCC -> IO a) -> IO a
withTestChat tmp = withTestChatCfgOpts tmp testCfg testOpts

withTestChatCfg :: HasCallStack => FilePath -> ChatConfig -> String -> (HasCallStack => TestCC -> IO a) -> IO a
withTestChatCfg tmp cfg = withTestChatCfgOpts tmp cfg testOpts

withTestChatOpts :: HasCallStack => FilePath -> ChatOpts -> String -> (HasCallStack => TestCC -> IO a) -> IO a
withTestChatOpts tmp = withTestChatCfgOpts tmp testCfg

withTestChatCfgOpts :: HasCallStack => FilePath -> ChatConfig -> ChatOpts -> String -> (HasCallStack => TestCC -> IO a) -> IO a
withTestChatCfgOpts tmp cfg opts dbPrefix = bracket (startTestChat tmp cfg opts dbPrefix) (\cc -> cc <// 100000 >> stopTestChat cc)

-- enable output for specific chat controller, use like this:
-- withNewTestChat tmp "alice" aliceProfile $ \a -> withTestOutput a $ \alice -> do ...
withTestOutput :: HasCallStack => TestCC -> (HasCallStack => TestCC -> IO a) -> IO a
withTestOutput cc runTest = runTest cc {printOutput = True}

readTerminalOutput :: VirtualTerminal -> TQueue String -> IO ()
readTerminalOutput t termQ = do
  let w = virtualWindow t
  winVar <- atomically $ newTVar . init =<< readTVar w
  forever . atomically $ do
    win <- readTVar winVar
    win' <- init <$> readTVar w
    if win' == win
      then retry
      else do
        let diff = getDiff win' win
        forM_ diff $ writeTQueue termQ
        writeTVar winVar win'
  where
    getDiff :: [String] -> [String] -> [String]
    getDiff win win' = getDiff_ 1 (length win) win win'
    getDiff_ :: Int -> Int -> [String] -> [String] -> [String]
    getDiff_ n len win' win =
      let diff = drop (len - n) win'
       in if drop n win <> diff == win'
            then map (dropWhileEnd (== ' ')) diff
            else getDiff_ (n + 1) len win' win

withTmpFiles :: IO () -> IO ()
withTmpFiles =
  bracket_
    (createDirectoryIfMissing False "tests/tmp")
    (removeDirectoryRecursive "tests/tmp")

testChatN :: HasCallStack => ChatConfig -> ChatOpts -> [Profile] -> (HasCallStack => [TestCC] -> IO ()) -> FilePath -> IO ()
testChatN cfg opts ps test tmp = do
  tcs <- getTestCCs (zip ps [1 ..]) []
  test tcs
  concurrentlyN_ $ map (<// 100000) tcs
  concurrentlyN_ $ map stopTestChat tcs
  where
    getTestCCs :: [(Profile, Int)] -> [TestCC] -> IO [TestCC]
    getTestCCs [] tcs = pure tcs
    getTestCCs ((p, db) : envs') tcs = (:) <$> createTestChat tmp cfg opts (show db) p <*> getTestCCs envs' tcs

(<//) :: HasCallStack => TestCC -> Int -> Expectation
(<//) cc t = timeout t (getTermLine cc) `shouldReturn` Nothing

getTermLine :: HasCallStack => TestCC -> IO String
getTermLine cc =
  5000000 `timeout` atomically (readTQueue $ termQ cc) >>= \case
    Just s -> do
      -- remove condition to always echo virtual terminal
      -- when True $ do
      when (printOutput cc) $ do
        name <- userName cc
        putStrLn $ name <> ": " <> s
      pure s
    _ -> error "no output for 5 seconds"

userName :: TestCC -> IO [Char]
userName (TestCC ChatController {currentUser} _ _ _ _ _) =
  maybe "no current user" (\User {localDisplayName} -> T.unpack localDisplayName) <$> readTVarIO currentUser

testChat2 :: HasCallStack => Profile -> Profile -> (HasCallStack => TestCC -> TestCC -> IO ()) -> FilePath -> IO ()
testChat2 = testChatCfgOpts2 testCfg testOpts

testChatCfg2 :: HasCallStack => ChatConfig -> Profile -> Profile -> (HasCallStack => TestCC -> TestCC -> IO ()) -> FilePath -> IO ()
testChatCfg2 cfg = testChatCfgOpts2 cfg testOpts

testChatOpts2 :: HasCallStack => ChatOpts -> Profile -> Profile -> (HasCallStack => TestCC -> TestCC -> IO ()) -> FilePath -> IO ()
testChatOpts2 = testChatCfgOpts2 testCfg

testChatCfgOpts2 :: HasCallStack => ChatConfig -> ChatOpts -> Profile -> Profile -> (HasCallStack => TestCC -> TestCC -> IO ()) -> FilePath -> IO ()
testChatCfgOpts2 cfg opts p1 p2 test = testChatN cfg opts [p1, p2] test_
  where
    test_ :: HasCallStack => [TestCC] -> IO ()
    test_ [tc1, tc2] = test tc1 tc2
    test_ _ = error "expected 2 chat clients"

testChat3 :: HasCallStack => Profile -> Profile -> Profile -> (HasCallStack => TestCC -> TestCC -> TestCC -> IO ()) -> FilePath -> IO ()
testChat3 = testChatCfgOpts3 testCfg testOpts

testChatCfg3 :: HasCallStack => ChatConfig -> Profile -> Profile -> Profile -> (HasCallStack => TestCC -> TestCC -> TestCC -> IO ()) -> FilePath -> IO ()
testChatCfg3 cfg = testChatCfgOpts3 cfg testOpts

testChatCfgOpts3 :: HasCallStack => ChatConfig -> ChatOpts -> Profile -> Profile -> Profile -> (HasCallStack => TestCC -> TestCC -> TestCC -> IO ()) -> FilePath -> IO ()
testChatCfgOpts3 cfg opts p1 p2 p3 test = testChatN cfg opts [p1, p2, p3] test_
  where
    test_ :: HasCallStack => [TestCC] -> IO ()
    test_ [tc1, tc2, tc3] = test tc1 tc2 tc3
    test_ _ = error "expected 3 chat clients"

testChat4 :: HasCallStack => Profile -> Profile -> Profile -> Profile -> (HasCallStack => TestCC -> TestCC -> TestCC -> TestCC -> IO ()) -> FilePath -> IO ()
testChat4 = testChatCfg4 testCfg

testChatCfg4 :: HasCallStack => ChatConfig -> Profile -> Profile -> Profile -> Profile -> (HasCallStack => TestCC -> TestCC -> TestCC -> TestCC -> IO ()) -> FilePath -> IO ()
testChatCfg4 cfg p1 p2 p3 p4 test = testChatN cfg testOpts [p1, p2, p3, p4] test_
  where
    test_ :: HasCallStack => [TestCC] -> IO ()
    test_ [tc1, tc2, tc3, tc4] = test tc1 tc2 tc3 tc4
    test_ _ = error "expected 4 chat clients"

concurrentlyN_ :: [IO a] -> IO ()
concurrentlyN_ = mapConcurrently_ id

smpServerCfg :: ServerConfig
smpServerCfg =
  ServerConfig
    { transports = [(serverPort, transport @TLS)],
      tbqSize = 1,
      -- serverTbqSize = 1,
      msgQueueQuota = 16,
      queueIdBytes = 12,
      msgIdBytes = 6,
      storeLogFile = Nothing,
      storeMsgsFile = Nothing,
      allowNewQueues = True,
      -- server password is disabled as otherwise v1 tests fail
      newQueueBasicAuth = Nothing, -- Just "server_password",
      controlPortUserAuth = Nothing,
      controlPortAdminAuth = Nothing,
      messageExpiration = Just defaultMessageExpiration,
      inactiveClientExpiration = Just defaultInactiveClientExpiration,
      caCertificateFile = "tests/fixtures/tls/ca.crt",
      privateKeyFile = "tests/fixtures/tls/server.key",
      certificateFile = "tests/fixtures/tls/server.crt",
      logStatsInterval = Nothing,
      logStatsStartTime = 0,
      serverStatsLogFile = "tests/smp-server-stats.daily.log",
      serverStatsBackupFile = Nothing,
      smpServerVRange = supportedServerSMPRelayVRange,
      transportConfig = defaultTransportServerConfig {alpn = Just supportedSMPHandshakes},
      smpHandshakeTimeout = 1000000,
      controlPort = Nothing,
      smpAgentCfg = defaultSMPClientAgentConfig,
      allowSMPProxy = True,
      serverClientConcurrency = 16,
      information = Nothing
    }

withSmpServer :: IO () -> IO ()
withSmpServer = withSmpServer' smpServerCfg

withSmpServer' :: ServerConfig -> IO () -> IO ()
withSmpServer' cfg = serverBracket (`runSMPServerBlocking` cfg)

xftpTestPort :: ServiceName
xftpTestPort = "7002"

xftpServerFiles :: FilePath
xftpServerFiles = "tests/tmp/xftp-server-files"

xftpServerConfig :: XFTPServerConfig
xftpServerConfig =
  XFTPServerConfig
    { xftpPort = xftpTestPort,
      fileIdSize = 16,
      storeLogFile = Just "tests/tmp/xftp-server-store.log",
      filesPath = xftpServerFiles,
      fileSizeQuota = Nothing,
      allowedChunkSizes = [kb 64, kb 128, kb 256, mb 1, mb 4],
      allowNewFiles = True,
      newFileBasicAuth = Nothing,
      controlPortUserAuth = Nothing,
      controlPortAdminAuth = Nothing,
      fileExpiration = Just defaultFileExpiration,
      fileTimeout = 10000000,
      inactiveClientExpiration = Just defaultInactiveClientExpiration,
      caCertificateFile = "tests/fixtures/tls/ca.crt",
      privateKeyFile = "tests/fixtures/tls/server.key",
      certificateFile = "tests/fixtures/tls/server.crt",
      xftpServerVRange = supportedFileServerVRange,
      logStatsInterval = Nothing,
      logStatsStartTime = 0,
      serverStatsLogFile = "tests/tmp/xftp-server-stats.daily.log",
      serverStatsBackupFile = Nothing,
      controlPort = Nothing,
      transportConfig = defaultTransportServerConfig {alpn = Just supportedXFTPhandshakes},
      responseDelay = 0
    }

withXFTPServer :: IO () -> IO ()
withXFTPServer = withXFTPServer' xftpServerConfig

withXFTPServer' :: XFTPServerConfig -> IO () -> IO ()
withXFTPServer' cfg =
  serverBracket
    ( \started -> do
        createDirectoryIfMissing False xftpServerFiles
        runXFTPServerBlocking started cfg
    )

serverBracket :: (TMVar Bool -> IO ()) -> IO () -> IO ()
serverBracket server f = do
  started <- newEmptyTMVarIO
  bracket
    (forkIOWithUnmask ($ server started))
    (\t -> killThread t >> waitFor started "stop")
    (\_ -> waitFor started "start" >> f)
  where
    waitFor started s =
      5000000 `timeout` atomically (takeTMVar started) >>= \case
        Nothing -> error $ "server did not " <> s
        _ -> pure ()

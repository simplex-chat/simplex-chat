{-# LANGUAGE CPP #-}
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

import ChatTests.DBUtils
import Control.Concurrent (forkIOWithUnmask, killThread, threadDelay)
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Exception (bracket, bracket_)
import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader
import Data.Functor (($>))
import Data.List (dropWhileEnd, find)
import Data.Maybe (isNothing)
import qualified Data.Text as T
import Network.Socket
import Simplex.Chat
import Simplex.Chat.Controller (ChatCommand (..), ChatConfig (..), ChatController (..), ChatDatabase (..), ChatLogLevel (..), defaultSimpleNetCfg)
import Simplex.Chat.Core
import Simplex.Chat.Library.Commands
import Simplex.Chat.Options
import Simplex.Chat.Options.DB
import Simplex.Chat.Protocol (currentChatVersion, pqEncryptionCompressionVersion)
import Simplex.Chat.Store
import Simplex.Chat.Store.Profiles
import Simplex.Chat.Terminal
import Simplex.Chat.Terminal.Output (newChatTerminal)
import Simplex.Chat.Types
import Simplex.FileTransfer.Description (kb, mb)
import Simplex.FileTransfer.Server (runXFTPServerBlocking)
import Simplex.FileTransfer.Server.Env (XFTPServerConfig (..), defaultFileExpiration)
import Simplex.FileTransfer.Transport (supportedFileServerVRange)
import Simplex.Messaging.Agent (disposeAgentClient)
import Simplex.Messaging.Agent.Env.SQLite
import Simplex.Messaging.Agent.Protocol (currentSMPAgentVersion, duplexHandshakeSMPAgentVersion, pqdrSMPAgentVersion, supportedSMPAgentVRange)
import Simplex.Messaging.Agent.RetryInterval
import Simplex.Messaging.Agent.Store.Interface (closeDBStore)
import Simplex.Messaging.Agent.Store.Shared (MigrationConfirmation (..), MigrationError)
import qualified Simplex.Messaging.Agent.Store.DB as DB
import Simplex.Messaging.Client (ProtocolClientConfig (..))
import Simplex.Messaging.Client.Agent (defaultSMPClientAgentConfig)
import Simplex.Messaging.Crypto.Ratchet (supportedE2EEncryptVRange)
import qualified Simplex.Messaging.Crypto.Ratchet as CR
import Simplex.Messaging.Protocol (srvHostnamesSMPClientVersion)
import Simplex.Messaging.Server (runSMPServerBlocking)
import Simplex.Messaging.Server.Env.STM (AServerStoreCfg (..), ServerConfig (..), ServerStoreCfg (..), StartOptions (..), StorePaths (..), defaultMessageExpiration, defaultIdleQueueInterval, defaultNtfExpiration, defaultInactiveClientExpiration)
import Simplex.Messaging.Server.MsgStore.Types (SQSType (..), SMSType (..))
import Simplex.Messaging.Transport
import Simplex.Messaging.Transport.Server (ServerCredentials (..), defaultTransportServerConfig)
import Simplex.Messaging.Version
import Simplex.Messaging.Version.Internal
import System.Directory (createDirectoryIfMissing, removeDirectoryRecursive)
import qualified System.Terminal as C
import System.Terminal.Internal (VirtualTerminal (..), VirtualTerminalSettings (..), withVirtualTerminal)
import System.Timeout (timeout)
import Test.Hspec (Expectation, HasCallStack, shouldReturn)
#if defined(dbPostgres)
import Database.PostgreSQL.Simple (ConnectInfo (..), defaultConnectInfo)
#else
import Data.ByteArray (ScrubbedBytes)
import qualified Data.Map.Strict as M
import Simplex.Messaging.Agent.Client (agentClientStore)
import Simplex.Messaging.Agent.Store.Common (withConnection)
import System.FilePath ((</>))
#endif

#if defined(dbPostgres)
testDBConnstr :: String
testDBConnstr = "postgresql://test_chat_user@/test_chat_db"

testDBConnectInfo :: ConnectInfo
testDBConnectInfo =
  defaultConnectInfo {
    connectUser = "test_chat_user",
    connectDatabase = "test_chat_db"
  }
#endif

serverPort :: ServiceName
serverPort = "7001"

testOpts :: ChatOpts
testOpts =
  ChatOpts
    { coreOptions = testCoreOpts,
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
    {
      dbOptions = ChatDbOpts
#if defined(dbPostgres)
        { dbConnstr = testDBConnstr,
          -- dbSchemaPrefix is not used in tests (except bot tests where it's redefined),
          -- instead different schema prefix is passed per client so that single test database is used
          dbSchemaPrefix = "",
          dbPoolSize = 3,
          dbCreateSchema = True
#else
        { dbFilePrefix = "./simplex_v1", -- dbFilePrefix is not used in tests (except bot tests where it's redefined)
          dbKey = "", -- dbKey = "this is a pass-phrase to encrypt the database",
          trackQueries = DB.TQAll,
          vacuumOnMigration = True
#endif
        },
      smpServers = ["smp://LcJUMfVhwD8yxjAiSaDzzGF3-kLG4Uh0Fl_ZIjrRwjI=:server_password@localhost:7001"],
      xftpServers = ["xftp://LcJUMfVhwD8yxjAiSaDzzGF3-kLG4Uh0Fl_ZIjrRwjI=:server_password@localhost:7002"],
      simpleNetCfg = defaultSimpleNetCfg,
      logLevel = CLLImportant,
      logConnections = False,
      logServerHosts = False,
      logAgent = Nothing,
      logFile = Nothing,
      tbqSize = 16,
      deviceName = Nothing,
      highlyAvailable = False,
      yesToUpMigrations = False
    }

#if !defined(dbPostgres)
getTestOpts :: Bool -> ScrubbedBytes -> ChatOpts
getTestOpts maintenance dbKey = testOpts {maintenance, coreOptions = testCoreOpts {dbOptions = (dbOptions testCoreOpts) {dbKey}}}
#endif

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
      smpCfg = (smpCfg testAgentCfg) {serverVRange = mkVersionRange minClientSMPRelayVersion sendingProxySMPVersion} -- v8
    }

testCfg :: ChatConfig
testCfg =
  defaultChatConfig
    { agentConfig = testAgentCfg,
      showReceipts = False,
      shortLinkPresetServers = ["smp://LcJUMfVhwD8yxjAiSaDzzGF3-kLG4Uh0Fl_ZIjrRwjI=@localhost:7001"],
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
      smpCfg = (smpCfg testAgentCfg) {serverVRange = versionToRange minClientSMPRelayVersion}
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

createTestChat :: TestParams -> ChatConfig -> ChatOpts -> String -> Profile -> IO TestCC
createTestChat ps cfg opts@ChatOpts {coreOptions} dbPrefix profile = do
  Right db@ChatDatabase {chatStore, agentStore} <- createDatabase ps coreOptions dbPrefix
  insertUser agentStore
  Right user <- withTransaction chatStore $ \db' -> runExceptT $ createUserRecord db' (AgentUserId 1) profile True
  startTestChat_ ps db cfg opts user

startTestChat :: TestParams -> ChatConfig -> ChatOpts -> String -> IO TestCC
startTestChat ps cfg opts@ChatOpts {coreOptions} dbPrefix = do
  Right db@ChatDatabase {chatStore} <- createDatabase ps coreOptions dbPrefix
  Just user <- find activeUser <$> withTransaction chatStore getUsers
  startTestChat_ ps db cfg opts user

createDatabase :: TestParams -> CoreChatOpts -> String -> IO (Either MigrationError ChatDatabase)
#if defined(dbPostgres)
createDatabase _params CoreChatOpts {dbOptions} dbPrefix = do
  createChatDatabase dbOptions {dbSchemaPrefix = "client_" <> dbPrefix} MCError

insertUser :: DBStore -> IO ()
insertUser st = withTransaction st (`DB.execute_` "INSERT INTO users DEFAULT VALUES")
#else
createDatabase TestParams {tmpPath} CoreChatOpts {dbOptions} dbPrefix = do
  createChatDatabase dbOptions {dbFilePrefix = tmpPath </> dbPrefix} MCError

insertUser :: DBStore -> IO ()
insertUser st = withTransaction st (`DB.execute_` "INSERT INTO users (user_id) VALUES (1)")
#endif

startTestChat_ :: TestParams -> ChatDatabase -> ChatConfig -> ChatOpts -> User -> IO TestCC
startTestChat_ TestParams {printOutput} db cfg opts@ChatOpts {maintenance} user = do
  t <- withVirtualTerminal termSettings pure
  ct <- newChatTerminal t opts
  cc <- newChatController db (Just user) cfg opts False
  void $ execChatCommand' (SetTempFolder "tests/tmp/tmp") `runReaderT` cc
  chatAsync <- async $ runSimplexChat opts user cc $ \_u cc' -> runChatTerminal ct cc' opts
  unless maintenance $ atomically $ readTVar (agentAsync cc) >>= \a -> when (isNothing a) retry
  termQ <- newTQueueIO
  termAsync <- async $ readTerminalOutput t termQ
  pure TestCC {chatController = cc, virtualTerminal = t, chatAsync, termAsync, termQ, printOutput}

stopTestChat :: TestParams -> TestCC -> IO ()
stopTestChat ps TestCC {chatController = cc@ChatController {smpAgent, chatStore}, chatAsync, termAsync} = do
  stopChatController cc
  uninterruptibleCancel termAsync
  uninterruptibleCancel chatAsync
  liftIO $ disposeAgentClient smpAgent
#if !defined(dbPostgres)
  chatStats <- withConnection chatStore $ readTVarIO . DB.slow
  atomically $ modifyTVar' (chatQueryStats ps) $ M.unionWith combineStats chatStats
  agentStats <- withConnection (agentClientStore smpAgent) $ readTVarIO . DB.slow
  atomically $ modifyTVar' (agentQueryStats ps) $ M.unionWith combineStats agentStats
#endif
  closeDBStore chatStore
  threadDelay 200000
#if !defined(dbPostgres)
  where
    combineStats
      DB.SlowQueryStats {count, timeMax, timeAvg, errs}
      DB.SlowQueryStats {count = count', timeMax = timeMax', timeAvg = timeAvg', errs = errs'} =
        DB.SlowQueryStats
          { count = count + count',
            timeMax = max timeMax timeMax',
            timeAvg = (timeAvg * count + timeAvg' * count') `div` (count + count'),
            errs = M.unionWith (+) errs errs'
          }
#endif

withNewTestChat :: HasCallStack => TestParams -> String -> Profile -> (HasCallStack => TestCC -> IO a) -> IO a
withNewTestChat ps = withNewTestChatCfgOpts ps testCfg testOpts

withNewTestChatV1 :: HasCallStack => TestParams -> String -> Profile -> (HasCallStack => TestCC -> IO a) -> IO a
withNewTestChatV1 ps = withNewTestChatCfg ps testCfgV1

withNewTestChatCfg :: HasCallStack => TestParams -> ChatConfig -> String -> Profile -> (HasCallStack => TestCC -> IO a) -> IO a
withNewTestChatCfg ps cfg = withNewTestChatCfgOpts ps cfg testOpts

withNewTestChatOpts :: HasCallStack => TestParams -> ChatOpts -> String -> Profile -> (HasCallStack => TestCC -> IO a) -> IO a
withNewTestChatOpts ps = withNewTestChatCfgOpts ps testCfg

withNewTestChatCfgOpts :: HasCallStack => TestParams -> ChatConfig -> ChatOpts -> String -> Profile -> (HasCallStack => TestCC -> IO a) -> IO a
withNewTestChatCfgOpts ps cfg opts dbPrefix profile runTest =
  bracket
    (createTestChat ps cfg opts dbPrefix profile)
    (stopTestChat ps)
    (\cc -> runTest cc >>= ((cc <// 100000) $>))

withTestChatV1 :: HasCallStack => TestParams -> String -> (HasCallStack => TestCC -> IO a) -> IO a
withTestChatV1 ps = withTestChatCfg ps testCfgV1

withTestChat :: HasCallStack => TestParams -> String -> (HasCallStack => TestCC -> IO a) -> IO a
withTestChat ps = withTestChatCfgOpts ps testCfg testOpts

withTestChatCfg :: HasCallStack => TestParams -> ChatConfig -> String -> (HasCallStack => TestCC -> IO a) -> IO a
withTestChatCfg ps cfg = withTestChatCfgOpts ps cfg testOpts

withTestChatOpts :: HasCallStack => TestParams -> ChatOpts -> String -> (HasCallStack => TestCC -> IO a) -> IO a
withTestChatOpts ps = withTestChatCfgOpts ps testCfg

withTestChatCfgOpts :: HasCallStack => TestParams -> ChatConfig -> ChatOpts -> String -> (HasCallStack => TestCC -> IO a) -> IO a
withTestChatCfgOpts ps cfg opts dbPrefix = bracket (startTestChat ps cfg opts dbPrefix) (\cc -> cc <// 100000 >> stopTestChat ps cc)

-- enable output for specific test.
-- usage: withTestOutput $ testChat2 aliceProfile bobProfile $ \alice bob -> do ...
withTestOutput :: HasCallStack => (HasCallStack => TestParams -> IO ()) -> TestParams -> IO ()
withTestOutput test ps = test ps {printOutput = True}

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

testChatN :: HasCallStack => ChatConfig -> ChatOpts -> [Profile] -> (HasCallStack => [TestCC] -> IO ()) -> TestParams -> IO ()
testChatN cfg opts ps test params =
  bracket (getTestCCs $ zip ps [1 ..]) endTests test
  where
    getTestCCs :: [(Profile, Int)] -> IO [TestCC]
    getTestCCs [] = pure []
    getTestCCs ((p, db) : envs') = (:) <$> createTestChat params cfg opts (show db) p <*> getTestCCs envs'
    endTests tcs = do
      mapConcurrently_ (<// 100000) tcs
      mapConcurrently_ (stopTestChat params) tcs

(<//) :: HasCallStack => TestCC -> Int -> Expectation
(<//) cc t = timeout t (getTermLine cc) `shouldReturn` Nothing

getTermLine :: HasCallStack => TestCC -> IO String
getTermLine cc@TestCC {printOutput} =
  5000000 `timeout` atomically (readTQueue $ termQ cc) >>= \case
    Just s -> do
      -- remove condition to always echo virtual terminal
      -- when True $ do
      when printOutput $ do
        name <- userName cc
        putStrLn $ name <> ": " <> s
      pure s
    _ -> error "no output for 5 seconds"

userName :: TestCC -> IO [Char]
userName (TestCC ChatController {currentUser} _ _ _ _ _) =
  maybe "no current user" (\User {localDisplayName} -> T.unpack localDisplayName) <$> readTVarIO currentUser

testChat :: HasCallStack => Profile -> (HasCallStack => TestCC -> IO ()) -> TestParams -> IO ()
testChat = testChatCfgOpts testCfg testOpts

testChatCfgOpts :: HasCallStack => ChatConfig -> ChatOpts -> Profile -> (HasCallStack => TestCC -> IO ()) -> TestParams -> IO ()
testChatCfgOpts cfg opts p test = testChatN cfg opts [p] test_
  where
    test_ :: HasCallStack => [TestCC] -> IO ()
    test_ [tc] = test tc
    test_ _ = error "expected 1 chat client"

testChat2 :: HasCallStack => Profile -> Profile -> (HasCallStack => TestCC -> TestCC -> IO ()) -> TestParams -> IO ()
testChat2 = testChatCfgOpts2 testCfg testOpts

testChatCfg2 :: HasCallStack => ChatConfig -> Profile -> Profile -> (HasCallStack => TestCC -> TestCC -> IO ()) -> TestParams -> IO ()
testChatCfg2 cfg = testChatCfgOpts2 cfg testOpts

testChatOpts2 :: HasCallStack => ChatOpts -> Profile -> Profile -> (HasCallStack => TestCC -> TestCC -> IO ()) -> TestParams -> IO ()
testChatOpts2 = testChatCfgOpts2 testCfg

testChatCfgOpts2 :: HasCallStack => ChatConfig -> ChatOpts -> Profile -> Profile -> (HasCallStack => TestCC -> TestCC -> IO ()) -> TestParams -> IO ()
testChatCfgOpts2 cfg opts p1 p2 test = testChatN cfg opts [p1, p2] test_
  where
    test_ :: HasCallStack => [TestCC] -> IO ()
    test_ [tc1, tc2] = test tc1 tc2
    test_ _ = error "expected 2 chat clients"

testChat3 :: HasCallStack => Profile -> Profile -> Profile -> (HasCallStack => TestCC -> TestCC -> TestCC -> IO ()) -> TestParams -> IO ()
testChat3 = testChatCfgOpts3 testCfg testOpts

testChatCfg3 :: HasCallStack => ChatConfig -> Profile -> Profile -> Profile -> (HasCallStack => TestCC -> TestCC -> TestCC -> IO ()) -> TestParams -> IO ()
testChatCfg3 cfg = testChatCfgOpts3 cfg testOpts

testChatCfgOpts3 :: HasCallStack => ChatConfig -> ChatOpts -> Profile -> Profile -> Profile -> (HasCallStack => TestCC -> TestCC -> TestCC -> IO ()) -> TestParams -> IO ()
testChatCfgOpts3 cfg opts p1 p2 p3 test = testChatN cfg opts [p1, p2, p3] test_
  where
    test_ :: HasCallStack => [TestCC] -> IO ()
    test_ [tc1, tc2, tc3] = test tc1 tc2 tc3
    test_ _ = error "expected 3 chat clients"

testChat4 :: HasCallStack => Profile -> Profile -> Profile -> Profile -> (HasCallStack => TestCC -> TestCC -> TestCC -> TestCC -> IO ()) -> TestParams -> IO ()
testChat4 = testChatCfg4 testCfg

testChatCfg4 :: HasCallStack => ChatConfig -> Profile -> Profile -> Profile -> Profile -> (HasCallStack => TestCC -> TestCC -> TestCC -> TestCC -> IO ()) -> TestParams -> IO ()
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
    { transports = [(serverPort, transport @TLS, False)],
      tbqSize = 1,
      msgQueueQuota = 16,
      maxJournalMsgCount = 24,
      maxJournalStateLines = 4,
      queueIdBytes = 24,
      msgIdBytes = 6,
      serverStoreCfg = ASSCfg SQSMemory SMSMemory $ SSCMemory Nothing,
      storeNtfsFile = Nothing,
      allowNewQueues = True,
      -- server password is disabled as otherwise v1 tests fail
      newQueueBasicAuth = Nothing, -- Just "server_password",
      controlPortUserAuth = Nothing,
      controlPortAdminAuth = Nothing,
      messageExpiration = Just defaultMessageExpiration,
      expireMessagesOnStart = False,
      idleQueueInterval = defaultIdleQueueInterval,
      notificationExpiration = defaultNtfExpiration,
      inactiveClientExpiration = Just defaultInactiveClientExpiration,
      smpCredentials =
        ServerCredentials
          { caCertificateFile = Just "tests/fixtures/tls/ca.crt",
            privateKeyFile = "tests/fixtures/tls/server.key",
            certificateFile = "tests/fixtures/tls/server.crt"
          },
      httpCredentials = Nothing,
      logStatsInterval = Nothing,
      logStatsStartTime = 0,
      serverStatsLogFile = "tests/smp-server-stats.daily.log",
      serverStatsBackupFile = Nothing,
      prometheusInterval = Nothing,
      prometheusMetricsFile = "tests/smp-server-metrics.txt",
      pendingENDInterval = 500000,
      ntfDeliveryInterval = 200000,
      smpServerVRange = supportedServerSMPRelayVRange,
      transportConfig = defaultTransportServerConfig,
      smpHandshakeTimeout = 1000000,
      controlPort = Nothing,
      smpAgentCfg = defaultSMPClientAgentConfig,
      allowSMPProxy = True,
      serverClientConcurrency = 16,
      information = Nothing,
      startOptions = StartOptions {maintenance = False, compactLog = False, skipWarnings = False, confirmMigrations = MCYesUp}
    }

persistentServerStoreCfg :: FilePath -> AServerStoreCfg
persistentServerStoreCfg tmp = ASSCfg SQSMemory SMSMemory $ SSCMemory $ Just StorePaths {storeLogFile = tmp <> "/smp-server-store.log", storeMsgsFile = Just $ tmp <> "/smp-server-messages.log"}

withSmpServer :: IO () -> IO ()
withSmpServer = withSmpServer' smpServerCfg

withSmpServer' :: ServerConfig -> IO a -> IO a
withSmpServer' cfg = serverBracket (\started -> runSMPServerBlocking started cfg Nothing)

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
      xftpCredentials =
        ServerCredentials
          { caCertificateFile = Just "tests/fixtures/tls/ca.crt",
            privateKeyFile = "tests/fixtures/tls/server.key",
            certificateFile = "tests/fixtures/tls/server.crt"
          },
      xftpServerVRange = supportedFileServerVRange,
      logStatsInterval = Nothing,
      logStatsStartTime = 0,
      serverStatsLogFile = "tests/tmp/xftp-server-stats.daily.log",
      serverStatsBackupFile = Nothing,
      controlPort = Nothing,
      transportConfig = defaultTransportServerConfig,
      responseDelay = 0
    }

withXFTPServer :: IO () -> IO ()
withXFTPServer = withXFTPServer' xftpServerConfig

withXFTPServer' :: XFTPServerConfig -> IO () -> IO ()
withXFTPServer' cfg =
  serverBracket
    ( \started -> do
        createDirectoryIfMissing False xftpServerFiles
        runXFTPServerBlocking started cfg Nothing
    )

serverBracket :: (TMVar Bool -> IO ()) -> IO a -> IO a
serverBracket server f = do
  started <- newEmptyTMVarIO
  bracket
    (forkIOWithUnmask ($ server started))
    (\t -> killThread t >> waitFor started "stop" >> threadDelay 100000)
    (\_ -> waitFor started "start" >> f)
  where
    waitFor started s =
      5000000 `timeout` atomically (takeTMVar started) >>= \case
        Nothing -> error $ "server did not " <> s
        _ -> pure ()

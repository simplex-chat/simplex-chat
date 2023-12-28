{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-ambiguous-fields #-}

module ChatClient where

import Control.Concurrent (forkIOWithUnmask, killThread, threadDelay)
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Exception (bracket, bracket_)
import Control.Monad
import Control.Monad.Except
import Data.ByteArray (ScrubbedBytes)
import Data.Functor (($>))
import Data.List (dropWhileEnd, find)
import Data.Maybe (isNothing)
import qualified Data.Text as T
import Network.Socket
import Simplex.Chat
import Simplex.Chat.Controller (ChatConfig (..), ChatController (..), ChatDatabase (..), ChatLogLevel (..))
import Simplex.Chat.Core
import Simplex.Chat.Options
import Simplex.Chat.Store
import Simplex.Chat.Store.Profiles
import Simplex.Chat.Terminal
import Simplex.Chat.Terminal.Output (newChatTerminal)
import Simplex.Chat.Types (AgentUserId (..), Profile, User (..))
import Simplex.FileTransfer.Description (kb, mb)
import Simplex.FileTransfer.Server (runXFTPServerBlocking)
import Simplex.FileTransfer.Server.Env (XFTPServerConfig (..), defaultFileExpiration)
import Simplex.Messaging.Agent.Env.SQLite
import Simplex.Messaging.Agent.RetryInterval
import Simplex.Messaging.Agent.Store.SQLite (MigrationConfirmation (..))
import Simplex.Messaging.Client (ProtocolClientConfig (..), defaultNetworkConfig)
import Simplex.Messaging.Server (runSMPServerBlocking)
import Simplex.Messaging.Server.Env.STM
import Simplex.Messaging.Transport
import Simplex.Messaging.Transport.Server (defaultTransportServerConfig)
import Simplex.Messaging.Version
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
    { coreOptions =
        CoreChatOpts
          { dbFilePrefix = undefined,
            dbKey = "",
            -- dbKey = "this is a pass-phrase to encrypt the database",
            smpServers = ["smp://LcJUMfVhwD8yxjAiSaDzzGF3-kLG4Uh0Fl_ZIjrRwjI=:server_password@localhost:7001"],
            xftpServers = ["xftp://LcJUMfVhwD8yxjAiSaDzzGF3-kLG4Uh0Fl_ZIjrRwjI=:server_password@localhost:7002"],
            networkConfig = defaultNetworkConfig,
            logLevel = CLLImportant,
            logConnections = False,
            logServerHosts = False,
            logAgent = Nothing,
            logFile = Nothing,
            tbqSize = 16,
            highlyAvailable = False
          },
      deviceName = Nothing,
      chatCmd = "",
      chatCmdDelay = 3,
      chatServerPort = Nothing,
      optFilesFolder = Nothing,
      showReactions = True,
      allowInstantFiles = True,
      autoAcceptFileSize = 0,
      muteNotifications = True,
      markRead = True,
      maintenance = False
    }

getTestOpts :: Bool -> ScrubbedBytes -> ChatOpts
getTestOpts maintenance dbKey = testOpts {maintenance, coreOptions = (coreOptions testOpts) {dbKey}}

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
    { reconnectInterval = (reconnectInterval aCfg) {initialInterval = 50000},
      xftpNotifyErrsOnRetry = False
    }

testCfg :: ChatConfig
testCfg =
  defaultChatConfig
    { agentConfig = testAgentCfg,
      showReceipts = False,
      testView = True,
      tbqSize = 16,
      xftpFileConfig = Nothing
    }

testAgentCfgVPrev :: AgentConfig
testAgentCfgVPrev =
  testAgentCfg
    { smpAgentVRange = prevRange $ smpAgentVRange testAgentCfg,
      smpClientVRange = prevRange $ smpClientVRange testAgentCfg,
      e2eEncryptVRange = prevRange $ e2eEncryptVRange testAgentCfg,
      smpCfg = (smpCfg testAgentCfg) {serverVRange = prevRange $ serverVRange $ smpCfg testAgentCfg}
    }

testAgentCfgV1 :: AgentConfig
testAgentCfgV1 =
  testAgentCfg
    { smpClientVRange = v1Range,
      smpAgentVRange = v1Range,
      e2eEncryptVRange = v1Range,
      smpCfg = (smpCfg testAgentCfg) {serverVRange = v1Range}
    }

testCfgVPrev :: ChatConfig
testCfgVPrev =
  testCfg
    { chatVRange = prevRange $ chatVRange testCfg,
      agentConfig = testAgentCfgVPrev
    }

testCfgV1 :: ChatConfig
testCfgV1 =
  testCfg
    { chatVRange = v1Range,
      agentConfig = testAgentCfgV1
    }

prevRange :: VersionRange -> VersionRange
prevRange vr = vr {maxVersion = maxVersion vr - 1}

v1Range :: VersionRange
v1Range = mkVersionRange 1 1

testCfgCreateGroupDirect :: ChatConfig
testCfgCreateGroupDirect =
  mkCfgCreateGroupDirect testCfg

mkCfgCreateGroupDirect :: ChatConfig -> ChatConfig
mkCfgCreateGroupDirect cfg = cfg {chatVRange = groupCreateDirectVRange}

groupCreateDirectVRange :: VersionRange
groupCreateDirectVRange = mkVersionRange 1 1

testCfgGroupLinkViaContact :: ChatConfig
testCfgGroupLinkViaContact =
  mkCfgGroupLinkViaContact testCfg

mkCfgGroupLinkViaContact :: ChatConfig -> ChatConfig
mkCfgGroupLinkViaContact cfg = cfg {chatVRange = groupLinkViaContactVRange}

groupLinkViaContactVRange :: VersionRange
groupLinkViaContactVRange = mkVersionRange 1 2

createTestChat :: FilePath -> ChatConfig -> ChatOpts -> String -> Profile -> IO TestCC
createTestChat tmp cfg opts@ChatOpts {coreOptions = CoreChatOpts {dbKey}} dbPrefix profile = do
  Right db@ChatDatabase {chatStore} <- createChatDatabase (tmp </> dbPrefix) dbKey False MCError
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
  chatAsync <- async . runSimplexChat opts user cc $ \_u cc' -> runChatTerminal ct cc' opts
  atomically . unless (maintenance opts) $ readTVar (agentAsync cc) >>= \a -> when (isNothing a) retry
  termQ <- newTQueueIO
  termAsync <- async $ readTerminalOutput t termQ
  pure TestCC {chatController = cc, virtualTerminal = t, chatAsync, termAsync, termQ, printOutput = False}

stopTestChat :: TestCC -> IO ()
stopTestChat TestCC {chatController = cc, chatAsync, termAsync} = do
  stopChatController cc
  uninterruptibleCancel termAsync
  uninterruptibleCancel chatAsync
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
userName (TestCC ChatController {currentUser} _ _ _ _ _) = maybe "no current user" (T.unpack . localDisplayName) <$> readTVarIO currentUser

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

serverCfg :: ServerConfig
serverCfg =
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
      messageExpiration = Just defaultMessageExpiration,
      inactiveClientExpiration = Just defaultInactiveClientExpiration,
      caCertificateFile = "tests/fixtures/tls/ca.crt",
      privateKeyFile = "tests/fixtures/tls/server.key",
      certificateFile = "tests/fixtures/tls/server.crt",
      logStatsInterval = Nothing,
      logStatsStartTime = 0,
      serverStatsLogFile = "tests/smp-server-stats.daily.log",
      serverStatsBackupFile = Nothing,
      smpServerVRange = supportedSMPServerVRange,
      transportConfig = defaultTransportServerConfig,
      smpHandshakeTimeout = 1000000,
      controlPort = Nothing
    }

withSmpServer :: IO () -> IO ()
withSmpServer = serverBracket (`runSMPServerBlocking` serverCfg)

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
      allowedChunkSizes = [kb 128, kb 256, mb 1, mb 4],
      allowNewFiles = True,
      newFileBasicAuth = Nothing,
      fileExpiration = Just defaultFileExpiration,
      inactiveClientExpiration = Just defaultInactiveClientExpiration,
      caCertificateFile = "tests/fixtures/tls/ca.crt",
      privateKeyFile = "tests/fixtures/tls/server.key",
      certificateFile = "tests/fixtures/tls/server.crt",
      logStatsInterval = Nothing,
      logStatsStartTime = 0,
      serverStatsLogFile = "tests/tmp/xftp-server-stats.daily.log",
      serverStatsBackupFile = Nothing,
      transportConfig = defaultTransportServerConfig
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

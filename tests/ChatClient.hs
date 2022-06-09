{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module ChatClient where

import Control.Concurrent (ThreadId, forkIOWithUnmask, killThread)
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Exception (bracket, bracket_)
import Control.Monad.Except
import Data.List (dropWhileEnd, find)
import Data.Maybe (fromJust)
import qualified Data.Text as T
import Network.Socket
import Simplex.Chat
import Simplex.Chat.Controller (ChatConfig (..), ChatController (..))
import Simplex.Chat.Core
import Simplex.Chat.Options
import Simplex.Chat.Store
import Simplex.Chat.Terminal
import Simplex.Chat.Terminal.Output (newChatTerminal)
import Simplex.Chat.Types (Profile, User (..))
import Simplex.Messaging.Agent.Env.SQLite
import Simplex.Messaging.Agent.RetryInterval
import Simplex.Messaging.Server (runSMPServerBlocking)
import Simplex.Messaging.Server.Env.STM
import Simplex.Messaging.Transport
import System.Directory (createDirectoryIfMissing, removePathForcibly)
import qualified System.Terminal as C
import System.Terminal.Internal (VirtualTerminal (..), VirtualTerminalSettings (..), withVirtualTerminal)
import System.Timeout (timeout)
import Test.Hspec (Expectation, shouldReturn)

testDBPrefix :: FilePath
testDBPrefix = "tests/tmp/test"

serverPort :: ServiceName
serverPort = "5001"

testOpts :: ChatOpts
testOpts =
  ChatOpts
    { dbFilePrefix = undefined,
      smpServers = ["smp://LcJUMfVhwD8yxjAiSaDzzGF3-kLG4Uh0Fl_ZIjrRwjI=@localhost:5001"],
      logConnections = False,
      logAgent = False,
      chatCmd = "",
      chatCmdDelay = 3,
      chatServerPort = Nothing,
      maintenance = False
    }

termSettings :: VirtualTerminalSettings
termSettings =
  VirtualTerminalSettings
    { virtualType = "xterm",
      virtualWindowSize = pure C.Size {height = 24, width = 1000},
      virtualEvent = retry,
      virtualInterrupt = retry
    }

data TestCC = TestCC
  { chatController :: ChatController,
    virtualTerminal :: VirtualTerminal,
    chatAsync :: Async (),
    termAsync :: Async (),
    termQ :: TQueue String
  }

aCfg :: AgentConfig
aCfg = agentConfig defaultChatConfig

cfg :: ChatConfig
cfg =
  defaultChatConfig
    { agentConfig =
        aCfg {reconnectInterval = (reconnectInterval aCfg) {initialInterval = 50000}},
      testView = True
    }

createTestChat :: ChatOpts -> String -> Profile -> IO TestCC
createTestChat opts dbPrefix profile = do
  let dbFilePrefix = testDBPrefix <> dbPrefix
  st <- createStore (dbFilePrefix <> "_chat.db") 1 False
  Right user <- runExceptT $ createUser st profile True
  startTestChat_ st opts dbFilePrefix user

startTestChat :: ChatOpts -> String -> IO TestCC
startTestChat opts dbPrefix = do
  let dbFilePrefix = testDBPrefix <> dbPrefix
  st <- createStore (dbFilePrefix <> "_chat.db") 1 False
  Just user <- find activeUser <$> getUsers st
  startTestChat_ st opts dbFilePrefix user

startTestChat_ :: SQLiteStore -> ChatOpts -> FilePath -> User -> IO TestCC
startTestChat_ st opts dbFilePrefix user = do
  t <- withVirtualTerminal termSettings pure
  ct <- newChatTerminal t
  cc <- newChatController st (Just user) cfg opts {dbFilePrefix} Nothing -- no notifications
  chatAsync <- async . runSimplexChat opts user cc . const $ runChatTerminal ct
  termQ <- newTQueueIO
  termAsync <- async $ readTerminalOutput t termQ
  pure TestCC {chatController = cc, virtualTerminal = t, chatAsync, termAsync, termQ}

stopTestChat :: TestCC -> IO ()
stopTestChat TestCC {chatController = cc, chatAsync, termAsync} = do
  stopChatController cc
  uninterruptibleCancel termAsync
  uninterruptibleCancel chatAsync

withNewTestChat :: String -> Profile -> (TestCC -> IO a) -> IO a
withNewTestChat = withNewTestChatOpts testOpts

withNewTestChatOpts :: ChatOpts -> String -> Profile -> (TestCC -> IO a) -> IO a
withNewTestChatOpts opts dbPrefix profile = bracket (createTestChat opts dbPrefix profile) (\cc -> cc <// 100000 >> stopTestChat cc)

withTestChat :: String -> (TestCC -> IO a) -> IO a
withTestChat = withTestChatOpts testOpts

withTestChatOpts :: ChatOpts -> String -> (TestCC -> IO a) -> IO a
withTestChatOpts opts dbPrefix = bracket (startTestChat opts dbPrefix) (\cc -> cc <// 100000 >> stopTestChat cc)

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
    (removePathForcibly "tests/tmp")

testChatN :: ChatOpts -> [Profile] -> ([TestCC] -> IO ()) -> IO ()
testChatN opts ps test = withTmpFiles $ do
  tcs <- getTestCCs (zip ps [1 ..]) []
  test tcs
  concurrentlyN_ $ map (<// 100000) tcs
  concurrentlyN_ $ map stopTestChat tcs
  where
    getTestCCs :: [(Profile, Int)] -> [TestCC] -> IO [TestCC]
    getTestCCs [] tcs = pure tcs
    getTestCCs ((p, db) : envs') tcs = (:) <$> createTestChat opts (show db) p <*> getTestCCs envs' tcs

(<//) :: TestCC -> Int -> Expectation
(<//) cc t = timeout t (getTermLine cc) `shouldReturn` Nothing

getTermLine :: TestCC -> IO String
getTermLine = atomically . readTQueue . termQ

-- Use code below to echo virtual terminal
-- getTermLine :: TestCC -> IO String
-- getTermLine cc = do
--   s <- atomically . readTQueue $ termQ cc
--   name <- userName cc
--   putStrLn $ name <> ": " <> s
--   pure s

userName :: TestCC -> IO [Char]
userName (TestCC ChatController {currentUser} _ _ _ _) = T.unpack . localDisplayName . fromJust <$> readTVarIO currentUser

testChat2 :: Profile -> Profile -> (TestCC -> TestCC -> IO ()) -> IO ()
testChat2 = testChatOpts2 testOpts

testChatOpts2 :: ChatOpts -> Profile -> Profile -> (TestCC -> TestCC -> IO ()) -> IO ()
testChatOpts2 opts p1 p2 test = testChatN opts [p1, p2] test_
  where
    test_ :: [TestCC] -> IO ()
    test_ [tc1, tc2] = test tc1 tc2
    test_ _ = error "expected 2 chat clients"

testChat3 :: Profile -> Profile -> Profile -> (TestCC -> TestCC -> TestCC -> IO ()) -> IO ()
testChat3 p1 p2 p3 test = testChatN testOpts [p1, p2, p3] test_
  where
    test_ :: [TestCC] -> IO ()
    test_ [tc1, tc2, tc3] = test tc1 tc2 tc3
    test_ _ = error "expected 3 chat clients"

testChat4 :: Profile -> Profile -> Profile -> Profile -> (TestCC -> TestCC -> TestCC -> TestCC -> IO ()) -> IO ()
testChat4 p1 p2 p3 p4 test = testChatN testOpts [p1, p2, p3, p4] test_
  where
    test_ :: [TestCC] -> IO ()
    test_ [tc1, tc2, tc3, tc4] = test tc1 tc2 tc3 tc4
    test_ _ = error "expected 4 chat clients"

concurrentlyN_ :: [IO a] -> IO ()
concurrentlyN_ = mapConcurrently_ id

serverCfg :: ServerConfig
serverCfg =
  ServerConfig
    { transports = [(serverPort, transport @TLS)],
      tbqSize = 1,
      serverTbqSize = 1,
      msgQueueQuota = 16,
      queueIdBytes = 12,
      msgIdBytes = 6,
      storeLogFile = Nothing,
      allowNewQueues = True,
      messageExpiration = Just defaultMessageExpiration,
      inactiveClientExpiration = Just defaultInactiveClientExpiration,
      caCertificateFile = "tests/fixtures/tls/ca.crt",
      privateKeyFile = "tests/fixtures/tls/server.key",
      certificateFile = "tests/fixtures/tls/server.crt",
      logStatsInterval = Just 86400,
      logStatsStartTime = 0,
      smpServerVRange = supportedSMPServerVRange
    }

withSmpServer :: IO a -> IO a
withSmpServer = serverBracket (`runSMPServerBlocking` serverCfg) (pure ()) . const

serverBracket :: (TMVar Bool -> IO ()) -> IO () -> (ThreadId -> IO a) -> IO a
serverBracket process afterProcess f = do
  started <- newEmptyTMVarIO
  bracket
    (forkIOWithUnmask ($ process started))
    (\t -> killThread t >> afterProcess >> waitFor started "stop")
    (\t -> waitFor started "start" >> f t)
  where
    waitFor started s =
      5000000 `timeout` atomically (takeTMVar started) >>= \case
        Nothing -> error $ "server did not " <> s
        _ -> pure ()

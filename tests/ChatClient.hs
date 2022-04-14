{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module ChatClient where

import Control.Concurrent (ThreadId, forkIOWithUnmask, killThread)
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Exception (bracket, bracket_)
import Control.Monad.Except
import Data.List (dropWhileEnd)
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
import System.Directory (createDirectoryIfMissing, removeDirectoryRecursive)
import qualified System.Terminal as C
import System.Terminal.Internal (VirtualTerminal (..), VirtualTerminalSettings (..), withVirtualTerminal)
import System.Timeout (timeout)
import Test.Hspec (Expectation, shouldReturn)

testDBPrefix :: FilePath
testDBPrefix = "tests/tmp/test"

serverPort :: ServiceName
serverPort = "5001"

opts :: ChatOpts
opts =
  ChatOpts
    { dbFilePrefix = undefined,
      smpServers = ["smp://LcJUMfVhwD8yxjAiSaDzzGF3-kLG4Uh0Fl_ZIjrRwjI=@localhost:5001"],
      logConnections = False,
      logAgent = False,
      chatCmd = "",
      chatCmdDelay = 3
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

defaultTestCfg :: ChatConfig
defaultTestCfg =
  defaultChatConfig
    { agentConfig =
        aCfg {reconnectInterval = (reconnectInterval aCfg) {initialInterval = 50000}},
      testView = True
    }

virtualSimplexChat :: Profile -> ChatConfig -> FilePath -> IO TestCC
virtualSimplexChat profile cfg dbFilePrefix = do
  st <- createStore (dbFilePrefix <> "_chat.db") 1 False
  Right user <- runExceptT $ createUser st profile True
  t <- withVirtualTerminal termSettings pure
  ct <- newChatTerminal t
  cc <- newChatController st (Just user) cfg opts {dbFilePrefix} Nothing -- no notifications
  chatAsync <- async . runSimplexChat user cc . const $ runChatTerminal ct
  termQ <- newTQueueIO
  termAsync <- async $ readTerminalOutput t termQ
  pure TestCC {chatController = cc, virtualTerminal = t, chatAsync, termAsync, termQ}

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

testChatN :: [(Profile, ChatConfig)] -> ([TestCC] -> IO ()) -> IO ()
testChatN profilesConfigs test = withTmpFiles $ do
  let (ps, cfgs) = unzip profilesConfigs
      envs = zip3 ps cfgs $ map ((testDBPrefix <>) . show) [(1 :: Int) ..]
  tcs <- getTestCCs envs []
  test tcs
  concurrentlyN_ $ map (<// 100000) tcs
  where
    getTestCCs :: [(Profile, ChatConfig, FilePath)] -> [TestCC] -> IO [TestCC]
    getTestCCs [] tcs = pure tcs
    getTestCCs ((p, cfg, db) : envs') tcs = (:) <$> virtualSimplexChat p cfg db <*> getTestCCs envs' tcs

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
testChat2 p1 p2 = testChat2' (p1, defaultTestCfg) (p2, defaultTestCfg)

testChat2' :: (Profile, ChatConfig) -> (Profile, ChatConfig) -> (TestCC -> TestCC -> IO ()) -> IO ()
testChat2' (p1, cfg1) (p2, cfg2) test = testChatN [(p1, cfg1), (p2, cfg2)] test_
  where
    test_ :: [TestCC] -> IO ()
    test_ [tc1, tc2] = test tc1 tc2
    test_ _ = error "expected 2 chat clients"

testChat3 :: Profile -> Profile -> Profile -> (TestCC -> TestCC -> TestCC -> IO ()) -> IO ()
testChat3 p1 p2 p3 = testChat3' (p1, defaultTestCfg) (p2, defaultTestCfg) (p3, defaultTestCfg)

testChat3' :: (Profile, ChatConfig) -> (Profile, ChatConfig) -> (Profile, ChatConfig) -> (TestCC -> TestCC -> TestCC -> IO ()) -> IO ()
testChat3' (p1, cfg1) (p2, cfg2) (p3, cfg3) test = testChatN [(p1, cfg1), (p2, cfg2), (p3, cfg3)] test_
  where
    test_ :: [TestCC] -> IO ()
    test_ [tc1, tc2, tc3] = test tc1 tc2 tc3
    test_ _ = error "expected 3 chat clients"

testChat4 :: Profile -> Profile -> Profile -> Profile -> (TestCC -> TestCC -> TestCC -> TestCC -> IO ()) -> IO ()
testChat4 p1 p2 p3 p4 test = testChatN [(p1, defaultTestCfg), (p2, defaultTestCfg), (p3, defaultTestCfg), (p4, defaultTestCfg)] test_
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
      msgQueueQuota = 4,
      queueIdBytes = 12,
      msgIdBytes = 6,
      storeLog = Nothing,
      caCertificateFile = "tests/fixtures/tls/ca.crt",
      privateKeyFile = "tests/fixtures/tls/server.key",
      certificateFile = "tests/fixtures/tls/server.crt"
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
